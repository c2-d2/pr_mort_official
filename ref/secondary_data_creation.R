#final script used to generate datasets found in ../data


##SECONDARY DATASETS

## Summarize ages of individuals in our survey
age_summary <- individuals %>% 
  dplyr::select(age) %>% 
  mutate(age_cat = base::cut(age, 
                             c(seq(0, 80, 10), Inf), 
                             include.lowest = TRUE, 
                             right = FALSE, 
                             ordered = TRUE)) %>% 
  group_by(age_cat) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count / sum(count), 
         src = "survey")

## Munge it to match with ACS
acs_summary <- acs.age %>% 
  mutate(age_cat = base::cut(lower, 
                             c(seq(0, 80, 10), Inf), 
                             include.lowest = TRUE, 
                             right = FALSE, 
                             ordered = TRUE)) %>% 
  dplyr::select(age_cat, count) %>% 
  aggregate(count~age_cat,data=.,FUN=sum) %>%
  mutate(prop = count / sum(count), 
         src = "acs")

age_df <- dplyr::bind_rows(age_summary, acs_summary) %>% 
  mutate(src_cat = factor(src, 
                          levels = c("acs", "survey"), 
                          labels = c("ACS (2016)", "Survey"), 
                          ordered = TRUE))
levels(age_df$age_cat)[length(levels(age_df$age_cat))] <- "80+"
saveRDS(age_df, "../data/cleaned_age_counts_acs_vs_survey.RDS")

## Make a household size dataframe
hh_df <- bind_rows(
  households@data %>% 
    filter(hh_size > 0) %>% 
    mutate(hh_size = 
             case_when(
               hh_size > 7 ~ 7, 
               TRUE ~ hh_size)) %>% 
    group_by(hh_size) %>% 
    summarize(count = n()) %>% 
    mutate(prop = count / sum(count), 
           src = "survey"), 
  acs.hh_size %>% 
    mutate(prop = count / sum(count), 
           src = "acs")) %>% 
  mutate(src_cat = factor(src, 
                          levels = c("acs", "survey"), 
                          labels = c("ACS (2016)", "Survey"), 
                          ordered = TRUE), 
         hh_cat = factor(hh_size, 
                         levels = 1:7, 
                         labels = c(1:6, "7+")))
saveRDS(hh_df, "../data/cleaned_hh_size_acs_vs_survey.RDS")

## Merge individuals with HH strata
ind_hh <- individuals %>% 
  left_join(
    hh_main %>% 
      dplyr::select(hh_id, strata)
  ) %>% 
  mutate(
    left_final_cat = factor(left_final, 
                            levels = c(1:6, 0, "---"), 
                            labels = c("Somewhere\nelse in PR",
                                       "Florida", 
                                       "New York", 
                                       "Texas", 
                                       "Another\nState", 
                                       "Another\nCountry", 
                                       "Don't Know", 
                                       "NA"), 
                            ordered = TRUE))
saveRDS(ind_hh, "../data/ind_hh.RDS")

## Compare the age distribution of people who left vs people who stayed
left_df <- individuals %>% 
  mutate(left = ifelse(grepl("---", left_month, fixed = TRUE), 0, 1),
         age_cat = base::cut(age, 
                             c(seq(0, 85, 10), Inf), 
                             include.lowest = TRUE, 
                             right = FALSE, 
                             ordered = TRUE)) %>% 
  group_by(left, age_cat) %>% 
  summarize(count = n()) %>% 
  group_by(left) %>% 
  mutate(prop = count / sum(count), 
         left_cat = factor(left, 
                           levels = 0:1, 
                           labels = c("Still in household/Died in 2017", 
                                      "Left in 2017 and did not return/Missing"), 
                           ordered = TRUE))
levels(left_df$age_cat)[length(levels(left_df$age_cat))] <- "80+"

saveRDS(left_df, "../data/left_df.RDS")

## Where did they go?
where_df <- ind_hh %>% 
  filter(!is.na(strata)) %>% 
  group_by(strata, left_final, left_final_cat) %>% 
  summarize(count = n()) %>% 
  group_by(strata) %>% 
  mutate(prop = count / sum(count)) %>% 
  filter(left_final_cat != "NA")
where_df$rev_left_cat <- factor(where_df$left_final_cat, levels = rev(levels(where_df$left_final_cat)))

saveRDS(where_df, "../data/where_df.RDS")

#Impact of remoteness on water / power
#utilities plot
fig3.a <- hh_main %>%
  rowwise() %>%
  mutate_at(vars(electricity.sept, electricity.oct, electricity.nov, electricity.dec,
                 water.sept, water.oct, water.nov, water.dec,
                 cell.sept, cell.oct, cell.nov, cell.dec), 
            funs(case_when(
              is.na(as.numeric(.)) ~ 0.0,
              as.numeric(.) == 0 ~ 0.0,
              as.numeric(.) == 1 ~ 4,
              as.numeric(.) == 2 ~ 7.5,
              as.numeric(.) == 3 ~ 22.5,
              as.numeric(.) == 4 ~ 30.0,
              TRUE ~ 0.0))) %>%
  ungroup() %>%
  mutate(util_3 = electricity.sept+electricity.oct+electricity.nov+electricity.dec) %>%
  mutate(util_1 = water.sept+water.oct+water.nov+water.dec) %>%
  mutate(util_2 = cell.sept+cell.oct+cell.nov+cell.dec) %>%
  dplyr::select(strata,util_1,util_2,util_3) %>%
  gather(key,value,-strata) %>%
  mutate(key = factor(key,
                      levels=c("util_1", "util_2", "util_3"),
                      labels=c("Water", "Cellular Service", "Electricity")))

saveRDS(fig3.a, "../data/figure3a.RDS")

#Data frame for resrouces figure
resources_df <- hh_main %>% 
  group_by(strata) %>% 
  dplyr::select(strata, contains(".sept"), contains(".oct"), 
                contains(".nov"), contains(".dec")) %>% 
  mutate_all(.funs = function(x) {
    case_when(x == 1 ~ 1, 
              x == 2 ~ 8, 
              x == 3 ~ 15, 
              x == 4 ~ 30, 
              TRUE ~ 0)
  }
  ) %>% 
  transmute(electricity_tot = electricity.sept + electricity.oct +
              electricity.nov + electricity.dec, 
            water_tot = water.sept + water.oct + water.nov + water.dec, 
            cell_tot = cell.sept + cell.oct + cell.nov + cell.dec) %>% 
  summarize_all(funs(mean, sd, length)) %>%
  gather(resource, values, -strata)

resources_df[grep("mean", resources_df$resource),"sd"] <- resources_df[grep("sd", resources_df$resource),"values"]
resources_df[grep("mean", resources_df$resource),"length"] <- resources_df[grep("length", resources_df$resource),"values"]
resources_df <- resources_df[grep("mean", resources_df$resource),]

resources_df %>% 
  mutate(resource_cat = factor(resource, 
                               levels = c("water_tot_mean", "cell_tot_mean", 
                                          "electricity_tot_mean"), 
                               labels = c("Water", "Cellular Coverage", 
                                          "Electricity"), 
                               ordered = TRUE)) -> resources_df

saveRDS(resources_df, "../data/resources_df.RDS")

#data for s6
hh_main %>% 
  group_by(strata) %>% 
  dplyr::select(strata, contains(".sept"), contains(".oct"), 
                contains(".nov"), contains(".dec")) %>% 
  mutate_all(.funs = function(x) {
    case_when(x == 1 ~ 0, 
              x == 2 ~ 0, 
              x == 3 ~ 0, 
              x == 4 ~ 1, 
              TRUE ~ 0)
  }
  ) %>% 
  dplyr::select(electricity.sept, electricity.oct, electricity.nov, electricity.dec,
                water.sept, water.oct, water.nov, water.dec,
                cell.sept, cell.oct, cell.nov, cell.dec) %>%
  aggregate(.~strata, data=., FUN=mean) -> resources_perc

saveRDS(resources_perc, "../data/resources_perc.RDS")


##Flier Figures
deaths_official <- read.csv("../resources/deaths_official.csv", stringsAsFactors = F, header = T)
saveRDS(deaths_official, "../data/deaths_official.RDS")

#data for final graph 
cbind(
  "type" = c("surv", "govt", "alex", "nyt"), 
  "days" = c(102, 102, 41, 41),
  "est" = c(4603, 64, 1085, 1052), 
  "err" = c(3821, 0, 0, 0)
) %>% as.data.frame() -> est_data

saveRDS(est_data, "../data/est_data.RDS")
