## Figure 3

source("../ref/base.R")
## Imports ----
lib_eval("tidyverse")
lib_eval("ggsci")
lib_eval("cowplot")

## Data ----
load('../data/rdata/ACS2016.Rdata')
individuals <- readRDS("../data/rdata/ind_hh.RDS")
left_df1 <- readRDS("../data/rdata/left_df.RDS")
where_df <- readRDS("../data/rdata/where_df.RDS")
hh_main <- readRDS("../data/rdata/hh_main.RDS")


## Merge individuals with HH strata
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
  transmute(Water = water.sept + water.oct + water.nov + water.dec,
            Electricity = electricity.sept + electricity.oct +
            electricity.nov + electricity.dec, 
            Cellular = cell.sept + cell.oct + cell.nov + cell.dec) 


keycol <- "resource"
valuecol <- "values"
gathercols <- c("Water", "Cellular", "Electricity")

res_df_long <- gather_(resources_df, keycol, valuecol, gathercols)
res_df_long$resource <- factor(res_df_long$resource, levels = c("Water", "Cellular", "Electricity"))


fig3a <- ggplot(res_df_long, aes(x=factor(strata), y=values)) + 
  geom_boxplot() +
  #geom_jitter(width = 0.2, alpha = 0.1) +
  facet_grid(~resource) +
  theme_classic() + 
  scale_x_discrete("Remoteness", expand = c(0, 0)) + 
  scale_y_continuous("Number of days without access") + 
  theme(panel.spacing.x = unit(1, "cm"))


## 3b
households <- readRDS("../data/rdata/hh_main.RDS")
individuals <- readRDS("../data/rdata/individuals.RDS")
deaths <- readRDS("../data/rdata/deaths.RDS")
left <- readRDS("../data/rdata/left_df.RDS")

#analysis of medical access: at least one day without care
cats <- c("Unable to get medicine", "Unable to use respiration equipment", "Roads damaged", "Facility closed",
          "Doctors unavailable", "Unable to afford care", "Transport issues", "No 911 service", "Unable to have dialysis")


figure_access <- households %>%
  rowwise() %>%
  mutate_at(vars(access_med.no_911, access_med.no_transport, access_med.roads_damaged, 
                 access_med.facility_closed, access_med.no_doctors, access_med.no_dialysis,
                 access_med.no_resp_mach, access_med.no_meds, access_med.couldnt_afford
  ), 
  funs(case_when(
    as.numeric(.) == 99 ~ 0,
    as.numeric(.) == 0 ~ 0,
    as.numeric(.) == 1 ~ 1,
    as.numeric(.) == 2 ~ 1,
    as.numeric(.) == 3 ~ 1,
    as.numeric(.) == 4 ~ 1,
    as.numeric(.) == 5 ~ 1,
    TRUE ~ 0))) %>%
  ungroup() %>%
  dplyr::select(access_med.no_911, access_med.no_transport, access_med.roads_damaged, 
                access_med.facility_closed, access_med.no_doctors, access_med.no_dialysis,
                access_med.no_resp_mach, access_med.no_meds, access_med.couldnt_afford)




figure_df <- figure_access %>% colMeans() %>% as.data.frame()
names(figure_df) <- c("Freq")
figure_df$Var1 <- rownames(figure_df)  
figure_df <- figure_df[order(-figure_df$Freq),]



fig3b <- ggplot(figure_df, aes(reorder(Var1, Freq), Freq)) + 
  geom_bar(stat="identity") +
  theme_classic()  +
  scale_x_discrete("",labels = cats[order(figure_df$Freq)]) +
  scale_y_continuous("Proportion of sample", expand = c(0, 0.01)) +
  coord_flip()

fig3 <- plot_grid(fig3a, fig3b, labels = c("A", "B"), nrow = 2)

ggsave('../figures/figure3.pdf', fig3, 
       scale = 1.25, width = 5.5, height = 6)
