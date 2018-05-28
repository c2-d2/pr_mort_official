#Table S3
source("../ref/base.R")

lib_eval("tidyverse")

households <- readRDS("../data/rdata/hh_main.RDS")
individuals <- readRDS("../data/rdata/individuals.RDS")
deaths <- readRDS("../data/rdata/deaths.RDS")
left <- readRDS("../data/rdata/left_df.RDS")
utilities <- readRDS("../data/rdata/resources_df.RDS")

#analysis of medical access: at least one day without care

access <- names(households) %>% {starts_with(match = "access_med", vars = .)} %>%
{households[,.]} %>% names

table1_access <- households %>%
  rowwise() %>%
  mutate_at(vars(access_med.no_911, access_med.no_transport, access_med.roads_damaged, 
                 access_med.facility_closed, access_med.no_doctors, access_med.no_dialysis,
                 access_med.no_resp_mach, access_med.no_meds, access_med.couldnt_afford,
                 access_med.ot_reasons), 
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
                access_med.no_resp_mach, access_med.no_meds, access_med.couldnt_afford,
                access_med.ot_reasons, strata)

table1_df <- aggregate(. ~ strata, data = table1_access, FUN =mean) %>% t %>% as.data.frame()
table1_se <- aggregate(. ~ strata, data = table1_access, FUN = function(x) sd(x)/sqrt(length(x)))

table_s1 <- aggregate(. ~ strata, data = table1_access, 
                       FUN = function(x) paste0(signif(mean(x),2), 
                                                " (",
                                                signif(mean(x) - 1.96*(sd(x)/sqrt(length(x))),2),
                                                ", ",
                                                signif(mean(x) + 1.96*(sd(x)/sqrt(length(x))),2),
                                                ")"
                       ))

#for utilities
utilities %>%
  mutate(est_ci = paste0(
    signif(values,2),
    " (",
    signif(values - 1.96*(sd/sqrt(length)),2),
    ", ",
    signif(values + 1.96*(sd/sqrt(length)),2),
    ")"
  )) -> utilities
