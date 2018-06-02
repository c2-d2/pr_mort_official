source("../ref/base.R")
lib_eval("magrittr")
lib_eval("dplyr")
lib_eval("survey")

#loading resources
hh_main <- readRDS("../data/rdata/hh_main.RDS")
deaths <- readRDS("../data/rdata/deaths.RDS")
deaths_official <- readRDS("../data/rdata/deaths_official.RDS")
weighted_pop_est <- readRDS("../data/rdata/pop_est.RDS")
adj_rates <- readRDS("../data/rdata/adj_rates.RDS")

#raw data
deaths_after_hurricane <- 
  deaths %>% 
  subset(as.numeric(died_month) >= 10 | 
           (died_month == 9 & died_b_p_hurricane == 2)) %>%
  nrow

deaths_before_hurricane <-
  deaths %>% 
  subset(as.numeric(died_month) <= 8 | 
           (died_month == 9 & died_b_p_hurricane == 1)) %>%
  nrow


total_pop <- sum(hh_main$hh_size)

adj_pop <- (total_pop-deaths_before_hurricane) * (102/365)

rate <- deaths_after_hurricane / adj_pop

se <- sqrt(deaths_after_hurricane)/adj_pop

ll <- rate - 1.96*se

ul <- rate + 1.96*se

#to cacluate excess deaths from rate 
pop_in_2016 <- 3406520
adj_pop_2016 <- pop_in_2016*(102/365)
deaths_in_2016 <- deaths_official %>% 
  subset(Year == 2016) %>%
  dplyr::select(Sep,Oct,Nov,Dec) %>%
  mutate(Sep = Sep*(1/3)) %>% sum
deaths_after_hurricane_2016 <- deaths_in_2016 

rate_2016 <- deaths_after_hurricane_2016/adj_pop_2016

#rate difference
diff <- rate - rate_2016
diff_ll <- ll - rate_2016
diff_ul <- ul - rate_2016

#excess deaths calculation
weighted_pop_est*(102/365)*diff
weighted_pop_est*(102/365)*diff_ll
weighted_pop_est*(102/365)*diff_ul


#adjusted calculations
#generated in adjust-for-missing-households.RMD
adj_rate <- round(adj_rates$rate_after,1) / 1000
adj_ll <- round(adj_rates$lower_after,1) / 1000
adj_ul <- round(adj_rates$upper_after,1) / 1000

#excess deaths calculation
weighted_pop_est*(102/365)*(adj_rate-rate_2016)
weighted_pop_est*(102/365)*(adj_ll-rate_2016)
weighted_pop_est*(102/365)*(adj_ul-rate_2016)




