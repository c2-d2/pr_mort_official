##DATA CLEANING
source("../ref/base.R")
lib_eval("readxl")
lib_eval("tidyverse")
lib_eval("lubridate")

##here we take the raw, but censored, data and convert it into our 
#final analytic dataset. 

#loading raw but censored data
## import
file <- "../data/raw-censored.xlsx"
households <- readxl::read_excel(file, sheet = "household", na = "---") %>%
  mutate(hh_id = parse_number(hh_id))
deaths <- readxl::read_excel(file, sheet = "deaths", na = "---")
individuals <- readxl::read_excel(file, sheet = "individuals", na = "---")

#basic data cleaning and classifying from raw data
households$completed_time <- as.POSIXlt(households$completed_time,format="%Y-%m-%d %H:%M:%S") - 3600*4
deaths <- (merge(households[,c("hh_id", "username", "completed_time")], deaths[,-1], by.x = "hh_id", by.y = "hh_id__0")) %>% subset(!(tolower(username) == "test")) %>% subset(!(died_month == "---"))
individuals <- (merge(households[,c("hh_id", "username", "completed_time")], individuals[,-1], by.x = "hh_id", by.y = "hh_id__0")) %>% subset(!(tolower(username) == "test"))
households <- subset(households, !(tolower(username) == "test"))
households$hh_size <- as.numeric(households$hh_size)
individuals$age <- as.numeric(individuals$age)
deaths$mo <- as.numeric(as.character(deaths$died_month))
deaths[which(as.numeric(as.character(deaths$died_b_p_hurricane)) == 1),"mo"] <- 9.1
deaths[which(as.numeric(as.character(deaths$died_b_p_hurricane)) == 2),"mo"] <- 9.2
deaths <- merge(deaths, individuals[,c("hh_id","hh_id__1", "age")], by = c("hh_id","hh_id__1") )
households <- households %>% subset(hh_size > 0)
individuals <- individuals %>% subset(!is.na(age)) 

#PRIMARY DATASETS 
#households, individuals and deaths generated in init.R

hh_main <- households %>% 
  subset(consent == "1")
hh_main <- hh_main %>% dplyr::select(-username,-notas,-formid,-completed_time,
                                     -started_time, -received_on,-location)
#saveRDS(hh_main, "../data/rdata/hh_main.RDS")

individuals <- individuals %>% dplyr::select(-username,-completed_time) 
#saveRDS(individuals, "../data/rdta/individuals.RDS")

deaths <- deaths %>% dplyr::select(-username,-completed_time)
#saveRDS(deaths, "../data/rdata/deaths.RDS")

write.csv(hh_main, "../data/cleaned_data/households.csv", row.names = F)
write.csv(individuals, "../data/cleaned_data/individuals.csv", row.names = F)
write.csv(deaths, "../data/cleaned_data/deaths.csv", row.names = F)
