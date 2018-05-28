#GENERATING SPATIAL INFORMATION FOR ANALYSIS

##NOTE: SOME RESOURCES THAT ARE REFERENCED IN THIS DOCUMENT
##ARE NOT PROVIDED AS THEY REFER TO SPECIFIC BARRIOS SAMPLED
##IN THIS ANALYSIS, THEY ARE CENSORED TO PROTECT RESPONDENT PRIVACY
##THIS SCRIPT IS PROVIDED TO TRANSPARENTLY SHOW THE STEPS TAKEN TO MOVE
##FROM THE RAW DATA PROVIDED TO AN INTERMEDIATE CENSORED DATASET

#load functions, raw_data, and packages

###########
#FUNCTIONS#
###########
source("../ref/base.R")
source('../ref/spatial_functions.R')

##########
#PACKAGES#
##########
lib_eval("tidyverse")
lib_eval("tidyselect")
lib_eval("gridExtra")
lib_eval("magrittr")
lib_eval("rgdal")
lib_eval("proj4")
lib_eval("sp")
lib_eval("raster")
lib_eval("geosphere")
lib_eval('readxl')
lib_eval('ggsci')
lib_eval('grid')
lib_eval('scales')


## This script has 4 main parts
## 1 - read in official count data
## 2- reading sheets from the survey excel file and creating three tables
## 3- cleaning the spatial data
## 4- assigning a barrio and distance strata to each household

## Tables created at the end are
#households: table of household information
#households_spatial: spatial data frame version fo households
#individuals: table of individuals linked to household by hh_id
#deaths: table of deaths linked to household by hh_id
#barrio_frame: spatial polygon data frame of all barrios in sample frame
#sel: spatial polygon data frame of all selected barrios
#population_2010: table of census population estimates from 2010

##################################
#IMPORT AND WRANGLE OFFICAL COUNT#
##################################

official <- readxl::read_excel("../data/defunciones_2010_17.xlsx", skip = 3) %>%
  slice(1:8) %>%
  setNames(c("year", 1:12, "total")) %>%
  mutate(year = parse_number(year)) %>%
  select(-total) %>%
  gather(month, deaths, -year, convert = TRUE) %>% 
  mutate(deaths = parse_number(deaths)) %>% 
  arrange(year, month)

pop10 <- read.csv(file="../data/mun_pop.csv", fileEncoding = "latin1")

##CLEANING UP ENCODING ISSUES FROM THE OFFICIAL POP DATA
names(pop10)[1] <- "barrio"
pop10$barrio <- as.character(pop10$barrio) 
Encoding(pop10$barrio) <- "UTF-8"
pop10$municipio <- as.character(pop10$municipio) 
Encoding(pop10$municipio) <- "UTF-8"

sel$barrio <- as.character(sel$barrio) 
Encoding(sel$barrio) <- "UTF-8"
sel$municipio <- as.character(sel$municipio)
Encoding(sel$municipio) <- "UTF-8"

barrio_frame$barrio <- as.character(barrio_frame$barrio)
Encoding(barrio_frame$barrio) <- "UTF-8"
barrio_frame$municipio <- as.character(barrio_frame$municipio)
Encoding(barrio_frame$municipio) <- "UTF-8"

pop10$barrio <- trimws(pop10$barrio)
pop10$municipio <- trimws(pop10$municipio)
pop10[which(pop10$barrio == pop10$municipio),"barrio"] <- "Barrio Pueblo"
pop10[440,1] <- "Lajas"
pop10[454,1] <- "Lares"
pop10[372,1] <- "Hormigueros"
pop10[176,1] <- "Canóvanas"
pop10[602,1] <- "Orocovis"
pop10[367,1] <- "Hatillo"

##########
#RAW DATA#
##########
#load - data from CommCare in Excel 
file <- "../data/raw.xlsx"
households <- readxl::read_excel(file, sheet = "household")
deaths <- readxl::read_excel(file, sheet = "deaths")
individuals <- readxl::read_excel(file, sheet = "individuals")

###################
#CLEANING RAW DATA#
###################

## basic data cleaning and classifying from raw data
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

###############
#SPATIAL STEPS#
###############

#load mapping data
#change dsn and make sure it includes the selected and bario_td shape files

##NOTE THIS DATA IS NOT INCLUDED TO PROTECT RESPONDENT PRIVACY

dsn <- "../../resources"
barrio_frame <- readOGR(dsn = dsn, layer = "final_barrio_frame")
sel <- readOGR(dsn = dsn, layer = "selected")


#adding pop10 data to both selected and barrio spatial data adjusting
#for changes in population over 7 years
sel <- (merge(sel, pop10[row.names(unique(pop10[,c(1,2)])),], by = c("barrio", "municipio")))
barrio_frame <- merge(barrio_frame, pop10[row.names(unique(pop10[,c(1,2)])),], by = c("barrio", "municipio"))

sel$pop17 <- round(sel$pop * .86,0) #assuming 14% loss of pop10ulation from 2010
sel$hh <- round(sel$pop17 / 3, 0) #assuming average household size of 3
barrio_frame$pop17 <- round(barrio_frame$pop * .86,0)
barrio_frame$hh <- round(barrio_frame$pop17 / 3, 0)

#drop barrios that didn't have people in them
sel <- sel %>% subset(!(barrio == "Machos" | barrio == "Mosquito"))

#expanding lat / long data from the space separated string given in data
households[which(nchar(households$location) > 0 ),"long"] <- 
  households[which(nchar(households$location) > 0 ),"location"] %>% 
  separate("location", c("lat", "long", "alt", "acc"), sep=" ") %>% 
  {.["long"]} %>% sapply(as.numeric)
households[which(nchar(households$location) > 0 ),"lat"] <- 
  households[which(nchar(households$location) > 0 ),"location"] %>% 
  separate("location", c("lat", "long", "alt", "acc"), sep=" ") %>% 
  {.["lat"]} %>% sapply(as.numeric)

############
#LOADED DATA
#barrio_frame: spatial polygon data frame of all barrios in sample frame
#sel: spatial polygon data frame of all selected barrios
#pop10: table of census population estimates from 2010
#households: table of household information
#individuals: table of individuals linked to household by hh_id
#deaths: table of deaths linked to household by hh_id

#impute household gps which were not collectd on one day
#when two individuals forgot to turn their GPS on, we randomly
#selected from a list of individuals who were working that day
#all in the same location
households <- impute_hh_gps(households)

#match household gps to closest barrio, the general "over" function
#available in sp only does direct intersection. This function also
#calculates the closest barrio in cases when points fall just outside
#the border of a barrio
households <- over_near(households, sel)

#generate weights for all barrios in the dataframe
weights <- create_weights(barrio_frame, households)

write.csv(weights, "../data/weights.csv", row.names = F)

#variables in 'weights'
##id: id number of barrio
##samp_h: households captured in each barrio
##strata: matched strata of each barrio
#pop17: estimated pop in barrio as defined in init.R
#strata7: estimated pop in strata s defined in init.R
##strata_w: inverse fraction of total population in each strata
##barrio_w: inverse fraction of population in barrio as proportion of population in strata
##hh_w: inverse fraction of households in sample out of total estimated households

households <- merge(households@data, weights[c("id", "strata")], 
                 by = "id", all.x = TRUE)

##NOTE: THE STRATA AND BARRIO NUMBERS GENERATED BY THIS ANALYSIS
##ARE MERGED BACK INTO THE RAW DATASET SO THAT ANOTHER REPRODUCIBLE
##STEP CAN SHOW THE CLEANING PROCESS FOR TRANSPARENCY

#MERGING IS PERFORMED BY HH_ID WHICH IS UNIQUE TO EACH FORM

