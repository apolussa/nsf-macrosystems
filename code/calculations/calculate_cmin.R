

# This code produces 5 data products
#   1. carbon mineralization CSVs in CO2-C hr-1 for each time point (includind standard values and extra data)
#   2. cumulative carbon mineralization CSV in CO2-C hr-1 g-1 litter across all time points (disaggregated; value for each microcosm lab.id)
#   3. cumulative carbon mineralization CSV in CO2-C hr-1 g-1 litter across all time points (aggregated; averaged across any replicates of each microcosm.id)
#   4. carbon mineralization CSV for soil only standards for each time point
#   5. cumulative carbon mineralization CSV for soil only standards across all time points. 


# Import Libraries

library(tidyverse)
library(DescTools)


# Set working directory to Dropbox folder 

setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024")

# Function "cmin_calc_fun" calculates CO2 production hr-1

source("code/calculations/fun_cmin_calc.R")


# This function calculates the CO2 production per hour from raw integral values from an infrrared gas analyser (IRGA)
# The input format is standardized and found in the folder raw-data/lab-experiment/experiment-1
# It needs to include microcosm flushing time and date, sampling time and date, irga values, and co2 standard values
# 
# @param cmin Data frame including flush time, date, irga sampling time & date, integral values, and co2 standard values
# @param date Character string for date in the format "day-XX_YYYYMMDD" (e.g. "day-02_20200227")
# @return cmin_calc a dataframe that includes lab.id, unique.id, soil.volume, CO2CperHour
# @export .csv A detailed calculated dataframe with the title irga_calc_+"date"+.csv" 

# Import metadata and mass data to normalize soil standards and soil + litter microcosms with substrate masses

microcosmDesign <- read_csv("metadata/lab-experiment/exp-1_design.csv")
mass_initial_raw <- read_csv("calculated-data/lab-experiment/experiment-1/actualMass_exp-1_Spring-2020_initial.csv")
mass_20200312_raw <- read_csv("calculated-data/lab-experiment/experiment-1/actualMass_exp-1_Spring-2020_20200312.csv") # day 16


##### calculate carbon flux ##### 


# Create list of raw IRGA file names 

IRGA.files <- list.files(path = "raw-data/lab-experiment/experiment-1/", pattern = "IRGA_exp-1*")


# Import raw IRGA CSVs into list 

time.list <- lapply(paste("raw-data/lab-experiment/experiment-1/", IRGA.files, sep = ""), read_csv)


# Name list from IRGA filenames; format: day-XX_YYYYMMDD

names(time.list) <- substr(IRGA.files, start = 12, stop = 27)

# View(time.list)


# Calculate time-resolved carbon mineralization, saves files in "calulcated-data/lab-experiment/experiement-1/mid_calcs"

time.list <- mapply(cmin_calc_fun, time.list, names(time.list), SIMPLIFY = F)


##### normalize flux by mass #####

# append mass and metadata

mass_initial <- left_join(microcosmDesign, mass_initial_raw, by = c("lab.id", "moist.trt")) %>% select(-c("X1.x", "X1.y", "tube.mass", "targetFreshMass", "targetTotalMass"))
mass_20200312 <- left_join(microcosmDesign, mass_20200312_raw, by = "lab.id") %>% select(-c("X1.x", "X1.y", "tube.mass", "targetFreshMass", "targetTotalMass"))

# Create continuous dataframe with day and date as variables

cmin <- mapply(cbind, time.list, "date" = names(time.list), SIMPLIFY = F) %>%
  bind_rows(.) %>% 
  separate(col = date, into = c("day", "date"), sep = "_") %>% 
  mutate (day = substr(day, start = 5, stop = 7)
  ) %>% transform(., day = as.numeric(day), 
                  date = as.Date(date, format = "%Y%m%d"))

# Add mass data 
# before day 16, there is intial masses, after day 16, there was a change in some of the masses

cflux <- bind_rows(filter(cmin, day < 16)%>%
            left_join(., mass_initial,  by = c("lab.id", "unique.id")),
  filter(cmin, day > 16)%>%
    left_join(., mass_20200312,  by = c("lab.id", "unique.id"))
)

# # for naming
# days <- factor(cflux$day)
# dates <- factor(cflux$date)

# calculate co2 flux for standards
#   units of ug CO2C hr-1 g-1 dry soil

cflux %>% group_by(day) %>%
  filter(., microcosm.type == "b") %>% # calc standards
  mutate(.,
         standardCO2CperHourpergSoil = CO2CperHour / actualSoilDryMass # ug CO2C hr-1 g-1 dry soil
  ) %>% 
  select(unique.id, microcosm.type, lab.id, 
         moist.trt, actualSoilDryMass, standardCO2CperHourpergSoil) -> standards

# data product (4) soil standards
write_csv(standards, "calculated-data/lab-experiment/experiment-1/cmin_soil_calc_exp-1.csv") 


# correct co2 flux for amount of soil in experimental microcosms
#     then standardize by mass of litter


cflux %>% group_by(day) %>%
  filter(., microcosm.type == "a") %>%
  left_join(., standards, by = c("unique.id", "moist.trt", "day")) %>%
  mutate(
    totalCO2CfromSoil = standardCO2CperHourpergSoil*actualSoilDryMass.x, # ugCO2 gSoil-1 h-1
    CO2CfromLitterCorrectedforSoil = CO2CperHour - totalCO2CfromSoil, # ugCO2 h-1
    CO2CpergLitter = CO2CfromLitterCorrectedforSoil / actualLitFreshMass # ugCO2 gLitter-1 h-1
    
  ) %>% rename(microcosm.type = microcosm.type.x, lab.id = lab.id.x) %>%
  select(unique.id, lab.id, microcosm.id, replicate, day, date,  moist.trt, 
         CO2CpergLitter, standardCO2CperHourpergSoil) -> cmin_corrected_calc


# Write CSV with time resolved CO2-C flux (CO2-C hr-1 g-1 litter) for each microcosm (lab.id)
write_csv(cmin_corrected_calc, "calculated-data/lab-experiment/experiment-1/cmin_calc_exp-1.csv") 


# Aggregate replicates and export an aggregated CSV for each unique microcosm (microcosm.id)
# Do not use the aggregate function because it removes NAs which important to include

cmin_corrected_calc %>% 
  group_by(day, date, moist.trt, unique.id, microcosm.id) %>%
  summarise(CO2CpergLitter = mean(CO2CpergLitter),
            standardCO2CperHourpergSoil = mean(standardCO2CperHourpergSoil)) -> cmin_aggregate_calc


write.csv(cmin_aggregate_calc, "calculated-data/lab-experiment/experiment-1/cmin_calc_aggregate_exp-1.csv")



##### calculate cumulative carbon mineralization ####

cmin_cum <- ungroup(cmin_corrected_calc)

# data with t0, with respiration beginning at 0 (t0 get data for 0 -> day 1)
bind_rows(
  cmin_cum %>% 
    filter(day == 1) %>%
    mutate(
      day=replace(day, day == 1, 0),
      date= replace(date, day == 0, "2020-02-25"),
      standardCO2CperHourpergSoil=replace(standardCO2CperHourpergSoil, day == 0, 0),
      CO2CpergLitter=replace(CO2CpergLitter, day == 0, 0)
    ),
  cmin_cum) %>% drop_na()-> cumulativeData # remove points with NA values

summary(cumulativeData)

# Calculate integral for each microcosm @ specific moisture treatment

cumulativeData %>%
  group_by(unique.id, moist.trt, replicate) %>%
  dplyr::summarize(cumulativeCO2Flux = DescTools::AUC(day*24, CO2CpergLitter),
            soil_cumulativeCO2C = DescTools::AUC(day*24, standardCO2CperHourpergSoil)) %>%
  mutate(cumulativeCO2Flux = cumulativeCO2Flux/1000,
         soil_cumulativeCO2C = soil_cumulativeCO2C/1000) -> cumulativeDataCalc # ug to mg CO2-C

write_csv(cumulativeDataCalc, "calculated-data/lab-experiment/experiment-1/cumulative_cmin_calc_exp-1.csv")


# aggregate c-min data 


# Read in cmin data
aggregate_cmin_cum <- ungroup(cmin_aggregate_calc)

# data with t0, with respiration beginning at 0 (t0 get data for 0 -> day 2)
bind_rows(
  aggregate_cmin_cum %>% 
    filter(day == 2) %>%
    mutate(
      day=replace(day, day == 2, 0),
      date= replace(date, day == 0, "2020-02-25"),
      CO2CpergLitter=replace(CO2CpergLitter, day == 0, 0),
      standardCO2CperHourpergSoil=replace(standardCO2CperHourpergSoil, day == 0, 0)
    ),
  cmin_cum) %>% drop_na() -> aggregate_cumulativeData


# Calculate integral for each microcosm @ specific moisture treatment

aggregate_cumulativeData %>%
  group_by(unique.id, moist.trt) %>%
  dplyr::summarize(cumulativeCO2Flux = DescTools::AUC(day*24, CO2CpergLitter),
            soil_cumulativeCO2C = DescTools::AUC(day*24, standardCO2CperHourpergSoil)) %>%
  mutate(cumulativeCO2Flux = cumulativeCO2Flux/1000,
         soil_cumulativeCO2C = soil_cumulativeCO2C/1000) -> aggregate_cumulativeDataCalc # ug to mg CO2-C

write_csv(aggregate_cumulativeDataCalc, "calculated-data/lab-experiment/experiment-1/cumulative_aggregate_cmin_calc_exp-1.csv")



