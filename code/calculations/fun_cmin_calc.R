# last worked on 20200720AP
# function to process on IRGA raw data
# output is a file with co2 flux per hour for each microcosm

library(tidyverse)
library(roxygen2)


#' Calculate carbon mineralization
#' 
#' This function calculates the CO2 production per hour from raw integral values from an infrrared gas analyser (IRGA)
#' The input format is standardized and found in the folder raw-data/lab-experiment/experiment-1
#' It needs to include microcosm flushing time and date, sampling time and date, irga values, and co2 standard values
#' 
#' @param cmin Data frame including flush time, date, irga sampling time & date, integral values, and co2 standard values
#' @param date Character string for date in the format "day-XX_YYYYMMDD" (e.g. "day-02_20200227")
#' @return cmin_calc a dataframe that includes lab.id, unique.id, soil.volume, CO2CperHour
#' @export .csv A detailed calculated dataframe with the title irga_calc_+"date"+.csv" 

# cmin <- time.list[[13]]
# date <- IRGA.files[13]

cmin_calc_fun <- function(cmin, date){
  
  # Calculate standard values for all places with standards
  
  not_any_na <- function(x) all(!is.na(x))
  
  cmin[is.na(cmin$std.value.1)==FALSE,] -> stds
  stds %>% add_column(meanStandard=NA_real_) -> stds
  
  
  for(i in 1:nrow(stds)){
    bind_rows(
      stds[i,c("std.value.1","std.value.2","std.value.3","std.value.4")] %>%
        select_if(not_any_na) %>% gather()
    ) %>%
      dplyr::summarize(mean = mean(value)) -> stds[(i),'meanStandard']
  }
  
  # calculate differences in standards by using slope method
  
  
  corr_std <- numeric() # corrected standard based on slope between the standards that delimit each set
  the.time <- numeric()
  the.slope <- numeric()
  

  v.num <- 1
  for(j in 2:length(stds$cmin.id)){
    
    for(i in 1:(stds$cmin.id[j] - stds$cmin.id[j-1])){
      # interpolate between two sets of standards
      
      # previous (initial) standard + [(change between previous and current standard)/(change in time)]*(change in time between end of standard measurement to sample in set)
      
      corr_std[v.num] <- as.numeric(stds$meanStandard[j-1]) +  
        ((as.numeric(stds$meanStandard[j]) - as.numeric(stds$meanStandard[j-1])) / 
           as.numeric(difftime(as.POSIXct(stds$std.start.time[j], format = "%m/%d/%Y %H:%M"),
                               as.POSIXct(stds$std.start.time[j-1], format = "%m/%d/%Y %H:%M"), 
                               units = "min"))) *
        as.numeric(difftime(as.POSIXct(cmin$time.irga[stds$cmin.id[j-1] + i - 1], format = "%H:%M"),
                            as.POSIXct(cmin$time.irga[stds$cmin.id[j-1]], format = "%H:%M"), 
                            units = "min"))
      
      
      the.time[v.num] <- as.numeric(difftime(as.POSIXct(cmin$time.irga[stds$cmin.id[j-1] + i - 1], format = "%H:%M"),
                                             as.POSIXct(stds$std.end.time[j-1], format = "%H:%M"),
                                             
                                             # on October 24, 2020 this previous line was changed from 
                                             # as.POSIXct(cmin$time.irga[stds$cmin.id[j-1]], format = "%H:%M"),
                                             # to what it is currently - now reflecting the end of the standard measurement
                                             # instead of the first sample measurement in the set! 
                                             
                                             units = "min"))
      
      the.slope[v.num] <- ((as.numeric(stds$meanStandard[j]) - as.numeric(stds$meanStandard[j-1])) / 
                             as.numeric(difftime(as.POSIXct(stds$std.start.time[j], format = "%m/%d/%Y %H:%M"),
                                                 as.POSIXct(stds$std.start.time[j-1], format = "%m/%d/%Y %H:%M"), 
                                                 units = "min")))
      v.num <- v.num + 1
    }
    
  }
  cmin <- cmin[1:(dim(cmin)[1] - 1),] # remove last row, it is where the last standard is placed
  
  mutate(cmin, 
         correctedStandard = as.numeric(corr_std),
         the.time = the.time,
         the.slope = the.slope) %>% 
    select(-c("std.value.1","std.value.2" , "std.value.3", "std.value.4",
              "std.start.time", "std.end.time", "notes.flushing", 
              "notes.irga")) -> cmin
  
  # calculate co2 flux for all microcosms
  
  cmin %>%
    mutate(
      incubationTime = lubridate::time_length( lubridate::interval(
        lubridate::mdy_hms(paste(cmin$date.flush, cmin$time.flush, sep = " ")),
        lubridate::mdy_hms(paste(cmin$date.irga, cmin$time.irga, sep = " "))
      ), unit = 'hour'), # Hours
      dilutionFactor = ((5*times.sampled)/(57.15-soil.volume))+1,    
      measuredCO2 = irga.integral*(standard.co2/correctedStandard),             # ppm
      concentrationCO2 = measuredCO2*dilutionFactor,                       # ppm
      volumeCO2 = concentrationCO2*((57.15-soil.volume)/1000),             # uL
      molesCO2 = (volumeCO2/22.414)*273.15/293.15,                         # umol
      CO2C = molesCO2*12.011,                                              # ug
      CO2CperHour = CO2C/incubationTime                                    # ug h-1
    ) %>% select(c(lab.id, unique.id, soil.volume, standard.co2, correctedStandard, the.time, the.slope,
                   irga.integral, incubationTime, CO2C, CO2CperHour)) -> cmin_calc 
  
  setwd("calculated-data/lab-experiment/experiment-1/mid_calcs/")
  write.csv(cmin_calc, paste("irga_calc_", date, ".csv", sep = "")) 
  setwd("../../../..")
  cmin_calc %>% select(c(lab.id, unique.id, soil.volume, CO2CperHour)) -> cmin_calc
  
  return(cmin_calc)
}
