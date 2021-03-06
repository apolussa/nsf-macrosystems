# sir and normalize by dry weight soil

# remember that the raw data needs "replicate" in it even if there was just one - august 19,2020

setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024")
# Function "sir_calc_fun" calculates CO2 production hr-1

# import sir
sir <- read_csv("raw-data/field-experiment/prelim/litterSIR_volume_prelim-2_Spring-2020.csv")

# import gwc

gwc <- read_csv("calculated-data/field-experiment/prelim/litterGWC_prelim-2_Spring-2020.csv")


##### calculate sir ##### 
# double check that the template and column names are all good
# also double check formats of time, days, flushing, standards, everything
# need a column with irga.id etc... 
# not in replicate right now


calc_sir_fun <- function(sir){
  
  # Calculate standard values for all places with standards
  not_any_na <- function(x) all(!is.na(x))
  
  sir[is.na(sir$std.value.1)==FALSE,] -> stds
  stds %>% add_column(meanStandard=NA_real_) -> stds
  
  for(i in 1:nrow(stds)){
    bind_rows(
      stds[i,c("std.value.1","std.value.2","std.value.3","std.value.4")] %>%
        select_if(not_any_na) %>% gather()
    ) %>%
      summarize(mean = mean(value)) -> stds[(i),'meanStandard']
  }
  
  # calculate differences in standards by using slope method
  
  
  corr_std <- numeric() # corrected standard based on slope between the standards that delimit each set
  the.time <- numeric()
  the.slope <- numeric()
  
  # corr_std[1] <- stds$meanStandard[1] # manually enter first standard value for the first sample
  # 
  # v.num <- 2
  
  v.num <- 1
  for(j in 2:length(stds$irga.id)){
    
    for(i in 1:(stds$irga.id[j] - stds$irga.id[j-1])){
      # interpolate between two sets of standards
      # previous (initial) standard integral + [(change in standards)/(change in time)]*(change in time between start of set to sample in set)
      
      corr_std[v.num] <- as.numeric(stds$meanStandard[j-1]) +  
        ((as.numeric(stds$meanStandard[j]) - as.numeric(stds$meanStandard[j-1])) / 
           as.numeric(difftime(as.POSIXct(stds$std.start.time[j], format = "%m/%d/%Y %H:%M"),
                               as.POSIXct(stds$std.start.time[j-1], format = "%m/%d/%Y %H:%M"), 
                               units = "min"))) *
        as.numeric(difftime(as.POSIXct(sir$time.irga[stds$irga.id[j-1] + i - 1], format = "%m/%d/%Y %H:%M"),
                            as.POSIXct(sir$time.irga[stds$irga.id[j-1]], format = "%m/%d/%Y %H:%M"), 
                            units = "min"))
      
      
      the.time[v.num] <- as.numeric(difftime(as.POSIXct(sir$time.irga[stds$irga.id[j-1] + i - 1], format = "%m/%d/%Y %H:%M"),
                                             as.POSIXct(stds$std.end.time[j-1], format = "%m/%d/%Y %H:%M"),
                                             units = "min"))
      
      the.slope[v.num] <- ((as.numeric(stds$meanStandard[j]) - as.numeric(stds$meanStandard[j-1])) / 
                             as.numeric(difftime(as.POSIXct(stds$std.start.time[j], format = "%m/%d/%Y %H:%M"),
                                                 as.POSIXct(stds$std.start.time[j-1], format = "%m/%d/%Y %H:%M"), 
                                                 units = "min")))
      v.num <- v.num + 1
    }
    
  }
  sir <- sir[1:(dim(sir)[1] - 1),] # remove last row, it is where the last standard is placed
  
  mutate(sir, 
         correctedStandard = as.numeric(corr_std),
         the.time = the.time,
         the.slope = the.slope) %>% 
    dplyr::select(-c("std.value.1","std.value.2" , "std.value.3", "std.value.4",
              "std.start.time", "std.end.time")) -> sir
  
  # calculate co2 flux for all microcosms
  
  sir %>%
    mutate(
      incubationTime = as.numeric(difftime(as.POSIXct(time.irga,  format = "%m/%d/%Y %H:%M"),
                                    as.POSIXct(time.flush,  format = "%m/%d/%Y %H:%M"), units = "hours")), # Hours
      dilutionFactor = ((5*times.sampled)/(57.15-volume))+1,    
      measuredCO2 = irga.integral*(standard.co2/correctedStandard),             # ppm
      concentrationCO2 = measuredCO2*dilutionFactor,                       # ppm
      volumeCO2 = concentrationCO2*((57.15-volume)/1000),             # uL
      molesCO2 = (volumeCO2/22.414)*273.15/293.15,                         # umol
      CO2C = molesCO2*12.011,                                              # ug
      CO2CperHour = CO2C/incubationTime                                    # ug h-1
    ) %>% dplyr::select(c(irga.id, unique.id, replicate, tag, volume, actual.fresh.mass, standard.co2, correctedStandard, the.time, the.slope,
                   irga.integral, incubationTime, CO2C, CO2CperHour)) -> sir_calc 
  
# this is to check standard values etc 
  
  sir_calc %>% dplyr::select(c(irga.id, unique.id, tag, replicate, the.time, actual.fresh.mass,incubationTime, CO2CperHour)) -> sir_calc
  
  return(sir_calc)
}


sir_calc <- calc_sir_fun(sir)


sir_calc_normalized <- left_join(sir_calc, gwc) %>%
  mutate(
    DryMass =  actual.fresh.mass*(1-moistureFraction),
    CO2CperHourperg = CO2CperHour / DryMass # ug CO2C hr-1 g-1 dry soil
  ) %>% dplyr::select(c(unique.id, tag, replicate, CO2CperHourperg))



  aggregate(CO2CperHourperg ~ unique.id + tag, 
            data = sir_calc_normalized,
            FUN = mean) %>%
  write.csv("calculated-data/field-experiment/prelim/litterSIR_prelim-2_Spring-2020.csv")
