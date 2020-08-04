# Track microcosm moisture levels throughout experiement

# August 3, 2020
# A Polussa

library(tidyverse)

setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

# Import tube mass

tube.mass <- read_csv("raw-data/lab-experiment/experiment-1/initialMass-exp-1_Spring-2020.csv")


# Import list of files from "wet-up" and raw "wet-up" CSVs into list 

moisture.filenames <- list.files(path = "raw-data/lab-experiment/experiment-1/", pattern = "wetup_exp-1*")
moisture.list <- lapply(paste("raw-data/lab-experiment/experiment-1/", moisture.filenames, sep = ""), read_csv)


# Name list from wet-up filenames; format: day-XX_YYYYMMDD

names(moisture.list) <- substr(moisture.filenames, start = 13, stop = 28)


# create continuous dataframe

moisture_df <- mapply(cbind, moisture.list, "date" = names(moisture.list), SIMPLIFY = F) %>%
  bind_rows(.) %>% 
  separate(col = date, into = c("day", "date"), sep = "_") %>% 
  mutate (day = substr(day, start = 5, stop = 7)
  ) %>% transform(., day = as.numeric(day), 
                  date = as.Date(date, format = "%Y%m%d")) %>%
  drop_na(lab.id) # some csvs include empty rows that autopopulate

# Many values for total.mass (which implies final after wet-up) were not input
# because they are assumed to be within 0.01 g of target value

# For simplicity, replace the NAs with the target value to look at percent change difference

for(i in 1:length(moisture_df$lab.id)){
  if(is.na(moisture_df$total.mass[i]) == TRUE){  
    moisture_df$total.mass[i] <- moisture_df$targetTotalMass[i]
  }
}


moisture_change <- left_join(moisture_df, tube.mass, by = "lab.id") %>%
  mutate(
    TotalMassTarget = targetTotalMass - tube.mass,          # target mass, constant
    TotalMassInitial = total.mass.before.wetup - tube.mass, # mass of litter and soil before wetup
    TotalMassAfter = total.mass - tube.mass,                # mass of litter and soil after wet-up
    TotalMassPrevious = lag(TotalMassAfter, 408),           # mass of litter and soil after previous wet-up 
    PercentChange = ( (TotalMassInitial - TotalMassPrevious) / TotalMassPrevious) * 100
  ) %>%
  select(-c(total.mass.before.wetup:tube.fresh.soil.and.litter.mass))

write.csv(moisture_change, "calculated-data/lab-experiment/experiment-1/microcosmMoisture-exp-1_Spring-2020.csv")
