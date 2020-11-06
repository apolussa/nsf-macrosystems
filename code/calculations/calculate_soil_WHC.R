# Last updated March 16, 2020 by AP adapted from Stephen Wood's original code
# Cleaned up variable names

# Calculates Water holding capacity for soil


library(tidyverse)


# Read in raw data
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")


soil_WHC <- read_csv("raw-data/field-experiment/prelim/soilWHC_prelim-2_Spring-2020.csv")

# Set directory to calculated data folder
setwd("calculated-data/field-experiment/prelim/")

# Calculate water holding capacity and export

aggregate(. ~ site + plot + unique.id + sampling.period,
          data =soil_WHC %>%
            mutate(
              freshSoil = tin.fresh.soil - tin.mass,
              ovenDriedSoil = tin.oven.dried.soil - tin.mass,
              moistureMass = freshSoil - ovenDriedSoil, # Calculated in grams
              moistureMasspergDrySoil = moistureMass / ovenDriedSoil, # g H2O g soil-1 at 100% WHC
              moistureFraction = moistureMass / freshSoil, # No units, fraction
              moisturePercent = moistureFraction * 100
            ) %>%
            select(-'repeat':-ovenDriedSoil),
          FUN = mean) %>%
  write.csv("soilWHC_prelim-2_Spring-2020.csv")


# Calculate disaggregated water holding capacity and export

soil_WHC %>%
            mutate(
              freshSoil = tin.fresh.soil - tin.mass,
              ovenDriedSoil = tin.oven.dried.soil - tin.mass,
              moistureMass = freshSoil - ovenDriedSoil, # Calculated in grams
              moistureMasspergDrySoil = moistureMass / ovenDriedSoil, # g H2O g soil-1 at 100% WHC
              moistureFraction = moistureMass / freshSoil, # No units, fraction
              moisturePercent = moistureFraction * 100
            ) %>%
            select(-tin.mass:-ovenDriedSoil) %>%
  write.csv("disaggregated_soilWHC_prelim-2_Spring-2020.csv")


