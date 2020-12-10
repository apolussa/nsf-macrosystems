library(tidyverse)

# Read in raw data
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

gravimetric_moisture <- read_csv("raw-data/field-experiment/prelim/litter_chemistry_prelim-2_Spring-2020.csv")

# Set directory to calculated data folder
setwd("calculated-data/field-experiment/prelim/")

# Calculate gravimetric moisture and export
# gravimetric_moisture %>%
#   mutate(
#     freshMass = tin.fresh.litter - tin.mass,
#     ovenDriedMass = tin.oven.dried.litter - tin.mass,
#     moistureMass = freshMass - ovenDriedMass, # Calculated in grams
#     moistureFraction = moistureMass / freshMass, # No units, fraction
#     moisturePercent = moistureFraction * 100
#   ) %>%
#   select(-tin.mass:-ovenDriedMass) %>%
#   write.csv("soilGWC_prelim-2_Spring-2020_disaggregated.csv")


# not aggregated by plot, keeping tag number for sir calculatations

gravimetric_moisture %>%
  mutate(
    freshMass = tin.fresh.litter - tin.mass,
    ovenDriedMass = tin.oven.dried.litter - tin.mass,
    moistureMass = freshMass - ovenDriedMass, # Calculated in grams
    moistureFraction = moistureMass / freshMass, # No units, fraction
    moisturePercent = moistureFraction * 100
  ) %>%
  dplyr::select(-sampling.period:-ovenDriedMass) %>%
  write.csv("litterGWC_prelim-2_Spring-2020.csv")
