# litter decomposition measurements
# last updated 10/16/2020 with 8 month data from both sites
# A Polussa

# 1. convert initial air dry mass to initial oven dry mass
# 2. convert 4 month fresh weight to 4 month oven dry mass
# 3. estimate loss from travel bag assay
# 4. calculate mass loss between prelim-1 and prelim-2

##### Import Data #####
library(tidyverse)
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

# initial, prelim-1
litter_prelim_1 <- read_csv("raw-data/field-experiment/prelim/litter_chemistry_prelim-1_Nov-2019.csv")

# 4 months, prelim-2
litter_prelim_2 <- read_csv("raw-data/field-experiment/prelim/litter_chemistry_prelim-2_Spring-2020.csv")

# 8 months, prelim-2
litter_prelim_3 <- read_csv("raw-data/field-experiment/prelim/litter_chemistry_prelim-3_Summer-2020.csv")

# travel bag
travel_bag <- read_csv("raw-data/field-experiment/prelim/travel_bags_prelim-1.csv")

##### mass loss #####

litter_prelim_1 %>%
  mutate(
    ovenAirDryFraction = (vial.oven.dried.litter-vial.mass)/(vial.air.dried.litter-vial.mass),
    litterDryMass_1 = litterbag.leaf.mass * ovenAirDryFraction # oven dry leaf mass (g)
  )  %>%  dplyr::select(tag, site, plot, species, litterDryMass_1) -> litter_prelim_1_conversion


litter_prelim_2[is.na(litter_prelim_2$tin.label)==FALSE,] %>%
  mutate(
    ovenFreshFraction = (tin.oven.dried.litter-tin.mass)/(tin.fresh.litter-tin.mass),
    moistureFraction = ((tin.fresh.litter-tin.mass)- (tin.oven.dried.litter-tin.mass))/(tin.fresh.litter-tin.mass),
    litterDryMass_2 = litterbag.leaf.mass * ovenFreshFraction # oven dry leaf mass (g)
  ) %>%  dplyr::select(unique.id, tag, site, plot, species, litterDryMass_2) -> litter_prelim_2_conversion


litter_prelim_3[is.na(litter_prelim_3$tin.label)==FALSE,] %>%
  mutate(
    ovenFreshFraction = (tin.oven.dried.litter-tin.mass)/(tin.fresh.litter-tin.mass),
    moistureFraction = ((tin.fresh.litter-tin.mass)- (tin.oven.dried.litter-tin.mass))/(tin.fresh.litter-tin.mass),
    litterDryMass_3 = litterbag.leaf.mass * ovenFreshFraction # oven dry leaf mass (g)
  ) %>%  dplyr::select(unique.id, tag, site, plot, species, litterDryMass_3) -> litter_prelim_3_conversion



travel_bag %>% 
  mutate(
    ovenAirDryFraction = (vial.oven.dried.litter-vial.mass)/(vial.air.dried.litter-vial.mass),
    initialLitterDryMass = litterbag.leaf.mass * ovenAirDryFraction,
    travelMassLoss = oven.dry.litter.mass - initialLitterDryMass,
    travelLossFraction = travelMassLoss/initialLitterDryMass # loss of litter as fraction of initial mass
    ) %>%
  dplyr::select(species, travelLossFraction)%>%
  group_by(species) %>%
  dplyr::summarize(travelLossFraction = mean(travelLossFraction)) -> travelBagCorrection

# travel bags increased in weight mostly

# in final dataframe, "_wcorr" means with travel litterbag correction
# exaggerates decomposition 

# 

litterMassLoss <- bind_rows(
  
  litter_prelim_1_conversion %>%
    mutate(
      samplingPeriod = "prelim-1",
      fractionMassLoss_wcorr = 0,
      fractionMassLoss = 0,
      plot = as.character(plot)
    ) %>% mutate(litterDryMass = litterDryMass_1) %>%
    select(-litterDryMass_1),
  
  left_join(litter_prelim_2_conversion, litter_prelim_1_conversion, by = c("tag", "site", "plot", "species")) %>%
    separate(col = unique.id, into = c("site", "plot", "samplingPeriod"), sep = "-", extra = "merge") %>% 
    left_join(., travelBagCorrection, by = "species") %>%
    mutate(
      litterDryMass_initial_wcorr = litterDryMass_1 + (litterDryMass_1*travelLossFraction), # with litterbag correction
      fractionMassLoss_wcorr = (litterDryMass_2 - litterDryMass_initial_wcorr)/litterDryMass_initial_wcorr,
      fractionMassLoss = (litterDryMass_2 - litterDryMass_1)/litterDryMass_1,
      litterDryMass = litterDryMass_2
    ) %>% dplyr::select(c(site, plot, samplingPeriod, tag, species,fractionMassLoss_wcorr,fractionMassLoss,litterDryMass ))
  ,
  
  left_join(litter_prelim_3_conversion, litter_prelim_1_conversion, by = c("tag", "site", "plot", "species")) %>%
    separate(col = unique.id, into = c("site", "plot", "samplingPeriod"), sep = "-", extra = "merge") %>% 
    left_join(., travelBagCorrection, by = "species") %>%
    mutate(
      litterDryMass_initial_wcorr = litterDryMass_1 + (litterDryMass_1*travelLossFraction), # with litterbag correction
      fractionMassLoss_wcorr = (litterDryMass_3 - litterDryMass_initial_wcorr)/litterDryMass_initial_wcorr,
      fractionMassLoss = (litterDryMass_3 - litterDryMass_1)/litterDryMass_1,
      litterDryMass = litterDryMass_3
    ) %>% dplyr::select(c(site, plot, samplingPeriod, tag, species,fractionMassLoss_wcorr,fractionMassLoss,litterDryMass ))
)




write.csv(litterMassLoss, "calculated-data/field-experiment/prelim/litterbag_decomp_prelim-3_Spring-2020.csv")


