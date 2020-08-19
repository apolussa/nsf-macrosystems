# code to read irga raw data
# draft 1 by ap, still need to add some ease of use edits

# takes raw txt file from irga log with at least columns:
  # Time, Integral

# identifies integral values when it drops from a steady number to ~ 0
# IMPORTANT: it only takes integral values that are over 200. If you know there are values smaller than that, you can decrease that numner
# Also note that it cannot idntify which values aline with which sample, so this must be carefully noted if there are repeats, errrors, etc


irga.data <- read.delim("raw-data/lab-experiment/experiment-1/IRGA raw/Microcosm_Day16_08052020.txt", skip = 2, header = T)
irga.data <- rbind(irga.data, 0) # add extra row with 0 in integral in case last value is static (sometimes this happens)


samples <- data.frame()

for(i in 1:length(irga.data$Integral)){
  if(irga.data$Integral[i] - irga.data$Integral[i+1] > 200){
    samples <- bind_rows(samples, irga.data[i,])
  }
}

# View(samples)

write.csv(samples, "irga.values.csv")


