#################################################
# Author: Robin Elahi
# Date: 151102

# Temporal shifts in gastropod size-frequency distributions are
# consistent with climate warming

# Plotting summarized temperature data
#################################################

# Set working directory to main project folder
setwd("~/github/sbs_analysis")
getwd()

# Load packages
library(ggplot2)
theme_set(theme_classic(base_size = 8))

# Load daily iButton data from output folder
dat <- read.csv("./output/temp_daily_iButton.csv", header = TRUE)
head(dat)

dat$day <- as.Date(dat$day)
str(dat)
###########################

ggplot(dat, aes(day, mean_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")
  
