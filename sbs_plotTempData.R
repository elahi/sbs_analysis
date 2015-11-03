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

###########################
# Load daily iButton data from output folder
dat <- read.csv("./output/temp_daily_iButton.csv", header = TRUE)
head(dat)
dat$day <- as.Date(dat$day)

# Mean temperatures
ggplot(dat, aes(day, mean_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Max temperatures
ggplot(dat, aes(day, max_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Min temperatures
ggplot(dat, aes(day, min_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

###########################
# Now plot summarized data for each logger position 
# (summarizes across two iButtons per position)
# Except for: WaraD_mid2; WaraD_hi; WaraD_low
# (these only had one logger per position)

dat <- read.csv("./output/temp_daily_position.csv", header = TRUE)
head(dat)
dat$day <- as.Date(dat$day)

# Mean temperatures
ggplot(dat, aes(day, mean_tempC, color = position)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Max temperatures
ggplot(dat, aes(day, max_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Min temperatures
ggplot(dat, aes(day, min_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Max temperatures
ggplot(dat, aes(day, max_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

###########################
# Now plot summarized data for each species

dat <- read.csv("./output/temp_daily_species.csv", header = TRUE)
head(dat)
dat$day <- as.Date(dat$day)

# Mean temperatures
ggplot(dat, aes(day, mean_tempC)) + 
  geom_line() + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ code) + theme(legend.position = "none")

# Max temperatures
ggplot(dat, aes(day, max_tempC)) + 
  geom_line() + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ code) + theme(legend.position = "none")

# Min temperatures
ggplot(dat, aes(day, min_tempC)) + 
  geom_line() + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ code) + theme(legend.position = "none")

### Violin plots (?)
ggplot(dat, aes(code, mean_tempC)) + 
  geom_violin() + 
  ylab("Temperature (C)") + xlab("Species") + 
  theme(legend.position = "none") + coord_flip()


