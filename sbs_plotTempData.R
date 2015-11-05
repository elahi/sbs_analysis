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
library(dplyr)
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

# Mean temperatures
names(dat)
ggplot(dat, aes(day, mean_tempC, color = position)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ code) + theme(legend.position = "none")

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

# Relevel the species codes
dat$code <- factor(dat$code, levels = rev(c('LIKE', 'LODI', 'CHFU')), 
                   ordered = TRUE)

# Figure out how many daily records per species, and mean tidal height
dat %>% group_by(code) %>% summarise(totalN = n(), 
                                     meanHT = mean(tidalHT, na.rm = TRUE), 
                                     sdHT = sd(tidalHT, na.rm = TRUE))

### Violin plot
ggplot(dat, aes(code, mean_tempC)) + 
  geom_violin(fill = "gray") + 
  geom_boxplot(width = 0.3, notch = TRUE, color = "black") + 
  ylab("Temperature (C)") + xlab("Species") + 
  theme(legend.position = "none") + 
  scale_x_discrete("", labels = c("CHFU" = "Chlorostoma funebralis\nn=344", 
                                  "LODI" = "Lottia digitalis\nn=258", 
                                  "LIKE" = "Littorina keenae\nn=344")) +
  coord_flip()

ggsave("./figs/temp_violin_spp.pdf", width = 3.5, height = 3.5)



