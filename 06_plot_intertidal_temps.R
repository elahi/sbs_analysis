################################################################################
##' @title Plot intertidal temperatures
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-06
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# Load size data
source("03_identify_size_cutoff.R")
head(dat4)
spCodes <- dat4 %>% select(species, sp) %>% distinct()

# Load temperature data
source("05_summarise_intertidal_temps.R")

##### PLOTS ######

## Plot means of daily mean, max and min
tempMeans %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  labs(x = "Tidal height (m)", y = "Mean temperature (C)") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

# ggsave("figs/elahi_temp_tidal.png", height = 3.5, width = 7)

## Plot median, max, min
tempMedL %>% filter(metric != "cv") %>% 
  ggplot(aes(tidalHT, tempC, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  labs(x = "Tidal height (m)", y = "Mean temperature (C)") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

# ggsave("figs/elahi_temp_median_tidal.png", height = 3.5, width = 7)

## Plot median, max, min
tempMedL %>% filter(metric == "cv") %>% 
  ggplot(aes(tidalHT, tempC, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  labs(x = "Tidal height (m)", y = "Mean temperature (C)") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

