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
spCodes <- dat4 %>% select(species, sp) %>% distinct() %>% 

# Load daily temperature data
# Has daily mean, daily max, and daily min
tempDat <- read.csv("output/temp_daily_position.csv") %>% 
  select(-X) %>% 
  rename(sp = code, daily_mean = mean_tempC, 
         daily_max = max_tempC, 
         daily_min = min_tempC)

head(tempDat)

tempDat2 <- tempDat %>% inner_join(., spCodes, by = "sp")
head(tempDat2)

tempDatL <- tempDat2 %>% gather(key = metric, value = tempC, daily_mean:daily_min)
head(tempDatL)
summary(tempDatL)

tempMeans <- tempDatL %>% 
  group_by(species, position, tidalHT, aspect, slope, lon, lat, metric) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()



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
ggsave("figs/elahi_temp_tidal.png", height = 3.5, width = 7)


