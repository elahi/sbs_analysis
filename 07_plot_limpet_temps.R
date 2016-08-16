################################################################################
##' @title Plot limpet temperatures
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-16
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

# Load temperature data and size data
source("summarise_limpet_temps.R")

# Load size data
source("03_identify_size_cutoff.R")
head(dat4)
spCodes <- dat4 %>% select(species, sp) %>% distinct()
levels(spCodes$species)

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# Load the file that I sent to Luke for the hindcasts
positionDF <- read_csv("output/positionDF_edit.csv")[-1] %>% 
  select(position, sp, nest1:tidalHT, aspect, slope_normal, lon, lat, run_id) %>%
  rename(azimuth = aspect, aspect = slope_normal)
positionDF

lim_daily <- read_csv("output/limpet_daily_temps.csv")[-1]
lim_daily

##### PLOTS ######
lds3 <- inner_join(lds2, spCodes, by = "sp")
unique(lds3$metric)

## Plot means of daily median, max and min
lds3 %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  labs(x = "Tidal height (m)", y = "Mean temperature (C)") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  geom_point(data = subset(lds3, metric == "maximum"), 
             aes(tidalHT, max, color = NULL), alpha = 0.6, size = 2)

##### PLOT ANNUAL MAX, MEDIAN, MIN BY TIDAL HEIGHT ######

annual_summary %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  ylab(expression(paste("Predicted body temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

ggsave("figs/elahi_predicted_temp_tidal.png", height = 3.5, width = 7)

##### PLOT ANNUAL MAX, MEDIAN, MIN TIME SERIES ######

la_summL2 %>% 
  ggplot(aes(year, tempC, color = metric)) +
  facet_wrap(~ species, ncol = 5) + 
  geom_point(alpha = 0.6) + geom_smooth(method = "lm") + 
  ylab(expression(paste("Predicted body temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

