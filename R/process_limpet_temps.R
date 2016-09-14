################################################################################
##' @title Process Luke Miller's limpet temperature data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-16
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

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

# Load size data
source("03_identify_size_cutoff.R")
head(dat4)
spCodes <- dat4 %>% select(species, sp) %>% distinct()
levels(spCodes$species)

# Get limpet temperature data
limpet <- read_csv("../bigFiles/Limpet_temp_estimates.csv")
glimpse(limpet)

# Get dates, months, years
limpet2 <- limpet %>% 
  mutate(dateR = as.Date(TimePST), 
         month = month(TimePST), 
         year = year(TimePST))
glimpse(limpet2)

# Convert to long format
limpetL <- limpet2 %>% gather(key = "run_id", value = "tempC", Run118:Run135)
head(limpetL)

##### GET DAILY MAXIMA, MINIMA, MEDIANS #####

lim_daily <- limpetL %>% group_by(run_id, dateR, month, year) %>% 
  summarise(maximum = max(tempC), 
            median = median(tempC), 
            minimum = min(tempC), 
            mean = mean(tempC), 
            sd = sd(tempC), 
            cv = sd/mean) %>% 
  ungroup() 

lim_daily

write.csv(lim_daily, "output/limpet_daily_temps.csv")
