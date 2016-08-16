################################################################################
##' @title Summarise HMS weather station
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-15
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)
library(readr)

# Column	Units or definition
# year	
# dayofyear	
# hourmin	
# ws	meters per second
# wd	north is 0°
# wd-sd	standard deviation of wind direction
# panel C	instrument panel temperature  °C
# box C	
# LiCor	watts per square meter
# LiCor-sd	standard deviation of LiCor
# LiCor-min	min reading during the last ten minutes
# LiCor-max	max reading during the last ten minutes
# KZ	non-functioning
# KZ-sd	non-functioning
# KZ-min	non-functioning
# KZ-max	non-functioning
# Air C	Air temperature °C
# Air C-sd	standard deviation of air temperature during the last ten minutes
# Air C-min	min temperature during the last ten minutes
# Air C-max	max temperature during the last ten minutes
# RH	relative humidity - Not very useful this close to the ocean as it is usually high all the time
# RH-sd	standard deviation of RH
# RH-min	min RH during the last ten minutes
# RH-max	max RH during the last ten minutes
# A2 C	second air temperature reading
# A2 C-sd	
# A2 C-min	
# A2 C-max	
# volts	voltage of the internal backup battery

# Get august and september data
hmsw8 <- read_csv("data/climate_monterey/2015-08.txt")
hmsw9 <- read_csv("data/climate_monterey/2015-09.txt")

# Get august and september data
hmswO <- read_csv("data/hms_weather/2009-05.txt")
hmswP <- read_csv("data/hms_weather/2009-05p.txt")
names(hmswO)
names(hmswP)


hmsw <- rbind(hmsw8, hmsw9)
glimpse(hmsw)

# Get relevant columns and rename them
hmsw2 <- hmsw %>% select(year:wd, LiCor, `Air C`) %>% 
  rename(wind_speed = ws, wind_dir = wd, 
         licor = LiCor, tempC = `Air C`)

# If licor values are < 0, change to NA
hmsw3 <- hmsw2 %>% 
  mutate(licor = ifelse(licor < 0, NA, licor))
summary(hmsw3)

hmswL <- hmsw3 %>% gather(key = climate_var, value = value, wind_speed:tempC) %>% 
  mutate(dateR = as.POSIXct(strptime(paste(year, dayofyear, sep = " "), "%Y %j")))
         
unique(hmswL$hourmin)  


##### SUMMARISE DAILY MEASUREMENTS #####

hmsw_daily <- hmswL %>% filter(!is.na(value)) %>%
  group_by(climate_var, year, dayofyear) %>% 
  summarise(maximum = max(value), 
            median = median(value), 
            minimum = min(value)) %>% 
  ungroup() %>% 
  mutate(dateR = as.POSIXct(strptime(paste(year, dayofyear, sep = " "), "%Y %j")), 
         month = month(dateR))

hmsw_daily

hmsw_daily %>%
  ggplot(aes(dateR, median)) +
  geom_line() + 
  facet_wrap(~ climate_var, scales = "free")

