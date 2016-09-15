################################################################################
##' @title Summarise limpet temperatures
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-16
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

##' For the temperature logger data, I calculated the average daily maximum, median, and minimum temperature across 6 weeks (August1 - September 12, 2015)
##' Because this is just a small snapshot in time, I was also interested in annual variation in temperature - so Luke Miller hindcasted limpet body temperatures from 1999-2013
##' For comparison, I calculated average daily max, median, min for each month between 1999 and 2013. I then used these data in two ways:
##' 1. I extracted the hottest monthly maximum temperature, the coldest minimum temperature, and the median median temperature for each run_id (position) across the entire 15 year period
##' 2. I extracted hottest monthly maximum temperature, the coldest minimum temperature, and the median median temperature for each run_id (position) and year, then I took the average

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

# Load daily summary data (what was the max, median, min temp each day?)
# source("process_limpet_temps.R")
lim_daily <- read_csv("output/limpet_daily_temps.csv")[-1]
lim_daily
tail(lim_daily)

# Long format
lim_dailyL <- lim_daily %>% select(-c(mean:cv)) %>% 
  gather(., key = "metric", value = "tempC", maximum:minimum)

lim_dailyL <- lim_dailyL %>% 
  mutate(month = month(dateR), 
         jDay = yday(dateR)) 

##### SUMMARISE MONTHLY #####
lim_monthly_summary <- lim_dailyL %>% group_by(run_id, metric, month, year) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se, 
            max = max(tempC)) %>% 
  ungroup() %>% 
  mutate(dateR = as.Date(paste(year, month, "15", sep = "-")))

# Join with position data
lms2 <- inner_join(lim_monthly_summary, positionDF, by = "run_id")

lms3 <- inner_join(lms2, spCodes, by = "sp")
unique(lms3$metric)

head(lms3)

##### EXTRACT SINGLE HOTTEST, COLDEST MONTH FOR EACH RUN ACROSS 14 YEARS #####

# Extract the row with the maximum monthly 
monthly_max <- lms3 %>% filter(metric == "maximum") %>% 
  group_by(run_id) %>% arrange(desc(mean)) %>% ungroup() %>%
  group_by(run_id) %>% slice(1)

# Extract the row with the minimum monthly 
monthly_min <- lms3 %>% filter(metric == "minimum") %>% 
  group_by(run_id) %>% arrange(mean) %>% ungroup() %>%
  group_by(run_id) %>% slice(1)

# Extract the row with the median median? 

# 168 observations for each run_id metric 
lms3 %>% filter(metric == "median") %>% 
  group_by(run_id) %>% 
  summarise(n = n())

monthly_med <- lms3 %>% filter(metric == "median") %>% 
  group_by(run_id) %>% arrange(desc(mean)) %>% ungroup() %>% 
  group_by(run_id) %>% slice(168/2)

monthly_extremes <- rbind(monthly_max, monthly_med, monthly_min)

##### SUMMARISE ANNUAL #####
head(lms3)

lim_annual <- lms3 %>% group_by(run_id, metric, year) %>% 
  summarise(maximum = max(mean), 
            median = median(mean), 
            minimum = min(mean)) %>% 
  ungroup()

la_max <- lim_annual %>% filter(metric == "maximum") %>% 
  select(run_id, year, maximum) 
la_med <- lim_annual %>% filter(metric == "median") %>% 
  select(run_id, year, median) 
la_min <- lim_annual %>% filter(metric == "minimum") %>% 
  select(run_id, year, minimum) 

unique(la_min$run_id)
unique(la_min$year)

la_summary <- inner_join(la_max, la_med, by = c("run_id", "year")) %>% 
  inner_join(., la_min, by = c("run_id", "year"))

la_summL <- gather(la_summary, key = "metric", value = "tempC", maximum:minimum)

la_summL2 <- la_summL %>% 
  inner_join(., positionDF, by = "run_id") %>% 
  inner_join(., spCodes, by = "sp")

annual_summary <- la_summL2 %>% 
  group_by(position, tidalHT, azimuth, aspect, species, metric) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()
