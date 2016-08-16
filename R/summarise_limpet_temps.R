################################################################################
##' @title Summarise Luke Miller's limpet temperature data
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

##### SUMMARISE DAILY #####

lim_dailyL <- limpetL %>% group_by(run_id, dateR, month, year) %>% 
  summarise(maximum = max(tempC), 
            median = median(tempC), 
            minimum = min(tempC)) %>% 
  ungroup() %>%
  gather(., key = "metric", value = "tempC", maximum:minimum)

lim_dailyL

# Get means of daily mean, max and min
lim_daily_means <- lim_dailyL %>% 
  group_by(run_id, metric) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()

# Join with position data
lim_daily_means2 <- inner_join(lim_daily_means, positionDF, by = "run_id")

lim_daily_means2 %>% 
  ggplot(aes(tidalHT, mean, color = metric)) + 
  geom_point() + 
  facet_wrap(~ sp) +


##### SUMMARISE MONTHLY #####

lim_monthly <- lim_dailyL %>% group_by(run_id, metric, month, year ) %>% 
  summarise(monthly_mean = mean(tempC)) %>% 
  ungroup() %>% 
  mutate(dateR = as.Date(paste(year, month, "15", sep = "-")))

lim_monthly

lim_monthly %>% 
  ggplot(aes(dateR, monthly_mean, color = metric)) +
  facet_wrap(~ run_id, ncol = 5) + 
  geom_point() + geom_smooth(method = "lm")

##### SUMMARISE ANNUAL #####

lim_annual <- lim_dailyL %>% group_by(run_id, metric, year) %>% 
  summarise(annual_mean = mean(tempC)) %>% 
  ungroup()

lim_annual %>% 
  ggplot(aes(year, annual_mean, color = metric)) +
  facet_wrap(~ run_id, ncol = 5) + 
  geom_point() + geom_smooth(method = "lm")




