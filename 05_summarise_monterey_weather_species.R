################################################################################
##' @title Summarise Monterey weather data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2017-08-18
##' 
##' @log 
##' 2016-11-08 Checking other weather stations against Monterey
##' Summarise by species
################################################################################

#rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# Get snail size data
source("05_summarise_size_data.R")

source("R/process_weather_data.R")

path_to_weather_data <- "data/uc_ipm/"

# Need a list of file names
fileNames <- dir(path = path_to_weather_data, recursive = TRUE, 
                 pattern = ".txt") 

### FUNCTION TO LOAD AND CLEAN RAW WEATHER DATA

monterey <- format_weather_data(path = path_to_weather_data, 
                                weather_file = fileNames[3], 
                                lines_to_skip = 55)

# Are some months poorly sampled?
# Remove months that have fewer than 15 samples
monterey %>% filter(!is.na(air_max)) %>% 
  group_by(year, month) %>% tally() %>% ungroup() %>% 
  filter(n < 15)

monterey2 <- monterey %>% filter(!is.na(air_max)) %>% 
  group_by(year, month) %>% 
  mutate(n = n()) %>% ungroup()

# Are some weeks poorly sampled?
monterey2 <- monterey2 %>% 
  mutate(weekN = week(dateR)) %>% 
  group_by(weekN, year) %>% 
  mutate(n_week = n()) %>% ungroup()

unique(monterey2$n_week)

# Remove weeks that have fewer than 3 samples
monterey3 <- monterey2 %>% filter(n_week > 2)

# Select data
weather <- monterey3

head(weather)
tail(weather)
summary(weather)
glimpse(weather)


# Get relevant years for each species
spYrs <- dat5 %>% select(sp, era, year) %>%
  distinct() %>% 
  spread(key = era, value = year)

sp_code = "LIKE"

subset_weather_period <- function(spYrs, temperature_data = weather, duration = 10) {
  
  nYrsPrior <- duration - 1
  
  # Get relevant years
  spYrs_sp <- spYrs 
  past2 <- spYrs_sp$past
  present2 <- spYrs_sp$present
  past1 <- past2 - nYrsPrior
  present1 <- present2 - nYrsPrior
  
  past_temperature <- temperature_data %>% filter(year >= past1 & year <= past2) %>% 
    mutate(era = "past")
  present_temperature <- temperature_data %>% filter(year >= present1 & year <= present2) %>% 
    mutate(era = "present")
  
  df_temperature <- rbind(past_temperature, present_temperature)
  
  return(df_temperature)
  
}

weather_sp <- spYrs %>% group_by(sp) %>% 
  do(., subset_weather_period(., weather, duration = 10)) %>% 
  ungroup()

weather_sp %>% 
  ggplot(aes(dateR, air_obs, color = era)) + 
  facet_wrap(~ sp, nrow = 3) + 
  geom_line()

##### SUMMARISE MONTHLY #####
unique(weather_sp$month)

weather_sp_monthly <- weather_sp %>% 
  filter(!is.na(air_obs)) %>%
  group_by(sp, month, era) %>% 
  summarise(median = median(air_obs, na.rm = TRUE), 
            mean = mean(air_obs, na.rm = TRUE), 
            sd = sd(air_obs, na.rm = TRUE), 
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se, 
            upper = quantile(air_obs, probs = 0.75), 
            lower = quantile(air_obs, probs = 0.25)) %>% 
  ungroup() 

# median and confidence intervals
weather_sp_monthly %>% 
  ggplot(aes(month, median, color = era)) + 
  facet_wrap(~ sp) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, color = NULL, fill = era), alpha = 0.5) + 
  geom_line() 

# mean and sd
weather_sp_monthly %>% 
  ggplot(aes(month, mean, color = era)) + 
  facet_wrap(~ sp) + 
  geom_ribbon(aes(ymin = mean - CI, ymax = mean + CI, color = NULL, fill = era), alpha = 0.5) + 
  geom_line() 

##### SUMMARISE WEEKLY #####
unique(weather_sp$month)

weather_sp_weekly <- weather_sp %>% 
  filter(!is.na(air_obs)) %>%
  mutate(weekN = week(dateR)) %>% 
  group_by(sp, weekN, era) %>% 
  summarise(median = median(air_obs), 
            mean = mean(air_obs), 
            sd = sd(air_obs), 
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se, 
            upper = quantile(air_obs, probs = 0.75), 
            lower = quantile(air_obs, probs = 0.25)) %>% 
  ungroup() 

# median and confidence intervals
weather_sp_weekly %>% 
  ggplot(aes(weekN, median, color = era)) + 
  facet_wrap(~ sp) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, color = NULL, fill = era), alpha = 0.5) + 
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, color = NULL, fill = era), alpha = 0.5) + 
  geom_line() 

# mean and ci
weather_sp_weekly %>% 
  filter(sp == "CHFU") %>% 
  ggplot(aes(weekN, mean, color = era)) + 
  facet_wrap(~ sp) + 
  geom_ribbon(aes(ymin = mean - CI, ymax = mean + CI, color = NULL, fill = era), alpha = 0.5) + 
  geom_line() 
ggsave("figs/temp_monthly_air.png", height = 3.5, width = 5)


