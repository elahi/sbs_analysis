################################################################################
##' @title Summarise Monterey weather data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-09-13
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

weather <- read.csv(file = "data/climate_monterey/monterey_weather.txt", 
                    skip = 53, header = TRUE, stringsAsFactors = FALSE)

head(weather)
tail(weather)
summary(weather)

weather <- weather %>% select(Station:Wx) %>%
  mutate(dateR = ymd(Date), 
         month = month(dateR), 
         year = year(dateR)) %>%
  rename(precip = Precip, air_max = Air.max, air_min = min, air_obs = obs)
glimpse(weather)

unique(weather$Wx)

weather2 <- weather %>% select(dateR, month, year, air_max:air_obs) %>% 
  filter(year < 2016)

head(weather2)

wL <- weather2 %>%
  gather(., key = climate_var, value = value, air_max:air_obs)

tail(wL)
glimpse(wL)

wL %>% 
  ggplot(aes(climate_var, value)) + 
  geom_boxplot() 

# What are the warmest and coldest months?
wL %>% group_by(month, climate_var) %>% 
  summarise(meanTemp = mean(value, na.rm = TRUE)) %>% ungroup() %>% 
  arrange(climate_var, meanTemp) # %>% View()

# Coldest == Dec, Jan, Feb
# Warmest == Aug, Sep, Oct

wL %>% filter(!is.na(value)) %>% 
  group_by(month, year, climate_var) %>% tally() %>% 
  filter(n < 28)

##### SUMMARISE MONTHLY #####
head(weather2)
tail(weather2)

# Use air_obs only
wM <- weather2 %>% filter(!is.na(air_obs)) %>% 
  group_by(year, month) %>% 
  summarise(median = median(air_obs, na.rm = TRUE), 
            maximum = max(air_obs, na.rm = TRUE), 
            minimum = min(air_obs, na.rm = TRUE)) %>% 
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

# Use air_obs, air_max, air_min
wM <- weather2 %>% group_by(year, month) %>% 
  summarise(median = median(air_obs, na.rm = TRUE), 
            maximum = median(air_max, na.rm = TRUE), 
            minimum = median(air_min, na.rm = TRUE)) %>% 
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

wM
tail(wM)
summary(wM)

##### GET ANNUAL TIME SERIES #####

# Calculate mean of hottest months for maximum per year
monthly_max <- wM %>% filter(!is.na(maximum)) %>% 
  filter(month == 10 | month == 8 | month == 9) %>% 
  group_by(year) %>% 
  summarise(maximum = mean(maximum)) %>% ungroup()

# Calculate mean of coldest months for minimum per year
monthly_min <- wM %>% filter(!is.na(minimum)) %>% 
  filter(month == 12 | month == 1 | month == 2) %>% 
  group_by(year) %>% 
  summarise(minimum = mean(minimum)) %>% ungroup()

# Calculate mean of observed temperatures for median per year
monthly_med <- wM %>% filter(!is.na(median)) %>% 
  group_by(year) %>% 
  summarise(median = mean(median)) %>% ungroup()

air_annual <- inner_join(monthly_max, monthly_min, 
                         by = "year") %>% 
  inner_join(., monthly_med, by = "year")

air_annual
tail(air_annual)

# Get in long format
air_annual_long <- air_annual %>% 
  gather(key = metric, value = tempC, maximum:median) %>%
  filter(!is.na(tempC))

tail(air_annual_long)

