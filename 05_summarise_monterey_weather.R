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

weather2 <- weather %>% select(dateR, month, year, precip, air_max:air_obs)
head(weather2)

weather3 <- weather2 %>% filter(!is.na(air_obs))
tail(weather3)
tail(weather2)

wL <- weather2 %>% select(-precip) %>% 
  gather(., key = climate_var, value = value, air_max:air_obs) %>% 
  filter(year < 2016)

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

##### GET ANNUAL TIME SERIES #####
head(wL)
unique(wL$climate_var)

# Calculate mean of hottest months for maximum per year
wL_monthly_max <- wL %>% filter(!is.na(value)) %>% 
  filter(climate_var == "air_max") %>% 
  filter(month == 10 | month == 8 | month == 9) %>% 
  group_by(year) %>% 
  summarise(maximum = mean(value)) %>% ungroup()

# Calculate mean of coldest months for minimum per year
wL_monthly_min <- wL %>% filter(!is.na(value)) %>% 
  filter(climate_var == "air_min") %>% 
  #filter(month == 12 | month == 1 | month == 2) %>% 
  group_by(year) %>% 
  summarise(minimum = mean(value)) %>% ungroup()

# Calculate mean of observed temperatures for median per year
wL_monthly_med <- wL %>% filter(!is.na(value)) %>% 
  filter(climate_var == "air_obs") %>% 
  group_by(year) %>% 
  summarise(median = mean(value)) %>% ungroup()

air_annual <- inner_join(wL_monthly_max, wL_monthly_min, 
                         by = "year") %>% 
  inner_join(., wL_monthly_med, by = "year")

air_annual

# Get in long format
air_annual_long <- air_annual %>% 
  gather(key = metric, value = tempC, maximum:median)


