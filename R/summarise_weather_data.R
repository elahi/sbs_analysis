################################################################################
##' @title Process Monterey weather data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-12
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

head(sst_hms)

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

wL <- weather2 %>% gather(., key = climate_var, value = value, precip:air_obs)

wL %>% 
  ggplot(aes(climate_var, value)) + 
  geom_boxplot() + 
  facet_wrap(~ climate_var, scales = "free", ncol = 4)

wL %>% filter(climate_var != "precip") %>% 
  ggplot(aes(climate_var, value)) + 
  geom_boxplot() 

##### GET MONTHLY MEANS #####

wMonthly <- wL %>% filter(!is.na(value)) %>%
  group_by(climate_var, year, month) %>% 
  summarise(monthly_mean = mean(value),
            monthly_median = median(value), 
            monthly_max = max(value), 
            monthly_min = min(value), 
            monthly_sd = sd(value), 
            monthly_cv = monthly_sd/monthly_mean * 100) %>%
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

wMonthly <- wL %>% filter(!is.na(value)) %>%
  group_by(climate_var, year, month) %>% 
  summarise(maximum = max(value),
            mean = mean(value), 
            median = median(value), 
            minimum = min(value)) %>%
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

wMonthly %>% 
  ggplot(aes(climate_var, median)) + 
  geom_boxplot() + 
  facet_wrap(~ climate_var, scales = "free")

wMonthly %>% 
  ggplot(aes(climate_var, maximum)) + 
  geom_boxplot() + 
  facet_wrap(~ climate_var, scales = "free")

##### GET ANNUAL TIME SERIES #####

# I want a time-series starting 10 years prior to the first historic sampling year
# Childs sampled Littorina in 1947
# So, start year is 1938
names(wMonthly)

wAnnual <- wMonthly %>% 
  group_by(climate_var, year) %>% 
  summarise(mean = mean(mean), 
            median = mean(median), 
            maximum = mean(maximum), 
            minimum = mean(minimum)) %>% 
  ungroup()

# Get in long format
wAnnualL <- wAnnual %>% gather(key = metric, value = value, mean:minimum)

# write.csv(wAnnualL, "output/air_temp_monterey.csv")
