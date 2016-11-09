################################################################################
##' @title Process Monterey weather data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-12
##' 
##' 2016-11-08 Checking other weather stations against Monterey
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES #####

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

source("R/process_weather_data.R")

path_to_weather_data <- "data/uc_ipm/"

# Need a list of file names
fileNames <- dir(path = path_to_weather_data, recursive = TRUE, 
                 pattern = ".txt") 

fileNames

#### LOAD DATA #####

carmel <- format_weather_data(path = path_to_weather_data, 
                               weather_file = fileNames[1], 
                               lines_to_skip = 61)

kingcity <- format_weather_data(path = path_to_weather_data, 
                              weather_file = fileNames[2], 
                              lines_to_skip = 55)

monterey <- format_weather_data(path = path_to_weather_data, 
                                weather_file = fileNames[3], 
                                lines_to_skip = 55)

salinas <- format_weather_data(path = path_to_weather_data, 
                                weather_file = fileNames[4], 
                                lines_to_skip = 53)

santacruz <- format_weather_data(path = path_to_weather_data, 
                               weather_file = fileNames[5], 
                               lines_to_skip = 61)

watsonville <- format_weather_data(path = path_to_weather_data, 
                                 weather_file = fileNames[6], 
                                 lines_to_skip = 55)

head(watsonville)

weather <- rbind(carmel, kingcity, monterey, salinas, santacruz, watsonville)

##### PRELIM CHECKS #####
# Are some months poorly sampled?
# Remove months that have fewer than 15 samples
weather %>% filter(!is.na(air_max)) %>% 
  group_by(Station, year, month) %>% tally() %>% ungroup() %>% 
  filter(n < 10) %>% View()

weather2 <- weather %>% filter(!is.na(air_max)) %>% 
  group_by(Station, year, month) %>% 
  mutate(n = n()) %>% ungroup()

weather3 <- weather2 %>% filter(n > 10)
unique(weather3$Station)

weather3 %>% group_by(Station) %>% 
  summarise(mean = mean(air_min, na.rm = TRUE))

#### GET WARMEST AND COLDEST MONTHS #####
wL <- weather %>%
  gather(., key = climate_var, value = value, air_max:air_obs)

tail(wL)
glimpse(wL)

# What are the warmest and coldest months?
wL %>% group_by(month, climate_var) %>% 
  summarise(meanTemp = mean(value, na.rm = TRUE)) %>% ungroup() %>% 
  arrange(climate_var, meanTemp) %>% View()

# Coldest == Dec, Jan, Feb
# Warmest == Aug, Sep, Oct

wL %>% filter(!is.na(value)) %>% 
  group_by(Station, month, year, climate_var) %>% tally() %>% 
  filter(n < 28)

##### SUMMARISE MONTHLY #####
# Use air_obs, air_max, air_min
wM <- weather3 %>% group_by(Station, year, month) %>% 
  summarise(median = median(air_obs, na.rm = TRUE), 
            maximum = median(air_max, na.rm = TRUE), 
            minimum = median(air_min, na.rm = TRUE)) %>% 
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

wM
unique(wM$Station)
tail(wM)
summary(wM)

##### GET ANNUAL TIME SERIES #####

# Calculate mean of hottest months for maximum per year
monthly_max <- wM %>% filter(!is.na(maximum)) %>% 
  filter(month == 10 | month == 8 | month == 9) %>% 
  group_by(Station, year) %>% 
  summarise(maximum = mean(maximum), 
            n = n()) %>% ungroup()

monthly_max %>% filter(n < 3)
unique(monthly_max$Station)

monthly_max %>% 
  ggplot(aes(year, maximum, color = as.factor(n))) + 
  geom_point() + 
  facet_wrap(~ Station)

# Calculate mean of coldest months for minimum per year
monthly_min <- wM %>% filter(!is.na(minimum)) %>% 
  filter(month == 12 | month == 1 | month == 2) %>% 
  group_by(Station, year) %>% 
  summarise(minimum = mean(minimum), 
            n = n()) %>% ungroup()
monthly_min %>% filter(n < 3)


# Calculate mean of observed temperatures for median per year
monthly_med <- wM %>% filter(!is.na(median)) %>% 
  group_by(Station, year) %>% 
  summarise(median = mean(median), 
            n = n()) %>% ungroup()

monthly_med %>% filter(n < 3)

monthly_med %>% 
  ggplot(aes(Station, median)) + 
  geom_boxplot()

air_annual <- full_join(monthly_max, monthly_min, 
                         by = c("Station", "year")) %>% 
  full_join(., monthly_med, c("Station", "year"))

monthly_med %>% group_by(Station) %>% 
  summarise(minYr = min(year), 
            maxYr = max(year))

tail(air_annual)

# Get in long format
air_annual_long <- air_annual %>% 
  gather(key = metric, value = tempC, maximum:median) %>%
  filter(!is.na(tempC))

unique(air_annual_long$Station)

tail(air_annual_long)

air_annual_long %>% 
  ggplot(aes(year, tempC, color = metric)) + 
  geom_line(alpha = 1) +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  facet_wrap(~ Station) + 
  theme(strip.background = element_blank()) 



####################
####################
####################
####################
####################

####################
#### LOAD DATA #####

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


##' The HMS SST data were daily measurements, and I calculated monthly max, median and min. These weather data provide max, observed (at a given time), and minimum temperatures daily (2010-2015 do not have observed daily temperatures)
##' 

##' For comparison the HMS SST data, for each month I will calculate the monthly medians for daily max, obs, and min temperatures. 

wMonthly <- wL %>% filter(!is.na(value)) %>%
  group_by(climate_var, year, month) %>% 
  summarise(median = median(value)) %>%
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

# This is what I did for the HMS SST Data
wMonthly <- wL %>% filter(!is.na(value)) %>%
  group_by(climate_var, year, month) %>%
  summarise(mean = mean(value),
            median = median(value),
            maximum = max(value),
            minimum = min(value),
            sd = sd(value),
            cv = sd/mean * 100) %>%
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
head(wMonthly)

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
