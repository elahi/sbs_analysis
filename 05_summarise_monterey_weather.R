################################################################################
##' @title Summarise Monterey weather data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-09-13
##' 
##' @log 
##' 2016-11-08 Checking other weather stations against Monterey
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
theme_set(theme_bw(base_size = 12))
library(lubridate)

path_to_weather_data <- "data/climate_monterey/"
weather_file <- "monterey_weather_backup.txt"

### FUNCTION TO LOAD AND CLEAN RAW WEATHER DATA

format_weather_data <- function(path, weather_file, back_up = TRUE){
  
  weather <- read.csv(file = paste(path_to_weather_data, weather_file, sep = ""), 
           skip = 55, header = TRUE, stringsAsFactors = FALSE)
  
  weather <- weather %>% select(Station:Wx) %>%
    mutate(dateR = ymd(Date), 
           month = month(dateR), 
           year = year(dateR)) %>%
    rename(precip = Precip, air_max = Air.max, air_min = min, air_obs = obs)
  
  weather2 <- weather %>% select(Station, dateR, month, year, air_max:air_obs) %>% 
    filter(year < 2016) 
  
  weather2$backup_status <- ifelse(back_up == TRUE, "backup", "backup_no")
  
  return(weather2)
  
}

# Get cleaned weather data
mry_backup <- format_weather_data(path_to_weather_data, "monterey_weather_backup.txt", back_up = TRUE)
head(mry_backup)

mry_backup_no <- format_weather_data(path_to_weather_data, "monterey_weather_backup_no.txt", back_up = FALSE)
head(mry_backup_no)

summary(mry_backup_no)

# Are some months poorly sampled?
# Remove months that have fewer than 15 samples
mry_backup_no %>% filter(!is.na(air_max)) %>% 
  group_by(year, month) %>% tally() %>% ungroup() %>% 
  filter(n < 15)

mry_backup_no2 <- mry_backup_no %>% filter(!is.na(air_max)) %>% 
  group_by(year, month) %>% 
  mutate(n = n()) %>% ungroup()

mry_backup_no3 <- mry_backup_no2 %>% filter(n > 15)

weather <- mry_backup_no3

head(weather)
tail(weather)
summary(weather)
glimpse(weather)

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
wM <- weather %>% group_by(Station, backup_status, year, month) %>% 
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

