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

#rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
theme_set(theme_bw(base_size = 12))
library(lubridate)

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

monterey3 <- monterey2 %>% filter(n > 15)

weather <- monterey3

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

##### SUMMARISE MONTHLY #####
# Use air_obs, air_max, air_min
wM <- weather %>% group_by(Station, year, month) %>% 
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

##### GET DECADAL MEANS #####

weather2 <- weather %>% 
  mutate(era = ifelse(year < 1961, "past", 
                      ifelse(year > 2000, "present", "between"))) %>% 
  filter(era != "between")

decade_max <- weather2 %>% group_by(era, month) %>% 
  summarise(mean = mean(air_max, na.rm = TRUE), 
            sd = sd(air_max, na.rm = TRUE),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se,
            upper = mean + CI, 
            lower = mean - CI)

decade_max_long <- decade_max %>% 
  select(era, month, mean, upper, lower) %>% 
  gather(key = summary_stat, value = tempC, mean:lower) %>% 
  mutate(line_type = ifelse(summary_stat == "mean", "average", "ci"), 
         metric = "maximum")

decade_min <- weather2 %>% group_by(era, month) %>% 
  summarise(mean = mean(air_min, na.rm = TRUE), 
            sd = sd(air_min, na.rm = TRUE),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se,
            upper = mean + CI, 
            lower = mean - CI)

decade_min_long <- decade_min %>% 
  select(era, month, mean, upper, lower) %>% 
  gather(key = summary_stat, value = tempC, mean:lower) %>% 
  mutate(line_type = ifelse(summary_stat == "mean", "average", "ci"), 
         metric = "minimum")

decade_obs <- weather2 %>% group_by(era, month) %>% 
  summarise(mean = mean(air_obs, na.rm = TRUE), 
            sd = sd(air_obs, na.rm = TRUE),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se,
            upper = mean + CI, 
            lower = mean - CI)

decade_obs_long <- decade_obs %>% 
  select(era, month, mean, upper, lower) %>% 
  gather(key = summary_stat, value = tempC, mean:lower) %>% 
  mutate(line_type = ifelse(summary_stat == "mean", "average", "ci"), 
         metric = "median")

decade_dat <- rbind(decade_max_long, decade_min_long, decade_obs_long)
decade_dat

##### SEASONAL PLOTS #####
  
decade_dat %>% 
  ggplot(aes(month, tempC, group = interaction(era, summary_stat, metric))) + 
  geom_line(aes(linetype = line_type, color = era, size = line_type)) + 
  #facet_wrap(~ metric, ncol = 3) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  scale_size_manual(values = c(0.75, 0.25)) + 
  theme(strip.background = element_blank()) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Month") +
  ggtitle("Air") + 
  scale_x_continuous(breaks = seq(1, 12, 1)) + 
  guides(linetype = FALSE, size = FALSE) + 
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) + 
  theme(legend.title = element_blank()) 

#ggsave("figs/seasonal_era_temp_air.png", height = 3.5, width = 3.5)



