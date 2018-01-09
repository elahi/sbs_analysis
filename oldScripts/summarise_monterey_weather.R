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
unique(monterey$backup_status)
unique(monterey$Station)

dat <- monterey %>% rename(tempC = air_obs) %>% 
  select(dateR, month, year, tempC) %>% tbl_df()


##### SELECT SUMMER, WINTER TEMPS #####

# I will use the top 25th percentile of summer temps and bottom 25th percentile of winter temps
remove_data = T

summer_annual <- get_summer_annual_means(dat, my_quant = 0.75, remove = remove_data)
winter_annual <- get_winter_annual_means(dat, my_quant = 0.25, remove = remove_data)

annual_mean <- dat %>% group_by(year) %>% 
  summarise(mean = mean(tempC, na.rm = TRUE)) %>% 
  ungroup() %>% filter(!is.na(mean))

## Collate annual data
summer_annual <- summer_annual %>% filter(year < 2011)
winter_annual <- winter_annual %>% filter(year < 2011)

annual_df <- annual_mean %>% 
  mutate(minimum = winter_annual$tempC, 
         maximum = summer_annual$tempC)

# Get in long format
annual_df_long <- annual_df %>% 
  gather(key = metric, value = tempC, mean:maximum) %>%
  filter(!is.na(tempC))

# Resave
air_annual_long <- annual_df_long %>% 
  mutate(dataset = "Air")

# Plot
air_annual_long %>%
  ggplot(aes(year, tempC, color = metric)) + 
  geom_line() + 
  geom_smooth(method = "lm") 

