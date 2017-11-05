################################################################################
##' @title Summarise SST data from HMS website
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-12
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
theme_set(theme_bw(base_size = 12))
library(lubridate)

source("R/temperature_functions.R")

# Get hms temperature data
#source("R/get_hms_sst.R")
sst_hms <- read_csv("output/sst_hms.csv")[-1]
glimpse(sst_hms)

# I want a time-series starting 10 years prior to the first historic sampling year
# Childs sampled Littorina in 1947
# So, start year is 1938
dat <- sst_hms %>% filter(year > 1937 & year < 2016) 

# What are the warmest and coldest months?
dat %>% group_by(month) %>% 
  summarise(meanTemp = mean(tempC)) %>% ungroup() %>% 
  arrange(meanTemp)

# Coldest == Dec, Jan, Feb
# Warmest == July, Aug, Sep

##### SELECT SUMMER, WINTER TEMPS #####

# I will use the top 25th percentile of summer temps and bottom 25th percentile of winter temps
remove_data = F

summer_annual <- get_summer_annual_means(dat, my_quant = 0.75, remove = remove_data)
winter_annual <- get_winter_annual_means(dat, my_quant = 0.25, remove = remove_data)

annual_mean <- dat %>% group_by(year) %>% 
  summarise(mean = mean(tempC, na.rm = TRUE)) %>% 
  ungroup() %>% filter(!is.na(mean))

## Collate annual data
annual_df <- annual_mean %>% 
  mutate(minimum = winter_annual$tempC, 
         maximum = summer_annual$tempC)

# Get in long format
annual_df_long <- annual_df %>% 
  gather(key = metric, value = tempC, mean:maximum) %>%
  filter(!is.na(tempC))

# Resave
sst_annual_long <- annual_df_long %>% 
  mutate(dataset = "Seawater")

# Plot
sst_annual_long %>%
  ggplot(aes(year, tempC, color = metric)) + 
  geom_line() + 
  geom_smooth(method = "lm") 

