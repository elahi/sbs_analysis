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

# Get hms temperature data
#source("R/get_hms_sst.R")
sst_hms <- read_csv("output/sst_hms.csv")[-1]
glimpse(sst_hms)

# Get snail size data
source("05_summarise_size_data.R")

# Get hms temperature functions
source("R/hms_sst_functions.R")

# Original data were single, daily measurements
glimpse(sst_hms)

sst_hms %>% 
  ggplot(aes(as.factor(month), tempC)) + geom_boxplot()

# I want a time-series starting 10 years prior to the first historic sampling year
# Childs sampled Littorina in 1947
# So, start year is 1938

sst <- sst_hms %>% filter(year > 1937 & year < 2016) 

# What are the warmest and coldest months?
sst %>% group_by(month) %>% 
  summarise(meanTemp = mean(tempC)) %>% ungroup() %>% 
  arrange(meanTemp)

# Coldest == Dec, Jan, Feb
# Warmest == July, Aug, Sep


##### SUMMARISE MONTHLY #####

sst_monthly <- sst %>% group_by(year, month) %>% 
  summarise(median = median(tempC), 
            maximum = max(tempC), 
            minimum = min(tempC)) %>% 
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

##### GET ANNUAL TIME SERIES #####

# Calculate mean of hottest months for maximum per year
sst_monthly_max <- sst_monthly %>% 
  filter(month == 7 | month == 8 | month == 9) %>% 
  group_by(year) %>% 
  summarise(maximum = mean(maximum)) %>% ungroup()

# Calculate mean of coldest months for minimum per year
sst_monthly_min <- sst_monthly %>% 
  filter(month == 12 | month == 1 | month == 2) %>% 
  group_by(year) %>% 
  summarise(minimum = mean(minimum)) %>% ungroup()

# Calculate mean of median temperatures for median per year
sst_monthly_med <- sst_monthly %>% 
  group_by(year) %>% 
  summarise(median = mean(median)) %>% ungroup()

sst_annual <- inner_join(sst_monthly_max, sst_monthly_min, 
                         by = "year") %>% 
  inner_join(., sst_monthly_med, by = "year")

sst_annual

# Get in long format
sst_annual_long <- sst_annual %>% 
  gather(key = metric, value = tempC, maximum:median)




