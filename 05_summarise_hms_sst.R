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

# rm(list=ls(all=TRUE)) 

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

##### GET TIME SERIES BY SPECIES #####

temp_series_dat <- get_hms_sst_series(size_data = dat4)

##### GET MONTHLY MEANS #####

## Entire hms dataset
sst_monthly <- sst_hms %>% group_by(year, month) %>% 
  summarise(monthly_mean = mean(tempC),
            monthly_median = median(tempC), 
            monthly_max = max(tempC), 
            monthly_min = min(tempC), 
            monthly_sd = sd(tempC), 
            monthly_cv = monthly_sd/monthly_mean * 100) %>%
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

## For each species
sst_monthly_species <- temp_series_dat %>% group_by(species, year, month) %>% 
  summarise(monthly_mean = mean(tempC),
            monthly_median = median(tempC), 
            monthly_max = max(tempC), 
            monthly_min = min(tempC), 
            monthly_sd = sd(tempC), 
            monthly_cv = monthly_sd/monthly_mean * 100) %>%
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

# Get in long format
sst_monthly_species_L <- sst_monthly_species %>% 
  gather(key = metric, value = tempC, monthly_mean:monthly_cv)

##### GET ANNUAL TIME SERIES #####

# I want a time-series starting 10 years prior to the first historic sampling year
# Childs sampled Littorina in 1947
# So, start year is 1938

elahi_sst_monthly <- sst_monthly %>% filter(year > 1936) %>% 
  select(-monthly_sd)

elahi_sst <- elahi_sst_monthly %>% filter(year > 1936) %>% 
  group_by(year) %>% 
  summarise(mean_C = mean(monthly_mean), 
            med_C = mean(monthly_median), 
            max_C = mean(monthly_max), 
            min_C = mean(monthly_min), 
            cv_C = mean(monthly_cv))

# Get in long format
monthly_sstL <- elahi_sst_monthly %>% 
  gather(key = metric, value = tempC, monthly_mean:monthly_cv)

sstL <- elahi_sst %>% gather(key = metric, value = tempC, mean_C:cv_C)

### Do the same, but for each species separately

head(sst_monthly_species)

species_sst_annual <- sst_monthly_species %>% 
  group_by(species, year) %>% 
  summarise(mean_C = mean(monthly_mean), 
            med_C = mean(monthly_median), 
            max_C = mean(monthly_max), 
            min_C = mean(monthly_min), 
            cv_C = mean(monthly_cv)) %>%
  ungroup()

sp_sst_ann_L <- species_sst_annual %>% 
  gather(key = metric, value = tempC, med_C:min_C)

sp_sst_ann_L %>% 
  ggplot(aes(year, tempC, color = metric)) + 
  geom_point() + 
  facet_wrap(~ species, nrow = 3)

##### GET YEARS OF SNAIL SAMPLES #####
head(dat4)

spYrs <- dat4 %>% ungroup() %>% select(species, year) %>% distinct() %>% 
  arrange(species, year) %>% 
  group_by(species) %>% 
  mutate(genus = unlist(strsplit(as.character(species), split = " +"))[1]) %>%
  ungroup()

spYrs

spYrs$genus <- factor(spYrs$genus, levels = c("Chlorostoma", "Lottia", "Littorina"))

##### GET TEMPERATURE BY ERA #####

temp_period_dat <- get_hms_two_periods(size_data = dat4)

##### SUMMARISE TEMPERATURE BY ERA #####

names(temp_period_dat)

dodge <- position_dodge(width = 0.9)

# Daily temperature comparison
temp_period_dat %>% 
  ggplot(aes(species, tempC, fill = era)) +
  geom_boxplot(position = dodge, notch = TRUE) 

## Get monthly values
temp_monthly <- temp_period_dat %>% group_by(species, era, year, month) %>% 
  summarise(monthly_mean = mean(tempC), 
            monthly_med = median(tempC), 
            monthly_max = max(tempC), 
            monthly_min = min(tempC), 
            monthly_sd = sd(tempC), 
            monthly_cv = monthly_sd/monthly_mean * 100) %>%
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

temp_monthlyL <- temp_monthly %>% gather(key = metric, value = tempC, 
                                         monthly_mean:monthly_min)

## Get annual values
temp_annual <- temp_monthly %>% 
  group_by(species, era, year) %>% 
  summarise(mean_C = mean(monthly_mean),
            med_C = mean(monthly_med), 
            max_C = mean(monthly_max), 
            min_C = mean(monthly_min), 
            cv_C = mean(monthly_cv))

# Get in long format
temp_annualL <- temp_annual %>% gather(key = metric, value = tempC, mean_C:cv_C)
