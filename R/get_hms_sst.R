################################################################################
##' @title Get SST data from HMS website
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-04
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

##### GET SST FOR HOPKINS #####

## hms sst from website (most current)
hms_raw <- read.table("http://mlo.stanford.edu/HMS-SST.txt",
                      header = TRUE, na.strings = "NaN")

hms_raw %>% filter(is.na(SST)) %>% tally()

# Remove NAs, SST = 18U
hms_raw2 <- hms_raw %>% filter(!is.na(SST)) %>% filter(SST != "18U") %>%
  mutate(dateR = as.Date(DATE, origin = "1899-12-30"), 
         year = year(dateR)) %>% 
  rename(time = TIME_COL, tempC = SST) %>% 
  select(dateR, year, time, tempC)

# Convert tempC to numeric
glimpse(hms_raw2)
hms_raw3 <- hms_raw2 %>% 
  mutate(tempC = as.numeric(as.character(tempC)))
glimpse(hms_raw3)

# Get hms raw data after 2004
hms_raw4 <- hms_raw3 %>% filter(year > 2004)

## Get corrected temperature data (up to 2004)
hms_corr <- read.table("./data/HMStemp.corrected.txt", 
                       skip = 13, header = TRUE, na.strings = "NaN")

hms_corr$dateR <- as.Date(with(hms_corr, ISOdate(year, month, day)))
head(hms_corr)

hms_corr2 <- hms_corr %>%
  rename(tempC = new) %>% select(dateR, year, time, tempC) 

## Bind the two datasets
sst_hms <- rbind(hms_corr2, hms_raw4) %>% 
  mutate(month = lubridate::month(dateR)) %>% 
  filter(!is.na(tempC))

head(sst_hms)
tail(sst_hms)

sst_hms %>% filter(tempC < 8)

write.csv(sst_hms, "output/sst_hms.csv")

##### COMPARE CORRECTED AND UNCORRECTED TIME SERIES #####

head(sst_hms)
head(hms_raw3)
glimpse(sst_hms)
glimpse(hms_raw3)

sst_hms_corr <- sst_hms %>% mutate(hmsTemp = "corrected")
glimpse(sst_hms_corr)

sst_hms_uncorr <- hms_raw3 %>%
  mutate(month = lubridate::month(dateR), 
         hmsTemp = "uncorrected")
glimpse(sst_hms_uncorr)

sst_hms_combined <- rbind(sst_hms_corr, sst_hms_uncorr) %>% filter(!is.na(tempC))
glimpse(sst_hms_combined)

sst_monthly <- sst_hms_combined %>% group_by(hmsTemp, year, month) %>% 
  summarise(monthly_mean = mean(tempC),
            monthly_median = median(tempC), 
            monthly_max = max(tempC), 
            monthly_min = min(tempC), 
            monthly_sd = sd(tempC), 
            monthly_cv = monthly_sd/monthly_mean * 100) %>%
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

sst_monthly %>% 
  ggplot(aes(dateR, monthly_median, color = hmsTemp)) + 
  geom_line()

sst_annual <- sst_monthly %>% group_by(hmsTemp, year) %>% 
  summarise(mean_C = mean(monthly_mean), 
            max_C = mean(monthly_max), 
            min_C = mean(monthly_min), 
            cv_C = mean(monthly_cv)) %>%
  ungroup()

sst_annual %>% 
  ggplot(aes(year, mean_C, color = hmsTemp)) + 
  geom_line()

sst_annual %>% 
  ggplot(aes(year, max_C, color = hmsTemp)) + 
  geom_line()

sst_annual %>% 
  ggplot(aes(year, cv_C, color = hmsTemp)) + 
  geom_line()

