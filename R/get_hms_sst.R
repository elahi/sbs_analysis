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

hms_raw2 <- hms_raw %>% 
  mutate(dateR = as.Date(DATE, origin = "1899-12-30"), 
         year = year(dateR)) %>% 
  rename(time = TIME_COL, tempC = SST) %>% 
  select(dateR, year, time, tempC)

# Get hms raw data after 2004
hms_raw3 <- hms_raw2 %>% filter(year > 2004)

## Get corrected temperature data (up to 2004)
hms_corr <- read.table("./data/HMStemp.corrected.txt", 
                       skip = 13, header = TRUE, na.strings = "NaN")

hms_corr$dateR <- as.Date(with(hms_corr, ISOdate(year, month, day)))
head(hms_corr)

hms_corr2 <- hms_corr %>%
  rename(tempC = new) %>% select(dateR, year, time, tempC) 

## Bind the two datasets
sst_hms <- rbind(hms_corr2, hms_raw3) %>% 
  mutate(month = lubridate::month(dateR))

head(sst_hms)
tail(sst_hms)
