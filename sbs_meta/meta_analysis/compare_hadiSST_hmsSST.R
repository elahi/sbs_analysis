################################################################################
##' @title Compare hadisst data with hopkins temperature data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-03
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)
library(caTools)   

## hadisst
dfHad <- read.csv("sbs_meta/output/dfHad.csv")
head(dfHad)
had_hms <- dfHad %>% filter(study == "Elahi2015")

##### GET SST FOR HOPKINS #####
## hms sst
hms_raw <- read.table("http://mlo.stanford.edu/HMS-SST.txt",
                      header = TRUE, na.strings = "NaN")

hms_raw2 <- hms_raw %>% 
  mutate(dateR = as.Date(DATE, origin = "1899-12-30"), 
         year = year(dateR)) %>% 
  rename(time = TIME_COL, tempC = SST) %>% 
  select(dateR, year, time, tempC)

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
tail(sst_hms)

##### COMBINE SST WITH HAD #####
head(sst_hms)
glimpse(sst_hms)

sst_hms2 <- sst_hms %>% 
  mutate(tempC = as.numeric(tempC)) %>% 
  group_by(year, month) %>% 
  summarise(tempC = mean(tempC, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(tempSource = "hms_sst")
sst_hms2

head(had_hms)

had_hms2 <- had_hms %>% select(Date, Temperature_C, Year) %>% 
  rename(year = Year, tempC = Temperature_C) %>% 
  mutate(month = lubridate::month(Date), 
         tempSource = "had_sst") %>% 
  select(-Date)
head(had_hms2)

sstDat <- rbind(sst_hms2, had_hms2)
sstDat <- sstDat %>% 
  mutate(dateR = as.Date(ISOdate(year, month, 15)))
sstDat

sstDat2 <- sstDat %>% group_by(tempSource) %>%
  mutate(runMean = runmean(tempC, 12)) %>% ungroup()

sstDat2 %>% filter(year > 1919) %>%
  ggplot(aes(dateR, runMean, color = tempSource)) + 
  geom_line() + 
  labs(x = "", y = "Temperature, C (12-month running mean)")
ggsave("sbs_meta/meta_figs/sst_comparison_v_time.png")


# wide format



sstDat2 %>% filter(year > 1919) %>%
  spread(key = tempSource, value = runMean) %>% 
  ggplot(aes(dateR, runMean, color = tempSource)) + 
  geom_line() + 
  labs(x = "", y = "Temperature, C (12-month running mean)")
ggsave("sbs_meta/meta_figs/sst_comparison_v_time.png")


