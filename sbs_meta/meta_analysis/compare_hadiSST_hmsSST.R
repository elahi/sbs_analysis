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

## hms sst
source("R/get_hms_sst.R")

##### COMBINE HMS WITH HAD #####

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
  mutate(runMean = runmean(tempC, 12)) %>% ungroup() %>% 
  filter(year > 1919)

##### TIME-SERIES PLOTS #####

sstDat2 %>%
  ggplot(aes(dateR, runMean, color = tempSource)) + 
  geom_line() + 
  labs(x = "", y = "Temperature (C)\n(12-month running mean)") + 
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) + 
  theme(legend.title = element_blank())

# ggsave("sbs_meta/meta_figs/sst_comparison_v_time.png", height = 3.5, width = 5)


# wide format

sstDatW <- sstDat2 %>% select(tempSource, dateR, runMean, month) %>% 
  spread(key = tempSource, value = runMean) 

sstDatW %>% 
  ggplot(aes(had_sst, hms_sst)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") + 
  labs(x = "SST-HADI (C)", y = "SST-HMS (C)") + 
  facet_wrap(~ month)

# ggsave("sbs_meta/meta_figs/had_v_hms_sst.png", height = 7, width = 7)

##### COMPARE RELEVANT PERIODS #####


