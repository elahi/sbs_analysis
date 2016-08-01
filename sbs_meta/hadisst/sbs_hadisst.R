################################################################################
##' @title Extracting hadsst data for sites in meta-analysis
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-01
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

library(raster)
library(xts)
library(caTools)   

# Get coordinates for each study
dat <- read.csv("sbs_meta/hadisst/sbs_hadisst_gps.csv")
dat

###### PREPARE SST DATA #####

## Function from Jillian Dunic
load_hadsst <- function(file = "../bigFiles/HadISST_sst.nc") {
  b <- raster::brick(file)
  raster::NAvalue(b) <- -1000
  return(b)
}

sst <- load_hadsst()
names(sst)

Date <- substr(names(sst), 2, 11) 
Date
Date <- gsub('\\.', '\\-', Date)
Date <- as.Date(Date)
head(Date)

###### EXTRACT TEMPERATURES FOR EACH SET OF COORDINATES #####

dat

study <- unique(dat$study)
i = 1

study.i <- study[i]
dat.i <- dat %>% filter(study == study.i)

tserie.i <- as.vector(extract(sst, cbind(dat.i$hadLong, dat.i$hadLat)))

df.i <- data.frame(Date, tserie.i) %>%
  mutate(study = study.i)

tserie.df <- df.i

tserie.df %>% 
  mutate(runMean = runmean(tserie.i, 12)) %>% 
  ggplot(aes(Date, tserie.i)) + geom_line(col = "gray") + 
  geom_line(aes(Date, runMean), col = "red") %>%
  facet_wrap(~ study)

## Repeat for other studies

for(i in 2:length(study)){
  
  study.i <- study[i]
  
  dat.i <- dat %>% filter(study == study.i)
  
  tserie.i <- as.vector(extract(sst, cbind(dat.i$hadLong, dat.i$hadLat)))
  
  df.i <- data.frame(Date, tserie.i) %>%
    mutate(study = study.i)
  
  tserie.df <- rbind(tserie.df, df.i) 
  
}

# Rename and calculate moving average (12-month)
dfHad <- tserie.df %>% group_by(study) %>% 
  mutate(Moving_average_C = runmean(tserie.i, 12), 
         Year = year(Date)) %>% 
  ungroup() %>%
  rename(Temperature_C = tserie.i) %>%
  inner_join(., dat, by = "study")

dfHad

# Combine
dfAnnual <- dfHad %>% group_by(study, Year) %>%
  summarise(mean_C = mean(Temperature_C), 
            max_C = max(Temperature_C), 
            min_C = min(Temperature_C), 
            hadYear1 = mean(hadYear1), 
            hadYear2 = mean(hadYear2)) %>%
  ungroup()

dfAnnL <- gather(dfAnnual, key = metric, value = Temperature_C, 
                 mean_C:min_C)
glimpse(dfAnnL)

###### PLOT COMPLETE TRENDS #####
dat

dfHad %>% 
  ggplot(aes(Date, Moving_average_C, col = study)) + geom_line() 

dfAnnL %>% 
  ggplot(aes(Year, Temperature_C, col = metric)) + 
  geom_line(alpha = 0.5) +
  facet_wrap(~ study) + 
  geom_vline(aes(xintercept = hadYear1), linetype = "dashed") + 
  geom_vline(aes(xintercept = hadYear2), linetype = "dashed") + 
  geom_smooth(span = 0.1)

ggsave("sbs_meta/meta_figs/hadisst_by_study.png", width = 7, height = 7)



