################################################################################
##' @title Cleaning up sbs dataframe
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-28
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# load modern data
dat <- read.csv("./output/sbsMaster.csv", na.strings = "NA", 
                stringsAsFactors = FALSE) %>%
  select(-c(X, row))

##### PREPARE DATA #####

# change ft to meters
dat$tideHTm <- dat$tideHTm/3.28084

# create numeric lat-long columns
dat$lat2 <- as.numeric(substr(dat$lat, 1, 8))
dat$long2 <- as.numeric(paste("-", substr(dat$long, 1, 9), sep = ""))
dat$LL <- with(dat, paste(lat2, long2, sep = ","))

##### DEFINE SAMPLING AREAS #####

### Need to define 'sampling areas' for each species, 
### that were sampled in historic and modern studies

# Littorina keenae = nest1 (zoneA, zoneB, zoneC, zoneD)
dat %>% filter(sp == "LIKE") %>% distinct(nest1)

# Lottia digitalis = site (areaA, areaB, areaC) and nest1 (zone1, zone2, zone3)
dat %>% filter(sp == "LODI") %>% distinct(site, nest1)

# Chlorostoma funebralis = site (Wara.B, Wara.D)
dat %>% filter(sp == "CHFU") %>% distinct(site)

# Create new column called 'sampleArea' based on 
# the areas that were resampled in modern surveys
unique(dat$site)
unique(dat$nest1)

dat$sampleArea <- as.factor(ifelse(dat$sp == "CHFU", 
                                   paste(dat$site), 
                                   paste(dat$site, dat$nest1, sep = "_")))

##### SIMPLIFY DATAFRAME #####

dat2 <- dat %>% 
  select(-c(lat, long)) %>% 
  rename(lat = lat2, long = long2) %>% 
  mutate(year = lubridate::year(dmy(date)))
head(dat2)
unique(dat2$year)
