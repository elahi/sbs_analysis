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

# Get tidal correction
source("R/buttonPositions.R")

# load data - repeated size bins
dat <- read.csv("./output/sbsMaster.csv", na.strings = "NA", 
                stringsAsFactors = FALSE) %>%
  select(-c(X, row))

# load data - normally approximated sizes
dat_norm <- read.csv("./output/sbsMaster_norm.csv", na.strings = "NA", 
                stringsAsFactors = FALSE) %>%
  select(-c(X, row))

# Add normally approximated sizes to original dataset
dat$size1mm_norm <- dat_norm$size1mm
summary(dat$size1mm_norm)
summary(dat$size1mm)

# par(mfrow = c(1,2))
# with(dat, boxplot(size1mm ~ era + sp, notch = TRUE))
# with(dat_norm, boxplot(size1mm ~ era + sp, notch = TRUE))
# 
# library(ggplot2)
# 
# dat %>%
#   ggplot(aes(sp, size1mm, fill = era)) +
#   geom_violin()
# 
# dat_norm %>%
#   ggplot(aes(sp, size1mm, fill = era)) +
#   geom_violin()
# 
# dat %>% 
#   filter(era == "past") %>%
#   ggplot(aes(size1mm, size1mm_norm)) + 
#   geom_point() + 
#   facet_wrap(~ sp, scales = "free") + 
#   geom_abline(slope = 1, intercept = 0, col = "red")


##### PREPARE DATA #####

# change ft to meters
# dat$tideHTm <- dat$tideHTm/3.28084
# apply tidal correction, using mod1

summary(mod1) # all three species included
summary(mod3) # excluding littorina

dat <- dat %>% 
  mutate(tideHTm_orig = tideHTm/3.28084, 
         tideHTm = tideHTm_orig * coef(mod1)[2] + coef(mod1)[1])

# create numeric lat-long columns
dat$lat2 <- as.numeric(substr(dat$lat, 1, 8))
dat$long2 <- as.numeric(paste("-", substr(dat$long, 1, 9), sep = ""))
dat$LL <- with(dat, paste(lat2, long2, sep = ","))

##### DEFINE SAMPLING AREAS #####

### Need to define 'sampling areas' for each species, 
### that were sampled in historic and modern studies

# Littorina keenae = nest1 (zoneA, zoneB, zoneC, zoneD)
dat %>% filter(sp == "LIKE") %>% distinct(nest1, .keep_all = TRUE)

# Lottia digitalis = site (areaA, areaB, areaC) and nest1 (zone1, zone2, zone3)
dat %>% filter(sp == "LODI") %>% distinct(site, nest1,.keep_all = TRUE)

# Chlorostoma funebralis = site (Wara.B, Wara.D)
dat %>% filter(sp == "CHFU") %>% distinct(site, .keep_all = TRUE)

# Create new column called 'sampleArea' based on 
# the areas that were resampled in modern surveys
unique(dat$site)
unique(dat$nest1)

dat$sampleArea <- as.factor(ifelse(dat$sp == "CHFU", 
                                   paste(dat$site), 
                                   paste(dat$site, dat$nest1, sep = "_")))


##### DEFINE SAMPLING UNITS FOR EACH SPECIES #####

# Littorina keenae = nest1 + nest2 = sampleUnit
dat %>% filter(sp == "LIKE") %>% group_by(sampleUnit) %>% tally()

# Chlorostoma funebralis = nest1 + nest2 = sampleUnit
dat %>% filter(sp == "CHFU") %>% group_by(sampleUnit) %>% tally() 
dat %>% filter(sp == "CHFU") %>% distinct(site, nest1, nest2, .keep_all = TRUE) %>% 
  select(nest1:long) %>% 
  head()

# Lottia digitalis = site + nest1 + nest2 (quadrats within tidal heights)
dat %>% filter(sp == "LODI") %>% group_by(sampleUnit) %>% tally()
dat %>% filter(sp == "LODI") %>% group_by(site, nest1, nest2) %>% tally()

# Fix Lottia in present data
dat2 <- dat %>% 
  mutate(sampleUnit = ifelse(era == "present" & sp == "LODI", 
                             paste(site, nest1, nest2, sep = "_"), sampleUnit))

unique(dat2$sampleUnit)

##### SUMMARISE TIDAL HEIGHT FOR EACH SAMPLE AREA #####

tidalHTdf <- dat2 %>% filter(!is.na(tideHTm)) %>% 
  group_by(sampleArea) %>% 
  summarise(sample_area_tidal_ht = mean(tideHTm), 
            sample_area_tidal_ht_orig = mean(tideHTm_orig))

tidalHTdf %>% 
  ggplot(aes(sample_area_tidal_ht_orig, sample_area_tidal_ht)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0)

# ### Revising values using temp logger measures (ibutton_deployment.csv)
# # get rows that need revising using grep
# high_rows <- grep("High", x = tidalHTdf$sampleArea)
# # save new vector to be revised
# new_tidal_ht <- tidalHTdf$sample_area_tidal_ht
# # replace old values with new ones
# new_tidal_ht[high_rows[1]:high_rows[4]] <- c(2.7, 3.9, 5.9, 6.9)
# # replace in df
# tidalHTdf$sample_area_tidal_ht <- new_tidal_ht

dat2 <- tidalHTdf %>% select(sampleArea, sample_area_tidal_ht) %>% 
  inner_join(dat2, ., by = "sampleArea")

##### TRANSFORM SIZES TO PERCENTAGE OF MAXIMUM SIZE BY SPECIES #####

dat2 <- dat2 %>% group_by(species) %>% 
  mutate(size_prop = size1mm/max(size1mm, na.rm = TRUE)) %>% 
  ungroup()

##### SIMPLIFY DATAFRAME #####

dat2 <- dat2 %>% 
  select(-c(lat, long)) %>% 
  rename(lat = lat2, long = long2) %>% 
  mutate(year = lubridate::year(dmy(date)))
head(dat2)
unique(dat2$year)

##### GET SAMPLE DATES #####

dat2 %>% group_by(species, era) %>% 
  summarise(minDate = min(date), 
            maxDate = max(date))

