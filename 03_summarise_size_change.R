################################################################################
##' @title Summarising snail size frequency data - change over time
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-05
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##' To minimize the possibility that we sampled the smallest size classes more efficiently than the historic investigators, I will identify a minimum size cut-off for each of the three species

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# load cleaned up data
source("02_sbs_size_dataPrep2.R")
head(dat2)

##### IDENTIFY SIZE CUT-OFF #####

##' To be conservative, I will set the minimum size to be above the 5% quantile size for each species from the historic data

cutoffDF <- dat2 %>% filter(era == "past") %>%
  group_by(species) %>% 
  summarise(size0.05 = quantile(size1mm, 0.05, na.rm = TRUE), 
            size0.1 = quantile(size1mm, 0.1, na.rm = TRUE), 
            size0.25 = quantile(size1mm, 0.25, na.rm = TRUE))
  
dat3 <- inner_join(dat2, cutoffDF, by = c("species"))

dat3 %>% 
  ggplot(aes(size1mm)) + 
  geom_histogram(binwidth = 1, fill = "gray", col = "black") + 
  facet_wrap(~ era + species, nrow = 2, scales = "free_y") + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red") + 
  labs(x = "Size (mm)", y = "Frequency")

ggsave("figs/elahi_histo_cutoff.png", height = 5, width = 7)

# Create column to identify whether the size is above the arbitrary cut-off
dat4 <- dat3 %>%
  mutate(size_threshold = ifelse(size1mm > size0.05, 
                                 "keep", "remove"))

dat4 %>% group_by(size_threshold, species, era) %>% tally()

##### SUMMARISE DATA - COMPLETE DATASET #####

### I want mean size by species-era-sampleArea

datMeans <- dat4 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, sampleArea) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_max = quantile(size1mm, 0.95)) %>% 
  ungroup()

datMeans

##### SUMMARISE DATA - SUBSET OF DATASET #####

dat5 <- dat4 %>% filter(size_threshold == "keep")

datMeansSub <- dat5 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, sampleArea) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_max = quantile(size1mm, 0.95)) %>% 
  ungroup()

datMeansSub

##### COMBINE 2 SUMMARIES #####

datMeans2 <- datMeans %>% mutate(studySub = "complete")
datMeansSub2 <- datMeansSub %>% mutate(studySub = "subset")

dm <- rbind(datMeans2, datMeansSub2)

##### JOIN ENVIRONMENT INFO #####

### Summarise lat-longs and tidal heights
envDat <- dat2 %>% filter(era == "present") %>%
  group_by(species, site, era, sampleArea) %>%
  summarise(lat_mean = mean(lat), 
            long_mean = mean(long), 
            tidalHeight = mean(tideHTm)) %>%
  ungroup()

### Join snail data with env data
names(envDat)
names(datMeans)

dm2 <- envDat %>% select(sampleArea, lat_mean:tidalHeight) %>%
  inner_join(dm, .,  by = "sampleArea")

dm2

# Replace "." with "_" in species name
dm2$species <- gsub("[.]", "_", dm2$species)

##### PLOT MEAN SIZES BY DATA SET #####

glimpse(dm2)

dm2 %>% 
  ggplot(aes(year, size_mean, color = sampleArea)) + 
  geom_point() + geom_line(linetype = "dashed") + 
  facet_grid(studySub ~ species) + 
  theme(legend.position = "none") + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 3) + 
  labs(x = "Year", y = "Mean size (mm)")

ggsave("figs/elahi_size_change_summary.png", height = 3.5, width = 7)

##### PLOT MEAN CHANGE BY TIDAL HEIGHT #####

datSub <- dm2 %>% filter(studySub == "subset") %>% 
  select(-c(size_se:size_CI, studySub))
datSub

# Get in wide format
datSubW <- datSub %>% group_by(species, sampleArea) %>% 
  mutate(size_mean2 = lead(size_mean), 
         size_max2 = lead(size_max)) %>%
  ungroup() %>% filter(!is.na(size_mean2)) %>% 
  select(-c(era, year))
datSubW

datSubW2 <- datSubW %>% 
  mutate(delta_mean = size_mean2 - size_mean, 
         delta_mean_per = delta_mean/size_mean * 100, 
         delta_max = size_max2 - size_max, 
         delta_max_per = delta_max/size_max * 100)

datSubW2 %>% 
  ggplot(aes(tidalHeight, delta_mean_per, color = species)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") # + 
  geom_smooth(method = "lm")

datSubW2 %>% 
  ggplot(aes(tidalHeight, delta_max_per, color = species)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") # + 
  geom_smooth(aes(color = NULL), method = "lm") 

  
  
##### BRIEF MAPPING INTERLUDE #####

datLL <- dat %>% filter(era == "present") %>%
  select(sp, site, lat2, long2) %>% distinct()

llSummary <- datLL %>% group_by(sp, site) %>%
  summarise(meanLat = mean(lat2, na.rm = TRUE), 
            meanLong = mean(long2, na.rm = TRUE)) 
llSummary

library(ggmap)

hms1 <- get_map(location = c(lon = -121.9045, lat = 36.6218),
                color = "color", source = "google",
                maptype = "satellite", zoom = 18)

hmsMap <- ggmap(hms1, extent = "device",
                ylab = "Latitude", xlab = "Longitude")

hmsMap + 
  geom_point(aes(meanLong, meanLat, color = sp, 
                 shape = sp), data = llSummary)

hmsMap + 
  geom_point(aes(long2, lat2, color = sp, 
                 shape = sp), data = datLL)

llSummary %>% ungroup() %>%
  summarise(grandLat = mean(meanLat), 
            grandLong = mean(meanLong))