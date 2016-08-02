################################################################################
##' @title Assembling scraped data, Elahi 2015
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-28
##' 
##' @log 
##' 2016-08-02: added summary stats for 50% cutoff - max size
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# load cleaned up data
source("sbs_size_dataPrep2.R")
head(dat2)

##### IDENTIFY SIZE CUT-OFF #####

medianSizeDF <- dat2 %>% #filter(era == "present") %>%
  group_by(species) %>% 
  summarise(size0.05 = quantile(size1mm, 0.05, na.rm = TRUE), 
            size0.1 = quantile(size1mm, 0.1, na.rm = TRUE), 
            size0.25 = quantile(size1mm, 0.25, na.rm = TRUE), 
            size0.5 = median(size1mm, na.rm = TRUE))
  
dat3 <- inner_join(dat2, medianSizeDF, by = c("species"))
head(dat3)
summary(dat3)

dat3 %>% 
  ggplot(aes(size1mm)) + 
  geom_histogram(binwidth = 1, fill = "gray", col = "black") + 
  facet_wrap(~ era + species, nrow = 2, scales = "free_y") + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red") + 
  labs(x = "Size (mm)", y = "Frequency")
ggsave("sbs_meta/meta_figs/elahi_histo.png", height = 5, width = 7)

head(dat3)

dat4 <- dat3 %>%
  mutate(size_threshold = ifelse(size1mm > size0.05, 
                                 "keep", "remove"))

dat4 %>% group_by(size_threshold, species, era) %>% tally()

##### IDENTIFY 95% MAXIMUM SIZE FOR PAST POPULATIONS #####
pastSnailSummary <- dat2 %>% filter(era == "past") %>% 
  group_by(species) %>% filter(!is.na(size1mm)) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se) %>% 
  ungroup() %>% select(-size_n, -size_sd)

pastSnailSummary$past_max95 <- with(pastSnailSummary, size_mean + size_CI)
pastSnailSummary

##### SUMMARISE DATA - COMPLETE DATASET #####

# # http://www.r-tutor.com/elementary-statistics/interval-estimation/interval-estimate-population-mean-unknown-variance
# survey %>% filter(!is.na(Height)) %>%
#   summarise(size_mean = mean(Height, na.rm = TRUE), 
#             size_sd = sd(Height, na.rm = TRUE),
#             size_n = n(), 
#             size_se = size_sd/sqrt(size_n), 
#             size_CI = qt(0.975, df = size_n - 1) * size_se)


### I want mean size by species-era-sampleArea
summary(dat2)
names(dat2)

datMeans <- dat2 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, sampleArea) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se) %>% 
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
            size_CI = qt(0.975, df = size_n - 1) * size_se) %>% 
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

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dm2)

df_final <- dm2  %>%
  rename(size_rep = size_mean, size_error = size_CI, 
         sample_size = size_n) %>%
  arrange(species, site, year)

head(df_final)

dfMeta <- data.frame(
  study = "Elahi2015", 
  studySub = df_final$studySub, 
  fig_table = "raw_data", 
  species = df_final$species, 
  site = df_final$sampleArea, 
  size_rep = df_final$size_rep, 
  size_units = "mm", 
  size_error = df_final$size_error, 
  size_error_type = "CI", 
  time_rep = NA, 
  time_error = NA, 
  year = df_final$year, 
  year_error = NA, 
  year_error_type = NA, 
  sample_size = df_final$sample_size, 
  sample_size_units = "number of snails"
)

write.csv(dfMeta, "sbs_meta/output/dfMeta_Elahi2015.csv")

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