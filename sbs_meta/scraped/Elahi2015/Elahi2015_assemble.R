################################################################################
##' @title Assembling scraped data, Elahi 2015
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-28
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)

# load cleaned up data
source("sbs_size_dataPrep2.R")

##### SUMMARISE DATA #####
head(dat2)

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
  inner_join(datMeans, .,  by = "sampleArea")

dm2

# Replace "." with "_" in species name
dm2$species <- gsub("[.]", "_", dm2$species)


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

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dm2)

df_final <- dm2  %>%
  rename(size_rep = size_mean, size_error = size_CI, 
         sample_size = size_n) %>%
  arrange(species, site, year)

head(df_final)


dfMeta <- data.frame(
  study = "Elahi2015", 
  studySub = NA, 
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
