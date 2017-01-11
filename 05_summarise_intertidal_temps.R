################################################################################
##' @title Summarise intertidal temperatures
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-06
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# Load size data
source("03_identify_size_cutoff.R")
head(dat4)
spCodes <- dat4 %>% select(species, sp) %>% distinct()

# load lat long info with positions of nearest loggers
sizeLL <- read.csv("output/sizeLL_edit.csv")
head(sizeLL)

sizeLL %>% select(sampleArea, position) %>% distinct()

##### RAW IBUTTON DATA #####
# Load raw ibutton data
# (on gitignore list, I will save this as .RData file)
# rawDat <- read.csv("output/temp_raw_iButton.csv") %>%
#   select(-X) %>% 
#   rename(sp = code) %>% tbl_df
# save(rawDat, file = "output/temp_raw_iButton.RData")
load("output/temp_raw_iButton.RData")
rawDat

# Fix tidal heights
unique(rawDat$position)
unique(loggerPositions_fixed$position)

rawDat2 <- rawDat %>% rename(tidalHT_old = tidalHT)
names(rawDat2)
tail(rawDat2)

rawDat2 <- loggerPositions %>% select(position, tidalHT) %>% 
  inner_join(rawDat2, ., by = "position")
names(rawDat2)

rawDat2 <- inner_join(rawDat2, spCodes, by = "sp")
names(rawDat2)

rawDat2 %>% select(position, tidalHT, tidalHT_old) %>% distinct() %>% 
  mutate(microhabitat = ifelse(grepl("Crack", position) == TRUE, 
                               "crevice", "face"))

# Add category for crevice or not
rawDat2$microhabitat <- ifelse(grepl("Crack", rawDat2$position) == TRUE, 
                               "crevice", "face")

##### MAKE DF LINKING POSITIONS TO SAMPLE AREA FOR SIZE CHANGE ANALYSIS #####

lodi <- sizeLL %>% filter(species == "Lottia digitalis") %>% 
  select(sampleArea, position) %>% distinct()
lodi

sampleAreas <- dat4 %>% filter(species != "Lottia digitalis") %>% 
  select(sampleArea) %>% distinct() %>% unlist(use.names = FALSE)

loggerPositions <- rawDat %>% filter(sp != "LODI") %>% 
  select(position) %>% distinct() %>% unlist(use.names = FALSE)

lpDF <- data.frame(position = loggerPositions)

lpDF$sampleArea <- c(rep("Wara.B", 3), 
                     rep("Wara.D", 5), 
                     rep("HighRock_zoneA", 2), 
                     rep("HighRock_zoneB", 2), 
                     rep("HighRock_zoneC", 2), 
                     rep("HighRock_zoneD", 2))

link_sampleArea_position <- rbind(lpDF, lodi)

sizeLL2 <-  sizeLL %>% #filter(species != "Chlorostoma funebralis") %>% 
  select(sampleArea, position) %>% distinct()

##### DAILY SUMMARY PER POSITION #####

position_daily <- rawDat2 %>%
  group_by(position, sp, day, nest1, nest2, tidalHT, 
           aspect, slope, lon, lat, microhabitat) %>%
  summarise(daily_mean = mean(tempC), 
            daily_med = median(tempC), 
            daily_max = max(tempC),
            daily_min = min(tempC), 
            daily_cv = sd(tempC)/daily_mean) %>% 
  ungroup()

position_daily

position_daily2 <- inner_join(position_daily, spCodes, by = "sp")

# Get in long format
pdL <- position_daily2 %>% gather(key = metric, value = tempC, daily_mean:daily_cv)
head(pdL)
summary(pdL)

# Get means of daily mean, max and min
tempMeans <- pdL %>% 
  group_by(species, position, microhabitat, tidalHT, aspect, slope, lon, lat, metric) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()
tempMeans

##### DAILY SUMMARY PER SAMPLE AREA #####
names(rawDat)
unique(rawDat2$position)

names(rawDat2)
names(sizeLL2)

rawDat3 <- inner_join(rawDat2, sizeLL2, by = "position")
unique(rawDat3$position)
head(rawDat3)

sample_area_daily <- rawDat3 %>%
  group_by(sampleArea, species, day) %>%
  summarise(daily_mean = mean(tempC), 
            daily_med = median(tempC), 
            daily_max = max(tempC),
            daily_min = min(tempC), 
            daily_cv = sd(tempC)/daily_mean) %>% 
  ungroup()

sample_area_daily

# Get in long format
sadL <- sample_area_daily %>% gather(key = metric, value = tempC, 
                                     daily_mean:daily_cv)

# Get means of daily mean, max and min
sadL_means <- sadL %>% 
  group_by(species, sampleArea, metric) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()
