#################################################
# Author: Robin Elahi
# Date: 151102

# Temporal shifts in gastropod size-frequency distributions are
# consistent with climate warming

# Summarizing raw temperature data
# HMS
#################################################

library(ggplot2)
theme_set(theme_classic(base_size = 8))
library(dplyr)

rm(list=ls(all=TRUE)) 

# Set working directory to main project folder
setwd("~/github/sbs_analysis")
getwd()

# Source functions
# For plotting
source("./R/multiplotF.R")
# For collating temperature data
source("./R/process_iButtonFiles.R")

# Run function to create collated temperature dataframe
dat <- iButtonTempF(fls = fileNames)
unique(dat$iButtonID)
unique(dat$csvID)
head(dat)

with(dat, iButtonID == csvID)


###########################
# Merge the spreadsheetIDs to bagNames 

# Load file that links bagNames to iButtonIDs
loggers <- read.table("./data/iButton_retrieval.txt", header = TRUE)
unique(loggers$iButtonID)

# Load file that links bagNames to the details of the logger positions
positions <- read.csv("./data/iButton_deployment.csv", header = TRUE)

head(loggers)
head(positions)

envDat <- inner_join(loggers, positions, by = "bagName")
head(envDat)

###########################
# Merge environmental data to temperature data
unique(dat$iButtonID)
unique(envDat$iButtonID)

master <- inner_join(dat, envDat, by = "iButtonID")
head(master)
master$day <- as.Date(master$dateR)

boxplot(tempC ~ code, data = master)

###########################
# quick plots
ggplot(master, aes(dateR, tempC, color = position)) + 
  geom_point(size = 2, alpha = 0.2, pch = 21) + 
  ylab("Temperature (C)") + xlab("Date")	+
  geom_smooth(se = TRUE, size = 1) + facet_wrap(~ code)

ggplot(master, aes(dateR, tempC, color = iButtonID)) + 
  geom_line() +
  ylab("Temperature (C)") + xlab("Date")	+
  facet_wrap(~ position)

###########################
# There were two extra loggers for WaraD, which were deployed later
# Need to remove the first two days of logging (were sitting in the lab)
waraD_extra <- master %>% filter(position == "WaraD_mid2" | 
                                position == "WaraD_low2")

ggplot(waraD_extra, aes(dateR, tempC, color = iButtonID)) + 
  geom_line() +
  ylab("Temperature (C)") + xlab("Date")	+
  facet_wrap(~ position)

# Remove pre 10AM August 2
str(waraD_extra1$dateR)
waraD_extra1 <- waraD_extra %>% filter(dateR > "2015-08-01 10:00:00")

ggplot(waraD_extra1, aes(dateR, tempC, color = iButtonID)) + 
  geom_line() +
  ylab("Temperature (C)") + xlab("Date")	+
  facet_wrap(~ position)

# Now remove these two positions from master
master2 <- master %>% filter(position != "WaraD_mid2" & 
                                          position != "WaraD_low2")

# Add in the corrected positions
master3 <- rbind(master2, waraD_extra1)

ggplot(master3, aes(dateR, tempC, color = iButtonID)) + 
  geom_line() +
  ylab("Temperature (C)") + xlab("Date")	+
  facet_wrap(~ position)

# rename master file
master <- master3

###########################
#Make daily summaries for each iButton
names(master)
head(master)

iButton_daily <- master %>%
  group_by(iButtonID, day, code, nest1, nest2, position, tidalHT, 
           aspect, slope, lon, lat) %>%
  summarise(mean_tempC = mean(tempC, na.rm=T),
            max_tempC = max(tempC, na.rm=T),
            min_tempC = min(tempC, na.rm=T)) %>% 
  ungroup()

write.csv(iButton_daily, "./output/temp_daily_iButton.csv")

###########################
#Make daily summaries for each position (2 buttons per position for most)
unique(master$position)

position_daily <- master %>%
  group_by(position, day, code, nest1, nest2, tidalHT, 
           aspect, slope, lon, lat) %>%
  summarise(mean_tempC = mean(tempC, na.rm=T),
            max_tempC = max(tempC, na.rm=T),
            min_tempC = min(tempC, na.rm=T)) %>% 
  ungroup()

write.csv(position_daily, "./output/temp_daily_position.csv")

###########################
#Make daily summaries for each species 
unique(master$code)

species_daily <- master %>%
  group_by(code, day) %>%
  summarise(mean_tempC = mean(tempC, na.rm=T),
            max_tempC = max(tempC, na.rm=T),
            min_tempC = min(tempC, na.rm=T)) %>% 
  ungroup()

write.csv(position_daily, "./output/temp_daily_species.csv")

###########################
#Make hourly summaries for each species 
unique(master$code)

species_daily <- master %>%
  group_by(code, day) %>%
  summarise(mean_tempC = mean(tempC, na.rm=T),
            max_tempC = max(tempC, na.rm=T),
            min_tempC = min(tempC, na.rm=T)) %>% 
  ungroup()

write.csv(position_daily, "./output/temp_daily_species.csv")

