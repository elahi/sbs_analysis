################################################################################
##' @title Processing hadisst data for sites in meta-analysis
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

source("sbs_meta/meta_analysis/01_assemble_size_data.R")
head(dat4)

source("sbs_meta/hadisst/hadisst_functions.R")

# Get extracted hadisst data
dfHad <- read.csv("sbs_meta/output/dfHad.csv")
head(dfHad)

##### GET HADISST FOR EACH SPECIES - ENTIRE DURATION #####

# Use do to apply function across each species
datHad <- dat4 %>% group_by(species) %>% 
  do(get_had_series(size_data = .)) %>% ungroup() %>% 
  mutate(Date = ymd(Date))

datHad2 <- datHad %>% group_by(species) %>% 
  mutate(runMean1 = runmean(Temperature_C, 12), 
         runMean10 = runmean(Temperature_C, 12*10)) %>% ungroup()
head(datHad2)

datHad2 %>% 
  ggplot(aes(Date, runMean1, col = species)) + 
  geom_line() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ study + species)

# Get annual metrics of temperature
dfAnnual <- datHad2 %>% group_by(study, species, Year) %>%
  summarise(mean_C = mean(Temperature_C), 
            max_C = max(Temperature_C), 
            min_C = min(Temperature_C)) %>%
  ungroup()

dfAnnL <- gather(dfAnnual, key = metric, value = Temperature_C, 
                 mean_C:min_C)
glimpse(dfAnnL)

dfAnnL %>% 
  ggplot(aes(Year, Temperature_C, col = metric)) + 
  geom_line(alpha = 0.5) +
  facet_wrap(~ study + species) + 
  geom_smooth(method = "lm")

##### GET HADISST FOR 10 YEAR PERIODS FOR EACH SPECIES #####

# Use do to apply function across each species
datHad <- dat4 %>% group_by(species) %>% 
  do(get_had_two_periods(size_data = .)) %>% ungroup() %>% 
  mutate(Date = ymd(Date))

datHad2 <- datHad %>% group_by(species, era) %>% 
  mutate(runMean1 = runmean(Temperature_C, 12), 
         runMean10 = runmean(Temperature_C, 12*10)) %>% ungroup()
head(datHad2)

datHad2 %>% 
  ggplot(aes(era, Temperature_C, fill = species)) + 
  geom_boxplot(notch = TRUE) + 
  facet_wrap(~ study + species) + 
  geom_boxplot(aes(era, runMean1), notch = TRUE, col = "black", width = 0.5)

# Get annual metrics of temperature
dfAnnual <- datHad2 %>% group_by(study, species, era, Year) %>%
  summarise(mean_C = mean(Temperature_C), 
            max_C = max(Temperature_C), 
            min_C = min(Temperature_C)) %>%
  ungroup()

dfAnnual

dfAnnL <- gather(dfAnnual, key = metric, value = Temperature_C, 
                 mean_C:min_C)
glimpse(dfAnnL)

dfAnnL %>% 
  ggplot(aes(era, Temperature_C, col = metric)) + 
  geom_boxplot(notch = FALSE) + 
  facet_wrap(~ study + species) 

# Get era metrics of temperature
dfEra <- dfAnnL %>% group_by(study, species, metric, era) %>%
  summarise(mean1 = mean(Temperature_C), 
            sd1 = sd(Temperature_C), 
            n1 = n(), 
            se1 = sd1/sqrt(n1), 
            CI1 = qt(0.975, df = n1 - 1) * se1) %>%
  ungroup() 

dfEra

# Calculate change
dfEra2 <- dfEra %>% group_by(study, species, metric) %>%
  mutate(delta_C = lead(mean1) - mean1) %>% ungroup()

dfEra3 <- dfEra2 %>% filter(!is.na(delta_C)) %>%
  select(species, metric, mean1, CI1, delta_C)

dfEra3
