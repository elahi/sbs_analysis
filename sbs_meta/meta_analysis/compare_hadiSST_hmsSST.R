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

##### COMPARE THE TEMPERATURE SLOPES ACROSS SOURCE FOR EACH SPECIES #####
elahi_dat <- dat4 %>% filter(study == "Elahi2015")

sstDat3 <- sstDat2 %>% mutate(study = "Elahi2015")

get_hms_had_series <- function(sst_df = sstDat3, size_data) {
  
  spYrs <- size_data %>% select(study, species, year, year2) %>%
    distinct() %>%
    mutate(hadYr1 = floor(year), 
           hadYr2 = floor(year2), 
           study = as.character(study)) %>% 
    group_by(study, species) %>% slice(1) %>%
    ungroup() %>% filter(!is.na(hadYr2)) %>% 
    select(-c(year, year2))
  
  dat.i <- spYrs
  
  sst.i <- inner_join(sst_df, dat.i, by = "study") %>% 
    mutate(species1 = ifelse(year >= hadYr1 & year <= hadYr2, 
                             "keep", "remove")) %>% 
    filter(species1 == "keep") %>% select(-c(species1))
  
  return(sst.i)
}

test1 <- elahi_dat %>% group_by(species) %>% 
  do(get_hms_had_series(size_data = .)) %>% ungroup()

test1 %>% 
  ggplot(aes(dateR, runMean, color = tempSource)) +
  geom_line() + 
  facet_wrap(~ species)

# Get annual metrics of temperature
dfAnnual <- test1 %>% group_by(tempSource, study, species, year) %>%
  summarise(mean_C = mean(tempC), 
            max_C = max(tempC), 
            min_C = min(tempC)) %>%
  ungroup()

dfAnnual %>% 
  ggplot(aes(year, mean_C, col = tempSource)) + 
  geom_line(alpha = 0.5) +
  facet_wrap(~ species) + 
  geom_smooth(method = "lm")
ggsave("sbs_meta/meta_figs/elahi_tempC_v_time.png", width = 7, height = 3.5)

##### COMPARE THE TEMPERATURE PERIODS ACROSS SOURCE FOR EACH SPECIES #####


get_hms_had_two_periods <- function(sst_df = sstDat3, size_data, duration = 10) {
  
  nYrsPrior <- duration - 1
  
  spYrs <- size_data %>% select(study, species, year, year2) %>%
    distinct() %>%
    mutate(hadPres1 = floor(year2 - nYrsPrior),
           hadPres2 = floor(year2), 
           hadPast1 = floor(year - nYrsPrior), 
           hadPast2 = floor(year), 
           study = as.character(study)) %>% 
    group_by(study, species) %>% slice(1) %>%
    ungroup() %>% filter(!is.na(year)) %>% 
    select(-c(year, year2))
  
  dat.i <- spYrs 
  
  sst.i <- inner_join(sst_df, dat.i, by = "study") %>% 
    mutate(past_data = ifelse(year >= hadPast1 & year <= hadPast2, 
                              "past", NA), 
           pres_data = ifelse(year >= hadPres1 & year <= hadPres2, 
                              "pres", NA)) %>% 
    filter(!is.na(past_data) | !is.na(pres_data)) %>% 
    mutate(era = ifelse(is.na(past_data), "present", past_data)) %>% 
    select(-c(hadPres1:pres_data))
  
  return(sst.i)
  
}

test2 <- elahi_dat %>% group_by(species) %>% 
  do(get_hms_had_two_periods(size_data = .)) %>% ungroup()

dodge <- position_dodge(width = 0.9)

test2 %>% 
  ggplot(aes(tempSource, tempC, fill = era)) +
  geom_violin(position = dodge) + 
  geom_boxplot(width = 0.3, position = dodge, notch = TRUE) + 
  facet_wrap(~ species)

test2 %>% 
  ggplot(aes(tempSource, tempC, fill = era)) +
  geom_boxplot(position = dodge, notch = TRUE) + 
  facet_wrap(~ species)
ggsave("sbs_meta/meta_figs/elahi_tempC_v_era.png", width = 7, height = 3.5)

test2 %>%
  ggplot(aes(dateR, tempC, color = tempSource)) + 
  geom_line() + 
  facet_wrap(~ species + era, ncol = 2, scales = "free_x")
ggsave("sbs_meta/meta_figs/elahi_tempC_v_time_era.png", width = 7, height = 7)

# Get annual metrics of temperature
dfAnnual2 <- test2 %>% group_by(era, tempSource, study, species, year) %>%
  summarise(mean_C = mean(tempC), 
            max_C = max(tempC), 
            min_C = min(tempC)) %>%
  ungroup()

dfAnnual2 %>% 
  ggplot(aes(tempSource, mean_C, fill = era)) +
  geom_boxplot(position = dodge) + facet_wrap(~ species)

dfAnnual2 %>% 
  ggplot(aes(tempSource, max_C, fill = era)) +
  geom_boxplot(position = dodge) + facet_wrap(~ species)

dfAnnual2 %>% 
  ggplot(aes(tempSource, min_C, fill = era)) +
  geom_boxplot(position = dodge) + facet_wrap(~ species)

