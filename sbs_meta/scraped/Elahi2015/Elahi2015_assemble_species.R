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
##' 2018-02-18: updated methods (e.g., uniform distribution for sizes)
################################################################################

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# Function to load cleaned data
source("R/choose_size_data.R")
source("R/choose_size_threshold.R")

# load data - approximated sizes
dat <- choose_size_data(method = "uniform")
names(dat)
unique(dat$sampleArea)
unique(dat$site)

# Fix site for wara
dat <- dat %>% 
  mutate(site = ifelse(sp == "LIKE", as.character(sampleArea), site))

# Size threshold for museum comparison
dat_sub <- choose_size_threshold(dat, era = "combined", my_quantile = 0.5)
dat <- dat %>% mutate(size_threshold = 0)

dat %>% count(species, era)
dat_sub %>% count(species, era)

##### SUMMARISE #####

## Summarise across sample areas
dat_summary_all <- dat %>% 
  group_by(species, sp, year, era, size_threshold) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n()) %>%
  ungroup() %>% 
  mutate(studySub = "threshold_none", 
         museum = FALSE)

dat_summary_sub <- dat_sub %>% 
  group_by(species, sp, year, era, size_threshold) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n()) %>%
  ungroup() %>% 
  mutate(studySub = "threshold_median", 
         museum = TRUE)

dat_summary <- rbind(dat_summary_all, dat_summary_sub)

#### LAT LONG ####

ll_dat <- dat %>% 
  filter(!is.na(lat)) %>% 
  summarise(lat_mean = mean(lat), 
            long_mean = mean(long))
ll_dat

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dat_summary)

df_final <- dat_summary  %>%
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         sample_size_units = "number of snails", 
         time_rep = NA,          
         size_original = "raw", 
         site = "Hopkins Marine Station", 
         lat_mean = ll_dat$lat_mean, 
         long_mean = ll_dat$long_mean) %>% 
  arrange(species, site, year)

head(df_final)

dfMeta <- data.frame(
  study = "Elahi_2015", 
  studySub = df_final$studySub, 
  fig_table = "various", 
  species = df_final$species, 
  site = df_final$site, 
  size_original = df_final$size_original, 
  size_rep = df_final$size_rep, 
  size_units = "mm", 
  size_error = df_final$size_error, 
  size_error_type = "SD", 
  time_rep = df_final$time_rep, 
  time_error = NA, 
  era = df_final$era, 
  year = df_final$year, 
  year_error = NA, 
  year_error_type = NA, 
  sample_size = df_final$sample_size, 
  sample_size_units = df_final$sample_size_units, 
  museum = df_final$museum, 
  size_threshold_mm = df_final$size_threshold, 
  latitude = df_final$lat_mean, 
  longitude = df_final$long_mean
)

write.csv(dfMeta, "sbs_meta/output/dfMeta_Elahi2015_species.csv")
