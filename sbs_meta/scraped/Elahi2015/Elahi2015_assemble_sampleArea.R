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

# load data - approximated sizes
dat <- choose_size_data(method = "uniform")
names(dat)
unique(dat$sampleArea)
unique(dat$site)

# Fix site for wara
dat <- dat %>% 
  mutate(site = ifelse(sp == "LIKE", as.character(sampleArea), site))

# Exclude smallest snails?
dat %>% 
  ggplot(aes(size1mm, fill = era)) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~ species) + 
  geom_vline(xintercept = 5)

min_threshold <- 0

dat2 <- dat %>% filter(size1mm >= min_threshold)

##### SUMMARISE #####

## Summarise by site
dat_summary <- dat2 %>% 
  group_by(species, sp, site, year, era) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n()) %>%
  ungroup() 

dat_summary

#### LAT LONG ####

ll_dat <- dat %>% 
  filter(!is.na(lat)) %>% 
  group_by(species, site) %>% 
  summarise(lat_mean = mean(lat), 
            long_mean = mean(long)) %>% 
  ungroup()
ll_dat

dat_summary <- left_join(dat_summary, ll_dat, by = c("species", "site"))

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dat_summary)

df_final <- dat_summary  %>%
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         sample_size_units = "number of snails", 
         time_rep = NA,          
         size_original = "raw") %>% 
  arrange(species, site, year)

head(df_final)


dfMeta <- data.frame(
  study = "Elahi_2015", 
  studySub = NA, 
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
  museum = FALSE, 
  size_threshold_mm = 0, 
  latitude = lat_mean, 
  longitude = long_mean
)

write.csv(dfMeta, "sbs_meta/output/dfMeta_Elahi2015.csv")
