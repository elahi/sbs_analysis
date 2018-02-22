################################################################################
##' @title Assembling scraped data, Sagarin 2010
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2018-02-22
##' 
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

source("R/convert_histo_to_raw.R")
source("R/choose_size_threshold.R")

## Six bins between 20mm (Figure 2)
## Each bin = 3.333 mm
## So midpoint is ~ 1.666 mm

hist_past <- read_csv("sbs_meta/scraped/Sagarin_2010/Sagarin_2010_past.csv", col_names = F) %>% 
  mutate(era = "present") %>% 
  mutate(b = 1.66, # my desired incremental value
         size_bin = round(X1 / b) * b, 
         count = round(X2))

hist_pres <- read_csv("sbs_meta/scraped/Sagarin_2010/Sagarin_2010_present.csv", col_names = F) %>% 
  mutate(era = "past") %>% 
  mutate(b = 1.66, # my desired incremental value
         size_bin = round(X1 / b) * b, 
         count = round(X2))

# Remove values 0 or less
hist_pres <- hist_pres %>% filter(count > 0)

## Get raw sizes (use repeat sizes)
dat_past <- repeat_sizes(size_bin_vector = hist_past$size_bin, count_vector = hist_past$count)
hist(dat_past, breaks = 30)
n_past <- length(dat_past)

dat_present <- repeat_sizes(size_bin_vector = hist_pres$size_bin, count_vector = hist_pres$count)
hist(dat_present)
n_present <- length(dat_present)

## Compile dataset
dat <- data_frame(era = c(rep("past", n_past), rep("present", n_present)), 
                  size1mm = c(dat_past, dat_present))

dat %>% count(era)
dat %>% 
  ggplot(aes(size1mm, fill = era)) + 
  geom_histogram(binwidth = 3.3) + 
  facet_wrap(~ era)

# Size threshold for museum comparison
dat <- dat %>% mutate(size_threshold = 0)
dat_sub <- choose_size_threshold_general(dat, era = "combined", my_quantile = 0.5)

dat %>% count(era)
dat_sub %>% count(era)

##### SUMMARISE #####

## Summarise across sample areas
dat_summary_all <- dat %>% 
  group_by(era, size_threshold) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n()) %>%
  ungroup() %>% 
  mutate(studySub = "threshold_none", 
         museum = FALSE)

dat_summary_sub <- dat_sub %>% 
  group_by(era, size_threshold) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n()) %>%
  ungroup() %>% 
  mutate(studySub = "threshold_median", 
         museum = TRUE)

dat_summary <- rbind(dat_summary_all, dat_summary_sub)
dat_summary

##### GET LAT LONGS #####

ll_dat <- data_frame(lat_mean = 28.08, 
            long_mean = -114.61)

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dat_summary)

df_final <- dat_summary %>% 
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         year = rep(c("1900-1950", "2000-2010"), 2),  
         site = "Baja California", 
         size_original = "raw", 
         time_rep = NA, 
         species = "Plicopurpura columellaris", 
         sample_size_units = "total number of snails") %>%
  arrange(site, year)

head(df_final)


dfMeta <- data.frame(
  study = "Sagarin_2010", 
  studySub = df_final$studySub, 
  fig_table = "Figure_2", 
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
  museum = TRUE, 
  size_threshold_mm = df_final$size_threshold,
  latitude = ll_dat$lat_mean, 
  longitude = ll_dat$long_mean
)

dfMeta

write.csv(dfMeta, "sbs_meta/output/dfMeta_Sagarin_2010.csv")
