################################################################################
##' @title Assembling scraped data, Fisher 2009
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2018-09-28
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

fig2a <- read_csv("sbs_meta/scraped/Fisher2009_raw/Fisher_2009_fig2a.csv") %>% 
  mutate(figure = "fig2a",
         n = ifelse(era == "past", 219, 120))

fig2b <- read_csv("sbs_meta/scraped/Fisher2009_raw/Fisher_2009_fig2b.csv") %>% 
  mutate(figure = "fig2b",
         n = ifelse(era == "past", 436, 256))

fig2c <- read_csv("sbs_meta/scraped/Fisher2009_raw/Fisher_2009_fig2c.csv") %>% 
  mutate(figure = "fig2c",
         n = ifelse(era == "past", 498, 231))

dat <- rbind(fig2a, fig2b, fig2c)
dat <- dat %>% 
  mutate(size1mm = round(size_bin, 0), 
         freq = round(proportion * n, 0))

head(dat)
dat %>% count(size1mm) %>% print(n = 40) # should not be > 6 (3 sites, 2 eras)

# Now sum up across size bins within eras
fisher <- dat %>% 
  group_by(era, size1mm) %>% 
  summarise(freq = sum(freq)) %>% 
  ungroup()

# Sanity check (values should be 24.94 and 31.73mm, for past and present respectively)
fisher_past <- dat %>% filter(era == "past" & figure == "fig2c")
fisher_pres <- dat %>% filter(era == "present" & figure == "fig2c")

# All data
fisher_past <- fisher %>% filter(era == "past")
fisher_pres <- fisher %>% filter(era == "present")

## Get raw sizes (use repeat sizes)
dat_present <- repeat_sizes(size_bin_vector = fisher_pres$size1mm, 
                            count_vector = fisher_pres$freq)
n_present <- length(dat_present)

dat_past <- repeat_sizes(size_bin_vector = fisher_past$size1mm, 
                         count_vector = fisher_past$freq)
n_past <- length(dat_past)

df_past <- data_frame(era = rep("past", n_past), size1mm = dat_past)
df_present <- data_frame(era = rep("present", n_present), size1mm = dat_present)

df <- rbind(df_past, df_present) %>% 
  mutate(year = ifelse(era == "past", 1918, 2007))

df %>% 
  group_by(era) %>% 
  summarise(mean(size1mm))

dat <- data.frame(
  study = "Fisher", 
  studySub = NA, 
  species = "Nucella lapillus", 
  sp = "NULP", 
  site = "Maine",
  era = df$era, 
  date = NA, 
  nest1 = NA, 
  nest2 = NA, 
  nest3 = NA, 
  nest3note = NA, 
  sampleUnit = NA, 
  size1mm = df$size1mm,  
  size1mm_rand = df$size1mm,
  habitat = NA, 
  tideHTm = NA, 
  lat = 44.2553654,   
  long = -68.36960568, 
  Shaw_hab = NA, 
  notes = "", 
  notes2 = "", 
  year = df$year
)

# Save this file
write.csv(dat, "sbs_meta/scraped/Fisher2009_raw/Fisher_raw.csv")

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
         museum = TRUE)

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

ll_dat <- dat %>% summarise(lat_mean = mean(lat), 
                            long_mean = mean(long))

year_dat <- dat %>% 
  group_by(era) %>% 
  summarise(year_mean = mean(year))

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dat_summary)

df_final <- dat_summary %>% 
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         year = rep(c(1918, 2007), 2),  
         site = "Mount Desert Island, Maine", 
         size_original = "raw", 
         time_rep = NA, 
         species = "Nucella lapillus", 
         sample_size_units = "total number of snails") %>%
  arrange(site, year)

head(df_final)

dfMeta <- data.frame(
  study = "Fisher_2009", 
  studySub = df_final$studySub, 
  fig_table = "Figure_1", 
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

write.csv(dfMeta, "sbs_meta/output/dfMeta_Fisher2009_raw.csv")


