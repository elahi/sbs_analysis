################################################################################
##' @title Assembling scraped data, Fisher 2009
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2018-02-18
##' 
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

source("R/convert_histo_to_raw.R")

fig2b <- read_csv("sbs_meta/scraped/Wilson-Brodie_2017/Wilson-Brodie_2017_Fig2b.csv", col_names = F) %>% 
  mutate(era = "present") %>% 
  mutate(b = 0.5, # my desired incremental value
         size_bin = round(X1 / b) * b, 
         count = round(X2))

fig2c <- read_csv("sbs_meta/scraped/Wilson-Brodie_2017/Wilson-Brodie_2017_Fig2c.csv", col_names = F) %>% 
  mutate(era = "past") %>% 
  mutate(b = 0.5, # my desired incremental value
         size_bin = round(X1 / b) * b, 
         count = round(X2))

## Get raw sizes (use repeat sizes)
dat_present <- repeat_sizes(size_bin_vector = fig2b$size_bin, count_vector = fig2b$count)
hist(dat_present)
n_present <- length(dat_present)

dat_past <- repeat_sizes(size_bin_vector = fig2c$size_bin, count_vector = fig2c$count)
hist(dat_past, breaks = 30)
n_past <- length(dat_past)

df_past <- data_frame(era = rep("past", n_past), size1mm = dat_past)
df_present <- data_frame(era = rep("present", n_present), size1mm = dat_present)
df <- rbind(df_past, df_present)

dat <- data.frame(
  study = "Wilson-Brodie", 
  studySub = NA, 
  species = "Nucella lapillus", 
  sp = "NULP", 
  site = "Southern UK",
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
  lat = 50.508,   
  long = -1.705, 
  Shaw_hab = NA, 
  notes = "", 
  notes2 = ""
)

dat %>% count(era)

dat <- dat %>%
  mutate(year = ifelse(era == "past", 1950, 2015))

# Save this file
write.csv(dat, "sbs_meta/scraped/Wilson-Brodie_2017/Wilson-Brodie_raw.csv")

dat %>% 
  ggplot(aes(size1mm, fill = era)) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~ era, scales = "free_y")

## Compare size threshold with max size
df_max_size <- dat %>% 
  filter(!is.na(era)) %>% 
  group_by(species, sp) %>% 
  summarise(size_max = max(size1mm, na.rm = TRUE)) %>% 
  ungroup()

## Add the size threshold used in Roy 2003
df_max_size <- df_max_size %>% 
  mutate(size_threshold = 30, 
         size_threshold_percent = size_threshold / size_max * 100) 

df_max_size 

write.csv(df_max_size, "sbs_meta/output/max_size_wilson.csv")

##### SUMMARISE DATA #####
## Get means for entire site

dat_summary <- dat %>% 
  group_by(era) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n()) %>%
  ungroup() 
dat_summary

##### GET LAT LONGS #####

ll_dat <- data_frame(lat_mean = 50.508, 
            long_mean = -1.705)

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dat_summary)

df_final <- dat_summary %>% 
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         year = c("pre-1922-1997", "2014-2016"), 
         site = "Southern UK", 
         size_original = "raw", 
         time_rep = NA, 
         species = "Nucella lapillus", 
         sample_size_units = "total number of snails", 
         min_threshold = 30, 
         size_max = c(43, 62)) %>%
  arrange(site, year)

head(df_final)


dfMeta <- data.frame(
  study = "WilsonBrodie_2017", 
  studySub = NA, 
  fig_table = "Figure 2", 
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
  size_threshold_mm = df_final$min_threshold,
  latitude = ll_dat$lat_mean, 
  longitude = ll_dat$long_mean
)

dfMeta

write.csv(dfMeta, "sbs_meta/output/dfMeta_Wilson-Brodie_2017.csv")
