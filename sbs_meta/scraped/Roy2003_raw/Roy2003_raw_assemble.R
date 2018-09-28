################################################################################
##' @title Assembling raw data, Roy 2003
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

source("R/choose_size_threshold.R")

acsp <- read_csv("workspace/roy_raw_data/roy_a_spirata.csv") %>% 
  mutate(sp = "ACSP", species = "Acanthinucella spirata")

logi <- read_csv("workspace/roy_raw_data/roy_l_gigantea.csv") %>% 
  mutate(sp = "LOGI", species = "Lottia gigantea")

teau <- read_csv("workspace/roy_raw_data/roy_t_aureo.csv") %>% 
  mutate(sp = "TEAU", species = "Tegula aureotincta")

fivo <- read_csv("workspace/roy_raw_data/roy_f_volcano.csv") %>% 
  mutate(sp = "FIVO", species = "Fissurella volano")

## Compile, and assign past as pre-1960, as Roy did in paper
roy <- rbind(acsp, logi, teau, fivo) %>% 
  mutate(era = case_when(Year < 1960 ~ "past", 
                         Year > 2000 ~ "present"))
roy %>% count(species, era)

# Note that Size is categorical
roy %>% count(Size)
roy <- roy %>% 
  mutate(size1mm = as.numeric(Size))
        
roy %>% 
  ggplot(aes(Year, size1mm)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ species)

roy %>% count(species, Period)

# Get average year for species x era
roy <- roy %>% 
  group_by(species, era) %>% 
  mutate(year = mean(Year)) %>% ungroup()

roy %>% count(species, year)

# Add lat and long (from google maps)
roy <- roy %>% 
  mutate(lat = case_when(County == "LA" ~ 33.898, 
                         County == "Or" ~ 33.567, 
                         County == "SD" ~ 33.021), 
         long = case_when(County == "LA" ~ -118.561, 
                         County == "Or" ~ -117.850, 
                         County == "SD" ~ -117.301))

dat <- data.frame(
  study = "Roy", 
  studySub = NA, 
  species = roy$species, 
  sp = roy$sp, 
  site = roy$County,
  era = roy$era, 
  date = paste("01/01", roy$Year, sep = "/"), 
  nest1 = NA, 
  nest2 = NA, 
  nest3 = NA, 
  nest3note = NA, 
  sampleUnit = NA, 
  size1mm = roy$size1mm,  
  size1mm_rand = roy$size1mm,
  habitat = NA, 
  tideHTm = NA, 
  lat = roy$lat,   
  long = roy$long, 
  Shaw_hab = NA, 
  notes = "", 
  notes2 = "",
  year = roy$year
)

dat %>% count(era)
dat %>% 
  filter(!is.na(era)) %>% 
  ggplot(aes(size1mm, fill = era)) + 
  geom_histogram(binwidth = 3.3) + 
  facet_grid(site ~ species)

dat %>% 
  filter(!is.na(era)) %>% 
  ggplot(aes(era, size1mm)) + 
  geom_violin() + 
  facet_grid(site ~ species)  

# Save this file
write.csv(dat, "sbs_meta/scraped/Roy2003_raw/Roy_raw.csv")

##### SUMMARISE #####

## Summarise across counties
dat_summary <- dat %>% 
  group_by(species, sp, era) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n(), 
            year = round(mean(year, na.rm = TRUE), 0)) %>%
  ungroup()  %>% 
  mutate(studySub = "threshold_none", 
         museum = TRUE)
dat_summary

##### GET LAT LONGS #####

ll_dat <- dat %>% 
  summarize(lat_mean = mean(lat), 
           long_mean = mean(long))

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dat_summary)

df_final <- dat_summary %>% 
  filter(!is.na(era)) %>% 
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         site = "Southern California", 
         size_original = "raw", 
         time_rep = NA, 
         sample_size_units = "total number of snails")

head(df_final)

## Add the size threshold used in Roy 2003
df_final <- df_final %>% mutate(size_threshold = ifelse(sp == "LOGI", 50, 20))

names(df_final)

dfMeta <- data.frame(
  study = "Roy_2003", 
  studySub = NA, 
  fig_table = "raw", 
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

write.csv(dfMeta, "sbs_meta/output/dfMeta_Roy2003_raw.csv")
