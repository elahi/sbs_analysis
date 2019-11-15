#################################################
# Author: Robin Elahi
# Date: 191114
# Correct tidal heights, using total station heights from November 2019
#################################################

library(tidyverse)
library(readxl)

# Load data
dat <- read_csv('./output/sbsMaster_unif.csv')
names(dat)

# Load new tidal heights
th_dat <- read_excel("output/gps_master_edit_tidal_heights_191113.xlsx", sheet = "tegula_lodi_like")
th_dat

# Covert to m and calculate tidal height
th_dat <- th_dat %>%
  mutate(adjustment_m = adjustment_ft * 0.3048,
         station_m = benchmark_m - benchmark_diff_m, 
         thm_new = station_m + height_z + adjustment_m)

th_dat %>% count(benchmark_diff_m)
th_dat %>% count(adjustment_m)
th_dat %>% count(station_m)
hist(th_dat$thm_new)

th_dat %>% distinct(species, site, position, nest1, thm_new) %>% 
  arrange(thm_new) %>% 
  filter(!is.na(thm_new)) %>% 
  print(n = 100)

##### COMPARE TIDAL HEIGHTS #####

th_old <- dat %>% 
  select(species, site, nest1, tideHTm) %>% 
  distinct()
th_old

th_old <- th_old %>% 
  mutate(species = gsub("[.]", " ", species), 
         species = gsub("Chlorostoma", "Tegula", species))

th_old
th_dat

th_dat <- left_join(th_dat, th_old, by = c("species", "site", "nest1"))

th_dat %>% 
  ggplot(aes(tideHTm, thm_new, color = species)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm")

