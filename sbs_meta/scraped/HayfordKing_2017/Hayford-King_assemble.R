################################################################################
##' @title Assembling scraped data, Hayford-King 2017
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
source("R/choose_size_threshold.R")

remove_last_char <- function(t) substr(t, 1, nchar(t)-1)

ackerman <- read_csv("sbs_meta/scraped/HayfordKing_2017/biogeo_size_change - size_raw_HHWK.csv")
ackerman
names(ackerman)

ackerman <- ackerman %>% mutate(Shaw_hab = NA, notes = "", notes2 = "")

# Get random sizes from binned data
ack_past <- ackerman %>% 
  filter(era == "past")

ack_past_histo <- ack_past %>% count(size1mm)

# Normally distributed sizes
ack_past_size_norm <- get_random_sizes(size_bin_vector = ack_past_histo$size1mm, 
                                       count_vector = ack_past_histo$n, size_interval = 1, 
                                       distribution = "normal")
# Uniformally distributed sizes
ack_past_size_unif <- get_random_sizes(size_bin_vector = ack_past_histo$size1mm, 
                                       count_vector = ack_past_histo$n, size_interval = 1, 
                                       distribution = "uniform")

plot(ack_past$size1mm, ack_past_size_norm)
plot(ack_past$size1mm, ack_past_size_unif)

hist(ack_past_size_norm, breaks = 30)
hist(ack_past_size_unif, breaks = 30)

names(ack_past)

# Use uniformly distributed sizes
ack_past$size1mm_rand <- ack_past_size_unif

ack_pres <- ackerman %>% filter(era == "present") %>% 
  mutate(size1mm_rand = size1mm)

ackerman2 <- rbind(ack_pres, ack_past)
ackerman2 %>% distinct(date)
ackerman2 <- ackerman2 %>% mutate(year = ifelse(era == "past", 1964, 2017))
ackerman2 %>% count(era)

ackerman2 <- ackerman2 %>% 
  mutate(lat = as.numeric(remove_last_char(as.character(lat))), 
         long = -as.numeric(remove_last_char(as.character(long))))

# Save this file
dat <- ackerman2 %>% 
  mutate(study = "Hayford-King")
write.csv(dat, "sbs_meta/scraped/HayfordKing_2017/Hayford-King_raw.csv")

# Size threshold for museum comparison
dat <- ackerman2 %>% mutate(size_threshold = 0)
dat_sub <- choose_size_threshold_general(dat, era = "combined", my_quantile = 0.5)

dat %>% count(species, era)
dat_sub %>% count(species, era)

##### PLOT HISTOS #####
library(ggplot2)
theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())) 

names(dat)

dat %>% 
  ggplot(aes(size1mm)) + 
  geom_histogram(binwidth = 2, color = "black", fill = "gray") + 
  facet_wrap(~ era, ncol = 1)

dat %>% 
  ggplot(aes(size1mm, fill = era)) + 
  geom_histogram(binwidth = 2, color = "black", alpha = 0.5) 


##### SUMMARISE #####

## Summarise across sample areas
dat_summary_all <- dat %>% 
  group_by(species, sp, year, era, size_threshold, site) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n()) %>%
  ungroup() %>% 
  mutate(studySub = "threshold_none", 
         museum = FALSE)

dat_summary_sub <- dat_sub %>% 
  group_by(species, sp, year, era, size_threshold, site) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n()) %>%
  ungroup() %>% 
  mutate(studySub = "threshold_median", 
         museum = TRUE)

dat_summary <- rbind(dat_summary_all, dat_summary_sub)

##### GET LAT LONGS #####
ll_dat <- ackerman2 %>% 
  filter(era == "present") %>% 
  summarise(lat_mean = mean(lat), 
            long_mean = mean(long))

##### FORMAT TABLE FOR META-ANALYSIS #####
df_final <- dat_summary %>% 
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         size_original = "raw", 
         species = "Nucella lamellosa", 
         time_rep = NA, 
         sample_size_units = "total number of snails", 
         lat = ll_dat$lat_mean, 
         long = ll_dat$long_mean) %>%
  arrange(species, year)

names(df_final)

dfMeta <- data.frame(
  study = "Hayford_2017", 
  studySub = df_final$studySub, 
  fig_table = NA, 
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
  latitude = df_final$lat, 
  longitude = df_final$long
)

dfMeta

write.csv(dfMeta, "sbs_meta/output/dfMeta_Hayford-King_2017.csv")
