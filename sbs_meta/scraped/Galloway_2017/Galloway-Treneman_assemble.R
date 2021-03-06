################################################################################
##' @title Assembling scraped data, Galloway-Treneman 2017
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

tren_past <- read_csv("sbs_meta/scraped/Galloway_2017/Galloway-Treneman_SunsetBay_Limpets_1979.csv")
summary(tren_past)

tren_pres <- read_csv("sbs_meta/scraped/Galloway_2017/Galloway-Treneman_SunsetBay_Limpets_2017.csv")

tren_past %>% 
  ggplot(aes(1, ave.size.mm)) + geom_boxplot()

tren_pres %>% 
  ggplot(aes(tide.ht.m, size.mm)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ species) + 
  geom_point(data = tren_past, aes(tide.ht.m, ave.size.mm), color = "red", alpha = 0.5)

##### SUMMARISE DATA #####

## In Treneman's original paper, she mentioned that it was difficult to identify limpet species when below 5mm
## Use 5mm as a threshold
min_threshold <- 5

## Get means for present data
tren_pres_summary <- tren_pres %>% 
  rename(size1mm = size.mm) %>% 
  filter(!is.na(size1mm)) %>%
  filter(size1mm >= min_threshold) %>% 
  group_by(species, sp, site) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm), 
            n = n()) %>% 
  ungroup() %>% 
  mutate(year = 2017, 
         era = "present")
tren_pres_summary

## format tren_past
tren_past_summary <- tren_past %>% 
  rename(size1mm = ave.size.mm) %>% 
  filter(!is.na(size1mm)) %>%
  filter(size1mm >= min_threshold) %>% 
  group_by(species, sp, site) %>% 
  summarise(size_mean = mean(size1mm, na.rm = TRUE), 
            size_sd = sd(size1mm, na.rm = TRUE), 
            n = n()) %>%
  ungroup() %>% 
  mutate(year = 1979, 
         era = "past")

tren_past_summary

# Replace sample size with the original number of limpets sampled in 1979
# L. scutum = 40
# L. persona = 61

tren_past_summary <- tren_past_summary %>% 
  mutate(n = c(40, 61))

## The SD for present are larger because they are based on raw size data for individuals
## So I will use these to be conservative

tren_past_summary
tren_pres_summary

tren_past_summary <- tren_past_summary %>% 
  mutate(size_sd = tren_pres_summary$size_sd)

## Combine mean size for past and present
tren_summary <- rbind(tren_past_summary, tren_pres_summary)

## Summarize for meta-analysis
tren_meta <- tren_summary %>% 
  mutate(size_original = "mean")
tren_meta

##### GET LAT LONGS #####

lat_mean = 43.3336
long_mean = -124.3769

##### FORMAT TABLE FOR META-ANALYSIS #####
names(tren_meta)

df_final <- tren_meta %>% 
  rename(size_rep = size_mean, size_error = size_sd, 
         sample_size = n) %>%
  mutate(species = gsub(species, pattern = "\\.", replacement = " "), 
         time_rep = NA, 
         sample_size_units = "mean size of snails by tidal height", 
         museum = FALSE, 
         studySub = NA) %>%
  arrange(species, year)

head(df_final)

dfMeta <- data.frame(
  study = "Galloway_2017", 
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
  museum = df_final$museum, 
  size_threshold_mm = min_threshold, 
  latitude = lat_mean, 
  longitude = long_mean
)

dfMeta

write.csv(dfMeta, "sbs_meta/output/dfMeta_Galloway-Treneman_2017.csv")
