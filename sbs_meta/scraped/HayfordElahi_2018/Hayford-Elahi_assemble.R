################################################################################
##' @title Assembling scraped data, Hayford-Elahi 2018
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2018-09-24
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

# Size-frequency raw data, Hayford-Elahi-FHL470, 2018
he <- read_csv("sbs_meta/scraped/HayfordElahi_2018/Shady_Cove_shelllength_rawdata_2018.csv")
he <- he %>% rename(size1mm = shell_length_mm, breeding = breeding_group, year = Year)

# Size-frequency counts, Spight 1968 at Shady Cove
# Spight 1974, Ecology, Figure 2 Population IV
# Student digitized data
spight <- read_csv("sbs_meta/scraped/HayfordElahi_2018/Spight_rawdata_fig1968_breedinggroup.csv") 
spight <- spight %>% arrange(Year, breeding_group, shell_length_mm)

# Format Spight data
spight <- spight %>% 
  mutate(size_bin = round(shell_length_mm, 0), 
         frequency_of_occurence = round(frequency_of_occurence, 0), 
         yr_bg = paste(Year, breeding_group, sep = "_"))

# Use repeated sizes, because increments are in 1mm for both past and present
# Need a loop to cycle through each year and breeding group to repeat the observed sizes
yr_bg_unique <- unique(spight$yr_bg)
past_size_df <- data.frame(size1mm = NA, 
                           year = NA, 
                           breeding = NA)

# Run loop
for(i in 1:length(yr_bg_unique)){
  
  dat_i <- spight %>% filter(yr_bg == yr_bg_unique[i])
  year_i <- unique(dat_i$Year)
  breeding_i <- unique(dat_i$breeding_group)
  past_size_i <- repeat_sizes(size_bin_vector = dat_i$size_bin,
                              count_vector = dat_i$frequency_of_occurence)
  
  past_size_df_i <- data.frame(size1mm = past_size_i, 
                             year = year_i, 
                             breeding = breeding_i)
  
  past_size_df <- rbind(past_size_df, past_size_df_i)
  
}

past_size_df %>% count(breeding, year)
past_size_df <- past_size_df %>% filter(!is.na(breeding))

# Combine past and present data
he <- rbind(past_size_df, he) %>% 
  mutate(population = "V_VI", 
         province = "TS_SB_43S_UCV_CV", 
         figure = "Spight_1974_Ecology_Fig2") 

head(he)

write.csv(he, "sbs_meta/scraped/HayfordElahi_2018/Spight_snail_resurvey_180925.csv")

##### FORMATTING DATA #####

he2 <- he %>% 
  mutate(date = ifelse(year == 2018, "4/19/2018", paste("4/15", year, sep = "/")), 
         notes = ifelse(year == 2018, NA, "day is unknown"), 
         era = ifelse(year > 2017, "present", "past")) %>% 
  tbl_df()
unique(he2$date)
unique(he2$notes)
he2

# Sampling began when the tide height was -0.15m below MLLW, I use 0 below. 

he3 <- data.frame(
  study = "Hayford-Elahi", 
  studySub = NA, 
  species = "Nucella lamellosa", 
  sp = "NULA", 
  site = "ShadyCove",
  era = he2$era, 
  date = he2$date, 
  nest1 = NA, 
  nest2 = NA, 
  nest3 = NA, 
  nest3note = NA, 
  sampleUnit = NA, 
  size1mm = he2$size1mm, 
  size1mm_rand = he2$size1mm, 
  habitat = NA, 
  tideHTm = 0, 
  lat = 48.552,  
  long = -123.006, 
  Shaw_hab = NA, 
  notes = he2$notes, 
  notes2 = "", 
  year = he2$year
)

head(he3)

# Save this file
write.csv(he3, "sbs_meta/scraped/HayfordElahi_2018/Hayford-Elahi_raw.csv")

##### SUMMARISE #####

# Select years to analyze
he3 %>% count(year)
dat <- he3 %>% filter(year == 1969 | year == 2018)

# Size threshold for museum comparison
dat <- dat %>% mutate(size_threshold = 0)
dat_sub <- choose_size_threshold_general(dat, era = "combined", my_quantile = 0.5)

dat %>% count(species, era)
dat_sub %>% count(species, era)

## Summarise
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

##### FORMAT TABLE FOR META-ANALYSIS #####
df_final <- dat_summary %>% 
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         size_original = "raw", 
         species = "Nucella lamellosa", 
         time_rep = NA, 
         sample_size_units = "total number of snails", 
         lat = unique(dat$lat), 
         long = unique(dat$long), 
         site = "Shady Cove") %>%
  arrange(species, year)

names(df_final)

dfMeta <- data.frame(
  study = "Hayford-Elahi_2018", 
  studySub = df_final$studySub, 
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
  museum = FALSE, 
  size_threshold_mm = df_final$size_threshold, 
  latitude = df_final$lat, 
  longitude = df_final$long
)

dfMeta

write.csv(dfMeta, "sbs_meta/output/dfMeta_Hayford-Elahi_2018.csv")
