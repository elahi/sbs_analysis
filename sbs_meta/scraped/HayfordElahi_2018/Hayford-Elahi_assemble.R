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
he

# Sampling began when the tide height was -0.15m below MLLW, I use 0 below. 

he_present <- data.frame(
  study = "Hayford-Elahi", 
  studySub = NA, 
  species = "Nucella lamellosa", 
  sp = "NULA", 
  site = "ShadyCove",
  era = "present", 
  date = "4/19/2018", 
  nest1 = NA, 
  nest2 = NA, 
  nest3 = NA, 
  nest3note = NA, 
  sampleUnit = NA, 
  size1mm = he$shell_length_mm, 
  size1mm_rand = he$shell_length_mm, 
  habitat = NA, 
  tideHTm = 0, 
  lat = 48.552,  
  long = -123.006, 
  Shaw_hab = NA, 
  notes = "", 
  notes2 = ""
)

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
spight %>% count(Year, breeding_group, size_bin) %>% print(n = 100)

# Breeding group is irrelevant, sum across size_bin
spight_sum <- spight %>%
  group_by(Year, size_bin) %>% 
  summarise(count = sum(frequency_of_occurence)) %>%
  ungroup()

# Use repeated sizes, because increments are in 1mm for both past and present
yr_bg_unique <- unique(spight$yr_bg)
past_size <- vector()
past_size
i = 1

for(i in 1:length(yr_bg_unique)){
  
  dat_i <- spight %>% filter(yr_bg == yr_bg_unique[i])
  past_size_i <- repeat_sizes(size_bin_vector = dat_i$size_bin,
                              count_vector = dat_i$frequency_of_occurence)
  
}

past_size <- spight %>% 
  group_by(Year, breeding_group) %>% 
  do(., repeat_sizes(size_bin_vector = .$size_bin, 
                     count_vector = .$frequency_of_occurence)) %>% 
  ungroup()
  

past_size <- repeat_sizes(size_bin_vector = spight$size_bin,
                          count_vector = spight$count)

past_size_norm <- get_random_sizes(size_bin_vector = spight$size_bin,
                                   count_vector = spight$count, 
                                   size_interval = 1, 
                                   distribution = "normal")

past_size_unif <- get_random_sizes(size_bin_vector = spight$size_bin,
                                   count_vector = spight$count, 
                                   size_interval = 1, 
                                   distribution = "uniform")

hist(past_size, breaks = 25)
hist(past_size_norm, breaks = 25)
par(mfrow = c(1,2))
hist(past_size_norm, breaks = 25)
hist(past_size_unif, breaks = 25)

# Use repeated sizes, because increments are in 1mm for both past and present

he_past <- data.frame(
  study = "Hayford-Elahi", 
  studySub = NA, 
  species = "Nucella lamellosa", 
  sp = "NULA", 
  site = "ShadyCove",
  era = "past", 
  date = "4/1/1968", 
  nest1 = NA, 
  nest2 = NA, 
  nest3 = NA, 
  nest3note = NA, 
  sampleUnit = NA, 
  size1mm = past_size, 
  size1mm_rand = past_size, 
  habitat = NA, 
  tideHTm = NA, 
  lat = NA,  
  long = NA, 
  Shaw_hab = NA, 
  notes = "day is unknown", 
  notes2 = ""
)

he2 <- rbind(he_present, he_past)

unique(he2$date)
he2 <- he2 %>%
  mutate(year = ifelse(era == "past", 1968, 2018))
he2 %>% count(era)
with(he2, plot(size1mm, size1mm_rand))

# Save this file
write.csv(he2, "sbs_meta/scraped/HayfordElahi_2018/Hayford-Elahi_raw.csv")

##### SUMMARISE #####

# Size threshold for museum comparison
dat <- he2 %>% mutate(size_threshold = 0)
dat_sub <- choose_size_threshold_general(dat, era = "combined", my_quantile = 0.5)

dat %>% count(species, era)
dat_sub %>% count(species, era)
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

##### GET LAT LONGS #####
str(frank2)

remove_last_char <- function(t) substr(t, 1, nchar(t)-1)
remove_last_char(frank2$lat)

ll_dat <- frank2 %>% 
  filter(era == "present") %>% 
  mutate(lat = as.numeric(remove_last_char(as.character(lat))), 
         long = -as.numeric(remove_last_char(as.character(long))))

ll_dat <- ll_dat %>% 
  summarise(lat_mean = mean(lat), 
            long_mean = mean(long))

##### FORMAT TABLE FOR META-ANALYSIS #####
df_final <- dat_summary %>% 
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         size_original = "raw", 
         species = "Tegula funebralis", 
         time_rep = NA, 
         sample_size_units = "total number of snails", 
         lat = ll_dat$lat_mean, 
         long = ll_dat$long_mean, 
         site = "South Cove") %>%
  arrange(species, year)

names(df_final)

dfMeta <- data.frame(
  study = "Galloway_2017", 
  studySub = df_final$studySub, 
  fig_table = "Table_4", 
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

write.csv(dfMeta, "sbs_meta/output/dfMeta_Galloway-Frank_2017.csv")
