################################################################################
##' @title Assembling scraped data, Galloway-Frank 2017
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

frank <- read_csv("sbs_meta/scraped/Galloway_2017/SouthCove_Tegula-Size-Counts_Spring2017_AG-Shared_2017-10-13_size.csv") # snail size

frank_counts <- read_csv("sbs_meta/scraped/Galloway_2017/SouthCove_Tegula-Size-Counts_Spring2017_AG-Shared_2017-10-13_counts.csv")

frank <- frank_counts %>% 
  select(SampleID, Density.m2, Elevation.above.MLLW) %>% 
  left_join(frank, ., by = "SampleID")

summary(frank)

frank_transects <- frank %>% distinct(`Transect#`) %>% 
  mutate(lat = c("43.301949N", "43.302368N", "43.302711N", "43.303055N"), 
         long = c("124.39942W", "124.399239W", "124.399057W", "124.398942W"))

frank <- left_join(frank, frank_transects, by = "Transect#")

frank_present <- data.frame(
  study = "Galloway", 
  studySub = NA, 
  species = "Chlorostoma.funebralis", 
  sp = "CHFU", 
  site = "SouthCove",
  era = "present", 
  date = frank$Date, 
  nest1 = frank$`Transect#`, 
  nest2 = frank$`Sample#`, 
  nest3 = NA, 
  nest3note = NA, 
  sampleUnit = frank$SampleID, 
  size1mm = round(frank$Width.mm, 1), 
  size1mm_rand = round(frank$Width.mm, 1), 
  habitat = NA, 
  tideHTm = frank$Elevation.above.MLLW, 
  lat = frank$lat,  
  long = frank$long, 
  Shaw_hab = NA, 
  notes = "", 
  notes2 = ""
)

head(frank_present)
unique(frank_present$date)

# Get past sizes (Frank 1975 - Table 4)
frank_past_freq <- c(3.6, 2.2, 46.3, 48.0, 0.1)
frank_past_n <- frank_past_freq/100 * 930
frank_past_bins <- c(3.5, 10.5, 17.5, 24.5, 31.5)
frank_past_n <- c(33, 20, 430, 446, 1)

frank_past_size <- repeat_sizes(size_bin_vector = frank_past_bins, 
                                count_vector = frank_past_n)

frank_past_size_norm <- get_random_sizes(size_bin_vector = frank_past_bins, 
                                         count_vector = frank_past_n, size_interval = 3.5, 
                                         distribution = "normal")

frank_past_size_unif <- get_random_sizes(size_bin_vector = frank_past_bins, 
                                         count_vector = frank_past_n, size_interval = 3.5, 
                                         distribution = "uniform")

hist(frank_past_size, breaks = 5)
hist(frank_past_size_norm, breaks = 5)
par(mfrow = c(1,2))
hist(frank_past_size_norm, breaks = 25)
hist(frank_past_size_unif, breaks = 25)

# Use uniformly distributed sizes
frank_past <- data.frame(
  study = "Galloway", 
  studySub = NA, 
  species = "Chlorostoma.funebralis", 
  sp = "CHFU", 
  site = "SouthCove",
  era = "past", 
  date = "5/1/1968", 
  nest1 = NA, 
  nest2 = NA, 
  nest3 = NA, 
  nest3note = NA, 
  sampleUnit = NA, 
  size1mm = frank_past_size, 
  size1mm_rand = round(frank_past_size_unif, 1), 
  habitat = NA, 
  tideHTm = NA, 
  lat = NA,  
  long = NA, 
  Shaw_hab = NA, 
  notes = "day-month is unknown", 
  notes2 = ""
)

frank2 <- rbind(frank_present, frank_past)
unique(frank2$date)
frank2 <- frank2 %>%
  mutate(year = ifelse(era == "past", 1968, 2017))
frank2 %>% count(era)
with(frank2, plot(size1mm, size1mm_rand))

##### SUMMARISE DATA #####
## Get means for entire site

frank_summary <- frank2 %>% 
  group_by(species, sp, site, era, year) %>% 
  summarise(size1mm_mean = mean(size1mm, na.rm = TRUE), 
            size1mm_sd = sd(size1mm, na.rm = TRUE), 
            sample_size = n()) %>%
  ungroup() 


##### GET LAT LONGS #####
str(frank2)

remove_last_char <- function(t) substr(t, 1, nchar(t)-1)
remove_last_char(frank2$lat)

frank2 <- frank2 %>% 
  mutate(lat = as.numeric(remove_last_char(as.character(lat))), 
         long = -as.numeric(remove_last_char(as.character(long))))

ll_dat <- frank2 %>% 
  filter(era == "present") %>% 
  summarise(lat_mean = mean(lat), 
            long_mean = mean(long))

##### FORMAT TABLE FOR META-ANALYSIS #####
names(frank2)

df_final <- frank_summary %>% 
  mutate(size_rep = size1mm_mean, 
         size_error = size1mm_sd, 
         size_original = "raw", 
         species = "Tegula funebralis", 
         time_rep = NA, 
         sample_size_units = "total number of snails", 
         lat = ll_dat$lat_mean, 
         long = ll_dat$long_mean) %>%
  arrange(species, year)

names(df_final)

dfMeta <- data.frame(
  study = "Galloway-Frank_2017", 
  studySub = NA, 
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
  museum = FALSE, 
  size_threshold_mm = 0, 
  latitude = df_final$lat, 
  longitude = df_final$long
)

dfMeta

write.csv(dfMeta, "sbs_meta/output/dfMeta_Galloway-Frank_2017.csv")
