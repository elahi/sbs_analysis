#################################################
# Author: Robin Elahi
# Date: 171018

# Analysis of historical and modern gastropod body sizes
# Hopkins Marine Station

# Prepare gastropod size-freq distributions for
# plotting and analysis
# I will use a normal distribution to expand size-frequency data from old studies
#################################################

library(dplyr)

rm(list=ls(all=TRUE)) 

# Functions to convert histogram data to vector of raw sizes
source("R/convert_histo_to_raw.R")

##### Load HayfordKing data #####
ackerman <- read_csv("sbs_meta/scraped/HayfordKing_2017/biogeo_size_change - size_raw_HHWK.csv")
ackerman
names(ackerman)

ackerman <- ackerman %>% mutate(Shaw_hab = NA, notes = "", notes2 = "")

# Get normally distributed sizes
ack_past <- ackerman %>% 
  filter(era == "past")

ack_past_histo <- ack_past %>% count(size1mm)

ack_past_size_norm <- get_random_sizes(size_bin_vector = ack_past_histo$size1mm, 
                                       count_vector = ack_past_histo$n, size_interval = 1)

plot(ack_past$size1mm, ack_past_size_norm)

ack_past$size1mm_norm <- ack_past_size_norm

ack_pres <- ackerman %>% filter(era == "present") %>% 
  mutate(size1mm_norm = size1mm)

ackerman2 <- rbind(ack_pres, ack_past)

##### Load Galloway data #####
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
                       size1mm_norm = round(frank$Width.mm, 1), 
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

# Get past sizes
frank_past_bins <- c(3.5, 10.5, 17.5, 24.5, 31.5)
frank_past_n <- c(33, 20, 431, 446, 1)

frank_past_size <- repeat_sizes(size_bin_vector = frank_past_bins, 
                                           count_vector = frank_past_n)

frank_past_size_norm <- get_random_sizes(size_bin_vector = frank_past_bins, 
                                       count_vector = frank_past_n, size_interval = 3.5)

hist(frank_past_size, breaks = 5)
hist(frank_past_size_norm, breaks = 5)

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
  size1mm_norm = round(frank_past_size_norm, 1), 
  habitat = NA, 
  tideHTm = NA, 
  lat = NA,  
  long = NA, 
  Shaw_hab = NA, 
  notes = "day-month is unknown", 
  notes2 = ""
)

frank2 <- rbind(frank_present, frank_past)
frank2 %>% count(era)
with(frank2, plot(size1mm, size1mm_norm))

master <- rbind(ackerman2, frank2)
names(master)

##### Load Elahi data #####

# load cleaned up data
source("02_sbs_size_dataPrep2.R")
names(dat2)
names(master)
unique(master$date)

dat3 <- dat2 %>% 
  select(-c(habitat, tideHTm_orig, sample_area_tidal_ht, size_prop, LL, Shaw_hab)) %>% 
  mutate(study = "Elahi2015", 
         studySub = NA)
names(dat3)

master <- master %>% 
  mutate(year = lubridate::year(mdy(date)), 
         sampleArea = NA) %>% 
  select(-c(habitat, Shaw_hab))
names(master)

master2 <- rbind(master, dat3)

master2 %>% 
  ggplot(aes(era, size1mm_norm)) + 
  geom_violin() + 
  facet_wrap(~ species + study)

