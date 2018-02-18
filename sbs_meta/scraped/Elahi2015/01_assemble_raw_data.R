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
library(readr)

# Functions to convert histogram data to vector of raw sizes
source("R/convert_histo_to_raw.R")

##### Load HayfordKing data #####
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

##### Load Galloway data (Tegula) #####
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

# Get past sizes
frank_past_bins <- c(3.5, 10.5, 17.5, 24.5, 31.5)
frank_past_n <- c(33, 20, 431, 446, 1)

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
frank2 %>% count(era)
with(frank2, plot(size1mm, size1mm_rand))

master <- rbind(ackerman2, frank2)
names(master)

##### Load Galloway data (Lottia) #####

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

## Get means by tidal height for present data
tren_pres_summary <- tren_pres %>% 
  group_by(species, sp, tide.ht.m) %>% 
  summarise(size1mm = mean(size.mm, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(year = 2017, 
         era = "present")

## format tren_past
tren_past2 <- tren_past %>% 
  select(species, sp, tide.ht.m, ave.size.mm) %>% 
  rename(size1mm = ave.size.mm) %>% 
  mutate(year = 1979, 
         era = "past")

## Combine mean size for past and present
tren_summary <- rbind(tren_past2, tren_pres_summary)

tren_summary %>% 
  ggplot(aes(era, size1mm)) + 
  geom_boxplot() + 
  facet_wrap(~ species)

min_threshold <- 5

## Summarize for meta-analysis
tren_meta <- tren_summary %>% 
  filter(!is.na(size1mm)) %>% 
  filter(size1mm >= min_threshold) %>% 
  group_by(species, sp, year, era) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm), 
            n = n()) %>% 
  ungroup() %>% 
  mutate(size_original = "mean")
tren_meta


##### Load Elahi data #####

# Function to load cleaned data
source("R/choose_size_data.R")

# load data - approximated sizes
dat <- choose_size_data(method = "uniform")
names(dat)
names(master)
unique(master$date)

dat2 <- dat %>% 
  select(-c(habitat, tideHTm_orig, sample_area_tidal_ht, size_prop, LL, Shaw_hab)) %>% 
  mutate(study = "Elahi2015", 
         studySub = NA, 
         size1mm_rand = size1mm)
names(dat2)

master <- master %>% 
  mutate(year = lubridate::year(mdy(date)), 
         sampleArea = NA) %>% 
  select(-c(habitat, Shaw_hab))
names(master)

dim(dat2); dim(master)

master2 <- rbind(master, dat2)

##### PRELIM PLOTS #####

master2 %>% 
  ggplot(aes(era, size1mm_rand)) + 
  geom_violin() + 
  facet_wrap(~ species + study)

master2 %>% 
  ggplot(aes(size1mm_rand, fill = era)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~ study + species, scales = "free_y") + 
  xlab("Size (mm)") + 
  ylab("Probability density") + 
  theme(legend.position = "top")
ggsave("sbs_meta/meta_figs/meta_density_era.pdf", height = 5, width = 7)

