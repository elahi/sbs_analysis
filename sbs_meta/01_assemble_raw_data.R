#################################################
# Author: Robin Elahi
# Date: 180929
# Analysis of historical and modern gastropod body sizes
# Prepare gastropod size-freq distributions for
# plotting and analysis
#################################################

library(dplyr)
library(readr)
source("R/choose_size_threshold.R")

## Field surveys
hay_king <- read_csv("sbs_meta/scraped/HayfordKing_2017/Hayford-King_raw.csv")

hay_elahi <- read_csv("sbs_meta/scraped/HayfordElahi_2018/Hayford-Elahi_raw.csv")
# Select one year for Hayford-Elahi past
hay_elahi %>% count(era, year)
hay_elahi <- hay_elahi %>% filter(year == 1969 | year == 2018)

gall_frank <- read_csv("sbs_meta/scraped/Galloway_2017/Galloway-Frank_raw.csv")

## Museum collections
sag <- read_csv("sbs_meta/scraped/Sagarin_2010/Sagarin_raw.csv")
wilson <- read_csv("sbs_meta/scraped/Wilson-Brodie_2017/Wilson-Brodie_raw.csv")
fisher <- read_csv("sbs_meta/scraped/Fisher2009_raw/Fisher_raw.csv")
roy <- read_csv("sbs_meta/scraped/Roy2003_raw/Roy_raw.csv")
roy %>% count(species, era)
roy <- roy %>% filter(!is.na(era))

## Compile
museum_dat <- rbind(sag, wilson, fisher, roy) %>% mutate(museum = "museum")
field_dat <- rbind(hay_king, hay_elahi, gall_frank) %>% mutate(museum = "field")
master <- rbind(museum_dat, field_dat) %>% 
  mutate(lat = as.numeric(lat))
master %>% count(study, era)
str(master)

##### Elahi 2015 data #####

# Function to load cleaned data
source("R/choose_size_data.R")

# load data - approximated sizes
dat <- choose_size_data(method = "uniform")

dat <- dat %>% 
  mutate(species = gsub(x = species, pattern = "\\.", replacement = " ")) %>% 
  mutate(species = ifelse(species == "Chlorostoma funebralis", "Tegula funebralis", species))

unique(dat$species)

dat2 <- dat %>% 
  select(-c(habitat, tideHTm_orig, sample_area_tidal_ht, 
            size_prop, LL, Shaw_hab)) %>% 
  mutate(study = "Elahi2015", 
         studySub = NA, 
         size1mm_rand = size1mm, 
         museum = "field")
unique(dat2$lat)

master2 <- master %>% 
  mutate(year = lubridate::year(mdy(date)), 
         sampleArea = NA) %>% 
  select(-c(X1, habitat, Shaw_hab))

df <- rbind(master2, dat2)

## Get latitude of study x species, create labels for plotting
df_plot <- df %>% 
  group_by(study, species) %>% 
  summarise(lat_study_sp = round(mean(lat, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  arrange(desc(lat_study_sp)) %>% 
  mutate(fig_legend = letters[seq(1:13)])

## Combine with df
df <- df %>% left_join(., df_plot, by = c("study", "species"))
df

df %>% group_by(study, species) %>% 
  filter(!is.na(size1mm)) %>% 
  summarise(max(size1mm), 
            min(size1mm))

##### Get size thresholds #####

df1 <- df %>%
  group_by(study, species) %>% 
  #do(choose_size_threshold_max(.), filter_data = FALSE) %>% 
  do(choose_size_threshold_general(., era = "combined", my_quantile = 0.5, filter_data = FALSE)) %>% 
  ungroup() %>% 
  filter(study != "Roy" & study != "Wilson-Brodie")

df1 %>% distinct(study, species, size_threshold)

df2 <- df %>%
  group_by(study, species) %>% 
  mutate(size_threshold = min(size1mm, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(study == "Roy" | study == "Wilson-Brodie") %>% 
  mutate(size_threshold = ifelse(study == "Roy", size_threshold, 30))

df2 %>% distinct(study, species, size_threshold)

## Rejoin data
df3 <- rbind(df1, df2)
df3 %>% distinct(study, species, size_threshold)

dfsub <- df3 %>% filter(size1mm >= size_threshold)
dfsub %>% group_by(study, species) %>% 
  summarise(max(size1mm), 
            min(size1mm))

df_size_threshold <- df3 %>% distinct(study, species, fig_legend, size_threshold)
df_size_threshold

