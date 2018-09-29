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
master <- rbind(hay_king, hay_elahi, gall_frank, sag, wilson, fisher, roy)
master %>% count(study, era)

##### Integrate Elahi data #####

# Function to load cleaned data
source("R/choose_size_data.R")

# load data - approximated sizes
dat <- choose_size_data(method = "uniform")

dat2 <- dat %>% 
  select(-c(habitat, tideHTm_orig, sample_area_tidal_ht, 
            size_prop, LL, Shaw_hab)) %>% 
  mutate(study = "Elahi2015", 
         studySub = NA, 
         size1mm_rand = size1mm)

master2 <- master %>% 
  mutate(year = lubridate::year(mdy(date)), 
         sampleArea = NA) %>% 
  select(-c(X1, habitat, Shaw_hab))

df <- rbind(master2, dat2)

##### PRELIM ANALYSIS #####
library(lme4)
names(df)
df %>% count(study, species, era) %>% print(n = 200)

mod1 <- lmer(log(size1mm) ~ era + (1 | species), data = df)
summary(mod1)


##### PRELIM PLOTS #####

df %>% 
  ggplot(aes(era, size1mm_rand)) + 
  geom_violin() + 
  facet_wrap(~ species + study)

df %>% 
  ggplot(aes(size1mm_rand, fill = era)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~ study + species, scales = "free_y") + 
  xlab("Size (mm)") + 
  ylab("Probability density") + 
  theme(legend.position = "top")

ggsave("sbs_meta/meta_figs/meta_density_era.pdf", height = 5, width = 7)

df %>% 
  ggplot(aes(size1mm_rand, fill = era, color = era)) + 
  geom_histogram(alpha = 0.5, binwidth = 2, 
                 aes(y = ..density..), position = "identity") + 
  facet_wrap(~ study + species, scales = "free_y", ncol = 3) + 
  xlab("Size (mm)") + 
  ylab("Density") + 
  theme(legend.position = "top")

ggsave("sbs_meta/meta_figs/meta_hist_era.pdf", height = 7, width = 7)

