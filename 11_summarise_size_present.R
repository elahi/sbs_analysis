################################################################################
##' @title Summarising snail size frequency data - present data only 
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-05
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# load cleaned up data
source("02_sbs_size_dataPrep2.R")
head(dat2)
summary(dat2)

pres <- dat2 %>% filter(era == "present")

##### DEFINE SAMPLING UNITS FOR EACH SPECIES #####

unique(pres$sampleUnit)
pres %>% group_by(species, sampleUnit) %>% tally() %>% View()

# Littorina keenae = nest1 + nest2 = sampleUnit
pres %>% filter(sp == "LIKE") %>% group_by(sampleUnit) %>% tally()

# Chlorostoma funebralis = nest1 + nest2 = sampleUnit
pres %>% filter(sp == "CHFU") %>% group_by(sampleUnit) %>% tally() %>% View()
pres %>% filter(sp == "CHFU") %>% distinct(site, nest1, nest2) %>% select(nest1:long) %>% 
  head()

# Lottia digitalis = site + nest1 + nest2 (quadrats within tidal heights)
pres %>% filter(sp == "LODI") %>% group_by(sampleUnit) %>% tally()
pres %>% filter(sp == "LODI") %>% group_by(site, nest1, nest2) %>% tally()

# Fix Lottia
pres2 <- pres %>% 
  mutate(sampleUnit = ifelse(sp == "LODI", 
                             paste(site, nest1, nest2, sep = "_"), sampleUnit))

unique(pres2$sampleUnit)

##### SUMMARISE DATA BY SAMPLE UNIT #####

head(pres2)
suMeans <- pres2 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, nest1, nest2, sampleUnit, sampleArea, year, lat, long, tideHTm) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se) %>% 
  ungroup()

suMeans %>% 
  ggplot(aes(tideHTm, size_mean, color = species)) +
  geom_point() + 
  # geom_errorbar(aes(ymax = size_mean + size_CI, ymin = size_mean - size_CI)) + 
  geom_smooth(method = "lm") + 
  labs(x = "Tidal height (m)", y = "Mean size (mm)") + 
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) + 
  theme(legend.title = element_blank())

##### SUMMARISE DATA BY NEST1 #####

nest1Means <- pres2 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, nest1, year, lat, long, tideHTm) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            tidalHT_mean = mean(tideHTm, na.rm = TRUE), 
            size_max = quantile(size1mm, 0.95)) %>% 
  ungroup()

nest1Means %>% 
  ggplot(aes(tideHTm, size_mean, color = species)) +
  geom_point(alpha = 0.5) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, ymin = size_mean - size_CI), alpha = 0.5) + 
  geom_smooth(method = "lm") + 
  labs(x = "Tidal height (m)", y = "Mean size (mm)") + 
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) + 
  theme(legend.title = element_blank())
ggsave("figs/elahi_size_by_tide.png", height = 3.5, width = 5)

nest1Means %>% 
  ggplot(aes(tideHTm, size_max, color = species)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") + 
  labs(x = "Tidal height (m)", y = "Max size (mm)") + 
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) + 
  theme(legend.title = element_blank())
ggsave("figs/elahi_maxsize_by_tide.png", height = 3.5, width = 5)

