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

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# load cleaned up data
source("03_identify_size_cutoff.R")
pres <- dat4 %>% filter(era == "present")

# load lat long info with positions of nearest loggers
sizeLL <- read.csv("output/sizeLL_edit.csv")
unique(pres$sampleUnit)

# load temperature data
source("05_summarise_intertidal_temps.R")

##### SUMMARISE SIZE DATA BY SAMPLE UNIT #####
head(pres)

suMeans <- pres %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, nest1, nest2, sampleUnit, 
           sampleArea, year, lat, long, tideHTm) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_max = quantile(size1mm, 0.95), 
            size_median = median(size1mm)) %>% 
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

nest1Means <- pres %>% filter(!is.na(size1mm)) %>% 
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

# ggsave("figs/elahi_size_by_tide.png", height = 3.5, width = 5)

nest1Means %>% 
  ggplot(aes(tideHTm, size_max, color = species)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") + 
  labs(x = "Tidal height (m)", y = "Max size (mm)") + 
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) + 
  theme(legend.title = element_blank())

# ggsave("figs/elahi_maxsize_by_tide.png", height = 3.5, width = 5)

##### JOIN TEMPERATURE DATA TO SIZE DATA BY POSITION #####

names(tempMeans)
names(sizeLL)
names(suMeans)

# Join position to sampleUnit in size data
sizeLL %>% select(sampleUnit, position) %>% head()
names(pres)

unique(pres$sampleUnit)
unique(sizeLL$sampleUnit)
glimpse(pres)
glimpse(sizeLL)

pres2 <- sizeLL %>% select(sampleUnit, position) %>% 
  inner_join(pres, ., by = "sampleUnit")

suMeans2 <- sizeLL %>% select(sampleUnit, position) %>% 
  inner_join(suMeans, ., by = "sampleUnit")

# Now join tempMeans to size data by position

names(pres2)
names(tempMeans)
names(tempMedL)

pres3 <- tempMeans %>% select(position, aspect, slope, metric:min) %>% 
  inner_join(pres2, ., by = "position")

suMeans3 <- tempMeans %>% select(position, aspect, slope, metric:CI) %>% 
  inner_join(suMeans2, ., by = "position")

##### JOIN TEMPERATURE DATA TO SIZE DATA BY POSITION #####
head(suMeans3)
unique(suMeans3$metric)

suMeans3 %>% filter(metric != "daily_med") %>% 
  ggplot(aes(mean, size_median, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Median size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free") + 
  guides(size = FALSE)

suMeans3 %>% filter(metric != "daily_med") %>% 
  ggplot(aes(mean, size_mean, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Mean size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free") + 
  guides(size = FALSE)

suMeans3 %>% filter(metric != "daily_med") %>% 
  ggplot(aes(mean, size_max, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Maximum size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free") + 
  guides(size = FALSE)

suMeans3 %>% filter(metric == "daily_med") %>% 
  ggplot(aes(mean, size_median, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + 
  labs(x = "Temperature (C)", y = "Median size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ species, scales = "free") + 
  guides(size = FALSE)


