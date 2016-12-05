################################################################################
##' @title Summarise tidal data to get cumulative air exposure
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-11-30
##' 
##' @log 
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
#library(tibble)
#library(rtide)
#library(scales) # for the date_format function

theme_set(theme_bw(base_size = 12))

source("R/air_exposure_functions.R")

# 91 years of data, hourly intervals
load("output/monterey_tides_1930_2020.RData")
head(dat)

# Load data
source("05_summarise_size_data.R")

##### FINE SCALE DATA #####

##' Load fine scale data for 4 sampling years
load("output/monterey_tides_fine_scale.RData")
##' Load the missing set
load("output/monterey_tides_2014.RData")

dat_fine <- rbind(dat_fine, dat0)

dat <- dat_fine %>% 
  mutate(year = year(DateTime), 
         day = day(DateTime),
         month = month(DateTime), 
         intv = as.numeric(DateTime - lag(DateTime))) %>% 
  select(-Station)

# Fix wacky intervals
dat <- dat %>%
  mutate(intv = ifelse(intv == 10, intv, NA))

##' Identify whether exposed or submerged for each desired tidal height
##' using 'do'

new_df <- data.frame(tidal_height = seq(0, 2, by = 0.1))

dat2 <- new_df %>% group_by(tidal_height) %>% 
  do(classify_air_water(dat, .)) %>% ungroup()

dat2
unique(dat2$intv)

# Coldest == Dec, Jan, Feb
# Warmest == July, Aug, Sep

##' Summarise
##' Daily minutes exposed to air
summary(dat2)

daily_min_df <- dat2 %>% filter(air_water == "air") %>% 
  group_by(tidal_height, year, month, day) %>% 
  summarise(sum_intv = sum(intv, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(season = ifelse(month == 12 | month < 3, 
                         "winter", 
                         ifelse(month > 6 & month < 10, 
                                "summer", "spring_fall")))

daily_min_df %>% group_by(season) %>% tally()

##' Get summary stats by month
monthly_min_df <- daily_min_df %>% 
  group_by(tidal_height, year, month) %>% 
  summarise(mean = mean(sum_intv), 
            sd = sd(sum_intv), 
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()

monthly_min_df %>% 
  filter(tidal_height == 0 | tidal_height == 0.5 |
           tidal_height == 1 | tidal_height == 1.5) %>% 
  ggplot(aes(month, mean, color = as.factor(year))) + 
  geom_line() + 
  facet_wrap(~ tidal_height, scales = "free_y") + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.1, alpha = 0.6)

##' Get summary stats by year
year_min_df <- daily_min_df %>% 
  filter(season != "spring_fall") %>% 
  group_by(tidal_height, year, season) %>% 
  summarise(mean = mean(sum_intv), 
            sd = sd(sum_intv), 
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()

year_min_df %>% #filter(tidal_height < 0.5) %>% 
  ggplot(aes(tidal_height, mean, color = as.factor(year))) + 
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.01, alpha = 0.6) +
  facet_wrap(~ season)

year_min_df %>% filter(tidal_height < 0.5) %>% 
  ggplot(aes(tidal_height, mean, color = as.factor(year))) + 
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.01, alpha = 0.6) +
  facet_wrap(~ season)
  
  
year_min_df %>% filter(tidal_height > 1.5) %>% 
  ggplot(aes(tidal_height, mean, color = as.factor(year))) + 
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.01, alpha = 0.6) +
  facet_wrap(~ season)

##### MAKE LRR PLOT #####

##' Select appropriate data
##' LIKE: 1938-1947; 2014
##' LODI: 1941-1950; 2015
##' CHFU: 1954-1963; 2014
##' 

names(year_min_df)

## Lottia
dat_long <- year_min_df %>% filter(year == "1950" | year == "2015") %>% 
  select(tidal_height:mean) %>% 
  spread(key = year, value = mean) 
names(dat_long)[3:4] <- c("past", "present")  
dat_long <- dat_long %>% 
  mutate(lrr = log10(present/past))

dat_lodi <- dat_long %>% 
  mutate(species = "Lottia digitalis")

## Littorina
dat_long <- year_min_df %>% filter(year == "1947" | year == "2014") %>% 
  select(tidal_height:mean) %>% 
  spread(key = year, value = mean) 
names(dat_long)[3:4] <- c("past", "present")  
dat_long <- dat_long %>% 
  mutate(lrr = log10(present/past))

dat_like <- dat_long %>% 
  mutate(species = "Littorina keenae")

## Chlorostoma
dat_long <- year_min_df %>% filter(year == "1963" | year == "2014") %>% 
  select(tidal_height:mean) %>% 
  spread(key = year, value = mean) 
names(dat_long)[3:4] <- c("past", "present")  
dat_long <- dat_long %>% 
  mutate(lrr = log10(present/past))

dat_chfu <- dat_long %>% 
  mutate(species = "Chlorostoma funebralis")

dat_long <- rbind(dat_chfu, dat_lodi, dat_like)

# Reorder levels
dat_long$species <- factor(dat_long$species, levels = rev(c("Littorina keenae",
                                               "Lottia digitalis", 
                                               "Chlorostoma funebralis")))
# Get tidal range by species
tidal_range <- dm2 %>% group_by(species) %>%
  summarise(min_height = min(tidalHeight), 
            max_height = max(tidalHeight)) %>%
  filter(species != "Littorina keenae")

my_text <- data.frame(x = rep(2.5, 3), 
                               y = rep(0.03, 3), 
                               text1 = c("A", "B", "C"), 
                               species = c("Chlorostoma funebralis", 
                                           "Lottia digitalis", 
                                           "Littorina keenae")) %>%
  filter(species != "Littorina keenae")

dat_long %>% filter(species != "Littorina keenae") %>% 
  ggplot(aes(tidal_height, lrr, color = season, shape = species)) + 
  geom_vline(aes(xintercept = min_height), data = tidal_range, col = "black") + 
  geom_vline(aes(xintercept = max_height), data = tidal_range, col = "black") +  
  geom_point() + geom_line() + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") + 
  facet_wrap(~ species) + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(face = "italic")) + 
  labs(x = "Tidal height (m)", y = "Log change in daily emersion\nlog(present/past)") + 
  theme(legend.position = c(1, 0.0), legend.justification = c(1, 0)) + 
  theme(legend.title = element_blank())  + 
  guides(shape = FALSE) + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = my_text, size = 5, hjust = 1, show.legend = FALSE) 

ggsave("figs/elahi_lrr_emersion_tidal_2panel.png", height = 3.5, width = 7)

