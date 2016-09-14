################################################################################
##' @title Process Luke Miller's limpet temperature data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-16
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# Load the file that I sent to Luke for the hindcasts
positionDF <- read_csv("output/positionDF_edit.csv")[-1] %>% 
  select(position, sp, nest1:tidalHT, aspect, slope_normal, lon, lat, run_id) %>%
  rename(azimuth = aspect, aspect = slope_normal)
positionDF

# Load size data
source("03_identify_size_cutoff.R")
head(dat4)
spCodes <- dat4 %>% select(species, sp) %>% distinct()
levels(spCodes$species)

# Get limpet temperature data
limpet <- read_csv("../bigFiles/Limpet_temp_estimates.csv")
glimpse(limpet)

# Get dates, months, years
limpet2 <- limpet %>% 
  mutate(dateR = as.Date(TimePST), 
         month = month(TimePST), 
         year = year(TimePST))
glimpse(limpet2)

# Convert to long format
limpetL <- limpet2 %>% gather(key = "run_id", value = "tempC", Run118:Run135)
head(limpetL)

##### SUMMARISE DAILY #####

lim_daily <- limpetL %>% group_by(run_id, dateR, month, year) %>% 
  summarise(maximum = max(tempC), 
            median = median(tempC), 
            minimum = min(tempC), 
            mean = mean(tempC), 
            sd = sd(tempC), 
            cv = sd/mean) %>% 
  ungroup() 

lim_daily

# write.csv(lim_daily, "output/limpet_daily_temps.csv")

lim_dailyL <- lim_daily %>% select(-c(mean:cv)) %>% 
  gather(., key = "metric", value = "tempC", maximum:minimum)

lim_dailyL

# Get daily summary stats 
lim_daily_summary <- lim_dailyL %>% 
  group_by(run_id, metric) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se, 
            max = max(tempC)) %>% 
  ungroup()

# Join with position data
lds2 <- inner_join(lim_daily_summary, positionDF, by = "run_id")

##### SUMMARISE MONTHLY #####
unique(lim_dailyL$metric)

lim_monthly <- lim_dailyL %>% group_by(run_id, metric, month, year) %>% 
  summarise(monthly_mean = mean(tempC)) %>% 
  ungroup() %>% 
  mutate(dateR = as.Date(paste(year, month, "15", sep = "-")))

lim_monthly

##### SUMMARISE ANNUAL #####

lim_annual <- lim_monthly %>% group_by(run_id, metric, year) %>% 
  summarise(maximum = max(monthly_mean), 
            median = median(monthly_mean), 
            minimum = min(monthly_mean)) %>% 
  ungroup()

la_max <- lim_annual %>% filter(metric == "maximum") %>% 
  select(run_id, year, maximum) 
la_med <- lim_annual %>% filter(metric == "median") %>% 
  select(run_id, year, median) 
la_min <- lim_annual %>% filter(metric == "minimum") %>% 
  select(run_id, year, minimum) 

la_summary <- inner_join(la_max, la_med, by = c("run_id", "year")) %>% 
  inner_join(., la_min, by = c("run_id", "year"))
la_summL <- gather(la_summary, key = "metric", value = "tempC", maximum:minimum)

la_summL2 <- la_summL %>% 
  inner_join(., positionDF, by = "run_id") %>% 
  inner_join(., spCodes, by = "sp")



annual_summary <- la_summL2 %>% 
  group_by(position, tidalHT, azimuth, aspect, species, metric) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()

##
lim_annual <- lim_dailyL %>% group_by(run_id, metric, year) %>% 
  summarise(annual_mean = mean(tempC)) %>% 
  ungroup()

lim_annual %>% 
  ggplot(aes(year, annual_mean, color = metric)) +
  facet_wrap(~ run_id, ncol = 5) + 
  geom_point() + geom_smooth(method = "lm")




