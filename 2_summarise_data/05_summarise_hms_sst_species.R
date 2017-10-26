################################################################################
##' @title Summarise SST data from HMS website for each species
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2017-08-18
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# Get hms temperature data
#source("R/get_hms_sst.R")
sst_hms <- read_csv("output/sst_hms.csv")[-1]
glimpse(sst_hms)

# Get snail size data
source("05_summarise_size_data.R")

# Get hms temperature functions
source("R/hms_sst_functions.R")

# Original data were single, daily measurements
glimpse(sst_hms)

sst_hms %>% 
  ggplot(aes(as.factor(month), tempC)) + geom_boxplot()

# I want a time-series starting 10 years prior to the first historic sampling year
# Childs sampled Littorina in 1947
# So, start year is 1938

sst <- sst_hms %>% filter(year > 1937 & year < 2016) 

# What are the warmest and coldest months?
sst %>% group_by(month) %>% 
  summarise(meanTemp = mean(tempC)) %>% ungroup() %>% 
  arrange(meanTemp)

# Coldest == Dec, Jan, Feb
# Warmest == July, Aug, Sep

# Get relevant years for each species
spYrs <- dat5 %>% select(sp, era, year) %>%
  distinct() %>% 
  spread(key = era, value = year)

sp_code = "LIKE"

subset_sst_period <- function(spYrs, temperature_data = sst_hms, duration = 10) {
  
  nYrsPrior <- duration - 1
  
  # Get relevant years
  spYrs_sp <- spYrs #%>% filter(sp == sp_code)
  past2 <- spYrs_sp$past
  present2 <- spYrs_sp$present
  past1 <- past2 - nYrsPrior
  present1 <- present2 - nYrsPrior
  
  past_temperature <- temperature_data %>% filter(year >= past1 & year <= past2) %>% 
    mutate(era = "past")
  present_temperature <- temperature_data %>% filter(year >= present1 & year <= present2) %>% 
    mutate(era = "present")
  
  df_temperature <- rbind(past_temperature, present_temperature)
  
  return(df_temperature)
  
}

sst_sp <- spYrs %>% group_by(sp) %>% 
  do(., subset_sst_period(., sst_hms, duration = 10)) %>% 
  ungroup()

sst_sp %>% 
  ggplot(aes(dateR, tempC, color = era)) + 
  facet_wrap(~ sp, nrow = 3) + 
  geom_line()

##### SUMMARISE MONTHLY #####
unique(sst_sp$month)

sst_sp_monthly <- sst_sp %>% group_by(sp, month, era) %>% 
  summarise(median = median(tempC), 
            mean = mean(tempC), 
            sd = sd(tempC), 
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se, 
            upper = quantile(tempC, probs = 0.75), 
            lower = quantile(tempC, probs = 0.25)) %>% 
  ungroup() 

# median and confidence intervals
sst_sp_monthly %>% 
  ggplot(aes(month, median, color = era)) + 
  facet_wrap(~ sp) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, color = NULL, fill = era), alpha = 0.5) + 
  geom_line() 

# mean and sd
sst_sp_monthly %>% 
  ggplot(aes(month, mean, color = era)) + 
  facet_wrap(~ sp) + 
  geom_ribbon(aes(ymin = mean - CI, ymax = mean + CI, color = NULL, fill = era), alpha = 0.5) + 
  geom_line() 

##### SUMMARISE WEEKLY #####
unique(sst_sp$month)

sst_sp_weekly <- sst_sp %>% 
  mutate(weekN = week(dateR)) %>% 
  group_by(sp, weekN, era) %>% 
  summarise(median = median(tempC), 
            mean = mean(tempC), 
            sd = sd(tempC), 
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se, 
            upper = quantile(tempC, probs = 0.75), 
            lower = quantile(tempC, probs = 0.25)) %>% 
  ungroup() 

# median and confidence intervals
sst_sp_weekly %>% 
  ggplot(aes(weekN, median, color = era)) + 
  facet_wrap(~ sp) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, color = NULL, fill = era), alpha = 0.5) + 
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, color = NULL, fill = era), alpha = 0.5) + 
  geom_line() 

# mean and ci
sst_sp_weekly %>% 
  ggplot(aes(weekN, mean, color = era)) + 
  facet_wrap(~ sp) + 
  geom_ribbon(aes(ymin = mean - CI, ymax = mean + CI, color = NULL, fill = era), alpha = 0.5) + 
  geom_line() + 
  theme(legend.position = "top")
ggsave("figs/temp_monthly_sst.png", height = 4, width = 7)
