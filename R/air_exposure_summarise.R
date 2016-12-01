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

source("R/air_exposure_functions.R")

load("output/monterey_tides_1930_2020.RData")
head(dat)

##### FINE SCALE DATA #####

##' Load fine scale data for 4 sampling years
load("output/monterey_tides_fine_scale.RData")

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


##### DENNY FUNCTION FOR DATAFRAME #####

new_df <- data.frame(tidal_height = seq(0, 2, by = 0.1), Station = "Monterey")
head(new_df)
str(new_df)

new_df <- new_df[10, ]
new_df
str(new_df)
rtide_df = dat %>% filter(year == 1930)

classify_air_water <- function(rtide_df, new_df){
  
  tidal_height = new_df$tidal_height
  
  replacement_interval = 1 # this number should be the interval
  
  df <- rtide_df %>% 
    mutate(air_water = ifelse(TideHeight > tidal_height, "water", "air"), 
           intv = as.numeric(DateTime - lag(DateTime)), 
           intv = ifelse(is.na(intv), 
                         replacement_interval, intv)) 
  return(df)
}

test <- classify_air_water(rtide_df, new_df)
test

new_df <- data.frame(tidal_height = seq(0, 2.2, by = 0.1))
str(new_df)

#get_air_water_time(dat, new_df)

test <- new_df %>% group_by(tidal_height) %>% 
  do(classify_air_water(dat, .)) %>% ungroup()
head(test)
tail(test)

test %>% filter(air_water == "air") %>% 
  group_by(tidal_height, year, day) %>% 
  summarise(sum_hours = sum(intv))


##### FUNCTION FOR DATAFRAME #####

new_df <- data.frame(tidal_height = seq(0, 2, by = 0.1), Station = "Monterey")
head(new_df)
str(new_df)

new_df <- new_df[10, ]
new_df
str(new_df)
rtide_df = dat %>% filter(year == 1930)

get_air_water_time <- function(rtide_df, new_df){
  
  tidal_height = new_df$tidal_height
  
  df <- rtide_df %>% 
    mutate(air_water = ifelse(TideHeight > tidal_height, "water", "air"), 
           intv = as.numeric(DateTime - lag(DateTime)))
  
  total_hrs <- df %>% 
    summarise(sum_duration = (sum(intv, na.rm = TRUE))/60) %>%
    unlist(use.names = FALSE)
  
  df2 <- df %>% group_by(air_water) %>%
    summarise(duration_hrs = (sum(intv, na.rm = TRUE))/60, 
              duration_prop = duration_hrs/total_hrs)
  
  return(df2)
}

dat <-  dat %>% mutate(x = replace(x, x<0, NA))
df <- df %>%
  mutate(colname = ifelse(is.na(colname),0,colname))


get_air_water_time <- function(rtide_df, new_df){
  
  tidal_height = new_df$tidal_height
  
  replacement_interval = 1 # this number should be the interval
  
  df <- rtide_df %>% 
    mutate(air_water = ifelse(TideHeight > tidal_height, "water", "air"), 
           intv = as.numeric(DateTime - lag(DateTime)), 
           intv = ifelse(is.na(intv), 
                         replacement_interval, intv)) %>% # this line replaces 1st NA 
    group_by(year) %>% 
    mutate(total_hrs = sum(intv, na.rm = TRUE)) %>% 
    ungroup() 
  
  df2 <- df %>% group_by(year, air_water) %>%
    summarise(duration_hrs = (sum(intv, na.rm = TRUE)),
              total_hrs = mean(total_hrs), 
              duration_prop = duration_hrs/total_hrs) %>% 
    ungroup()
  
  return(df2)
}


new_df <- data.frame(tidal_height = seq(0, 2.2, by = 0.1))
str(new_df)

#get_air_water_time(dat, new_df)

test <- new_df %>% group_by(tidal_height) %>% 
  do(get_air_water_time(dat, .)) %>% ungroup()

head(test)
tail(test)

test %>% filter(air_water == "air") %>% 
  filter(tidal_height == 0 | tidal_height == 0.5 |
           tidal_height == 1 | tidal_height == 1.5) %>% 
  ggplot(aes(year, duration_prop)) +
  geom_line() + facet_wrap(~tidal_height, scales = "free_y")

test %>% filter(air_water == "air") %>% 
  filter(year == 1950 | year == 2015) %>% 
  ggplot(aes(tidal_height, duration_prop, color = as.factor(year))) +
  geom_line() + 
  facet_wrap(~ year, scales = "free_y")




test2 <- test %>% filter(air_water == "air") %>% 
  dplyr::select(tidal_height, duration_prop) %>% 
  rename(air_exposure_proportion = duration_prop)

## Everything > 2m will have air exposure = 100%
dummy_df <- data.frame(tidal_height = seq(2.01, 8, by = 0.01), 
                       air_exposure_proportion = rep(1))

## Combine

test3 <- rbind(test2, dummy_df)

write.csv(test3, "output/monterey_air_exposure.csv")


