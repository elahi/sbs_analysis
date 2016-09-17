################################################################################
##' @title Estimating duration of air exposure along an intertidal gradient
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-09-16
##' 
##' @log 
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(rtide)
library(scales) # for the date_format function

dat <- tide_height('Monterey Harbor',from = as.Date('2015-01-01'), 
                  to = as.Date('2015-12-31'), minutes = 10, tz ='PST8PDT')
monterey_tides_2015 <- dat %>% dplyr::select(Station)
save(monterey_tides_2015, file = "output/monterey_tides_2015.RData")

glimpse(dat)
str(dat)

ggplot(data = dat, aes(x = DateTime, y = TideHeight)) + 
  geom_line() + 
  scale_y_continuous(name = "Tide Height (m)") +
  ggtitle("Monterey Harbor")

##### FUNCTION TO RETURN AIR AND WATER DURATIONS FOR A GIVEN TIDAL HEIGHT #####

rtide_df = dat
tidal_height = 0

get_air_water_time <- function(rtide_df, tidal_height){
  
  df <- rtide_df %>% 
    mutate(air_water = ifelse(TideHeight > tidal_height, "water", "air"), 
           intv = as.numeric(DateTime - lag(DateTime)))
  
  total_hrs <- df %>% 
    summarise(sum_duration = (sum(intv, na.rm = TRUE))/60) %>%
    unlist(use.names = FALSE)
  
  df2 <- df %>% group_by(air_water) %>%
    summarise(duration_hrs = (sum(intv, na.rm = TRUE))/60, 
              duration_prop = duration_hrs/total_hrs) %>%
    complete(air_water)
  
  df2
  
  return(df2)
}

intertidal_heights <- seq(0, 2, by = 0.01)


##### FUNCTION FOR DATAFRAME #####

new_df <- data.frame(tidal_height = seq(0, 2, by = 0.01), 
                     Station = "Monterey")
head(new_df)
str(new_df)

new_df <- new_df[10, ]
str(new_df)

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

new_df <- data.frame(tidal_height = seq(0, 2, by = 0.01))
str(new_df)

#get_air_water_time(dat, new_df)

test <- new_df %>% group_by(tidal_height) %>% 
  do(get_air_water_time(dat, .)) %>% ungroup()

head(test)
tail(test)


test %>%
  ggplot(aes(tidal_height, duration_prop, color = air_water)) +
  geom_point()

test2 <- test %>% filter(air_water == "air") %>% 
  dplyr::select(tidal_height, duration_prop) %>% 
  rename(air_exposure_proportion = duration_prop)

## Everything > 2m will have air exposure = 100%
dummy_df <- data.frame(tidal_height = seq(2.01, 8, by = 0.01), 
                       air_exposure_proportion = rep(1))

## Combine

test3 <- rbind(test2, dummy_df)

write.csv(test3, "output/monterey_air_exposure.csv")


