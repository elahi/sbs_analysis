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
glimpse(dat)
str(dat)

ggplot(data = dat, aes(x = DateTime, y = TideHeight)) + 
  geom_line() + 
  scale_x_datetime(name = "January 1-31, 2015", 
                   labels = date_format("%H:%M", tz="PST8PDT")) +
  scale_y_continuous(name = "Tide Height (m)") +
  ggtitle("Monterey Harbor")

head(dat)

dat2 <- dat %>% dplyr::select(-Station) %>% 
  mutate(air_1m = ifelse(TideHeight > 1, "air", "water"), 
         intv = as.numeric(DateTime - lag(DateTime)))
dat2

unique(dat2$air_1m)

dat2 %>% group_by(air_1m) %>%
  summarise(sum_duration = (sum(intv, na.rm = TRUE))/60) 

##### FUNCTION TO RETURN AIR AND WATER DURATIONS FOR A GIVEN TIDAL HEIGHT #####

rtide_df = dat
tidal_height = 2

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

get_air_water_time(dat, tidal_height = 1.97)



