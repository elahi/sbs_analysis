################################################################################
##' @title Temperature functions
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-01-08
##' 
##' @log Add a log here
################################################################################

#' path_to_weather_data = path to get desired weather text file

#path_to_weather_data = "data/uc_ipm/"

load_sst_air_data <- function(path_to_weather_data = "data/uc_ipm/"){
  
  library(dplyr)
  library(readr)
  source("R/process_weather_data.R")

  ## Get HMS temperature data - one measurement per day
  sst <- read_csv("output/sst_hms.csv") %>% 
    select(-c(X1, time)) %>% 
    mutate(dataset = "Seawater")
  
  ## Get Monterey weather data
  monterey <- format_weather_data(weather_file = "monterey.txt", 
                                  path_to_weather_data = "data/uc_ipm/", 
                                  lines_to_skip = 55, back_up = FALSE) 
  
  # I use air_obs - one measurement per day
  air <- monterey %>% rename(tempC = air_obs) %>% 
    select(dateR, month, year, tempC) %>% tbl_df() %>% 
    mutate(dataset = "Air")
  
  dat <- rbind(sst, air)
  
  return(dat)
  
}

subset_temperature_period <- function(temperature_data, startYear, endYear, duration = 10) {
  
  nYrsPrior <- duration - 1
  
  # Get relevant years
  past2 <- startYear
  present2 <- endYear
  past1 <- past2 - nYrsPrior
  present1 <- present2 - nYrsPrior
  
  past_temperature <- temperature_data %>% filter(year >= past1 & year <= past2) %>% 
    mutate(era = "past")
  present_temperature <- temperature_data %>% filter(year >= present1 & year <= present2) %>% 
    mutate(era = "present")
  
  df_temperature <- rbind(past_temperature, present_temperature) %>% 
    mutate(year_range = paste(startYear, endYear, sep = "-"))
  
  return(df_temperature)
  
}


get_summer_annual_means <- function(dat, my_quant = 0.75, remove = FALSE){
  
  dat2 <- dat %>% filter(month == 7 | month == 8 | month == 9) 
  
  if(remove == TRUE){
    dat2 <- dat2 %>% 
      group_by(year, month) %>% 
      mutate(threshold = quantile(tempC, my_quant, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(tempC >= threshold) # note greater than
  }

  dat_annual <- dat2 %>% group_by(year) %>%
    summarise(tempC = mean(tempC, na.rm = TRUE), 
              n = n()) %>%
    ungroup()
  
  return(dat_annual)
}


get_winter_annual_means <- function(dat, my_quant = 0.25, remove = FALSE){
  
  dat2 <- dat %>% filter(month == 12 | month == 1 | month == 2)
  
  if(remove == TRUE){
    dat2 <- dat2 %>% 
      group_by(year, month) %>% 
      mutate(threshold = quantile(tempC, my_quant, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(tempC <= threshold) # note less than
  }
  
  dat_annual <- dat2 %>% group_by(year) %>%
    summarise(tempC = mean(tempC, na.rm = TRUE), 
              n = n()) %>%
    ungroup()
  
  return(dat_annual)
}
