################################################################################
##' @title Function to process weather data
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2016-11-08
################################################################################

##' path = path to files
##' weather_file = name of file to be loaded
##' back_up = if FALSE, then raw weather data did not include back_ups

# weather_file <- "monterey.txt"
# path_to_weather_data = "data/uc_ipm/"
# lines_to_skip <- 55

format_weather_data <- function(weather_file = "monterey.txt", 
                                path_to_weather_data = "data/uc_ipm/",
                                lines_to_skip = 55, back_up = FALSE){
  
  library(dplyr)
  library(lubridate)
  
  weather <- read.csv(file = paste(path_to_weather_data, weather_file, sep = ""), 
                      skip = lines_to_skip, header = TRUE, stringsAsFactors = FALSE)
  
  weather <- weather %>% select(Station:Wx) %>%
    mutate(dateR = ymd(Date), 
           month = month(dateR), 
           year = year(dateR)) %>%
    rename(precip = Precip, air_max = Air.max, air_min = min, air_obs = obs)
  
  weather2 <- weather %>% select(Station, dateR, month, year, air_max:air_obs) %>% 
    filter(year < 2016) 
  
  weather2$backup_status <- ifelse(back_up == TRUE, "backup", "backup_no")
  
  return(weather2)
  
}

# monterey <- format_weather_data(weather_file = "monterey.txt", 
#                                 path_to_weather_data = "data/uc_ipm/", 
#                                 lines_to_skip = 55, back_up = FALSE) 

