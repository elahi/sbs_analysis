################################################################################
##' @title Process HMS weather station data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-15
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# Compare two different file structures
hmswO <- read_csv("data/climate_monterey/hms_west_beach/2009-05.txt")
hmswP <- read_csv("data/climate_monterey/hms_west_beach/2009-05p.txt")
names(hmswO)
names(hmswP)

##### LIST OF FILE NAMES #####
# Need a list of file names for west beach

path_to_files <- "./data/climate_monterey/hms_west_beach/"
fileNames <- dir(path = path_to_files, recursive = TRUE, pattern = ".txt") 
fileNames

# Remove fileNames that have a p
# p means partial - so months with a p are not complete, therefore, I should not use them when aggregating to month or above
p_files <- grep("p", fileNames)

fileNames[p_files]
fileNames %in% fileNames[p_files]

fls <- fileNames[!fileNames %in% fileNames[p_files]]

##### COMBINE FILES #####
i = 1
file.i <- read_csv(file = paste(path_to_files, fls[i], sep = ""))
names(file.i)


remove_column_duplicates <- function(file.i){
  
  orig_columns <- names(file.i)
  dup_columns <- duplicated(names(file.i))
  
  if (TRUE %in% dup_columns == TRUE){
    file.i <- file.i[, orig_columns[!dup_columns]]
    return(file.i)
  }
  
  if (TRUE %in% dup_columns == FALSE){
    return(file.i)
  }
  
}

process_hms_weather <- function(file.i) {
  
  # Check for duplicates and remove the second set if relevant
  file.iNEW <- remove_column_duplicates(file.i)
  
  # rename first three columns
  names(file.iNEW)[1:3] <- c("year", "dayofyear", "hourmin")
  
  # Get desired columns and rename them
  rmd2 <- file.iNEW %>% select(year:wd, LiCor, `Air C`) %>% 
    rename(wind_speed = ws, wind_dir = wd, 
           licor = LiCor, tempC = `Air C`)
  
  # If licor values are < 0, change to NA
  rmd3 <- rmd2 %>% 
    mutate(licor = ifelse(licor < 0, NA, licor))
  
  return(rmd3)
} 

i = 1
fileName <- fls[i]
file.i <- read_csv(file = paste(path_to_files, fls[i], sep = ""))
hms_weather_compiled <- process_hms_weather(file.i)
hms_weather_compiled$fileName <- fileName

for (i in 2:length(fls)) {
  
  fileName <- fls[i]
  
  file.i <- read_csv(file = paste(path_to_files, fls[i], sep = ""))
  fileNew <- process_hms_weather(file.i)
  fileNew$fileName <- fileName
  
  hms_weather_compiled <- rbind(hms_weather_compiled, fileNew)
  
}

completedFiles <- unique(hms_weather_compiled$fileName)
!fls %in% completedFiles

range(hms_weather_compiled$year)
tail(hms_weather_compiled)
head(hms_weather_compiled)

# some problems::
hms_weather_compiled %>% filter(year < 2002)
hms_weather_compiled %>% filter(year > 2016)
glimpse(hms_weather_compiled)
hwc <- hms_weather_compiled %>% filter(year < 2016.1 & year > 2001.9)


hwcL <- hwc %>% gather(key = climate_var, value = value, wind_speed:tempC) %>% 
  mutate(dateR = as.POSIXct(strptime(paste(year, dayofyear, sep = " "), "%Y %j")))

##### SUMMARISE DAILY MEASUREMENTS #####

hwc_daily <- hwcL %>% filter(!is.na(value)) %>%
  group_by(climate_var, year, dayofyear, dateR) %>% 
  summarise(maximum = max(value), 
            median = median(value), 
            minimum = min(value)) %>% 
  ungroup() %>% 
  mutate(dateR = as.POSIXct(strptime(paste(year, dayofyear, sep = " "), "%Y %j")), 
         month = month(dateR))

hwc_daily

hwc_daily %>%
  ggplot(aes(dateR, median)) +
  geom_line() + 
  facet_wrap(~ climate_var, scales = "free")



##### LIST OF FILE NAMES #####
##### LIST OF FILE NAMES #####
##### LIST OF FILE NAMES #####
##### LIST OF FILE NAMES #####


# Column	Units or definition
# year	
# dayofyear	
# hourmin	
# ws	meters per second
# wd	north is 0°
# wd-sd	standard deviation of wind direction
# panel C	instrument panel temperature  °C
# box C	
# LiCor	watts per square meter
# LiCor-sd	standard deviation of LiCor
# LiCor-min	min reading during the last ten minutes
# LiCor-max	max reading during the last ten minutes
# KZ	non-functioning
# KZ-sd	non-functioning
# KZ-min	non-functioning
# KZ-max	non-functioning
# Air C	Air temperature °C
# Air C-sd	standard deviation of air temperature during the last ten minutes
# Air C-min	min temperature during the last ten minutes
# Air C-max	max temperature during the last ten minutes
# RH	relative humidity - Not very useful this close to the ocean as it is usually high all the time
# RH-sd	standard deviation of RH
# RH-min	min RH during the last ten minutes
# RH-max	max RH during the last ten minutes
# A2 C	second air temperature reading
# A2 C-sd	
# A2 C-min	
# A2 C-max	
# volts	voltage of the internal backup battery




hmsw <- rbind(hmsw8, hmsw9)
glimpse(hmsw)

# Get relevant columns and rename them
hmsw2 <- hmsw %>% select(year:wd, LiCor, `Air C`) %>% 
  rename(wind_speed = ws, wind_dir = wd, 
         licor = LiCor, tempC = `Air C`)

# If licor values are < 0, change to NA
hmsw3 <- hmsw2 %>% 
  mutate(licor = ifelse(licor < 0, NA, licor))
summary(hmsw3)

hmswL <- hmsw3 %>% gather(key = climate_var, value = value, wind_speed:tempC) %>% 
  mutate(dateR = as.POSIXct(strptime(paste(year, dayofyear, sep = " "), "%Y %j")))

unique(hmswL$hourmin)  


##### SUMMARISE DAILY MEASUREMENTS #####

hmsw_daily <- hmswL %>% filter(!is.na(value)) %>%
  group_by(climate_var, year, dayofyear) %>% 
  summarise(maximum = max(value), 
            median = median(value), 
            minimum = min(value)) %>% 
  ungroup() %>% 
  mutate(dateR = as.POSIXct(strptime(paste(year, dayofyear, sep = " "), "%Y %j")), 
         month = month(dateR))

hmsw_daily

hmsw_daily %>%
  ggplot(aes(dateR, median)) +
  geom_line() + 
  facet_wrap(~ climate_var, scales = "free")

