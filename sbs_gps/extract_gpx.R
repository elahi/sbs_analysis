################################################################################
##' @title Extract GPX from etrex10
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-20
################################################################################

# From Robinlovelace github
# https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/vignettes/gpx-files.Rmd

# Use plotKML
library(tidyverse)
library(plotKML)
library(lubridate)

## Specific date
p1 <- readGPX("sbs_gps/GPX/Waypoints_01-AUG-15.gpx")
names(p1)
p1
head(p1$waypoints[[1]][[1]])
plot(lat ~ lon, data = p1$waypoints)
write.csv(p1$waypoints, 'output/gps_ibuttons_150801.csv')


## Extract all waypoints
p2 <- readGPX("sbs_gps/GPX/Current/Current.gpx")
names(p2)
p2
p2$metadata
p2$bounds
p2$waypoints
p2$tracks
p2$routes

str(p2)
p2$tracks[[1]]
p2$tracks[[1]]

head(p2$tracks[[1]][[1]])
head(unlist(p2$tracks))
t1 <- do.call("rbind", unlist(p2$tracks, recursive=F))
plot(t1$lon, t1$lat)

## Extract all waypoints

model_path <- "sbs_gps/GPX/"

# Get the most recent file, assumes that the files are ordered chronologically
fileNames <- dir(path = model_path, recursive = FALSE, pattern = "Waypoints_")
nFiles <- length(fileNames)
fileNames

## Initialize dataframe

# First I need all the column names
i <- 15
p_i <- readGPX(paste(model_path, fileNames[i], sep = ""))
p_df <- p_i$waypoints
p_df

my_cols <- names(p_df)
my_cols

i <- 1
p_i <- readGPX(paste(model_path, fileNames[i], sep = ""))
p_df <- p_i$waypoints
p_df
p_df[my_cols[!(my_cols %in% colnames(p_df))]] = NA
p_df


## Some p_dfs are missing 'ele' and all but one are missing 'cmt'

for(i in 2:length(fileNames)){
  
  p_i <- readGPX(paste(model_path, fileNames[i], sep = ""))
  p_df_i <- p_i$waypoints
  
  print(ncol(p_df_i))
  p_df_i[my_cols[!(my_cols %in% colnames(p_df_i))]] = NA
  
  p_df <- rbind(p_df, p_df_i)

}

p_df

## Add dates
p_df2 <- p_df %>% 
  mutate(date = as_date(time)) %>% 
  as_tibble() %>% 
  select(-c(sym, cmt, time)) %>% 
  arrange(name)
p_df2

write.csv(p_df2, 'output/gps_etrex_2019.csv')
