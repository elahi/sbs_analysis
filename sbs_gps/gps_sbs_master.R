################################################################################
##' @title Get lat-longs for all snails measured at Hopkins
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-20
##' @log 
################################################################################

library(tidyverse)
library(lubridate)

# Load modern data, along with historic data for Lottia (Hexter)
mod <- read.csv("./data/SBSmaster_150717.csv", na.strings = c("NA"))
names(mod)
mod %>% 
  distinct(species, era, long, lat)

sbs_ll <- mod %>% 
  distinct(species, sp, site, nest1, long, lat)
  
sbs_ll

write.csv(sbs_ll, "output/gps_sbsMaster.csv")

