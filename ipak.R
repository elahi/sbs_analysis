################################################################################
##' @title Package installation
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-09-16
##' 
##' @log 
################################################################################

# https://gist.github.com/stevenworthington/3178163

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("ggplot2", "dplyr", "reshape2", "RColorBrewer", "scales", "grid", 
              "tidyr", "lme4", "nlme", "lubridate", "readr", 
              "sp", "spatstat", "leaflet", "broom", 
              "rgdal", "raster", "marmap", "mapdata", "rtide", "ggmap", 
              "AICcmodavg", "tibble")

ipak(packages)
