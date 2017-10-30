################################################################################
##' @title Prepare snail data for Bayesian analysis
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-10-30
##' 
##' @log 
################################################################################

rm(list=ls(all=TRUE)) 

##### PACKAGES, DATA #####
# Load data
source("2_summarise_data/summarise_size_density.R")
# Select relevant files
rm(list = setdiff(ls(), c("dat_dens", "datMeans4")))

source("R/HighstatLibV6.R")

statDat <- dat_dens 

# Subset data by species
waraDF <- statDat %>% filter(sp == "CHFU")
hexDF <- statDat %>% filter(sp == "LODI")
childsDF <- statDat %>% filter(sp == "LIKE")

# Get Wara means
wara_means <- datMeans4 %>% filter(sp == "CHFU")
