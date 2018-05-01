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

##### PACKAGES, DATA #####
# Load data
source("2_summarise_data/summarise_size_density.R")
# Select relevant files
rm(list = setdiff(ls(), c("dat_dens", "datMeans4")))

source("R/HighstatLibV6.R")
source("R/scale_gelman.R")

## Offset tidal height for plotting
dat_dens <- dat_dens %>% 
  mutate(tideHTm_offset = ifelse(era == "past" & sp == "LIKE", tideHTm + 0.125, 
                                 ifelse(era == "past" & sp == "LODI", tideHTm + 0.025,
                                        tideHTm)))

##### SUBSET DATA BY SPECIES #####
##' x1 = era
##' x2 = density
##' x3 = tide height

statDat <- dat_dens %>% 
  mutate(era01 = ifelse(era == "past", 0, 1), 
         x1 = era01, x2 = density_m2, x3 = tideHTm)

# Subset data by species
waraDF <- statDat %>% filter(sp == "CHFU") 
hexDF <- statDat %>% filter(sp == "LODI") 
childsDF <- statDat %>% filter(sp == "LIKE")

# Get Wara means
wara_means <- datMeans4 %>% filter(sp == "CHFU")

