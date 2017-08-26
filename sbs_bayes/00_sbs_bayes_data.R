################################################################################
##' @title Analyzing snail data with Bayes
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-08-21
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### PREP DATA #####

# load data
source("03_identify_size_cutoff.R")

### I need to recode some things for JAGS
## Recode era: past = 0, present = 1
## thc = scaled tidal height
datJ <- dat4 %>% 
  mutate(eraJ = ifelse(era == "past", 0, 1), 
         thc = as.numeric(scale(sample_area_tidal_ht, scale = F)))
head(datJ)
summary(datJ)

## Analyze species separately
## Use group level effects for Littorina and Lottia
## For Chlorostoma, analyze sites separately

## Make separate dataframes
childsDF <- droplevels(filter(datJ, sp == "LIKE"))
waraDF <- droplevels(filter(datJ, sp == "CHFU"))
hexDF <- droplevels(filter(datJ, sp == "LODI"))
