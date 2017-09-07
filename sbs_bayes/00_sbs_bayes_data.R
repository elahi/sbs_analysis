################################################################################
##' @title Prepare snail data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-08-21
##' 
##' @log 
################################################################################

rm(list=ls(all=TRUE)) 

# Get function to load data
source("sbs_bayes/sbs_load_data.R")
datJ <- load_sbs_data(min_cutoff = FALSE)
# Remove all but datJ
rm(list = setdiff(ls(), "datJ"))

## Analyze species separately
## Use group level effects for Littorina and Lottia
## For Chlorostoma, analyze sites separately

## Make separate dataframes
# Littorina keenae 
childsDF <- droplevels(filter(datJ, sp == "LIKE"))
childsPast <- childsDF %>% filter(era == "past")
childsPres <- childsDF %>% filter(era == "present")

# Chlorostoma funebralis
waraDF <- droplevels(filter(datJ, sp == "CHFU"))
waraPast <- waraDF %>% filter(era == "past")
waraPres <- waraDF %>% filter(era == "present")

# Lottia digitalis
hexDF <- droplevels(filter(datJ, sp == "LODI"))
hexPast <- hexDF %>% filter(era == "past")
hexPres <- hexDF %>% filter(era == "present")

##### PREDICTING NEW DATA #####

### Create range of tidal height values for prediction
thc <- scale(datJ$sample_area_tidal_ht, scale = FALSE)
mu_th <- mean(datJ$sample_area_tidal_ht)
sd_th <- sd(datJ$sample_area_tidal_ht)
th_predict <- seq(min(datJ$sample_area_tidal_ht) * 0.95, 
                  max(datJ$sample_area_tidal_ht * 1.05), by = 0.05)
thc_predict <- th_predict - mu_th

