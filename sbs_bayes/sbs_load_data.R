################################################################################
##' @title Function to load snail data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-09-05
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### PREP DATA #####

load_sbs_data <- function(min_cutoff = FALSE){
  
  # load data
  source("03_identify_size_cutoff.R")
  
  # Remove snails below 5%ile threshold (from historic samples)
  if(min_cutoff != FALSE){
    datJ <- dat5
  }
  
  # Do not remove any snails
  if(min_cutoff == FALSE){
    datJ <- dat4
  }
  
  # Recode era for JAGS
  # Center tidal height (thc)
  # Remove NA values for size
  datJ <- datJ %>% 
    filter(!is.na(size1mm)) %>% 
    mutate(eraJ = ifelse(era == "past", 0, 1), 
           thc = as.numeric(scale(sample_area_tidal_ht, scale = F))) 
  
  return(datJ)
}
