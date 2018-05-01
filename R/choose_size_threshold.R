################################################################################
##' @title Function to choose size cut-off and filter dataset
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-10-24
##' 
##' @log 
################################################################################

#### SPECIFIC TO ELAHI DATA #####

choose_size_threshold <- function(x, era = "past", my_quantile = 0.05, filter_data = TRUE){
  
  if(era == "past"){
    # Get size thresholds from past data only
    size_threshold_df <- x %>% filter(era == "past") %>% 
      group_by(species) %>% 
      summarise(size_threshold = quantile(size1mm, probs = my_quantile))
    
    x <- inner_join(x, size_threshold_df, by = c("species"))
  }
  
  if(era == "separate"){
    # Get size thresholds for each species x era combination
    x <- x %>% 
      group_by(era, species) %>% 
      mutate(size_threshold = quantile(size1mm, probs = my_quantile, na.rm = TRUE)) %>% 
      ungroup()
  }
  
  if(era == "separate_min"){
    # Get size thresholds for past and present data separately, then choose the smaller threshold and apply to both
    size_threshold <- x %>% group_by(era) %>% 
      mutate(size_threshold = quantile(size1mm, probs = my_quantile, na.rm = TRUE)) %>% ungroup() %>%
      distinct(era, size_threshold) %>% 
      arrange(size_threshold) %>% 
      slice(1) %>% select(size_threshold) %>% unlist(use.names = FALSE)
    
    x$size_threshold <- size_threshold
  }
  
  if(era == "combined"){
    # Get size thresholds for each species, across both eras
    x <- x %>% 
      group_by(species) %>% 
      mutate(size_threshold = quantile(size1mm, probs = my_quantile, na.rm = TRUE)) %>% 
      ungroup()
  }
  
  if(filter_data == TRUE){
    x <- x %>% filter(size1mm >= size_threshold)
  }
  
  ## Some formatting
  # Replace "." with " " in species name
  x$species <- gsub("[.]", " ", x$species)
  
  # Reorder levels
  species <- factor(x$species, levels = rev(c("Littorina keenae",
                                                 "Lottia digitalis", 
                                                 "Chlorostoma funebralis")))
  x$species <- species
  
  # Tidal heights for past data need fixing
  dat_pres <- x %>% filter(era == "present")
  dat_past <- x %>% filter(era == "past")
  
  # LODI is ok, but CHFU and LIKE need fixing
  # For past data, sampleUnit and sampleArea are the same
  dat_past2 <- dat_past %>% 
    mutate(sampleUnit = sampleArea, 
           tideHTm = ifelse(sp == "LODI", tideHTm, sample_area_tidal_ht))
  
  x2 <- rbind(dat_past2, dat_pres)
  
  return(x2)
  
}


#### GENERIC FUNCTION FOR ONE SPECIES #####

choose_size_threshold_general <- function(x, era = "past", my_quantile = 0.05, filter_data = TRUE){
  
  if(era == "past"){
    # Get size thresholds from past data only
    size_threshold_df <- x %>% filter(era == "past") %>% 
      summarise(size_threshold = quantile(size1mm, probs = my_quantile))
    
    x <- inner_join(x, size_threshold_df, by = c("species"))
  }
  
  if(era == "separate"){
    # Get size thresholds for each species x era combination
    x <- x %>% 
      group_by(era) %>% 
      mutate(size_threshold = quantile(size1mm, probs = my_quantile, na.rm = TRUE)) %>% 
      ungroup()
  }
  
  if(era == "combined"){
    # Get size thresholds for each species, across both eras
    x <- x %>% 
      mutate(size_threshold = quantile(size1mm, probs = my_quantile, na.rm = TRUE)) %>% 
      ungroup()
  }
  
  if(filter_data == TRUE){
    x <- x %>% filter(size1mm >= size_threshold)
  }
  
  return(x)
  
}
