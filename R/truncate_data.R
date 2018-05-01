
# Function to truncate data for one species at a time
truncate_data <- function(dat, era = "past", quant = 0.5, filter_data = TRUE){
  
  my_quantile = quant
  
  if(era == "past"){
    # Get size thresholds from past data only
    size_threshold <- dat %>% filter(era == "past") %>% 
      summarise(size_threshold = quantile(size1mm, probs = my_quantile)) %>% unlist(use.names = FALSE)
    dat$size_threshold <- size_threshold
  }
  
  if(era == "combined"){
    # Get size thresholds from past and present data combined
    size_threshold <- dat %>% 
      summarise(size_threshold = quantile(size1mm, probs = my_quantile)) %>% unlist(use.names = FALSE)
    dat$size_threshold <- size_threshold
  }
  
  if(era == "separate"){
    # Get size thresholds for past and present data separately
    dat <- dat %>% group_by(era) %>% 
      mutate(size_threshold = quantile(size1mm, probs = my_quantile)) %>% ungroup() 
  }
  
  if(era == "separate_min"){
    # Get size thresholds for past and present data separately, then choose the smaller threshold and apply to both
    size_threshold <- dat %>% group_by(era) %>% 
      mutate(size_threshold = quantile(size1mm, probs = my_quantile)) %>% ungroup() %>%
      distinct(era, size_threshold) %>% 
      arrange(size_threshold) %>% 
      slice(1) %>% select(size_threshold) %>% unlist(use.names = FALSE)
    
    dat$size_threshold <- size_threshold
  }

  if(filter_data == TRUE){
    dat <- dat %>% filter(size1mm >= size_threshold)
  }
  
  return(dat)
}

# truncate_data_unique <- function(dat, quant = 0.5){
#   my_quantile = quant
#   dat <- dat %>% group_by(era) %>% 
#     mutate(size_threshold = quantile(size1mm, probs = my_quantile)) %>% ungroup() 
#   
#   if(my_quantile > 0){
#     dat <- dat %>% filter(size1mm >= size_threshold)
#   }
# 
#   return(dat)
# }
