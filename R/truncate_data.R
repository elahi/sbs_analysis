
# Function to truncate data for one species at a time

# Function to truncate data
truncate_data <- function(dat, quant = 0.5){
  
  my_quantile = quant
  size_threshold <- dat %>% filter(era == "past") %>% 
    summarise(size_threshold = quantile(size1mm, probs = my_quantile)) %>% unlist(use.names = FALSE)
  dat$size_threshold <- size_threshold
  
  if(my_quantile > 0){
    dat <- dat %>% filter(size1mm >= size_threshold)
  }
  return(dat)
  
}

truncate_data_unique <- function(dat, quant = 0.5){
  my_quantile = quant
  dat <- dat %>% group_by(era) %>% 
    mutate(size_threshold = quantile(size1mm, probs = my_quantile)) %>% ungroup() 
  
  if(my_quantile > 0){
    dat <- dat %>% filter(size1mm >= size_threshold)
  }

  return(dat)
}
