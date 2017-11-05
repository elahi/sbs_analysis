

get_summer_annual_means <- function(dat, my_quant = 0.75, remove = FALSE){
  
  dat2 <- dat %>% filter(month == 7 | month == 8 | month == 9) 
  
  if(remove == TRUE){
    dat2 <- dat2 %>% 
      group_by(year, month) %>% 
      mutate(threshold = quantile(tempC, my_quant, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(tempC >= threshold) # note greater than
  }

  dat_annual <- dat2 %>% group_by(year) %>%
    summarise(tempC = mean(tempC, na.rm = TRUE), 
              n = n()) %>%
    ungroup()
  
  return(dat_annual)
}


get_winter_annual_means <- function(dat, my_quant = 0.25, remove = FALSE){
  
  dat2 <- dat %>% filter(month == 12 | month == 1 | month == 2)
  
  if(remove == TRUE){
    dat2 <- dat2 %>% 
      group_by(year, month) %>% 
      mutate(threshold = quantile(tempC, my_quant, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(tempC <= threshold) # note less than
  }
  
  dat_annual <- dat2 %>% group_by(year) %>%
    summarise(tempC = mean(tempC, na.rm = TRUE), 
              n = n()) %>%
    ungroup()
  
  return(dat_annual)
}
