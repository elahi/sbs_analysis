################################################################################
##' @title Functions to calculate air exposure along tidal gradient
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-11-30
##' 
##' @log 
################################################################################


##### CLASSIFY AIR VS WATER #####

classify_air_water <- function(rtide_df, new_df){
  
  tidal_height = new_df$tidal_height
  
  df <- rtide_df %>% 
    mutate(air_water = ifelse(TideHeight > tidal_height, "water", "air")) 
  
  return(df)
  
}
