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


choose_size_threshold <- function(x, era = "past", my_quantile = 0.05, filter_data = TRUE){
  
  if(era == "past"){
    # Get size thresholds from past data only
    size_threshold_df <- x %>% filter(era == "past") %>% 
      group_by(species) %>% 
      summarise(size_threshold = quantile(size1mm, probs = my_quantile))
    
    x <- inner_join(x, size_threshold_df, by = c("species"))
  }
  
  if(era == "both"){
    # Get size thresholds for each species x era combination
    x <- x %>% 
      group_by(era, species) %>% 
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
  
  
  return(x)
  
}
