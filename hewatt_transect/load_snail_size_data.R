################################################################################
##' @title Load snail data plus sizes
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2017-12-21
##' 
##' @log Add a log here
################################################################################

load_snail_size_data <- function(remove_rare = TRUE){
  
  library(dplyr)
  library(readr)
  library(readxl)
  
  ## Raw abundance data
  dat <- read_csv("workspace/hewatt_sagarin_data_snail.csv") %>% 
    select(-X1) 
  
  ## Remove old sizes
  dat <- dat %>% select(-c(size1:size1_dim))
  
  ## Load snail size dataframe
  snail_df <- read_excel("output/snail_df.xlsx", na = "NA")

  ## Join with data
  dat <- snail_df %>% select(spp, size1:size1_dim, size2) %>% 
    left_join(dat, ., by = "spp")
  
  ## Releval species according to size
  ## Create new quadrat factor for stats
  dat <- dat %>% 
    mutate(spp = reorder(as.factor(spp), size1), 
           quadrat_factor = as.factor(paste("q_", quadrat, sep = "")))
  
  ## Remove rare species (if TRUE)
  if(remove_rare == TRUE){
    # 19 quadrats per year, species must be present in > 5% of quads
    n_quads <- length(unique(dat$quadrat))
    n_yrs <- length(unique(dat$year))
    quad_threshold <- 0.05 * n_quads * n_yrs # returns 13 species
    
    # Get quad counts for each species
    spp_quad_counts <- dat %>% 
      filter(abundance > 0) %>% 
      distinct(spp, year, quadrat) %>% 
      count(spp)
    
    spp_keep <- spp_quad_counts %>% filter(n > quad_threshold)
    dat <- dat[dat$spp %in% spp_keep$spp, ]
  }
  
  return(dat)
  
}



