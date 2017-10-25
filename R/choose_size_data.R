################################################################################
##' @title Function to choose which size data to use
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2017-10-24
##' 
##' @log Add a log here
################################################################################


choose_size_data <- function(method = 'repeated'){
  
  library(dplyr)

  # load data - repeated size bins
  if(method == "repeated"){
    dat <- read.csv("./output/sbsMaster.csv", na.strings = "NA", 
             stringsAsFactors = FALSE) %>%
      select(-c(X, row))
  }

  # load data - normally approximated sizes
  if(method == "approximated"){
    dat <- read.csv("./output/sbsMaster_norm.csv", na.strings = "NA", 
             stringsAsFactors = FALSE) %>%
      select(-c(X, row))
  }

  return(dat)
}
# 
# dat_norm <- choose_size_data(method = "approximated")
# dat <- choose_size_data()
# 
# # Add normally approximated sizes to original dataset
# 
# dat$size1mm_norm <- dat_norm$size1mm
# summary(dat$size1mm_norm)
# summary(dat$size1mm)
# par(mfrow = c(1,2))
# with(dat, boxplot(size1mm ~ era + sp, notch = TRUE))
# with(dat_norm, boxplot(size1mm ~ era + sp, notch = TRUE))
# 
# library(ggplot2)
# 
# dat %>%
#   ggplot(aes(sp, size1mm, fill = era)) +
#   geom_violin()
# 
# dat_norm %>%
#   ggplot(aes(sp, size1mm, fill = era)) +
#   geom_violin()
# 
# dat %>%
#   filter(era == "past") %>%
#   ggplot(aes(size1mm, size1mm_norm)) +
#   geom_point() +
#   facet_wrap(~ sp, scales = "free") +
#   geom_abline(slope = 1, intercept = 0, col = "red")
# 
