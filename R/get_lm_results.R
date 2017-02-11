################################################################################
##' @title Function to get results from lm()
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2017-02-11
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE)) 

library(broom)

##### FUNCTION TO RETURN LM SUMMARY #####

get_lm_results <- function(df, dep_var, ind_var) {

  n <- dim(df)[1]
  
  # Run model
  mod.i <- lm(as.formula(paste(dep_var, "~", ind_var, sep = " ")), 
               data = df)
  
  # Get dataframe
  lm_results.i <- tidy(mod.i)

  # Estimate the total change in temperature over the study duration
  gls_results.i <- gls_results.i %>%
    mutate(coef_name = c("intercept", "slope"), 
           duration_yrs = nYrs, 
           change_temp_C = ifelse(coef_name == "slope", 
                               round(value * duration_yrs, 3), 
                               NA)) 

  return(gls_results.i)
}
