################################################################################
##' @title Function to get results from gls() (regression with autocorrelation)
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-09-14
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE)) 

library(nlme)
library(broom)

##### FUNCTION TO RETURN GLS SUMMARY #####

get_gls_results <- function(df, dep_var, ind_var) {

  nYrs <- dim(df)[1]
  
  # Run gls model
  gls.i <- gls(as.formula(paste(dep_var, "~", ind_var, sep = " ")), 
               data = df, correlation = corAR1())
  
  # Get dataframe
  gls_results.i <- as.data.frame(summary(gls.i)$tTable)
  names(gls_results.i) <- c("value", "std_error", "t_value", "p_value")
  
  # Estimate the total change in temperature over the study duration
  gls_results.i <- gls_results.i %>%
    mutate(coef_name = c("intercept", "slope"), 
           duration_yrs = nYrs, 
           change_temp_C = ifelse(coef_name == "slope", 
                               round(value * duration_yrs, 3), 
                               NA)) 

  return(gls_results.i)
}
