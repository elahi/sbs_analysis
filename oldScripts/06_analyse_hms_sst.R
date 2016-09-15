################################################################################
##' @title Analyze SST data from HMS website
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-12
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE)) 

# Get SST summary
source("05_summarise_hms_sst.R")
sst_annual_long

# Get GLS results function
source("R/get_gls_results.R")


library(nlme)
library(broom)
# library(AICcmodavg)

# long format

##' I want to test whether max, median, or min temperatures have changed during the relevant time period

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

##### TRENDS IN SST ANNUAL MEANS #####

gls_annual_summary <- sst_annual_long %>% group_by(metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()

gls_annual_summary2 <- gls_annual_summary %>% 
  filter(coef_name == "slope") %>% 
  select(-coef_name, -t_value) %>% 
  rename(slope = value)

gls_time_series_sst <- gls_annual_summary2

gls_time_series_sst

#write.csv(gls_time_series_sst, "output/gls_time_series_sst.csv")




