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

# rm(list=ls(all=TRUE)) 

source("05_summarise_hms_sst.R")

library(nlme)
library(broom)
# library(AICcmodavg)

# wide format
species_sst_annual
# long format
sp_sst_ann_L

##' I want to test whether max, median, or min temperatures have changed during the relevant time period for each species
##' 

##### FUNCTION TO RETURN GLS SUMMARY #####

df = species_sst_annual %>% filter(species == "Littorina keenae")
dep_var = "max_C"
ind_var = "year"

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

##### TRENDS IN ANNUAL MEANS OF MONTHLY TEMPERATURES #####

gls_annual_summary <- sp_sst_ann_L %>% group_by(species, metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()
gls_annual_summary

gls_annual_summary2 <- gls_annual_summary %>% 
  filter(coef_name == "slope") %>% 
  select(-coef_name, -t_value) %>% 
  rename(slope = value)

write.csv(gls_annual_summary2, "output/gls_annual_summary2.csv")
