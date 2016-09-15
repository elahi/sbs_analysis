################################################################################
##' @title Analyze weather data from NCDC
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

# Get monterey weather summary
source("05_summarise_monterey_weather.R")
air_annual_long

# Get GLS results function
source("R/get_gls_results.R")

##### TRENDS IN AIR ANNUAL MEANS #####

gls_annual_summary <- air_annual_long %>% group_by(metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()

gls_annual_summary2 <- gls_annual_summary %>% 
  filter(coef_name == "slope") %>% 
  select(-coef_name, -t_value) %>% 
  rename(slope = value)

gls_time_series_air <- gls_annual_summary2

gls_time_series_air

# write.csv(gls_time_series_air, "output/gls_time_series_air.csv")
