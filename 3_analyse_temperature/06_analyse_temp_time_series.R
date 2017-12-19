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

# Get monterey air summary
source("05_summarise_monterey_weather.R")
air_annual_long

# Get SST summary
source("05_summarise_hms_sst.R")
sst_annual_long

# Get GLS results function
source("R/get_gls_results.R")

##### TRENDS IN AIR ANNUAL MEANS #####

crap <- gls(maximum ~ year, data = air_annual, correlation = corAR1())
summary(crap)$tTable
anova(crap)

gls_annual_summary <- air_annual_long %>% group_by(metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()

gls_annual_summary2 <- gls_annual_summary %>% 
  filter(coef_name == "slope") %>% 
  select(-coef_name) %>% 
  rename(slope = value)

gls_time_series_air <- gls_annual_summary2

gls_time_series_air

write.csv(gls_time_series_air, "output/gls_time_series_air.csv")

##### TRENDS IN SST ANNUAL MEANS #####

gls_annual_summary <- sst_annual_long %>% group_by(metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()

gls_annual_summary2 <- gls_annual_summary %>% 
  filter(coef_name == "slope") %>% 
  select(-coef_name) %>% 
  rename(slope = value)

gls_time_series_sst <- gls_annual_summary2

gls_time_series_sst

write.csv(gls_time_series_sst, "output/gls_time_series_sst.csv")



