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

source("05_summarise_monterey_weather.R")

library(nlme)
library(broom)
# library(AICcmodavg)

##' I want to test whether max, median, or min temperatures have changed during the relevant time period for each species
##' 
air_annual_long

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

##### TRENDS IN AIR ANNUAL MEANS #####

gls_annual_summary <- air_annual_long %>% group_by(metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()

gls_annual_summary2 <- gls_annual_summary %>% 
  filter(coef_name == "slope") %>% 
  select(-coef_name, -t_value) %>% 
  rename(slope = value)

gls_annual_summary2

#write.csv(gls_annual_summary2, "output/gls_annual_summary2.csv")

##### TRENDS IN AIR ANNUAL MEANS #####

# Use median values only
unique(wAnnualL$metric)
weather_annual_summary <- wAnnualL %>% filter(metric == "median") %>% 
  group_by(climate_var) %>% 
  do(get_gls_results(df = ., dep_var = "value", ind_var = "year")) %>%
  ungroup()
weather_annual_summary

weather_annual_summary2 <- weather_annual_summary %>% 
  filter(coef_name == "slope") %>% 
  select(-coef_name, -t_value) %>% 
  rename(slope = value)

#write.csv(weather_annual_summary2, "output/weather_annual_summary2.csv")

##### PLOT ANNUAL AIR TEMP TIME SERIES #####

air_annual_long %>% 
  ggplot(aes(year, tempC, color = metric)) + 
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("red", "black", "blue")) + 
  ylab(expression(paste("Air temperature (", degree, "C)"))) + 
  xlab("Year") + 
  guides(color = FALSE) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  geom_smooth(data = subset(air_annual_long,  metric == "minimum"), 
              aes(year, tempC), 
              method = "lm")

#ggsave("figs/monterey_air_temperature.png", height = 3.5, width = 3.5)
