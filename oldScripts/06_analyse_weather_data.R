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

# rm(list=ls(all=TRUE)) 

source("05_summarise_hms_sst.R")

source("R/summarise_weather_data.R")

library(nlme)
library(broom)
# library(AICcmodavg)

##' I want to test whether max, median, or min temperatures have changed during the relevant time period for each species
##' 

##### FUNCTION TO RETURN GLS SUMMARY #####
wAnnualL
unique(airTempL$metric)

df = airTempL %>% filter(species == "Littorina keenae")
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
unique(wAnnualL$metric)
unique(wAnnualL$climate_var)

ggDat <- wAnnualL %>% filter(metric == "median" & climate_var != "precip") 

ggDat %>% 
  ggplot(aes(year, value, color = climate_var)) + 
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("red", "black", "blue")) + 
  ylab(expression(paste("Air temperature (", degree, "C)"))) + 
  xlab("Year") + 
  geom_point(aes(x = year, y = 5.5, shape = genus, color = NULL), data = spYrs, 
             alpha = 0.5, size = 3) + 
  guides(color = FALSE) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  geom_smooth(data = subset(ggDat,  climate_var == "air_max"), 
              aes(year, value), 
              method = "lm")

#ggsave("figs/monterey_air_temperature.png", height = 3.5, width = 3.5)

##### PLOT ANNUAL PRECIP TIME SERIES #####
unique(wAnnualL$metric)

ggDat <- wAnnualL %>% filter(climate_var == "precip" & metric == "median") 

ggDat %>% 
  ggplot(aes(year, value, color = metric)) + 
  geom_point(alpha = 0.5) + 
  #geom_smooth(method = "lm") + 
  scale_color_manual(values = c("red", "black", "blue")) + 
  ylab("Precipitation (mm)") + 
  xlab("Year") + 
  geom_point(aes(x = year, y = 0, shape = genus, color = NULL), data = spYrs, 
             alpha = 0.5, size = 3) + 
  guides(color = FALSE) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") 

