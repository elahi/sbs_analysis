################################################################################
##' @title Summarise temperatures for air and seawater - by species
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-01-08
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

source("R/temperature_functions.R")

dat <- load_sst_air_data()
dat  
tail(dat)

##### FILTER DATA #####

dat1 <- subset_temperature_period(dat, startYear = 1947, endYear = 2015, duration = 10)



##### SELECT SUMMER, WINTER TEMPS #####

# I will use the top 25th percentile of summer temps and bottom 25th percentile of winter temps
remove_data = F

summer_annual <- dat %>% group_by(dataset) %>% 
  do(get_summer_annual_means(., my_quant = 0.75, remove = remove_data)) %>% ungroup()

winter_annual <- dat %>% group_by(dataset) %>% 
  do(get_winter_annual_means(., my_quant = 0.25, remove = remove_data)) %>% ungroup()

annual_mean <- dat %>% group_by(dataset, year) %>% 
  summarise(mean = mean(tempC, na.rm = TRUE)) %>% 
  ungroup() 

## Collate annual data
annual_df <- annual_mean %>% 
  mutate(minimum = winter_annual$tempC, 
         maximum = summer_annual$tempC)

# Get in long format
annual_df_long <- annual_df %>% 
  gather(key = metric, value = tempC, mean:maximum) %>%
  filter(!is.na(tempC))

# # Plot
# annual_df_long %>%
#   ggplot(aes(year, tempC, color = metric, group = metric)) +
#   geom_line() +
#   #geom_smooth(method = "lm", linetype = "dashed", color = "black") +
#   facet_wrap(~ dataset) + 
#   theme(legend.position = "bottom")
# ggsave("figs_ms/plot_temp_timeseries.pdf", height = 3.5, width = 7)
