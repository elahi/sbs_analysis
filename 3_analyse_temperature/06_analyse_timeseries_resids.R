################################################################################
##' @title Analyze residuals of temperature time series in context of 18y lunar cycles
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-03-10
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

# Get temperature regression results
source("06_analyse_temp_time_series.R")

air_annual_long <- air_annual_long %>% 
  mutate(dataset = "Air")

sst_annual_long <- sst_annual_long %>% 
  mutate(dataset = "Seawater")

### Combine data
temp_records <- rbind(sst_annual_long, air_annual_long)
unique(temp_records$dataset)

gls_time_series_sst

library(cowplot)
library(broom)
library(zoo)

theme_set(theme_bw(base_size = 12))

##### CALCULATE RESIDUALS #####

 ## Seawater
gls_annual_summary <- sst_annual_long %>% group_by(metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()

sst_equation <- gls_annual_summary %>% select(metric, value, coef_name) %>% 
  spread(key = coef_name, value = value) %>% 
  mutate(dataset = "Seawater")

## Air
gls_annual_summary <- air_annual_long %>% group_by(metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()

air_equation <- gls_annual_summary %>% select(metric, value, coef_name) %>% 
  spread(key = coef_name, value = value) %>% 
  mutate(dataset = "Air")

equation_df <- rbind(air_equation, sst_equation)

## Join with time series data
temp_records2 <- inner_join(temp_records, equation_df, by = c("metric", "dataset")) %>% 
  mutate(resid_val = tempC- (slope * year + intercept))

temp_records2 %>% 
  ggplot(aes(year, resid_val, color = metric)) + 
  geom_line() + 
  facet_grid(dataset ~ metric) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray")

##### GET YEARS OF SNAIL SAMPLES #####
head(dat4)

spYrs <- dat4 %>% ungroup() %>% select(species, year) %>% distinct() %>% 
  arrange(species, year) %>% 
  group_by(species) %>% 
  mutate(genus = unlist(strsplit(as.character(species), split = " +"))[1]) %>%
  ungroup()

spYrs

spYrs$genus <- factor(spYrs$genus, levels = c("Chlorostoma", "Lottia", "Littorina"))

##### GET MOVING AVERAGES #####
temp_records2

temp_records3 <- temp_records2 %>% 
  arrange(dataset, metric, year) %>% 
  group_by(dataset, metric) %>% 
  mutate(temp_ma = rollmean(x = resid_val, 3, align = "center", fill = NA)) %>% 
  ungroup()
temp_records3

##### GET LUNAR CYCLES #####

# moons 18.61 yr cycle peaked in 2006 and 2024-25
# 18.61 
# 9.305
# sinusoidal

inclin_max <- seq(2024.6, 1940, by = -18.6)
inclin_min <- seq((2024.6 - 9.305), 1940, by = -18.6)

moon_df <- data.frame("inclin_min" = inclin_min, "inclin_max" = inclin_max) %>% 
  gather(key = inclination, value = value) %>%
  mutate(y_position = c(rep(1, 5), rep(-1, 5))) %>% 
  filter(value < 2016)
moon_df
