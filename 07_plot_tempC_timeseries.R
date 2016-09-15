################################################################################
##' @title Plot intertidal temperatures
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-09-13
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

### Load limpet hindcasts (15 yrs of modeled body temperatures)
source("05_summarise_limpet_temps.R")
monthly_extremes
la_summL2

### Load Monterey weather data and HMS sst data
source("06_analyse_weather_data.R")
head(wL); tail(wL)
summary(weather3) # air_obs is the measurement of interest
wAnnualL

### HMS
summary(sst_hms)
# wide format
species_sst_annual
# long format
sp_sst_ann_L
# entire time series
sstL

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank()))

##### PLOT SEAWATER TIME SERIES #####

sst_monthly

ggDat <- sstL %>% filter(metric != "cv_C" & metric != "mean_C")

ggDat %>%
  ggplot(aes(year, tempC, color = metric)) + 
  geom_point(alpha = 0.5) + 
  #geom_smooth(method = "lm") + 
  scale_color_manual(values = c("red", "black", "blue")) + 
  ylab(expression(paste("Sea surface temperature (", degree, "C)"))) + 
  xlab("Year") + 
  geom_point(aes(x = year, y = 10.5, shape = genus, color = NULL), data = spYrs, 
             alpha = 0.5, size = 3) + 
  guides(color = FALSE) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  geom_smooth(data = subset(ggDat,  metric == "max_C"), 
              aes(year, tempC), 
              method = "lm")

  
##### LIMPET HINDCASTS #####
la_summL2 %>% 
  ggplot(aes(year, tempC, color = metric)) +
  facet_wrap(~ species, ncol = 5) + 
  geom_point(alpha = 0.6) + geom_smooth(method = "lm") + 
  ylab(expression(paste("Predicted body temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

##### PLOT AIR TIME SERIES #####
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







##### COMBINE THREE DATASETS FOR ONE PLOT #####



##' Combine these two datasets
# species, mean, CI, tidalHT, metric, 

temp_logger <- tempMeans %>% 
  filter(metric != "daily_cv" & metric != "daily_mean") %>% 
  select(species, position, microhabitat, tidalHT, 
         metric, mean, sd, n, se, CI) %>% 
  mutate(month = NA, 
         year = NA,
         dataset = "Empirical")

# Rename levels of metric
metric <- temp_logger$metric
metric <- gsub("daily_max", "maximum", metric)
metric <- gsub("daily_med", "median", metric)
metric <- gsub("daily_min", "minimum", metric)

temp_logger$metric <- metric

temp_model <- monthly_extremes %>% ungroup() %>% 
  select(species, position, tidalHT, month, year, 
         metric, mean, sd, n, se, CI) %>% 
  mutate(microhabitat = NA, 
         dataset = "Model")

tempDF <- rbind(temp_logger, temp_model)
tempDF

##### PLOTS ######

dataset_description <- c(
  Model = "Predicted body temperature", 
  Empirical = "Empirical rock temperature"
)

tempDF %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  geom_point(data = subset(tempDF, microhabitat == "crevice" & metric == "maximum"), 
             aes(tidalHT, mean, shape = species, color = NULL), 
             alpha = 0.6, size = 2) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(color = FALSE) + 
  theme(legend.position = "top") + 
  #theme(legend.title = element_blank()) + 
  facet_wrap(~ dataset, ncol = 2, scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  guides(shape = guide_legend(title = "SAMPLING AREA",
                              title.position = "top", 
                              title.hjust = 0.5, 
                              title.theme = element_text(size = 10, face = "bold", angle = 0))) 
  

ggsave("figs/elahi_temp_body_rock.png", height = 3.5, width = 7)
