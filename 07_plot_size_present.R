################################################################################
##' @title Summarising snail size frequency data - present data only 
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-05
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

# Load data
source("05_summarise_size_data.R")

##### PLOT INDIVIDUAL SIZE V TEMPERATURE #####
pres3 %>% 
  ggplot(aes(mean, size1mm, color = species, 
             shape = species)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free")

pres3 %>% filter(metric == "daily_med") %>% 
  ggplot(aes(mean, size1mm, color = species, 
             shape = species)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), linetype = "dashed")



##### PLOT SAMPLE UNIT SIZE BY TEMPERATURE #####
head(suMeans3)
unique(suMeans3$metric)

suMeans3 %>% filter(metric != "daily_med") %>% 
  ggplot(aes(mean, size_median, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Median size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free") + 
  guides(size = FALSE)

suMeans3 %>% filter(metric != "daily_med") %>% 
  ggplot(aes(mean, size_mean, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Mean size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free") + 
  guides(size = FALSE)

suMeans3 %>% filter(metric != "daily_med") %>% 
  ggplot(aes(mean, size_max, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Maximum size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free") + 
  guides(size = FALSE)

suMeans3 %>% filter(metric == "daily_med") %>% 
  ggplot(aes(mean, size_median, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + 
  labs(x = "Temperature (C)", y = "Median size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ species, scales = "free") + 
  guides(size = FALSE)


