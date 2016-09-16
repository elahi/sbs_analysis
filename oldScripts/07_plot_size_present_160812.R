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
  xlab(expression(paste("Temperature (", degree, "C)"))) + 
  ylab("Size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free") + 
  theme(legend.title = element_blank()) 

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

ggDat <- suMeans3 %>% filter(metric != "daily_mean" & metric != "daily_cv")

### MEDIAN SIZE
ggDat %>% 
  ggplot(aes(mean, size_median, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm") + 
  xlab(expression(paste("Temperature (", degree, "C)"))) + 
  ylab("Median size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free") + 
  guides(size = FALSE) + 
  theme(strip.background = element_blank())

ggsave("figs/elahi_pres_medsize_temp.png", height = 3.5, width = 7)

### MAXIMUM SIZE
unique(ggDat$position)
unique(ggDat$sampleUnit)
unique(ggDat$sampleArea)

ggDat %>% 
  ggplot(aes(mean, size_max, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm") + 
  xlab(expression(paste("Temperature (", degree, "C)"))) + 
  ylab("Maximum size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free") + 
  guides(size = FALSE)

ggsave("figs/elahi_pres_maxsize_temp.png", height = 3.5, width = 7)


### MEDIAN SIZE WITH QUADRATIC
ggDat %>% filter(metric == "daily_med") %>% 
  ggplot(aes(mean, size_median, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + 
  xlab(expression(paste("Temperature (", degree, "C)"))) + 
  ylab("Median size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  guides(size = FALSE) 
ggsave("figs/elahi_pres_medsize_quad_temp.png", height = 3.5, width = 3.5)


