################################################################################
##' @title Plot limpet temperatures
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-16
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

source("05_summarise_limpet_temps.R")

##### PLOT ANNUAL MAX, MEDIAN, MIN BY TIDAL HEIGHT ######

annual_summary %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  ylab(expression(paste("Predicted body temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

# ggsave("figs/elahi_predicted_temp_tidal.png", height = 3.5, width = 7)

##### PLOT GLOBAL MONTHLY EXTREMES ######

monthly_extremes

monthly_extremes %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  ylab(expression(paste("Predicted temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ species)
  
# ggsave("figs/elahi_predicted_temp_tidal_global.png", height = 3.5, width = 7)

##### PLOT ANNUAL MAX, MEDIAN, MIN TIME SERIES ######

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

