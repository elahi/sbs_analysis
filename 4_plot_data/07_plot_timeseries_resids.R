################################################################################
##' @title Plot residuals of temperature time series in context of 18y lunar cycles
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-03-10
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

source("06_analyse_timeseries_spectra.R")

##### SIX-PANEL PLOT OF RESIDUALS #####

plot_resids <- temp_records3 %>% 
  ggplot(aes(year, resid_val, color = metric)) + 
  geom_point(aes(value, y_position, color = NULL), alpha = 0.3, size = 3, data = moon_df) + 
  geom_line(alpha = 0.5, size = 0.5) + 
  geom_point(alpha = 0.5, size = 1) + 
  facet_grid(dataset ~ metric, switch = "y") + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray") +
  ylab(expression(paste("Deviation from fitted temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") + 
  geom_point(aes(x = year, y = -2, shape = species, color = NULL), data = spYrs, 
             alpha = 0.6, size = 2) + 
  guides(shape = FALSE) + 
  geom_line(aes(year, temp_ma, color = metric), size = 1) + 
  theme(strip.background = element_blank(), 
        strip.placement = "outside") 
plot_resids

ggsave("figs/temp_timeseries_resids.png", height = 5, width = 7)

##### SEAWATER MINIMUM ONLY #####

plot_resids_min <- temp_records3 %>% 
  filter(dataset == "Seawater" & metric == "minimum") %>% 
  ggplot(aes(year, resid_val)) + 
  geom_point(aes(value, y_position, color = NULL), alpha = 0.3, size = 5, data = moon_df) + 
  geom_line(alpha = 0.5, size = 0.5) + 
  geom_point(alpha = 0.5, size = 1) + 
  #facet_grid(dataset ~ metric, switch = "y") + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray") +
  #ylab(expression(paste("Deviation from fitted temperature (", degree, "C)"))) + 
  ylab(expression(atop("Deviation from fitted", paste("temperature (", degree, "C)")))) + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") + 
  geom_point(aes(x = year, y = -2, shape = species, color = NULL), data = spYrs, 
             alpha = 0.6, size = 2) + 
  guides(shape = FALSE) + 
  geom_line(aes(year, temp_ma, color = NULL), size = 1) + 
  theme(strip.background = element_blank()) 

plot_resids_min
