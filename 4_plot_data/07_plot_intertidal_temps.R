################################################################################
##' @title Plot intertidal temperatures
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-06
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

# Load temperature data and size data
source("2_summarise_data/05_summarise_intertidal_temps.R")
library(viridis)

##### PLOTS ######

## Plot means of daily median, max and min
tempMeans %>% filter(metric != "daily_cv" & metric != "daily_mean") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  labs(x = "Tidal height (m)", y = "Mean temperature (C)") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

#ggsave("figs/elahi_temp_tidal.png", height = 3.5, width = 7)


tempMeans %>% filter(metric != "daily_cv" & metric != "daily_mean") %>% 
  ggplot(aes(tidalHT, mean, shape = microhabitat, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  labs(x = "Tidal height (m)", y = "Mean temperature (C)") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  #guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

##### PLOT TIME SERIES OF TEMPERATURE  #####
rawDat2 %>% count(aspect)
unique(rawDat2$iButtonID)
unique(rawDat2$csvID)
summary(rawDat2)

d <- rawDat2

rawDat2 %>% 
  slice(1:1000) %>% 
  ggplot(aes(tempC)) + 
  geom_line()

unique(pdL$sp)
pdL

theme_set(theme_gray(base_size = 12) + 
            theme(panel.grid.minor = element_blank()))

pdL %>% distinct(sp, nest1, position) %>% print(n = 100)

## Tegula
pdL %>% 
  filter(sp == "CHFU") %>% 
  filter(metric != "daily_cv" & metric != "daily_mean") %>% 
  ggplot(aes(date, tempC, color = tidalHT, group = position)) + 
  geom_line(size = 0.75, alpha = 1) +
  facet_grid(nest1 ~ metric) + 
  scale_color_viridis_c(option = "viridis") + 
  geom_line(aes(color = NULL), size = 0.25, alpha = 0.25) + 
  ggtitle("Horizontal benches, exposed")

ggsave("figs/intertidal_temps_chfu.pdf", height = 4, width = 8)

pdL %>% 
  filter(sp == "CHFU") %>% 
  filter(metric == "daily_max") %>% 
  ggplot(aes(date, tempC, color = tidalHT, group = position)) + 
  geom_line(size = 0.75, alpha = 1) +
  facet_wrap(~ position, ncol = 2) + 
  scale_color_viridis_c(option = "viridis") + 
  geom_line(aes(color = NULL), size = 0.25, alpha = 0.25) + 
  ggtitle("Horizontal benches, exposed (daily max)")

ggsave("figs/intertidal_temps_chfu_position.pdf", height = 8, width = 8)

## Littorina
pdL %>% 
  filter(sp == "LIKE") %>% 
  filter(metric != "daily_cv" & metric != "daily_mean") %>% 
  ggplot(aes(date, tempC, color = tidalHT, group = position, lty = nest2)) + 
  geom_line(size = 0.75, alpha = 1) +
  facet_grid(nest1 ~ metric) + 
  scale_color_viridis_c(option = "viridis") + 
  geom_line(aes(color = NULL), size = 0.5, alpha = 0.25) + 
  ggtitle("Vertical, west-facing; High Rock; exposed faces vs cracks")

ggsave("figs/intertidal_temps_like.pdf", height = 8, width = 8)

## Lottia
pdL %>% 
  filter(sp == "LODI") %>% 
  filter(metric != "daily_cv" & metric != "daily_mean") %>% 
  ggplot(aes(date, tempC, color = tidalHT, group = position)) + 
  geom_line(size = 0.75, alpha = 1) +
  facet_grid(nest1 ~ metric) + 
  scale_color_viridis_c(option = "viridis") + 
  geom_line(aes(color = NULL), size = 0.25, alpha = 0.25) + 
  ggtitle("Vertical, north facing; 3 sites")

ggsave("figs/intertidal_temps_lodi.pdf", height = 6, width = 8)

library(knitr)
names(loggerPositions_fixed)
loggerPositions_fixed %>% 
  select(position, nest1, tidalHT, aspect, slope, lon, lat) %>% 
  kable()
