################################################################################
##' @title Plotting duration of air exposure along an intertidal gradient
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-09-16
##' 
##' @log 
################################################################################


library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(readr)

# Load file created from "R/estimate_air_exposure"
df <- read_csv("output/monterey_air_exposure.csv")[-1]

df %>%
  ggplot(aes(tidal_height, air_exposure_proportion, color)) +
  geom_line() + 
  labs(x = "Tidal height (m)", y = "Air exposure\n(proportion)")

ggsave("figs/monterey_air_exposure.png", height = 1.75, width = 3.5)
