################################################################################
##' @title Process latitudinal gradient data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-09-16
##' 
##' @log 
################################################################################

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

lee <- read_csv("data/lat_grad_lee.csv")
frank <- read_csv("data/lat_grad_frank.csv") %>% 
  mutate(species = "Chlorostoma funebralis")

##### PROCESS LEE #####

##' Estimate size from linear and quadratic regression for each latitude
##' Lee and Boulding 2010
##' linear: y = 0.528x - 7.079
##' quad: y = -0.029x^2 + 2.618x - 44.253

lee

lee2 <- lee %>% group_by(Locality) %>% 
  mutate(Lat_common = mean(Latitude), 
         #size_est_lin = Lat_common * 0.528 - 7.079, 
         size_est_quad = -0.029 * Lat_common^2 + 2.618 * Lat_common - 44.253) %>%
  ungroup()

lee2 

lee_estimated <- lee2 %>% select(Locality:size_est_quad) %>% 
  distinct() %>% ungroup() %>% 
  tidyr::gather(., key = size_cat, value = size_mm, size_est_quad)

lee_estimated

lee3 <- lee2 %>% select(Locality, Lat_common, size_cat, size_mm)

lee4 <- rbind(lee3, lee_estimated) %>% 
  rename(Latitude = Lat_common) %>% 
  mutate(species = "Littorina keenae")
lee4

##### COMBINE DATASETS #####
frank2 <- frank %>% select(-c(Latitude_Frank, N)) %>%
  mutate(size_cat = NA)
frank2

lat_grad_df <- rbind(lee4, frank2)

lat_grad_df %>% 
  ggplot(aes(Latitude, size_mm, shape = species, color = species)) + 
  geom_point(alpha = 0.75, size = 2) + 
  # geom_smooth(method = "lm") + 
  scale_x_reverse() + 
  theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(face = "italic", size = 8)) + 
  scale_colour_grey() + 
  xlab(expression(paste("Temperature (", degree, "C)"))) + 
  ylab("Size (mm)") +     
  scale_shape_manual(values = c("Chlorostoma funebralis" = 16,
                                "Littorina keenae" = 15))
  


