################################################################################
##' @title Plot snail size data vs temperature - present data only 
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-05
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

### Load Elahi size present day data
source("05_summarise_size_data.R")
# summarized by position 
posMeans # size only
posMeans2 # density and size (by sample unit, not raw data like posMeans)

### Load latitudinal size gradients
source("R/process_lat_gradient_size.R")
lat_grad_df

###
source("R/multiplotF.R")
library(grid)
theme_set(theme_bw(base_size = 12))

##### PLOT POSITION SIZE MEANS VS MAX TEMPERATURE #####

ggDat <- posMeans %>% filter(metric == "daily_max")

ggDat %>% 
  ggplot(aes(mean, size_mean, color = species, 
             shape = species, size = NULL)) +
  geom_point(alpha = 0.9, size = 2) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI, 
                    size = NULL), width = 0.5, alpha = 0.9) + 
  geom_errorbarh(aes(xmax = mean + CI, 
                     xmin = mean - CI, 
                     size = NULL), height = 0.5, alpha = 0.9) + 
  xlab(expression(paste("Temperature (", degree, "C)"))) + 
  ylab("Size (mm)") +   
  theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(face = "italic", size = 8)) + 
  scale_colour_grey() + 
  geom_smooth(method = "lm")

ggsave("figs/elahi_pres_meansize_maxtemp.png", height = 3.5, width = 3.5)

##### Two panel plot #####

range(ggDat$size_mean)
range(ggDat$mean)

facet_A <- grobTree(textGrob("A", x = 0.1, y = 0.95, hjust = 0.5))

p1 <- ggDat %>% 
  ggplot(aes(mean, size_mean, color = species, 
             shape = species, size = NULL)) +
  geom_point(alpha = 0.9, size = 2) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI, 
                    size = NULL), width = 0.5, alpha = 0.9) + 
  geom_errorbarh(aes(xmax = mean + CI, 
                     xmin = mean - CI, 
                     size = NULL), height = 0.5, alpha = 0.9) + 
  xlab(expression(paste("Temperature (", degree, "C)"))) + 
  ylab("Size (mm)") +   
  theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(face = "italic", size = 8)) + 
  scale_colour_grey() +
  annotation_custom(facet_A)

p1

facet_B <- grobTree(textGrob("B", x = 0.05, y = 0.95, hjust = 0.5))

p2 <- lat_grad_df %>% 
  ggplot(aes(Latitude, size_mm, shape = species, color = species)) + 
  geom_point(alpha = 0.9, size = 2) + 
  # geom_smooth(method = "lm") + 
  scale_x_reverse() + 
  theme(legend.position = "none") + 
  #theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(face = "italic", size = 8)) + 
  scale_colour_grey() + 
  xlab("Latitude") + 
  ylab("Size (mm)") +     
  scale_shape_manual(values = c("Chlorostoma funebralis" = 16,
                                "Littorina keenae" = 15)) +
  annotation_custom(facet_B) + 
  geom_vline(xintercept = 36.64, alpha = 0.05, size = 6)

p2

png("./figs/spatial_temp_size.png", width = 7, height = 3.5, units = "in", res = 300)
multiplot(p1, p2, cols = 2)
dev.off()	

##### PLOT POSITION DENSITY MEANS VS MAX TEMPERATURE #####

ggDat2 <- posMeans2 %>% filter(metric == "daily_max")

facet_B <- grobTree(textGrob("B", x = 0.95, y = 0.95, hjust = 0.5))

ggDat2 %>% 
  ggplot(aes(mean, dens_mean, color = species, 
             shape = species, size = NULL)) +
  geom_point(alpha = 0.9, size = 2) + 
  geom_errorbarh(aes(xmax = mean + CI, 
                     xmin = mean - CI, 
                     size = NULL), height = 0, alpha = 0.9) + 
  xlab(expression(paste("Temperature (", degree, "C)"))) + 
  ylab(expression(paste("Density (no. ", m^-2, ")"))) + 
  theme(legend.position = c(1,0), legend.justification = c(1,0)) + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(face = "italic", size = 8)) + 
  scale_colour_grey() +
  scale_y_log10(limits = c(10, 1000)) +
  geom_smooth(method = "lm")

p3 <- ggDat2 %>% 
  ggplot(aes(mean, dens_mean, color = species, 
             shape = species, size = NULL)) +
  geom_point(alpha = 0.9, size = 2) + 
  geom_errorbarh(aes(xmax = mean + CI, 
                     xmin = mean - CI, 
                     size = NULL), height = 0, alpha = 0.9) + 
  xlab(expression(paste("Temperature (", degree, "C)"))) + 
  ylab(expression(paste("Density (no. ", m^-2, ")"))) + 
  theme(legend.position = c(1,0), legend.justification = c(1,0)) + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(face = "italic", size = 8)) + 
  scale_colour_grey() +
  scale_y_log10(limits = c(10, 1000)) + 
  annotation_custom(facet_C) + 
  theme(legend.position = "none")
p3
ggsave("figs/elahi_pres_meandens_maxtemp.png", height = 3.5, width = 3.5)
  
ggDat2 %>% 
  ggplot(aes(dens_mean, size_mean2, color = species, 
             shape = species, size = NULL)) +
  geom_point(alpha = 0.75, size = 2) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") +   
  #theme(legend.position = c(1,0), legend.justification = c(1,0)) + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(face = "italic", size = 8)) + 
  scale_colour_grey() +
  scale_x_log10(limits = c(10, 1000))

##### Microhabitat size and density vs temp two panel plot #####

png("./figs/spatial_temp_size_dens.png", width = 7, height = 3.5, units = "in", res = 300)
multiplot(p1, p3, cols = 2)
dev.off()	

