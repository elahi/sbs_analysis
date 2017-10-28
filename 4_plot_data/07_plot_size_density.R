################################################################################
##' @title Plot size-density results
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-02-10
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 
library(cowplot)

# Load data
source("summarise_size_density.R")

glimpse(datMeans4)

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "italic"))) 

##### PRELIM PLOTS #####

datMeans4 %>% filter(sp == "CHFU") %>% 
  ggplot(aes(density_m2, size_mm, color = era)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ site) + 
  geom_smooth(method = "lm") + 
  scale_x_log10()

datMeans4 %>% 
  ggplot(aes(tideHTm, size_mm, color = era)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ species) + 
  geom_smooth(method = "lm") 

datMeans4 %>% 
  ggplot(aes(tideHTm, density_m2, color = era)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ species) + 
  geom_smooth(method = "lm") 

give.n <- function(x){
  return(c(y = 0, label = length(x)))
}

datMeans4 %>% 
  ggplot(aes(era, size_mm)) + 
  geom_boxplot() + 
  facet_wrap(~ species, scales = "free_y") + 
  stat_summary(fun.data = give.n, geom = "text")

datMeans4 %>% 
  ggplot(aes(era, density_m2)) + 
  geom_boxplot(notch = FALSE) + 
  facet_wrap(~ species, scales = "free_y") + 
  stat_summary(fun.data = give.n, geom = "text")

datMeans4 %>% filter(sp == "CHFU") %>% 
  ggplot(aes(tideHTm, density_m2, color = era)) + 
  geom_point(alpha = 0.75, size = 2) + 
  facet_wrap(~ site, scales = "free") + 
  geom_line()

##### PLOT DENSITIES ONLY #####
size_dens_text <- data.frame(y = rep(1500, 3), 
                             x = rep(0.75, 3), 
                             text1 = c("A", "B", "C"), 
                             species = c("Chlorostoma funebralis", 
                                         "Lottia digitalis", 
                                         "Littorina keenae"))

give.n <- function(x){
  return(c(y = 0, label = length(x)))
}

datMeans4 %>% 
  ggplot(aes(era, density_m2, fill = era)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_y_log10() + 
  facet_wrap(~ species) + 
  stat_summary(fun.data = give.n, geom = "text") + 
  scale_fill_manual(values = c("darkgray", "black")) + 
  theme(legend.position = "none") + 
  geom_text(aes(x, y, label = text1, fill = NULL, shape = NULL), 
            data = size_dens_text, size = 5, hjust = 1, show.legend = FALSE)  + 
  xlab('') + 
  ylab(expression(paste("Density (no. ", m^-2, ")")))

ggsave("figs/elahi_era_density_3panel.pdf", height = 3.5, width = 7)


##### PLOT MEAN SIZES TIDAL HEIGHT - THREE PANELS #####

size_dens_text <- data.frame(x = rep(1500, 3), 
                      y = rep(6, 3), 
                      text1 = c("A", "B", "C"), 
                      species = c("Chlorostoma funebralis", 
                                  "Lottia digitalis", 
                                  "Littorina keenae"))

datMeans4 %>% 
  ggplot(aes(density_m2, size_mm, shape = species, color = era)) + 
  geom_point(alpha = 0.75, size = 2) + 
  facet_wrap(~ species) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_smooth(method = "lm") + 
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = size_dens_text, size = 5, hjust = 1, show.legend = FALSE)  + 
  scale_color_manual(values = c("darkgray", "black"))

ggsave("figs/elahi_size_era_density_3panel.pdf", height = 3.5, width = 7)

##### PLOT MEAN MASS - TIDAL HEIGHT - THREE PANELS #####

size_dens_text <- data.frame(x = rep(1500, 3), 
                             y = rep(0.005, 3), 
                             text1 = c("A", "B", "C"), 
                             species = c("Chlorostoma funebralis", 
                                         "Lottia digitalis", 
                                         "Littorina keenae"))

datMeans4 %>% 
  ggplot(aes(density_m2, mass_mean_mg, shape = species, color = era)) + 
  geom_point(alpha = 0.75, size = 2) + 
  facet_wrap(~ species) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_smooth(method = "lm") + 
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = size_dens_text, size = 5, hjust = 1, show.legend = FALSE)  + 
  scale_color_manual(values = c("darkgray", "black"))

ggsave("figs/elahi_size_era_density_3panel.pdf", height = 3.5, width = 7)

##### PLOT SIZE-DENSITY AND DENSITY BOXPLOTS #####

size_dens_text <- data.frame(x = rep(1500, 3), 
                             y = rep(5, 3), 
                             text1 = c("A", "B", "C"), 
                             species = c("Chlorostoma funebralis", 
                                         "Lottia digitalis", 
                                         "Littorina keenae"))
summary(datMeans4$density_m2)

x_limits <- range(datMeans4$density_m2)

p1 <- datMeans4 %>% 
  ggplot(aes(density_m2, size_mm, shape = species, color = era)) + 
  geom_point(alpha = 0.75, size = 2) + 
  facet_wrap(~ species) + 
  scale_x_log10(limits = x_limits) + 
  #scale_y_log10() + 
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = size_dens_text, size = 5, hjust = 1, show.legend = FALSE)  + 
  scale_color_manual(values = c("darkgray", "black")) + 
  #geom_smooth(method = "lm") + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
p1

size_dens_text <- data.frame(y = rep(1500, 3), 
                             x = rep(0.75, 3), 
                             text1 = c("D", "E", "F"), 
                             species = c("Chlorostoma funebralis", 
                                         "Lottia digitalis", 
                                         "Littorina keenae"))

p2 <- datMeans4 %>% 
  ggplot(aes(era, density_m2, fill = era)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_y_log10(limits = x_limits) + 
  facet_wrap(~ species) + 
  scale_fill_manual(values = c("darkgray", "black")) + 
  theme(legend.position = "none") + 
  geom_text(aes(x, y, label = text1, fill = NULL, shape = NULL), 
            data = size_dens_text, size = 5, hjust = 1, show.legend = FALSE)  + 
  xlab("Era") + 
  ylab(expression(paste("Density (no. ", m^-2, ")"))) + 
  coord_flip() + 
  scale_x_discrete(limits = c("present", "past")) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(strip.background = element_blank(), strip.text.x = element_blank()
  )
p2

final_plot <- ggdraw() + draw_plot(p1, x = 0, y = 0.3, width = 1, height = 0.7) + 
  draw_plot(p2, x = 0, y = 0, width = 1, height = 0.3) 

save_plot("figs/elahi_size_era_density_6panel.pdf", final_plot, 
          ncol = 1, nrow = 2, base_width = 10)

