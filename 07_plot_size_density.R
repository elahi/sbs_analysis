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

# Load data
source("05_summarise_size_density.R")

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


give.n <- function(x){
  return(c(y = median(x) + 5, label = length(x)))
}

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
geom_smooth(fill = NA) + 
  geom_smooth(show.legend = FALSE) 

##### PLOT MEAN SIZES TIDAL HEIGHT - THREE PANELS #####

size_dens_text <- data.frame(x = rep(1500, 3), 
                      y = rep(3, 3), 
                      text1 = c("A", "B", "C"), 
                      species = c("Chlorostoma funebralis", 
                                  "Lottia digitalis", 
                                  "Littorina keenae"))

datMeans4 %>% 
  ggplot(aes(density_m2, size_mm, shape = species, color = era)) + 
  geom_point(alpha = 0.75, size = 2) + 
  facet_wrap(~ species) + 
  geom_smooth(method = "lm", fill = NA) + 
  geom_smooth(method = "lm", show.legend = FALSE) + 
  scale_x_log10() + 
  #scale_color_manual(values = c("darkgray", "black")) + 
  #scale_color_brewer(type = 'qual') + 
  #scale_color_manual(values = c("#1b9e77", "#d95f02")) + 
  theme(legend.position = c(0, 0.0), legend.justification = c(0, 0)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = size_dens_text, size = 5, hjust = 1, show.legend = FALSE) #+
  # geom_boxplot(aes(5500, size_mm, color = era),
  #              width = 0.5, inherit.aes = FALSE) + 
  # geom_vline(xintercept = 2200, color = "darkgray", size = 0.25)


ggsave("figs/elahi_size_era_density_3panel.png", height = 3.5, width = 7)



