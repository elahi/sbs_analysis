################################################################################
##' @title Summarising snail size frequency data - change over time
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

glimpse(dm2)

theme_set(theme_bw(base_size = 12))

##### PLOT MEAN SIZES TIDAL HEIGHT - THREE PANELS #####

size_change_text <- data.frame(x = rep(7.5, 3), 
                      y = rep(20, 3), 
                      text1 = c("A", "B", "C"), 
                      species = c("Chlorostoma funebralis", 
                                  "Lottia digitalis", 
                                  "Littorina keenae"))

ggDat <- dm2 %>% filter(studySub == "subset")
ggDat_gray <- ggDat %>% select(- species)
dodge <- position_dodge(0.1)

ggDat %>% 
  ggplot(aes(tidalHeight, size_mean, color = era, shape = species)) + 
  geom_point(alpha = 0.8, size = 2) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 0.1, alpha = 0.8) + 
  labs(x = "Tidal height (m)", y = "Size (mm)") + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(face = "italic")) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = c(0.025, 0.025), legend.justification = c(0.025, 0.025)) + 
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = size_change_text, size = 5, hjust = 1, show.legend = FALSE) 

#ggsave("figs/elahi_size_era_tidal_3panel.png", height = 3.5, width = 7)
ggsave("figs/elahi_size_era_tidal_3panel.pdf", height = 3.5, width = 7)


##### PLOT MEAN SIZES TIDAL HEIGHT - ONE PANEL #####

ggDat %>% 
  ggplot(aes(tidalHeight, size_mean, shape = species, color = era)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 0.1, alpha = 0.5) + 
  labs(x = "Tidal height (m)", y = "Size (mm)") + 
  theme(strip.background = element_blank()) + 
  theme(legend.justification=c(1, 1), legend.position=c(0.9, 1)) + 
  theme(legend.box = "horizontal") + 
  theme(legend.box.just = "top") + 
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) + 
  scale_color_manual(values = c("darkgray", "black")) +
  guides(shape = guide_legend(title = "SPECIES",
                              title.position = "top", 
                              title.hjust = 0, 
                              title.theme = element_text(size = 8, face = "bold", angle = 0), 
                              label.theme = element_text(face = "italic", angle = 0, size = 6), 
                              direction = "vertical")) + 
  guides(color = guide_legend(title = "ERA",
                              title.position = "top", 
                              title.hjust = 0, 
                              title.theme = element_text(size = 8, face = "bold", angle = 0), 
                              label.theme = element_text(angle = 0, size = 6), 
                              direction = "vertical")) 


#ggsave("figs/elahi_size_era_tidal.png", height = 3.5, width = 5)

##### PLOT MEAN SIZES TIDAL HEIGHT - THREE PANELS - PPT #####

theme_set(theme_gray(base_size = 12))

ggDat %>% 
  ggplot(aes(tidalHeight, size_mean, color = era, shape = species)) + 
  geom_point(alpha = 1, size = 2) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 0.1, alpha = 0.6) + 
  labs(x = "Tidal height (m)", y = "Size (mm)") + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(face = "italic")) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = c(0, 0.0), legend.justification = c(0, 0)) + 
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c("darkgray", "black"))

#ggsave("figs/elahi_size_era_tidal_3panel_ppt.png", height = 3.5, width = 7)
