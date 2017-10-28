################################################################################
##' @title Plot size-density raw data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-02-10
##' 
##' @log 
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####
library(cowplot)
options(tibble.print_max = 50, tibble.print_min = 10)

# Load data
source("2_summarise_data/summarise_size_density.R")

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "italic"), 
                  panel.grid = element_blank())) 

names(dat_dens)

my_quants <- c(0.1, 0.5, 0.9)

##### PLOT SIZE VS DENSITY #####
# Mass
dat_dens %>% 
  ggplot(aes(density_m2, mass_mg, shape = NULL, color = era)) + 
  geom_point(alpha = 0.75, size = 0.1) + 
  scale_y_log10() + 
  facet_wrap(~ species, scales = "free") + 
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  #theme(legend.key = element_blank()) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_smooth(method = "lm", data = dat_dens) 

# Length, logged
dat_dens %>% 
  ggplot(aes(density_m2, log10(size1mm), shape = NULL, color = era)) + 
  geom_point(alpha = 0.75, size = 0.5) + 
  facet_wrap(~ species, scales = "free_x") + 
  theme(legend.position = c(0.99, 0.99), legend.justification = c(0.99, 0.99)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_smooth(method = "lm", data = dat_dens) 

# Length, not logged
dat_dens %>% 
  ggplot(aes(density_m2, size1mm, shape = NULL, color = era)) + 
  geom_point(alpha = 0.75, size = 0.5) + 
  facet_wrap(~ species, scales = "free_x") + 
  theme(legend.position = c(0.99, 0.99), legend.justification = c(0.99, 0.99)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_smooth(method = "lm", data = dat_dens) 

ggsave("figs/elahi_size_era_density_raw_scatter.png", height = 3.5, width = 7)

# Log-log plots
dat_dens %>% 
  ggplot(aes(dens_log, mass_log, shape = NULL, color = era)) + 
  geom_point(alpha = 0.75, size = 0.1) + 
  facet_wrap(~ species) + 
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  #theme(legend.key = element_blank()) + 
  xlab(expression(paste("Log density (no. ", m^-2, ")"))) + 
  ylab("Log mass (mg)") + 
  guides(shape = FALSE) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_smooth(method = "lm", data = dat_dens) 

##### PLOT SIZE VS TIDAL HEIGHT #####
# Offset the tidal heights to avoid overplotting
dat_dens2 <- dat_dens %>% 
  mutate(tideHTm_offset = ifelse(era == "past" & sp == "LIKE", tideHTm + 0.125, 
                                 ifelse(era == "past" & sp == "LODI", tideHTm + 0.025,
                                        tideHTm)))

# Log mass
dat_dens2 %>% 
  ggplot(aes(tideHTm_offset, mass_mg, shape = NULL, color = era)) + 
  geom_point(alpha = 0.75, size = 0.5) + 
  scale_y_log10() + 
  facet_wrap(~ species, scales = "free_x") + 
  theme(legend.position = c(0.99, 0.01), legend.justification = c(0.99, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  #theme(legend.key = element_blank()) + 
  xlab("Tidal height (m)") + 
  ylab("Mass (mg)") + 
  guides(shape = FALSE) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_smooth(method = "lm", data = dat_dens2, aes(tideHTm_offset, mass_mg)) 

# Unlogged length
dat_dens2 %>% 
  ggplot(aes(tideHTm_offset, size_log, shape = NULL, color = era)) + 
  geom_point(alpha = 0.75, size = 0.5) + 
  #scale_y_log10() + 
  facet_wrap(~ species, scales = "free") + 
  theme(legend.position = c(0.99, 0.01), legend.justification = c(0.99, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  #theme(legend.key = element_blank()) + 
  xlab("Tidal height (m)") + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_smooth(method = "lm", data = dat_dens2, aes(tideHTm_offset, size1mm)) 

ggsave("figs/elahi_size_era_tideht_raw_scatter.png", height = 3.5, width = 7)

