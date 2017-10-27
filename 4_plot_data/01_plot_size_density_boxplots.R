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

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####
library(cowplot)

# Load data
source("2_summarise_data/05_summarise_size_density.R")
dat %>% distinct(species, sampleArea)
dat %>% distinct(species, sampleUnit, era) %>% View()
glimpse(datMeans4)

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "italic"), 
                  panel.grid = element_blank())) 


## Summarise Wara data to area B and D
wara_dens <- datMeans4 %>% filter(sp == "CHFU")
wara_dens_summary <- wara_dens %>% group_by(species, sampleArea, era) %>% 
  summarise(mean_density = mean(density_m2), 
         median_density = median(density_m2), 
         n = n()) %>% 
  ungroup()
wara_dens_summary

## Get Wara past
wara_dens_past <- wara_dens_summary %>% 
  select(species, sampleArea, era, median_density) %>% rename(density_m2 = median_density) %>% 
  mutate(nest1 = NA, nest2 = NA)

## Get Hexter and Childs past
dens_past <- datMeans4 %>% filter(era == "past") %>% filter(sp != "CHFU") %>% 
  select(species, sampleArea, nest1, nest2, era, density_m2)

## Modern data
dens_pres <- datMeans4 %>% filter(era == "present") %>% 
  select(species, sampleArea, nest1, nest2, era, density_m2)

## Get Hexter and Childs data
density_df <- rbind(dens_pres, dens_past, wara_dens_past)

## Link this to raw data
names(dat)
dat_dens <- dat %>% select(species, sp, site, era, nest1, nest2, sampleUnit, 
                           size1mm, tideHTm, year, lat, long, LL, 
                           sampleArea, sample_area_tidal_ht) %>% 
  left_join(., density_df, by = c("species", "sampleArea", "nest1", "nest2", "era"))

## Get mass_mg for each snail
dat_dens <- dat_dens %>% 
  mutate(mass_mg = ifelse(sp == "CHFU", chfu_mmTOmg(size1mm), 
                          ifelse(sp == "LODI", lodi_mmTOmg(size1mm), 
                                 like_mmTOmg(size1mm))))

names(dat_dens)

my_quants <- c(0.1, 0.5, 0.9)

dat_dens %>% 
  ggplot(aes(density_m2, mass_mg, shape = NULL, color = era)) + 
  geom_point(alpha = 0.75, size = 0.1) + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ species) + 
  #geom_point(data = subset(wara_dens, era == "past"), aes(density_m2, size_mm), size = 2, alpha = 0.75) + 
  #geom_smooth(method = "lm", data = wara_dens, aes(density_m2, size_mm)) + 
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  #theme(legend.key = element_blank()) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_smooth(method = "lm", data = dat_dens) 

ggsave("figs/elahi_size_era_density_raw_scatter.png", height = 3.5, width = 7)
  #geom_quantile(quantiles = my_quants)

dat_dens %>% 
  ggplot(aes(sample_area_tidal_ht, mass_mg, shape = NULL, color = era)) + 
  geom_point(alpha = 0.75, size = 0.1) + 
  #scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ species) + 
  #geom_point(data = subset(wara_dens, era == "past"), aes(density_m2, size_mm), size = 2, alpha = 0.75) + 
  #geom_smooth(method = "lm", data = wara_dens, aes(density_m2, size_mm)) + 
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  #theme(legend.key = element_blank()) + 
  xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_smooth(method = "lm", data = dat_dens) 

##### PLOT SIZE VS TIDAL HEIGHT #####
options(tibble.print_max = 50, tibble.print_min = 10)
dat_dens %>% distinct(sp, era, tideHTm) # past LIKE and CHFU missing
dat_dens %>% distinct(sp, era, sampleArea, sample_area_tidal_ht)

dat_dens2 <- dat_dens %>% 
  mutate(tideHTm = ifelse(era == "past" & sp != "LODI", 
                          sample_area_tidal_ht, tideHTm)) %>% 
  mutate(tideHTm_offset = ifelse(era == "past", tideHTm + 0.1, tideHTm))

dat_dens2 %>% distinct(sp, era, tideHTm) # past LIKE and CHFU missing
dat_dens %>% distinct(sp, era, sampleArea, sample_area_tidal_ht)

dat_dens2 %>% 
  ggplot(aes(tideHTm_offset, mass_mg, shape = NULL, color = era)) + 
  geom_point(alpha = 0.75, size = 0.5) + 
  scale_y_log10() + 
  facet_wrap(~ species) + 
  theme(legend.position = c(0.99, 0.01), legend.justification = c(0.99, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) + 
  #theme(legend.key = element_blank()) + 
  xlab("Tidal height (m)") + 
  ylab("Size (mm)") + 
  guides(shape = FALSE) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_smooth(method = "lm", data = dat_dens2, aes(tideHTm_offset, mass_mg)) 

ggsave("figs/elahi_size_era_tideht_raw_scatter.png", height = 3.5, width = 7)

