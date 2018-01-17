################################################################################
##' @title Plot snail sizes by tide
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-01-16
##' @log 
################################################################################

source("3_analyse_data/01_sbs_bayes_data.R")

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "italic"), 
                  panel.grid = element_blank()))

dat_dens
names(dat_dens)

dat_dens <- dat_dens %>% 
  mutate(sample_area_tidal_ht_offset = 
           ifelse(era == "present", sample_area_tidal_ht + 0.1, sample_area_tidal_ht))

dat_dens %>% 
  ggplot(aes(sample_area_tidal_ht_offset, size1mm, color = era)) + 
  geom_jitter(alpha = 0.25, size = 0.5, pch = 1) + 
  facet_wrap(~ species) + 
  labs(x = "Tidal height (m)", y = "Size (mm)") + 
  theme(legend.position = c(0.05, 0.05), legend.justification = c(0.05, 0.025)) + 
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c("#e79f00", "black"))

ggsave("figs_ms/plot_size_era_tide.pdf", height = 3.5, width = 7)

datMeans <- dat_dens %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, sampleArea, 
           sample_area_tidal_ht, sample_area_tidal_ht_offset) %>% 
  summarise(size_mean = mean(size1mm), 
            size_med = median(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            tide_mean = mean(tideHTm)) %>% 
  ungroup()


# set desired dodge width
pd <- position_dodge(width = 0.2)

datMeans %>% 
  ggplot(aes(sample_area_tidal_ht_offset, size_mean, color = era, shape = species)) + 
  geom_point(alpha = 0.8, size = 2) + 
  geom_errorbar(aes(ymin = size_mean - size_sd, ymax = size_mean + size_sd), 
                width = 0.2, alpha = 0.8) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  labs(x = "Tidal height (m)", y = "Size (mm)") + 
  theme(legend.position = c(0.05, 0.05), legend.justification = c(0.05, 0.025)) + 
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c("black", "#e79f00"))

ggsave("figs_ms/plot_size_era_tide_means.pdf", height = 3.5, width = 7)

