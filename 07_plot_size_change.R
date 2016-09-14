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

##### PLOT MEAN SIZES BY DATA SET #####
dm2 %>% 
  ggplot(aes(year, size_mean, color = sampleArea)) + 
  geom_point() + geom_line(linetype = "dashed") + 
  facet_grid(studySub ~ species) + 
  theme(legend.position = "none") + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 3) + 
  labs(x = "Year", y = "Mean size (mm)")

# ggsave("figs/elahi_size_change_summary.png", height = 3.5, width = 7)

##### PLOT MEAN SIZES TIDAL HEIGHT #####
# What do I want to plot?
ggDat <- dm2 %>% filter(studySub == "subset")
ggDat_gray <- ggDat %>% select(- species)
dodge <- position_dodge(0.1)

ggDat %>% 
  ggplot(aes(tidalHeight, size_mean, color = era, shape = species)) + 
  # geom_point(data = ggDat_gray, color = "grey70") + 
  geom_point(alpha = 0.6, size = 2) + 
  #geom_line(aes(group = sampleArea), color = "gray", size = 0.5, position = dodge) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 0.2, alpha = 0.6) + 
  labs(x = "Tidal height (m)", y = "Size (mm)") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = c(0, 0.0), legend.justification = c(0, 0)) + 
  theme(legend.title = element_blank()) 

ggsave("figs/elahi_size_era_tidal.png", height = 3.5, width = 7)

ggDat %>% 
  ggplot(aes(tidalHeight, size_mean, color = era, shape = species)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 0.2, alpha = 0.5) + 
  labs(x = "Tidal height (m)", y = "Size (mm)") + 
  theme(strip.background = element_blank()) + 
  guides(shape = FALSE) + 
  theme(legend.position = c(0.5, 1), legend.justification = c(0.5, 1)) + 
  theme(legend.title = element_blank()) 

# ggsave("figs/elahi_size_era_tidal.png", height = 3.5, width = 3.5)

##### PLOT MEAN SIZES BY TEMPERATURE #####

ggDat <- dm3 %>% filter(studySub == "subset" & species == "Littorina keenae")
names(ggDat)

ggDat %>% filter(metric != "daily_cv" & metric != "daily_mean") %>% 
  ggplot(aes(mean, size_mean, color = era, shape = species)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 0.05, alpha = 0.5) + 
  geom_errorbarh(aes(xmax = mean + CI, 
                    xmin = mean - CI), height = 0.1, alpha = 0.5) +
  labs(x = "Mean temperature (C)", y = "Size (mm)") + 
  theme(strip.background = element_blank()) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  # theme(legend.position = c(0.5, 1), legend.justification = c(0.5, 1)) + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free")

ggDat <- dm3 %>% filter(studySub == "subset" & metric == "daily_max")

ggDat %>% #filter(species == "Littorina keenae") %>% 
  ggplot(aes(mean, size_mean, color = era, shape = species)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 0.05, alpha = 0.5) + 
  geom_errorbarh(aes(xmax = mean + CI, 
                     xmin = mean - CI), height = 0.1, alpha = 0.5) +
  labs(x = "Mean daily maximum temperature (C)", y = "Size (mm)") + 
  theme(strip.background = element_blank()) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ species)

ggsave("figs/elahi_size_temp_tidal.png", height = 3.5, width = 7)

##### PLOT MEAN CHANGE BY TIDAL HEIGHT #####

datSub <- dm2 %>% filter(studySub == "subset") %>% 
  select(-c(size_se:size_CI, studySub))
datSub

# Get in wide format
datSubW <- datSub %>% group_by(species, sampleArea) %>% 
  mutate(size_mean2 = lead(size_mean), 
         size_max2 = lead(size_max)) %>%
  ungroup() %>% filter(!is.na(size_mean2)) %>% 
  select(-c(era, year))
datSubW

datSubW2 <- datSubW %>% 
  mutate(delta_mean = size_mean2 - size_mean, 
         delta_mean_per = delta_mean/size_mean * 100, 
         delta_max = size_max2 - size_max, 
         delta_max_per = delta_max/size_max * 100)

datSubW2 %>% 
  ggplot(aes(tidalHeight, delta_mean_per, color = species)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") # + 
  geom_smooth(method = "lm")

datSubW2 %>% 
  ggplot(aes(tidalHeight, delta_max_per, color = species)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") # + 
  geom_smooth(aes(color = NULL), method = "lm") 

  

