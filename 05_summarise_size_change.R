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

# load data
source("03_identify_size_cutoff.R")

# load cleaned up data
source("02_sbs_size_dataPrep2.R")
head(dat4)
levels(dat4$species)

##### SUMMARISE DATA - COMPLETE DATASET #####
datMeans <- dat4 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, sampleArea) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_max = quantile(size1mm, 0.95)) %>% 
  ungroup()

datMeans

##### SUMMARISE DATA - SUBSET OF DATASET #####
datMeansSub <- dat5 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, sampleArea) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_max = quantile(size1mm, 0.95)) %>% 
  ungroup()

datMeansSub

##### COMBINE 2 SUMMARIES #####

datMeans2 <- datMeans %>% mutate(studySub = "complete")
datMeansSub2 <- datMeansSub %>% mutate(studySub = "subset")

dm <- rbind(datMeans2, datMeansSub2)

##### JOIN ENVIRONMENT INFO #####

### Summarise lat-longs and tidal heights
envDat <- dat2 %>% filter(era == "present") %>%
  group_by(species, site, era, sampleArea) %>%
  summarise(lat_mean = mean(lat), 
            long_mean = mean(long), 
            tidalHeight = mean(tideHTm)) %>%
  ungroup()

### Join snail data with env data
names(envDat)
names(datMeans)

dm2 <- envDat %>% select(sampleArea, lat_mean:tidalHeight) %>%
  inner_join(dm, .,  by = "sampleArea")

dm2
glimpse(dm2)

##### PLOT MEAN SIZES BY DATA SET #####

glimpse(dm2)

dm2 %>% 
  ggplot(aes(year, size_mean, color = sampleArea)) + 
  geom_point() + geom_line(linetype = "dashed") + 
  facet_grid(studySub ~ species) + 
  theme(legend.position = "none") + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 3) + 
  labs(x = "Year", y = "Mean size (mm)")

ggsave("figs/elahi_size_change_summary.png", height = 3.5, width = 7)

##### PLOT MEAN SIZES TIDAL HEIGHT #####

dodge <- position_dodge(0.1)

dm2 %>% filter(studySub == "subset") %>% 
  ggplot(aes(tidalHeight, size_mean, color = era, shape = species)) + 
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

dm2 %>% filter(studySub == "subset") %>% 
  ggplot(aes(tidalHeight, size_mean, color = era, shape = species)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI), width = 0.2, alpha = 0.5) + 
  labs(x = "Tidal height (m)", y = "Size (mm)") + 
  theme(strip.background = element_blank()) + 
  guides(shape = FALSE) + 
  theme(legend.position = c(0.5, 1), legend.justification = c(0.5, 1)) + 
  theme(legend.title = element_blank()) 
ggsave("figs/elahi_size_era_tidal.png", height = 3.5, width = 3.5)


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


