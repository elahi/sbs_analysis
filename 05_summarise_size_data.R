################################################################################
##' @title Summarising snail size frequency data 
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-05
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

# load temperature and size data
source("05_summarise_intertidal_temps.R")

##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### SUMMARISE SIZE CHANGE DATA #####
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 

##### SUMMARISE SIZE CHANGE DATA - COMPLETE #####
datMeans <- dat4 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, sampleArea) %>% 
  summarise(size_mean = mean(size1mm), 
            size_med = median(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_95 = quantile(size1mm, 0.95), 
            size_max = max(size1mm)) %>% 
  ungroup()

datMeans

##### SUMMARISE SIZE CHANGE DATA - SUBSET #####
datMeansSub <- dat5 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, sampleArea) %>% 
  summarise(size_mean = mean(size1mm), 
            size_med = median(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_95 = quantile(size1mm, 0.95), 
            size_max = max(size1mm)) %>% 
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

## Get intertidal temperatures
names(sadL_means)
names(dm2)

dm3 <- inner_join(dm2, sadL_means, by = c("species", "sampleArea"))
head(dm3)

##### ESTIMATE CHANGE IN SIZE #####

datSub <- dm2 %>% filter(studySub == "subset") %>% 
  select(-c(size_se:size_CI, studySub))
datSub

# Get in wide format
datSubW <- datSub %>% group_by(species, sampleArea) %>% 
  mutate(size_mean2 = lead(size_mean), 
         size_med2 = lead(size_med), 
         size_max2 = lead(size_max), 
         size_95_2 = lead(size_95)) %>%
  ungroup() %>% filter(!is.na(size_mean2)) %>% 
  select(-c(era, year))
datSubW


datSubW2 <- datSubW %>% 
  mutate(delta_mean = size_mean2 - size_mean, 
         delta_mean_per = delta_mean/size_mean * 100, 
         delta_max = size_max2 - size_max, 
         delta_max_per = delta_max/size_max * 100,
         delta_med = size_med2 - size_med, 
         delta_med_per = delta_med/size_med * 100, 
         delta_95 = size_95_2 - size_95, 
         delta_95_per = delta_95/size_95 * 100)

write.csv(datSubW2, "output/size_change_datSubW2.csv")

##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### SUMMARISE PRESENT DAY SIZE #####
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 

pres <- dat4 %>% filter(era == "present")
head(pres)
names(pres)

# Need to get density per sampleUnit (1/4 m2 quadrats for all samples)
unique(pres$sampleUnit)
pres <- pres %>% group_by(sampleUnit) %>% 
  mutate(snail_dens = n() * 4) %>% ungroup()
pres

##### JOIN TEMPERATURE DATA TO RAW SIZE DATA BY POSITION #####
pres2 <- sizeLL %>% select(sampleUnit, position) %>% 
  inner_join(pres, ., by = "sampleUnit")

# Now join tempMeans to size data by position
pres3 <- tempMeans %>% select(position, aspect, slope, metric:CI) %>% 
  inner_join(pres2, ., by = "position")

# Therea are 22 positions with distinct temperature estimates
tempMeans %>% filter(metric == "daily_max") %>% 
  select(position, mean) %>% View()

names(pres3)

pres3 %>% filter(metric == "daily_max") %>% 
  ggplot(aes(mean, size1mm, color = species, 
             shape = species)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

##### SUMMARISE BY SAMPLE UNIT #####

# Calculate size means and densities
suMeans <- pres %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, nest1, nest2, sampleUnit, 
           sampleArea, year, lat, long, tideHTm) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_max = quantile(size1mm, 0.95), 
            size_median = median(size1mm), 
            snail_dens = mean(snail_dens)) %>% 
  ungroup()

suMeans

suMeans %>% 
  ggplot(aes(snail_dens, size_mean, color = species)) + geom_point() + 
  scale_x_log10() + 
  geom_smooth(method = "lm")

##### JOIN TEMPERATURE DATA TO SUMMARISED SIZE DATA BY POSITION #####

suMeans2 <- sizeLL %>% select(sampleUnit, position) %>% 
  inner_join(suMeans, ., by = "sampleUnit")

# Now join tempMeans to size data by position
suMeans3 <- tempMeans %>% select(position, aspect, slope, metric:CI) %>% 
  inner_join(suMeans2, ., by = "position")

##### SUMMARISE SIZE DATA BY POSITION #####
unique(pres3$sampleArea)

# There are 17 positions with distinct temperature data
pres3 %>% filter(metric == "daily_max") %>% select(position, mean) %>% distinct()

posMeans <- pres3 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, position, year, 
           metric, mean, CI) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            tidalHT_mean = mean(tideHTm, na.rm = TRUE), 
            size_max = quantile(size1mm, 0.95), 
            dens_mean = mean(snail_dens), 
            dens_sd = sd(snail_dens)) %>% 
  ungroup()

posMeans$metric

posMeans %>% filter(metric != "daily_mean" & metric != "daily_cv") %>% 
  ggplot(aes(mean, size_mean, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.5) + 
  geom_errorbar(aes(ymax = size_mean + size_CI, 
                    ymin = size_mean - size_CI, 
                    size = NULL), width = 0, alpha = 1) + 
  geom_smooth(method = "lm", se = TRUE,  aes(color = NULL, shape = NULL)) + 
  labs(x = "Temperature (C)", y = "Size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free_x")

posMeans %>% filter(metric != "daily_mean" & metric != "daily_cv") %>% 
  ggplot(aes(mean, size_max, color = species, 
             shape = species, size = size_n)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "Temperature (C)", y = "Size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric, scales = "free_x")

##### SUMMARISE DENSITY DATA BY POSITION #####
unique(suMeans3$sampleArea)
unique(suMeans3$sampleUnit) # 90 quadrats
unique(suMeans3$metric)

suMeans3 %>% select(-n) %>%
  filter(metric == "daily_max") %>% 
  group_by(species) %>% tally()
  
# There are 17 positions with distinct temperature data
suMeans3 %>% filter(metric == "daily_max") %>% select(position, mean) %>% distinct()

# How many sample units per position?
suMeans3 %>% filter(metric == "daily_max") %>% 
  group_by(position) %>% 
  summarise(sampleUnit_n = n())

names(suMeans3)

# Calculate size and density means for each position
posMeans2 <- suMeans3 %>% 
  group_by(species, sp, position, year, 
           metric, mean, CI) %>% 
  summarise(size_mean2 = mean(size_mean), 
            size_sd2 = sd(size_mean),
            sample_n = n(), 
            size_se = size_sd2/sqrt(sample_n), 
            size_CI = qt(0.975, df = sample_n - 1) * size_se, 
            tidalHT_mean = mean(tideHTm, na.rm = TRUE), 
            dens_mean = mean(snail_dens), 
            dens_sd = sd(snail_dens), 
            dens_se = dens_sd/sqrt(sample_n), 
            dens_CI = qt(0.975, df = sample_n - 1) * dens_se) %>% 
  ungroup() 

posMeans2

# Density v temp
posMeans2 %>% filter(metric != "daily_mean" & metric != "daily_cv") %>% 
  ggplot(aes(mean, dens_mean, color = species, 
             shape = species)) +
  geom_point(alpha = 0.5) + 
  labs(x = "Temperature (C)", y = "Density (m-2)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric) + 
  geom_smooth(method = "lm")

# Size v temp
posMeans2 %>% filter(metric != "daily_mean" & metric != "daily_cv") %>% 
  ggplot(aes(mean, size_mean, color = species, 
             shape = species)) +
  geom_point(alpha = 0.5) + 
  labs(x = "Temperature (C)", y = "Density (m-2)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ metric)

