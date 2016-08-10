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
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_max = quantile(size1mm, 0.95)) %>% 
  ungroup()

datMeans

##### SUMMARISE SIZE CHANGE DATA - SUBSET #####
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

## Get intertidal temperatures
names(sadL_means)
names(dm2)

dm3 <- inner_join(dm2, sadL_means, by = c("species", "sampleArea"))
head(dm3)

##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### SUMMARISE PRESENT DAY SIZE #####
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 

pres <- dat4 %>% filter(era == "present")

##### JOIN TEMPERATURE DATA TO RAW SIZE DATA BY POSITION #####
pres2 <- sizeLL %>% select(sampleUnit, position) %>% 
  inner_join(pres, ., by = "sampleUnit")

# Now join tempMeans to size data by position
pres3 <- tempMeans %>% select(position, aspect, slope, metric:CI) %>% 
  inner_join(pres2, ., by = "position")

head(pres3)

pres3 %>% filter(metric == "daily_max") %>% 
  ggplot(aes(mean, size1mm, color = species, 
             shape = species)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  labs(x = "Temperature (C)", y = "Size (mm)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

##### SUMMARISE BY SAMPLE UNIT #####

suMeans <- pres %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, nest1, nest2, sampleUnit, 
           sampleArea, year, lat, long, tideHTm) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            size_max = quantile(size1mm, 0.95), 
            size_median = median(size1mm)) %>% 
  ungroup()

##### JOIN TEMPERATURE DATA TO SUMMARISED SIZE DATA BY POSITION #####

suMeans2 <- sizeLL %>% select(sampleUnit, position) %>% 
  inner_join(suMeans, ., by = "sampleUnit")

# Now join tempMeans to size data by position
suMeans3 <- tempMeans %>% select(position, aspect, slope, metric:CI) %>% 
  inner_join(suMeans2, ., by = "position")

##### SUMMARISE DATA BY NEST1 #####

nest1Means <- pres %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, nest1, year, lat, long, tideHTm) %>% 
  summarise(size_mean = mean(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            tidalHT_mean = mean(tideHTm, na.rm = TRUE), 
            size_max = quantile(size1mm, 0.95)) %>% 
  ungroup()
