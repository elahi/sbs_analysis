################################################################################
##' @title Assembling scraped data, Elahi 2015
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-06-24
##' 
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# load modern data
dat <- read.csv("./output/sbsMaster.csv", na.strings = "NA")
summary(dat)
head(dat)

head(dat)

# change ft to meters
range(dat$tideHTm, na.rm = TRUE)
dat$tideHTm <- dat$tideHTm/3.28084

# create numeric lat-long columns
dat$lat2 <- as.numeric(substr(dat$lat, 1, 8))
dat$long2 <- as.numeric(paste("-", substr(dat$long, 1, 9), sep = ""))
unique(dat$lat2)
unique(dat$long2)

dat$LL <- with(dat, paste(lat2, long2, sep = ","))
unique(dat$LL)

# Sampling dates
like_sampling <- c(1947,  2014, 2014-1947)
lodi_sampling <- c(1950,  2015, 2015-1950)
chfu_sampling <- c(1963,  2014, 2014-1963)
samplingDF <- data.frame(like_sampling, lodi_sampling, chfu_sampling)

apply(samplingDF[3,], 1, mean)
apply(samplingDF[3,], 1, sd)

##' Get lat-longs for each sampling location
##' 
##' 
unique(dat$sampleUnit)
unique(dat$LL)
unique(dat$site)

datLL <- dat %>% filter(era == "present") %>%
  select(sp, site, lat2, long2) %>% distinct()

llSummary <- datLL %>% group_by(sp, site) %>%
  summarise(meanLat = mean(lat2, na.rm = TRUE), 
            meanLong = mean(long2, na.rm = TRUE)) 
llSummary

library(ggmap)

hms1 <- get_map(location = c(lon = -121.9045, lat = 36.6218),
                       color = "color", source = "google",
                       maptype = "satellite", zoom = 18)

hmsMap <- ggmap(hms1, extent = "device",
                          ylab = "Latitude", xlab = "Longitude")

hmsMap + 
  geom_point(aes(meanLong, meanLat, color = sp, 
                 shape = sp), data = llSummary)

hmsMap + 
  geom_point(aes(long2, lat2, color = sp, 
                 shape = sp), data = datLL)

llSummary %>% ungroup() %>%
  summarise(grandLat = mean(meanLat), 
            grandLong = mean(meanLong))


##' x = length in 1915-1922 (mm)
##' y = length in 2007 (mm)
##' measurement = CI of x or y; mean
##' dimension = relevant era (x or y)

##### SUMMARIZE MEAN LENGTHS #####

# To extract the mean length in 1915, calculate mean x for the mean size and CI
xDat <- dat %>% filter(dimension != "y") %>% 
  group_by(point) %>% 
  summarise(length_1915 = mean(x))

# To extract the mean length in 2007, calculate mean y for the mean size and CI
yDat <- dat %>% filter(dimension != "x") %>% 
  group_by(point) %>% 
  summarise(length_2007 = mean(y))

lengthDat <- inner_join(xDat, yDat, by = "point")
lengthDat

##### SUMMARIZE CIs #####
head(dat)

# To extract the CI in 1915, calculate difference in x for the CI
xCI <- dat %>% filter(dimension != "x") %>% select(point, x, measurement) %>%
  spread(key = measurement, value = x) %>%
  mutate(CI_1915 = abs(CI_x_right - mean)) %>%
  select(point, CI_1915)

# To extract the CI in 2007, calculate difference in y for the CI
yCI <- dat %>% filter(dimension != "y") %>% select(point, y, measurement) %>%
  mutate(measurement = ifelse(measurement == "CI_y_lower", 
                               "CI_y_upper", 
                               measurement)) %>% 
  spread(key = measurement, value = y) %>%
  mutate(CI_2007 = abs(CI_y_upper - mean)) %>%
  select(point, CI_2007)

ciDat <- inner_join(xCI, yCI, by = "point")

##### COMBINE MEANS AND CIs #####
ciDat; lengthDat

fisherDat <- inner_join(lengthDat, ciDat, by = "point")
head(fisherDat)

##### GET LAT LONGS #####

siteDat <- read_csv("sbs_meta/scraped/Fisher2009/Fisher_2009_tableS1.csv")
head(siteDat)

exposure <- siteDat$Exposure
exposure <- gsub(x = exposure, pattern = "Exposed coast", "exposed")
exposure <- gsub(x = exposure, pattern = "Semiexposed shore", "semi-exposed")
exposure <- gsub(x = exposure, pattern = "Sheltered cove", "sheltered")
unique(exposure)

siteDat$exposure <- exposure

# mean lat long for study
siteDat %>% summarise(meanLat = mean(lat), meanLong = mean(long))

# mean sample sizes by exposure category
sampleSizeDat <- siteDat %>% gather(key = "Collector", value = "sampleSize", Nc:Nf) %>%
  group_by(exposure, Collector) %>%
  summarise(n_mean = mean(sampleSize), 
            n_sd = sd(sampleSize), 
            n_total = sum(sampleSize), 
            nSites = n()) %>% 
  ungroup()

##### GET SITE DETAILS #####
head(dat)

site_details <- dat %>% select(point, exposure) %>% distinct()
site_details

fisherDat <- inner_join(fisherDat, site_details, by = "point")

fisherDat %>% 
  ggplot(aes(x = length_1915, y = length_2007, color = exposure)) +
  geom_point() + 
  geom_errorbar(aes(ymin = length_2007 - CI_2007, 
                    ymax = length_2007 + CI_2007)) + 
  geom_errorbarh(aes(xmin = length_1915 - CI_1915, 
                     xmax = length_1915 + CI_1915)) + 
  coord_fixed(ratio = 1) + 
  scale_x_continuous(breaks = seq(18, 28, by = 2)) + 
  scale_y_continuous(breaks = seq(18, 36, by = 2)) + 
  geom_abline(slope = 1, intercept = 0, color = "black")

##### DEAL WITH SAMPLE SIZES #####

### Fisher does not provide sample sizes for each point, so I have to calculate average number of samples per exposure category, and merge that with the body size data

head(sampleSizeDat)

fisherDat$exposure
head(fisherDat)

ciDat; lengthDat

# Get fisher data in long format
lengthDatL <- lengthDat %>% gather(key = time_rep, value = length_mean, length_1915:length_2007)
ciDatL <- ciDat %>% gather(key = time_rep, value = length_CI, CI_1915:CI_2007)

fisherDatL <- cbind(lengthDatL, ciDatL$length_CI)
fisherDatL <- inner_join(fisherDatL, site_details, by = "point")

names(fisherDatL)[4] <- "length_CI"
fisherDatL$year <- c(rep(1915, 19), rep(2007, 19))
head(fisherDatL)

# Get year (weighted average), by exposure
mean(siteDat$Year, na.rm = TRUE)
weighted.mean(siteDat$Year, siteDat$Nc, na.rm = TRUE)

yearDat <- siteDat %>% group_by(exposure) %>%
  summarise(yearColton = weighted.mean(Year, Nc, na.rm = TRUE))

fisherDatL <- yearDat %>% 
  inner_join(., fisherDatL, by = "exposure")

head(fisherDatL)

# Change Colton year, create site
fisherDatL <- fisherDatL %>% 
  mutate(yearFinal = ifelse(year == 1915, yearColton, year), 
         site = paste(exposure, point, sep = "_"))

# Get sample sizes by exposure
fisherDatL <- sampleSizeDat %>% select(exposure, n_mean) %>%
  inner_join(fisherDatL, ., by = "exposure") %>% 
  rename(sample_size = n_mean) %>% 
  mutate(sample_size_units = "mean number of snails by exposure")


##### FORMAT TABLE FOR META-ANALYSIS #####
names(fisherDatL)

df_final <- fisherDatL %>% select(-c(exposure, yearColton, point, year)) %>%
  rename(size_rep = length_mean, size_error = length_CI, year = yearFinal) %>%
  mutate(species = "Nucella_lapillus") %>%
  arrange(site, year)

head(df_final)


dfMeta <- data.frame(
  study = "Fisher2009", 
  studySub = NA, 
  fig_table = "Figure_1", 
  species = df_final$species, 
  site = df_final$site, 
  size_rep = df_final$size_rep, 
  size_units = "mm", 
  size_error = df_final$size_error, 
  size_error_type = "CI", 
  time_rep = df_final$time_rep, 
  time_error = NA, 
  year = df_final$year, 
  year_error = NA, 
  year_error_type = NA, 
  sample_size = df_final$sample_size, 
  sample_size_units = df_final$sample_size_units
)


write.csv(dfMeta, "sbs_meta/output/dfMeta_Fisher2009.csv")
