################################################################################
##' @title Assembling scraped data, Fisher 2009
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2018-02-18
##' 
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

dat <- read_csv("sbs_meta/scraped/Fisher2009/Fisher_2009_fig1_r.csv")

head(dat)

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
siteDat

exposure <- siteDat$Exposure
exposure <- gsub(x = exposure, pattern = "Exposed coast", "exposed")
exposure <- gsub(x = exposure, pattern = "Semiexposed shore", "semi-exposed")
exposure <- gsub(x = exposure, pattern = "Sheltered cove", "sheltered")
unique(exposure)

siteDat$exposure <- exposure

# mean lat long by exposure
ll_exposure <- siteDat %>% 
  group_by(exposure) %>% 
  summarise(lat_mean = mean(lat), long_mean = mean(long))

# mean sample sizes by exposure category
sampleSizeDat <- siteDat %>% gather(key = "Collector", value = "sampleSize", Nc:Nf) %>%
  group_by(exposure, Collector) %>%
  summarise(n_mean = mean(sampleSize), 
            n_sd = sd(sampleSize), 
            n_total = sum(sampleSize), 
            nSites = n()) %>% 
  ungroup()

sampleSizeDat

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
fisherDat

ciDat; lengthDat

# Get fisher data in long format
lengthDatL <- lengthDat %>% 
  gather(key = time_rep, value = length_mean, length_1915:length_2007)
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

# Modify sample size data
repeat_this <- unique(fisherDatL$time_rep)
rep(repeat_this, 3)
sampleSizeDat2 <- sampleSizeDat %>%
  mutate(time_rep = rep(repeat_this, 3)) %>% select(-Collector)

# Get sample sizes by exposure
fisherDatL2 <- sampleSizeDat2 %>% select(exposure, time_rep, n_total) %>%
  inner_join(fisherDatL, ., by = c("exposure", "time_rep")) %>% 
  rename(sample_size = n_total) %>% 
  mutate(sample_size_units = "total number of snails by exposure") 

##### SUMMARISE BY EXPOSURE #####

fisher_summary <- fisherDatL2 %>% 
  group_by(exposure, yearFinal, sample_size, sample_size_units) %>% 
  summarise(size_mean = mean(length_mean),  
            size_sd = sd(length_mean)) %>% 
  ungroup()

fisher_summary <- inner_join(fisher_summary, ll_exposure, by = "exposure")

##### FORMAT TABLE FOR META-ANALYSIS #####
names(fisher_summary)

df_final <- fisher_summary %>% 
  mutate(size_rep = size_mean, 
         size_error = size_sd, 
         year = yearFinal, 
         site = exposure, 
         size_original = "mean", 
         time_rep = NA, 
         era = ifelse(year > 2000, "present", "past"), 
         species = "Nucella_lapillus") %>%
  arrange(site, year)

head(df_final)


dfMeta <- data.frame(
  study = "Fisher_2009", 
  studySub = NA, 
  fig_table = "Figure_1", 
  species = df_final$species, 
  site = df_final$site, 
  size_original = df_final$size_original, 
  size_rep = df_final$size_rep, 
  size_units = "mm", 
  size_error = df_final$size_error, 
  size_error_type = "SD", 
  time_rep = df_final$time_rep, 
  time_error = NA, 
  era = df_final$era, 
  year = df_final$year, 
  year_error = NA, 
  year_error_type = NA, 
  sample_size = df_final$sample_size, 
  sample_size_units = df_final$sample_size_units, 
  museum = TRUE, 
  size_threshold_mm = 0, 
  latitude = lat_mean, 
  longitude = long_mean
)

dfMeta

write.csv(dfMeta, "sbs_meta/output/dfMeta_Fisher2009.csv")
