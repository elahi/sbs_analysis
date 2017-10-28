################################################################################
##' @title Summarise size-density relationships
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-02-10
##' 
##' @log 
##' 2017-10-26 Updating size data
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

source("R/choose_size_data.R")
source("R/choose_size_threshold.R")
source("R/length_to_biomass.R")
source("1_prepare_data/prep_wara_past_transect.R")

options(tibble.print_max = 100, tibble.print_min = 10)

# load data - approximated sizes
dat <- choose_size_data(method = "approximated")

# Do not remove any data
dat <- choose_size_threshold(x = dat, era = "past", filter_data = F) %>% 
  filter(!is.na(size1mm))

# For each sample area, I have calculated tidal height (consistent among eras)
dat %>% distinct(species, sampleArea, era, sample_area_tidal_ht)

# But the modern data are more fine-grained for sampleUnits (sampleUnits = sampleAreas for past)
dat %>% distinct(species, sampleUnit, era, tideHTm) 

##### SUMMARISE SIZE-DENSITY DATA FOR SAMPLE UNITS #####
unique(dat$sampleArea)

## CHOOSE SIZE METRIC
names(dat)
datMeans <- dat %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, nest1, nest2, sampleUnit, sampleArea) %>% 
  summarise(size_mean = mean(size1mm), 
            size_med = median(size1mm), 
            size_sd = sd(size1mm),
            size_n = n(), 
            size_se = size_sd/sqrt(size_n), 
            size_CI = qt(0.975, df = size_n - 1) * size_se, 
            tide_mean = mean(tideHTm)) %>% 
  ungroup()

# Turban snails sampled in 0.25m2 quadrats in present
waraPres <- datMeans %>% filter(sp == "CHFU" & era == "present") %>% 
  mutate(area_sampled = 0.25, 
         density_factor = 1/area_sampled, 
         density_m2 = size_n * density_factor, 
         mass_mean_mg = chfu_mmTOmg(size_mean))

# Ribbed limpets sampled in 0.25m2 quadrats in present
hexPres <- datMeans %>% filter(sp == "LODI" & era == "present") %>% 
  mutate(area_sampled = 0.25, 
         density_factor = 1/area_sampled, 
         density_m2 = size_n * density_factor, 
         mass_mean_mg = lodi_mmTOmg(size_mean))
summary(hexPres)

# Ribbed limpets sampled in past:
# areaA = 0.61 x 0.61 m = 0.37m2
# areaB, C = 0.91 x 0.61 m = 0.55m2
hexPast <- datMeans %>% filter(sp == "LODI" & era == "past") %>% 
  mutate(area_sampled = ifelse(site == "areaA", 0.37, 0.55), 
         density_factor = 1/area_sampled, 
         density_m2 = size_n * density_factor, 
         mass_mean_mg = lodi_mmTOmg(size_mean))

# Periwinkles sampled in 2.3m2 quadrats in present and past
childsPres <- datMeans %>% filter(sp == "LIKE" & era == "present") %>% 
  mutate(area_sampled = 2.3, 
         density_factor = 1/area_sampled, 
         density_m2 = size_n * density_factor, 
         mass_mean_mg = like_mmTOmg(size_mean))
summary(childsPres)

# childs_tide <- childsPres %>% group_by(nest1) %>%
#   summarise(tide_mean = mean(tide_mean))

childsPast <- datMeans %>% filter(sp == "LIKE" & era == "past") %>% 
  mutate(area_sampled = 2.3, 
         density_factor = 1/area_sampled, 
         density_m2 = size_n * density_factor, 
         mass_mean_mg = like_mmTOmg(size_mean))

#childsPast$tide_mean <- childs_tide$tide_mean

datMeans2 <- rbind(waraPres, hexPres, hexPast, childsPres, childsPast)
datMeans2

names(datMeans2)
datMeans3 <- datMeans2 %>% 
  select(species, sp, site, era, tide_mean, size_mean, density_m2, 
         sampleUnit, sampleArea, nest1, nest2, size_n, mass_mean_mg) %>% 
  rename(tideHTm = tide_mean, 
         size_mm = size_mean)
datMeans3
unique(datMeans3$sampleUnit)

# Compare tidal range for Wara in past and present
range(waraPres$tide_mean)
range(wara_past_transect$tideHTm)

# Limit to plus or minus 0.5m on either side 
min_wara_tide <- min(waraPres$tide_mean) - 0.5

wara_past_transect <- wara_past_transect %>%
  mutate(sampleUnit = site, 
         sampleArea = site, 
         nest1 = NA, 
         nest2 = NA, 
         size_n = NA, 
         mass_mean_mg = chfu_mmTOmg(size_mm))

# Remove very low tides for wara_past_transect
wara_past_transect <- wara_past_transect %>% filter(tideHTm > min_wara_tide)
datMeans4 <- datMeans3 %>% rbind(., wara_past_transect)

# Reorder levels
species <- factor(datMeans4$species, levels = rev(c("Littorina keenae",
                                               "Lottia digitalis", 
                                               "Chlorostoma funebralis")))
datMeans4$species <- species

# Log-transform data
datMeans4 <- datMeans4 %>% 
  mutate(dens_log = log10(density_m2), 
         mass_log = log10(mass_mean_mg))

##### SUMMARISE SIZE-DENSITY RAW #####
## Summarise Wara density data to area B and D
wara_dens <- datMeans4 %>% filter(sp == "CHFU")
wara_dens_summary <- wara_dens %>% group_by(species, sp, sampleArea, era) %>% 
  summarise(mean_density = mean(density_m2), 
            median_density = median(density_m2), 
            n = n()) %>% 
  ungroup()
wara_dens_summary

## Get Wara past - use median density
wara_dens_past <- wara_dens_summary %>% 
  select(species, sp, sampleArea, era, median_density) %>% rename(density_m2 = median_density) %>% 
  mutate(nest1 = NA, nest2 = NA, 
         sampleUnit = sampleArea) %>% 
  filter(era == "past")

## Get Hexter and Childs past
dens_past <- datMeans4 %>% filter(era == "past") %>% filter(sp != "CHFU") %>% 
  select(species, sp, sampleUnit, sampleArea, nest1, nest2, era, density_m2)

## Modern data
dens_pres <- datMeans4 %>% filter(era == "present") %>% 
  select(species, sp, sampleUnit, sampleArea, nest1, nest2, era, density_m2)

## Bind datasets
density_df <- rbind(dens_pres, dens_past, wara_dens_past)
density_df %>% filter(era == "present" & sp == "LODI") %>% 
  distinct(tideHTm)

## Link this to raw data
names(dat)
dat_dens <- dat %>% select(species, sp, site, era, nest1, nest2, sampleUnit, 
                           size1mm, tideHTm, year, lat, long, LL, 
                           sampleArea, sample_area_tidal_ht) %>% 
  left_join(., density_df, by = c("species", "sp","sampleUnit","sampleArea", "nest1", "nest2", "era"))

## Get mass_mg for each snail; and log transform
dat_dens <- dat_dens %>% 
  mutate(mass_mg = ifelse(sp == "CHFU", chfu_mmTOmg(size1mm), 
                          ifelse(sp == "LODI", lodi_mmTOmg(size1mm), 
                                 like_mmTOmg(size1mm))), 
         mass_log = log10(mass_mg), 
         dens_log = log10(density_m2), 
         size_log = log10(size1mm))


dat_dens %>% filter(era == "present" & sp == "LODI") %>% 
  distinct(tideHTm)
