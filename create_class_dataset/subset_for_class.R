################################################################################
##' @title Plot snail size-frequency distributions
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2016-08-05
##' @log 
################################################################################

##### LOAD PACKAGES, DATA #####

library(grid)
source("R/choose_size_data.R")
source("R/choose_size_threshold.R")

# load data  sizes
dat <- choose_size_data(method = "normal")

# Do not remove any data
dat2 <- choose_size_threshold(x = dat, era = "combined", filter_data = F) %>% 
  filter(!is.na(size1mm))
names(dat2)
unique(dat2$species)


dat2 %>% count(species, era)

## Make separate dataframes

# Littorina keenae 
childsDF <- droplevels(filter(dat2, sp == "LIKE"))
childsPast <- childsDF %>% filter(era == "past")
childsPres <- childsDF %>% filter(era == "present")

# Chlorostoma funebralis
waraDF <- droplevels(filter(dat2, sp == "CHFU"))
waraPast <- waraDF %>% filter(era == "past")
waraPres <- waraDF %>% filter(era == "present")

# Lottia digitalis
hexDF <- droplevels(filter(dat2, sp == "LODI"))
hexPast <- hexDF %>% filter(era == "past")
hexPres <- hexDF %>% filter(era == "present")


## Subset waraPres 
names(waraPres)
waraPres %>% count(site, nest1, nest2)
waraPresB <- waraPres %>% filter(site == "Wara.B")
waraPresD <- waraPres %>% filter(site == "Wara.D")

waraPresD %>% count(nest1, nest2)

set.seed(101)
waraPresD_sub <- waraPresD %>% sample_frac(., 0.1, replace = FALSE)

waraPres_sub <- rbind(waraPresB, waraPresD_sub)

waraPres_sub %>% 
  ggplot(aes(nest1, size1mm)) + 
  geom_jitter(width = 0.1, alpha = 0.5) + 
  facet_wrap(~ site, scales = "free_y") + 
  coord_flip()

waraPresD_sub %>% 
  ggplot(aes(nest2, size1mm)) + 
  geom_jitter(width = 0.1, alpha = 0.5) + 
  facet_wrap(~ site + nest1, scales = "free_y") + 
  coord_flip()

## Join datasets
waraDF_sub <- rbind(waraPres_sub, waraPast)
names(waraDF_sub)
waraDF_sub <- waraDF_sub %>% 
  select(species, site, era, nest1, nest2, size1mm, habitat, tideHTm, 
         lat, long)

waraDF_sub %>%
  ggplot(aes(size1mm, fill = era)) + 
  geom_histogram(alpha = 0.5) + 
  facet_wrap(~ site, scales = "free_y")

waraDF_sub %>%
  ggplot(aes(tideHTm, size1mm)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ era, scales = "free_y")

write.csv(waraDF_sub, "../xdp/data/hms_tegula_funebralis_subset.csv")
