################################################################################
##' @title Prepare Wara size-density data from transect figure
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-10-26
##' 
##' @log 
################################################################################

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

##### UNDERSTANDING THE WARA HISTOGRAM #####
# Load the raw data from wara size-frequency distribution
wara_histo <- read_csv("data/wara_raw.csv") %>% 
  rename(Wara.B = count_B, Wara.D = count_D)

wara_histo

whl <- wara_histo %>% 
  select(length_mm, Wara.B, Wara.D) %>% 
  gather(key = site, value = snail_count, Wara.B:Wara.D)

whl %>% 
  ggplot(aes(length_mm, snail_count, color = site)) +
  geom_line()

# These snail counts are per m2
# Let's estimate the number of snails total per m2
# And estimate the area sampled (length of transect x 2m swath along transect)
total_snails <- whl %>% group_by(site) %>% 
  summarise(total_snails_m2 = sum(snail_count)) %>% 
  mutate(transect_length_m = c(14, 62.5), 
         transect_area = transect_length_m * 2, 
         transect_snail_count_total = total_snails_m2 * transect_area)

total_snails

##### SUMMARISING THE WARA TRANSECT FIGURES #####
# load size-density data from wara figs 2 and 5
wara <- read_csv("data/Wara_1963_fig2_fig6.csv")
wara <- wara %>% 
  mutate(transect_interval = ifelse(area == "areaB", 1, 2.5), 
         snail_count = density_m2 * transect_interval * 2)

wara %>% 
  ggplot(aes(m_along_transect, density_m2, color = area)) + 
  #facet_wrap(~ area, scales = "free", nrow = 2) + 
  geom_line()

wara_mean <- wara %>% group_by(area) %>% 
  summarise(dens_mean = mean(density_m2))

wara_area <- wara %>% group_by(area) %>%
  summarise(start_m = min(m_along_transect), 
            end_m = max(m_along_transect)) %>% 
  mutate(transect_dist = end_m - start_m, 
         area_m2 = transect_dist * 2)

wara_area2 <- inner_join(wara_area, wara_mean, by = "area")

wara_area2 <- wara_area2 %>%
  mutate(snail_number = area_m2 * dens_mean)

wara_area2

total_snails_transect <- wara %>% group_by(area) %>% summarise(total_snails = sum(snail_count))
total_snails
total_snails_transect

total_snails$transect_estimate <- total_snails_transect$total_snails

total_snails <- total_snails %>% 
  mutate(per_diff = 100 *(transect_estimate - transect_snail_count_total)/transect_snail_count_total)

wara_past_transect <- wara %>% 
  mutate(sp = "CHFU", 
         species = "Chlorostoma funebralis", 
         tideHTm = tide_ht_ft/3.28084, 
         site = ifelse(area == "areaB", "Wara.B", "Wara.D"), 
         era = "past") %>% 
  select(density_m2, size_mm, species, sp, tideHTm, site, era)

##### WARA FIGURE 8 ######

##' eyeballed estimates of transect length from figure 2 at each tidal height
##' eyeballed density estimates per tidal height from figure 8
##' 

transect_length_vector <- c(1, 3, 2, 3, 5)
density_vector <- c(160, 180, 450, 390, 290)

total_snails_vector <- transect_length_vector * 2 * density_vector
sum(total_snails_vector)
