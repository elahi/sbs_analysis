################################################################################
##' @title Summarise size-density relationships
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-02-10
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

# load data
source("03_identify_size_cutoff.R")

dat4 # complete dataset
dat5 # subset without smallest size classes

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

waraPast <- wara %>% 
  mutate(sp = "CHFU", 
         species = "Chlorostoma funebralis", 
         tideHTm = tide_ht_ft/3.28084, 
         site = ifelse(area == "areaB", "Wara.B", "Wara.D"), 
         era = "past") %>% 
  select(density_m2, size_mm, species, sp, tideHTm, site, era)
waraPast

##### FIGURE 8 ######

##' eyeballed estimates of transect length from figure 2 at each tidal height
##' eyeballed density estimates per tidal height from figure 8
##' 

transect_length_vector <- c(1, 3, 2, 3, 5)
density_vector <- c(160, 180, 450, 390, 290)

total_snails_vector <- transect_length_vector * 2 * density_vector
sum(total_snails_vector)


##### SUMMARISE SIZE-DENSITY DATA FOR ALL THREE SPECIES #####

dat4 %>% group_by(sp, site, era, year, nest1, nest2) %>% tally() %>% ungroup() %>% View()

unique(dat4$sampleArea)

datMeans <- dat4 %>% filter(!is.na(size1mm)) %>% 
  group_by(species, sp, site, era, year, nest1, nest2, sampleArea) %>% 
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
         density_m2 = size_n * density_factor)

# Ribbed limpets sampled in 0.25m2 quadrats in present
hexPres <- datMeans %>% filter(sp == "LODI" & era == "present") %>% 
  mutate(area_sampled = 0.25, 
         density_factor = 1/area_sampled, 
         density_m2 = size_n * density_factor)

# Ribbed limpets sampled in past:
# areaA = 0.61 x 0.61 m = 0.37m2
# areaB, C = 0.91 x 0.61 m = 0.55m2
hexPast <- datMeans %>% filter(sp == "LODI" & era == "past") %>% 
  mutate(area_sampled = ifelse(site == "areaA", 0.37, 0.55), 
         density_factor = 1/area_sampled, 
         density_m2 = size_n * density_factor)

# Periwinkles sampled in 2.3m2 quadrats in present and past
childsPres <- datMeans %>% filter(sp == "LIKE" & era == "present") %>% 
  mutate(area_sampled = 2.3, 
         density_factor = 1/area_sampled, 
         density_m2 = size_n * density_factor)

childs_tide <- childsPres %>% group_by(nest1) %>%
  summarise(tide_mean = mean(tide_mean))

childsPast <- datMeans %>% filter(sp == "LIKE" & era == "past") %>% 
  mutate(area_sampled = 2.3, 
         density_factor = 1/area_sampled, 
         density_m2 = size_n * density_factor)

childsPast$tide_mean <- childs_tide$tide_mean

datMeans2 <- rbind(waraPres, hexPres, hexPast, childsPres, childsPast)
datMeans2

names(datMeans2)
datMeans3 <- datMeans2 %>% 
  select(species, sp, site, era, tide_mean, size_mean, density_m2, 
         sampleArea, nest1, nest2, size_n) %>% 
  rename(tideHTm = tide_mean, 
         size_mm = size_mean)
datMeans3
unique(datMeans3$sampleArea)

waraPast <- waraPast %>%
  mutate(sampleArea = site, 
         nest1 = NA, 
         nest2 = NA, 
         size_n = NA)
datMeans4 <- datMeans3 %>% rbind(., waraPast)

# Reorder levels
species <- factor(datMeans4$species, levels = rev(c("Littorina keenae",
                                               "Lottia digitalis", 
                                               "Chlorostoma funebralis")))
datMeans4$species <- species

##### GET LOG CHANGE IN DENSITY #####

datAreaMeans <- datMeans4 %>% 
  group_by(species, site, sampleArea, era) %>%
  summarise(tideHTm = mean(tideHTm), 
            size_mm = mean(size_mm), 
            density_m2 = mean(density_m2)) %>% 
  mutate(size_lrr = log10(size_mm/lag(size_mm)), 
         dens_rr = log10(density_m2/lag(density_m2))) %>%
           ungroup()

datAreaMeans %>% 
  ggplot(aes(dens_rr, size_lrr, color = species, shape = species)) + 
  geom_smooth(method = "lm") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  scale_x_continuous(limits = c(-1.1, 1.1)) + 
  scale_y_continuous(limits = c(-1.1, 1.1)) + 
  geom_point(alpha = 0.75, size = 2) + 
  theme(legend.position = "top")

ggsave("figs/size_dens_log_change.png", height = 3.5, width = 3.5)





