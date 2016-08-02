################################################################################
##' @title Assemble scraped datasets
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-28
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

elahi <- read.csv("sbs_meta/output/dfMeta_Elahi2015.csv")
# Subset elahi dataset
elahi <- elahi %>% filter(studySub == "subset")
fisher <- read.csv("sbs_meta/output/dfMeta_Fisher2009.csv")
roy <- read.csv("sbs_meta/output/dfMeta_Roy2003.csv")

dat <- rbind(elahi, fisher, roy)
summary(dat)

##### PLOT RAW DATA #####
head(dat)
summary(dat)
unique(dat$site)

dat %>% #filter(site != "CNM") %>%
  ggplot(aes(year, size_rep, color = site)) + 
  geom_point() + geom_line() + 
  facet_wrap(~ species + study, scales = "free_y") + 
  theme(legend.position = "none") + 
  geom_errorbar(aes(ymax = size_rep + size_error, 
                    ymin = size_rep - size_error), width = 3)

ggsave("sbs_meta/meta_figs/size_change_by_spp.png", height = 7,width = 7)


##### CALCULATE DELTA SIZE PER YEAR #####

# remove CNM and field data from Roy
dat2 <- dat %>% filter(site != "CNM" &
                         site != "Field") %>%
  arrange(study, site, year)

# Get in long format
dat3 <- dat2 %>% group_by(species, site) %>% 
  mutate(size_rep2 = lag(size_rep), 
         size_error2 = lag(size_error), 
         year2 = lag(year), 
         sample_size2 = lag(sample_size)) %>%
  ungroup() %>% filter(!is.na(size_rep2)) %>% 
  select(-c(X, size_error_type:time_error, size_units, 
            year_error:year_error_type, sample_size_units))

dat4 <- dat3 %>% 
  mutate(delta_year = year - year2, 
         delta_size = size_rep - size_rep2, 
         delta_size_yr = delta_size/delta_year)

dat4 %>% 
  ggplot(aes(delta_year, delta_size_yr, color = species, shape = study)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

##### GET HADISST FOR EACH SPECIES #####

dfHad <- read.csv("sbs_meta/output/dfHad.csv")
head(dfHad)

# I need to extract the data for each species
head(dat4)
spYrs <- dat4 %>% select(study, species, year, year2) %>%
  distinct() %>%
  mutate(hadYr1 = floor(year2), 
         hadYr2 = floor(year)) %>% 
  group_by(study, species) %>% slice(1) %>%
  ungroup() %>% filter(!is.na(hadYr2))
spYrs


species <- unique(spYrs$species)
i = 1

species.i <- species[i]
dat.i <- spYrs %>% filter(species == species.i)
dat.i

datHad.i <- inner_join(dfHad, dat.i, by = "study") %>% 
  mutate(species1 = ifelse(Year >= hadYr1 & Year <= hadYr2, 
                           "keep", "remove")) %>% 
  filter(species1 == "keep") %>% select(-c(X, species1))

head(datHad.i)

datHad <- datHad.i

for(i in 2:length(species)){
  
  species.i <- species[i]
  dat.i <- spYrs %>% filter(species == species.i)

  datHad.i <- inner_join(dfHad, dat.i, by = "study") %>% 
    mutate(species1 = ifelse(Year >= hadYr1 & Year <= hadYr2, 
                             "keep", "remove")) %>% 
    filter(species1 == "keep") %>% select(-c(X, species1))
  
  datHad <- rbind(datHad, datHad.i) 
  
}

datHad2 <- datHad %>% group_by(species) %>% 
  mutate(runMean1 = runmean(Temperature_C, 12), 
         runMean10 = runmean(Temperature_C, 12*10),
         Date = ymd(Date)) %>% 
  ungroup()
head(datHad2)

datHad2 %>% 
  ggplot(aes(Date, runMean1, col = species)) + 
  geom_line() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ study + species)

##### CALCULATE HADISST SUMMARY METRICS PER SPECIES #####

# Combine
dfAnnual <- datHad2 %>% group_by(study, species, Year) %>%
  summarise(mean_C = mean(Temperature_C), 
            max_C = max(Temperature_C), 
            min_C = min(Temperature_C)) %>%
  ungroup()

dfAnnL <- gather(dfAnnual, key = metric, value = Temperature_C, 
                 mean_C:min_C)
glimpse(dfAnnL)

dfAnnL %>% 
  ggplot(aes(Year, Temperature_C, col = metric)) + 
  geom_line(alpha = 0.5) +
  facet_wrap(~ study + species) + 
  geom_smooth(method = "lm")

##### CALCULATE LINEAR RATE OF CHANGE PER SPECIES #####




