################################################################################
##' @title Processing hadisst data for sites in meta-analysis
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-03
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)
library(caTools)   

##### GET HADISST FOR EACH SPECIES - ENTIRE DURATION #####

dfHad <- read.csv("sbs_meta/output/dfHad.csv")
head(dfHad)

# Extract the data for each species - year of first and last sampling
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

# Get annual metrics of temperature
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

##### GET HADISST FOR 10 YEAR PERIODS FOR EACH SPECIES #####

head(dfHad)

# Extract the data for each species
head(dat4)
nYrsPrior <- 9
spYrs <- dat4 %>% select(study, species, year, year2) %>%
  distinct() %>%
  mutate(hadPres1 = floor(year2 - nYrsPrior),
         hadPres2 = floor(year2), 
         hadPast1 = floor(year - nYrsPrior), 
         hadPast2 = floor(year)) %>% 
  group_by(study, species) %>% slice(1) %>%
  ungroup() %>% filter(!is.na(year))
spYrs

species <- unique(spYrs$species)
i = 1

species.i <- species[i]
dat.i <- spYrs %>% filter(species == species.i)
dat.i

datHad.i <- inner_join(dfHad, dat.i, by = "study") %>% 
  mutate(past_data = ifelse(Year >= hadPast1 & Year <= hadPast2, 
                            "past", NA), 
         pres_data = ifelse(Year >= hadPres1 & Year <= hadPres2, 
                            "pres", NA)) %>% 
  filter(!is.na(past_data) | !is.na(pres_data)) %>% 
  mutate(era = ifelse(is.na(past_data), "present", past_data)) %>% 
  select(-c(X, hadPres1:pres_data))

head(datHad.i)

datHad <- datHad.i

for(i in 2:length(species)){
  
  species.i <- species[i]
  dat.i <- spYrs %>% filter(species == species.i)
  
  datHad.i <- inner_join(dfHad, dat.i, by = "study") %>% 
    mutate(past_data = ifelse(Year >= hadPast1 & Year <= hadPast2, 
                              "past", NA), 
           pres_data = ifelse(Year >= hadPres1 & Year <= hadPres2, 
                              "pres", NA)) %>% 
    filter(!is.na(past_data) | !is.na(pres_data)) %>% 
    mutate(era = ifelse(is.na(past_data), "present", past_data)) %>% 
    select(-c(X, hadPres1:pres_data))
  
  datHad <- rbind(datHad, datHad.i) 
  
}

datHad2 <- datHad %>% group_by(species, era) %>% 
  mutate(runMean1 = runmean(Temperature_C, 12), 
         runMean10 = runmean(Temperature_C, 12*10),
         Date = ymd(Date)) %>% 
  ungroup()
head(datHad2)

datHad2 %>% 
  ggplot(aes(era, Temperature_C, fill = species)) + 
  geom_boxplot(notch = TRUE) + 
  facet_wrap(~ study + species) + 
  geom_boxplot(aes(era, runMean1), notch = TRUE, col = "black", width = 0.5)

# Get annual metrics of temperature
dfAnnual <- datHad2 %>% group_by(study, species, era, Year) %>%
  summarise(mean_C = mean(Temperature_C), 
            max_C = max(Temperature_C), 
            min_C = min(Temperature_C)) %>%
  ungroup()

dfAnnual

dfAnnL <- gather(dfAnnual, key = metric, value = Temperature_C, 
                 mean_C:min_C)
glimpse(dfAnnL)

dfAnnL %>% 
  ggplot(aes(era, Temperature_C, col = metric)) + 
  geom_boxplot(notch = FALSE) + 
  facet_wrap(~ study + species) 

# Get era metrics of temperature
dfEra <- dfAnnL %>% group_by(study, species, metric, era) %>%
  summarise(mean1 = mean(Temperature_C), 
            sd1 = sd(Temperature_C), 
            n1 = n(), 
            se1 = sd1/sqrt(n1), 
            CI1 = qt(0.975, df = n1 - 1) * se1) %>%
  ungroup() 

dfEra

# Calculate change
dfEra2 <- dfEra %>% group_by(study, species, metric) %>%
  mutate(delta_C = lead(mean1) - mean1) %>% ungroup()

dfEra3 <- dfEra2 %>% filter(!is.na(delta_C)) %>%
  select(species, metric, mean1, CI1, delta_C)

dfEra3
