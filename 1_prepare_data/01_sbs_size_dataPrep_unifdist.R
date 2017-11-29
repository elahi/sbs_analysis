#################################################
# Author: Robin Elahi
# Date: 171018

# Analysis of historical and modern gastropod body sizes
# Hopkins Marine Station

# Prepare gastropod size-freq distributions for
# plotting and analysis
# I will use a uniform distribution to expand size-frequency data from old studies
#################################################

rm(list=ls(all=TRUE)) 

library(dplyr)
library(lubridate)

# Functions to convert histogram data to vector of raw sizes
source("R/convert_histo_to_raw.R")

# load modern data, along with historic data for Lottia (Hexter)
mod <- read.csv("./data/SBSmaster_150717.csv", na.strings = c("NA"))
mod %>% distinct(era, year, date)

# Historic data for Lottia are already presented as individual values to the nearest 0.1 mm
mod %>% filter(sp == "LODI" & era == "past")

# Separate Lottia data and change year
hexPast <- mod %>% filter(era == "past") %>% 
  mutate(date = "7/1/1950") %>% 
  mutate(date = mdy(date), 
         year = lubridate::year(date))
hexPast %>% distinct(era, year, date)

mod <- mod %>% 
  filter(era == "present") %>% 
  mutate(date = mdy(date), 
         year = lubridate::year(date))
mod %>% distinct(era, year, date)

# load historic data
childs <- read.csv("./data/childs_raw.csv", na.strings = c("NA", ""))
wara <- read.csv("./data/wara_raw.csv", na.strings = c("NA", ""))

# need to expand these size-frequency tables
childs
wara # only resampled area B and area D

##### CHILDS #####
# Childs - Littorina keenae
childsA <- get_random_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_A, 
                            size_interval = 1, distribution = "uniform")
childsB <- get_random_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_B, 
                            size_interval = 1, distribution = "uniform")
childsC <- get_random_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_C, 
                            size_interval = 1, distribution = "uniform")
childsD <- get_random_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_D, 
                            size_interval = 1, distribution = "uniform")

mod %>% filter(sp == "LIKE") %>% glimpse()

childsPast <- data.frame(row = "", 
                         species = "Littorina.keenae", 
                         sp = "LIKE", 
                         site = "HighRock", 
                         era = "past", 
                         date = "1947-06-14",
                         nest1 = c(rep("zoneA", length(childsA)), 
                                   rep("zoneB", length(childsB)), 
                                   rep("zoneC", length(childsC)), 
                                   rep("zoneD", length(childsD))), 
                         nest2 = "area1", 
                         nest3 = NA, 
                         nest3note = NA, 
                         sampleUnit = "", 
                         size1mm = round(c(childsA, childsB, childsC, childsD), 1),
                         habitat = "bare rock", 
                         tideHTm = "", 
                         lat = "36.62183N", 
                         long = "121.90516W", 
                         Shaw_hab = NA, 
                         notes = "", 
                         notes2 = ""
                         )

childsPast <- childsPast %>% mutate(year = lubridate::year(date))
min(childsPast$size1mm)

# For comparison
childsA <- repeat_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_A)
childsB <- repeat_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_B)
childsC <- repeat_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_C)
childsD <- repeat_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_D)

childs_past_repeat <- c(childsA, childsB, childsC, childsD)
plot(density(childs_past_repeat), main = "")

lines(density(childsPast$size1mm), col = "red")

##### WARA #####
wara
### Wara - Chlorostoma funebralis
waraB <- get_random_sizes(size_bin_vector = wara$length_mm, count_vector = wara$count_B, 
                          size_interval = 2, distribution = "uniform")
waraD <- get_random_sizes(size_bin_vector = wara$length_mm, count_vector = wara$count_D, 
                          size_interval = 2, distribution = "uniform")

mod %>% filter(sp == "CHFU") %>% glimpse()

waraPast <- data.frame(row = "", 
                       species = "Chlorostoma.funebralis", 
                         sp = "CHFU", 
                         site = c(rep("Wara.B", length(waraB)), 
                                rep("Wara.D", length(waraD))), 
                         era = "past", 
                         date = "1963-06-01", 
                         nest1 = NA, 
                         nest2 = NA, 
                         nest3 = NA, 
                         nest3note = NA, 
                         sampleUnit = NA, 
                         size1mm = round(c(waraB, waraD), 1), 
                         habitat = NA, 
                         tideHTm = NA, 
                         lat = NA,  
                         long = NA, 
                         Shaw_hab = NA, 
                         notes = "", 
                         notes2 = ""
)

waraPast <- waraPast %>% mutate(year = lubridate::year(date))
min(waraPast$size1mm)

plot(density(waraPast$size1mm))

# For comparison
waraB <- repeat_sizes(size_bin_vector = wara$length_mm, count_vector = wara$count_B)
waraD <- repeat_sizes(size_bin_vector = wara$length_mm, count_vector = wara$count_D)
wara_past_repeat <- c(waraB, waraD)
plot(density(wara_past_repeat), main = "")

lines(density(waraPast$size1mm), col = "red")

#################################################
# Combine all three
sbsMaster <- rbind(mod, childsPast, waraPast, hexPast)
unique(sbsMaster$date)
glimpse(sbsMaster)
summary(sbsMaster)
sbsMaster$row <- seq(1:length(sbsMaster$sp))

# Subset the three species with historical size-frequency distributions 
sbsMaster2 <- filter(sbsMaster, sp == "LIKE" | 
                       sp == "CHFU" |
                       sp == "LODI")

sbsMaster2 <- droplevels(sbsMaster2)
summary(sbsMaster2)

# remove CHFU that were hermit crabs or brunnei
unique(sbsMaster2$notes)
with(sbsMaster2, table(notes))

sbsMaster3 <- sbsMaster2 %>% filter(notes != "hermit crab" &
                                      notes != "brunnei"&
                                      notes != "(hc)" &
                                      notes != "2 snails - calliostoma and brunnei")

sbsMaster3 <- droplevels(sbsMaster3)
unique(sbsMaster3$notes)
with(sbsMaster3, table(notes))

write.csv(sbsMaster3, './output/sbsMaster_unif.csv')
