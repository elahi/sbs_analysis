#################################################
# Author: Robin Elahi
# Date: 150717

# Analysis of historical and modern gastropod body sizes
# Hopkins Marine Station

# Data preparation
#################################################

library(dplyr)

rm(list=ls(all=TRUE)) 

# plotting functions
source("./R/multiplotF.R")

# load modern data, along with historic data for Lottia (Hexter)
mod <- read.csv("./data/SBSmaster_150717.csv", na.strings = c("NA", ""))

# load historic data
childs <- read.csv("./data/childs_raw.csv", na.strings = c("NA", ""))
wara <- read.csv("./data/wara_raw.csv", na.strings = c("NA", ""))

# need to expand these size-frequency tables
childs
wara # only resampled area B and area D

# use package "mirt"
# frequency column needs to be rightmost
library(mirt)

#################################################
# Childs - Littorina keenae
childsA <- childs %>% select(length_mm, count_A) %>% expand.table()
childsB <- childs %>% select(length_mm, count_B) %>% expand.table()
childsC <- childs %>% select(length_mm, count_C) %>% expand.table()
childsD <- childs %>% select(length_mm, count_D) %>% expand.table()

mod %>% filter(sp == "LIKE") %>% glimpse()

childsPast <- data.frame(row = "", 
                         species = "Littorina.keenae", 
                         sp = "LIKE", 
                         site = "HighRock", 
                         era = "past", 
                         date = "6/14/47", 
                         nest1 = c(rep("zoneA", length(childsA)), 
                                   rep("zoneB", length(childsB)), 
                                   rep("zoneC", length(childsC)), 
                                   rep("zoneD", length(childsD))), 
                         nest2 = "area1", 
                         nest3 = NA, 
                         nest3note = NA, 
                         sampleUnit = "", 
                         size1mm = c(childsA, childsB, childsC, childsD),
                         habitat = "bare rock", 
                         tideHTm = "", 
                         lat = "36.62183N", 
                         long = "121.90516W", 
                         Shaw_hab = NA, 
                         notes = "", 
                         notes2 = ""
                         )

#################################################
### Wara - Chlorostoma funebralis
waraB <- wara %>% select(length_mm, count_B) %>% expand.table()
waraD <- wara %>% select(length_mm, count_D) %>% expand.table()

mod %>% filter(sp == "CHFU") %>% glimpse()

waraPast <- data.frame(size1mm = c(waraB, waraD), 
                       site = c(rep("Wara.B", length(waraB)), 
                                rep("Wara.D", length(waraD))))


waraPast <- data.frame(row = "", 
                       species = "Chlorostoma.funebralis", 
                         sp = "CHFU", 
                         site = c(rep("Wara.B", length(waraB)), 
                                rep("Wara.D", length(waraD))), 
                         era = "past", 
                         date = "6/1/63", 
                         nest1 = NA, 
                         nest2 = NA, 
                         nest3 = NA, 
                         nest3note = NA, 
                         sampleUnit = NA, 
                         size1mm = c(waraB, waraD),
                         habitat = NA, 
                         tideHTm = NA, 
                         lat = NA,  
                         long = NA, 
                         Shaw_hab = NA, 
                         notes = "", 
                         notes2 = ""
)

#################################################
# Combine all three
sbsMaster <- rbind(mod, childsPast, waraPast)
glimpse(sbsMaster)
sbsMaster$row <- seq(1:length(sbsMaster$sp))

write.csv(sbsMaster, './output/sbsMaster.csv')

