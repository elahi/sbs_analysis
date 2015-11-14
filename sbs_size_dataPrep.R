#################################################
# Author: Robin Elahi
# Date: 150717

# Analysis of historical and modern gastropod body sizes
# Hopkins Marine Station

# Prepare gastropod size-freq distributions for
# plotting and analysis
#################################################

library(dplyr)

rm(list=ls(all=TRUE)) 

# load modern data, along with historic data for Lottia (Hexter)
mod <- read.csv("./data/SBSmaster_150717.csv", na.strings = c("NA"))
summary(mod)

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

detach("package:mirt", unload = TRUE)
#################################################
# Combine all three
library(dplyr)
sbsMaster <- rbind(mod, childsPast, waraPast)
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

write.csv(sbsMaster3, './output/sbsMaster.csv')

