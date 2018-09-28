#################################################
# Author: Robin Elahi
# Date: 180218
# Assemble data for meta-analysis
# In preparation for use with metafor
#################################################

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(readr)
library(tidyr)
library(metafor)
library(ggplot2)

### Field data
elahi <- read_csv("sbs_meta/output/dfMeta_Elahi2015_species.csv") # averaged across sample areas

elahi <- elahi %>% 
  mutate(species = gsub(x = species, pattern = "\\.", replacement = " ")) %>% 
  mutate(species = ifelse(species == "Chlorostoma funebralis", "Tegula funebralis", species)) %>% 
  mutate(museum = "field")

gall_frank <- read_csv("sbs_meta/output/dfMeta_Galloway-Frank_2017.csv") %>% 
  mutate(museum = "field")

gall_tren <- read_csv("sbs_meta/output/dfMeta_Galloway-Treneman_2017.csv") %>% 
  mutate(museum = "field")

hay_king <- read_csv("sbs_meta/output/dfMeta_Hayford-King_2017.csv") %>% 
  mutate(museum = "field")

hay_elahi <- read_csv("sbs_meta/output/dfMeta_Hayford-Elahi_2018.csv") %>% 
  mutate(museum = "field")

### Museum data

## Fisher summary data is based on the mean sizes from 19 sites on Mount Desert Island
fisher <- read_csv("sbs_meta/output/dfMeta_Fisher2009_MDI.csv") %>% 
  mutate(museum = "museum")

## Wilson-Brodie 2017
wilson <- read_csv("sbs_meta/output/dfMeta_Wilson-Brodie_2017.csv") %>% 
  mutate(museum = "museum")

## Sagarin 2010
sagarin <- read_csv("sbs_meta/output/dfMeta_Sagarin_2010.csv") %>% 
  mutate(museum = "museum")

## Select first museum sample and field for Roy
roy <- read_csv("sbs_meta/output/dfMeta_Roy2003.csv") %>% filter(studySub != "Baseline_2") %>% 
  mutate(museum = "museum")

roy_present <- roy %>% filter(studySub != "Present_CNM") %>% 
  mutate(study = "Roy_2003-Unprotected", studySub = "Unprotected")
roy_presentCNM <- roy %>% filter(studySub != "Present") %>% 
  mutate(study = "Roy_2003-Protected", studySub = "Protected")

## Compile
dat <- rbind(elahi, gall_frank, gall_tren, hay_king, hay_elahi, 
             fisher, wilson, roy_present, roy_presentCNM, sagarin) 

### Add UK to lapillus
dat <- dat %>% 
  mutate(species2 = ifelse(study == "WilsonBrodie_2017", 
                           paste(species, "(UK)", sep = " "), species), 
         species2 = ifelse(study == "Galloway_2017" & species == "Tegula funebralis", 
                           paste(species, "(OR)", sep = " "), species2))

##### PREP FOR METAFOR #####
## For metafor, I want the data in wide format
names(dat)

dat2 <- dat %>% 
  select(study, studySub, species, species2, museum, latitude, era, 
         size_rep, size_error, sample_size) %>% 
  rename(size_n = sample_size)
dat2
names(dat2)

datM <- dat2 %>% 
  gather(key = temp, value = value, starts_with("size")) %>% 
  unite(col = temp1, era, temp, sep = "_") %>%
  spread(temp1, value) %>% 
  arrange(desc(latitude))
datM

### Re-order by latitude
datM <- datM %>% 
  mutate(species2 = reorder(species2, latitude))

## Column names for metafor
names(datM)
names(datM) <- c("study", "studySub", "species", "species2", "museum", "latitude", 
                 "sd2i", "n2i", "m2i", "sd1i", "n1i", "m1i")
datM

### calculate log ratio of means and corresponding sampling variances
datM <- escalc(measure = "ROM", m1i = m1i, sd1i = sd1i, n1i = n1i, 
               m2i = m2i, sd2i = sd2i, n2i = n2i, data = datM)
datM

### Get standard error and CI (following Vuorre)
datM <- datM %>% 
  mutate(sei = as.numeric(sqrt(vi)), 
         upper = yi + 2*sei, 
         lower = yi - 2*sei)

### meta-analysis of log ratio of means using a random-effects model
res <- rma(yi, vi, method = "DL", data = datM)
res
forest(res)

