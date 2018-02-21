#################################################
# Author: Robin Elahi
# Date: 180218
# Assemble data for meta-analysis
# In preparation for use with metafor
#################################################

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())) 
library(viridis)

## Field data
elahi <- read_csv("sbs_meta/output/dfMeta_Elahi2015_species.csv") # averaged across sample areas
elahi <- elahi %>% 
  mutate(species = gsub(x = species, pattern = "\\.", replacement = " ")) %>% 
  mutate(species = ifelse(species == "Chlorostoma funebralis", "Tegula funebralis", species))

gall_frank <- read_csv("sbs_meta/output/dfMeta_Galloway-Frank_2017.csv")
gall_tren <- read_csv("sbs_meta/output/dfMeta_Galloway-Treneman_2017.csv")
hay_king <- read_csv("sbs_meta/output/dfMeta_Hayford-King_2017.csv")

## Museum data

## Fisher summary data is based on the mean sizes from 19 sites on Mount Desert Island
fisher <- read_csv("sbs_meta/output/dfMeta_Fisher2009_MDI.csv")
# roy <- read_csv("sbs_meta/output/dfMeta_Roy2003.csv")

dat <- rbind(elahi, gall_frank, gall_tren, hay_king, fisher) 

## For metafor, I want the data in wide format
names(dat)
dat2 <- dat %>% select(study, species, era, size_rep, size_error, sample_size) %>% 
  rename(size_n = sample_size)
dat2

datM <- dat2 %>% 
  gather(key = temp, value = value, starts_with("size")) %>% 
  unite(col = temp1, era, temp, sep = "_") %>% 
  spread(temp1, value)

## Try metafor
library(metafor)
names(datM)
names(datM) <- c("study", "species", "sd2i", "n2i", "m2i", "sd1i", "n1i", "m1i")
datM

### calculate log ratio of means and corresponding sampling variances
datM <- escalc(measure = "ROM", m1i = m1i, sd1i = sd1i, n1i = n1i, 
               m2i = m2i, sd2i = sd2i, n2i = n2i, data = datM)
datM

### Get standard error (following Vuorre)
datM <- datM %>% 
  mutate(sei = as.numeric(sqrt(vi)), 
         upper = yi + 2*sei, 
         lower = yi - 2*sei)

### Plot this
ggplot(datM, aes(x = yi, y = species, color = study)) +
  geom_segment(aes(x = lower, xend = upper, y = species, yend = species)) +
  geom_point() + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  labs(x = "Proportional change in body size", 
       y = "") + 
  scale_color_viridis_d()

### meta-analysis of log ratio of means using a random-effects model
res <- rma(yi, vi, method = "DL", data = datM)
res


