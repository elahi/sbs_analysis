################################################################################
##' @title Identify size cut-off and filter dataset
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-05
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##' To minimize the possibility that we sampled the smallest size classes more efficiently than the historic investigators, I will identify a minimum size cut-off for each of the three species

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# load cleaned up data
source("02_sbs_size_dataPrep2.R")

##### IDENTIFY SIZE CUT-OFF #####

##' To be conservative, I will set the minimum size to be above the 5% quantile size for each species from the historic data

cutoffDF <- dat2 %>% filter(era == "past") %>%
  group_by(species) %>% 
  summarise(size0.05 = quantile(size1mm, 0.05, na.rm = TRUE), 
            size0.1 = quantile(size1mm, 0.1, na.rm = TRUE), 
            size0.25 = quantile(size1mm, 0.25, na.rm = TRUE))
  
dat3 <- inner_join(dat2, cutoffDF, by = c("species"))

dat3 %>% 
  ggplot(aes(size)) + 
  geom_histogram(binwidth = 1, fill = "gray", col = "black") + 
  facet_wrap(~ era + species, nrow = 2, scales = "free_y") + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red") + 
  labs(x = "Size (mm)", y = "Frequency")

# ggsave("figs/elahi_histo_cutoff.png", height = 5, width = 7)

# Create column to identify whether the size is above the arbitrary cut-off
dat4 <- dat3 %>%
  mutate(size_threshold = ifelse(size1mm > size0.05, 
                                 "keep", "remove"))

dat4 %>% group_by(size_threshold, species, era) %>% tally()

# Replace "." with " " in species name
dat4$species <- gsub("[.]", " ", dat4$species)
unique(dat4$species)

# Reorder levels
species <- factor(dat4$species, levels = rev(c("Littorina keenae",
                                           "Lottia digitalis", 
                                           "Chlorostoma funebralis")))
dat4$species <- species

# Create subset of data above size threshold
dat5 <- dat4 %>% filter(size_threshold == "keep")
