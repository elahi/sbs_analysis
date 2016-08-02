################################################################################
##' @title Determining the size cut-off for each study
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-02
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE)) 

##' In this script I will compare the maximum size reported for the oldest specimens collected for each study to the size that was deemed a cut-off for the modern resampling.  I will then use the calculated percentage as a guide for analyses that present raw data (e.g., my Hopkins data)

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# Load Roy2003
roy <- read.csv("sbs_meta/output/dfMeta_Roy2003.csv")
# Load Fisher2009
fisher <- read.csv("sbs_meta/output/dfMeta_Fisher2009.csv")

##### ROY2003 #####
names(roy)
# Get the oldest samples for each species
roy2 <- roy %>% filter(year < 1960) %>% 
  select(study, species, site, size_rep, size_error, size_error_type)
roy2

##### FISHER2009 #####
names(fisher)
# Get the oldest samples for each species
fisher2 <- fisher %>% filter(year < 1960) %>% 
  select(study, species, site, size_rep, size_error, size_error_type)
fisher2

##### COMPILE HISTORIC DATASETS #####
historic <- rbind(roy2, fisher2)

# Get the largest sizes per species
historic %>% group_by(study, species) %>% 
  arrange(desc(size_rep)) %>% 
  slice(1)

# Get the largest sizes per species (alternate method)
historic %>% group_by(study, species) %>% 
  top_n(., 1, size_rep)

historicMax <- historic %>% group_by(study, species) %>% 
  top_n(., 1, size_rep) %>% 
  mutate(size_max = size_rep + size_error)

# Now enter size cut-off by hand (from each paper)
historicMax

historicMax$size_cutoff <- c(20, 20, 50, 20, 15)

# Calculate % of max
hm2 <- historicMax %>% 
  mutate(size_cut_per = size_cutoff/size_max) %>% 
  select(-size_error_type, -site)

hm2

