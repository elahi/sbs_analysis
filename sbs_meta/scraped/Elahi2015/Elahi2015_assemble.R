################################################################################
##' @title Assembling scraped data, Elahi 2015
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-28
##' 
##' @log 
##' 2016-08-02: added summary stats for 50% cutoff - max size
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# load cleaned up data
source("03_summarise_size_data.R")
head(dm2)

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dm2)

df_final <- dm2  %>%
  rename(size_rep = size_mean, size_error = size_CI, 
         sample_size = size_n) %>%
  arrange(species, site, year)

head(df_final)

dfMeta <- data.frame(
  study = "Elahi2015", 
  studySub = df_final$studySub, 
  fig_table = "raw_data", 
  species = df_final$species, 
  site = df_final$sampleArea, 
  size_rep = df_final$size_rep, 
  size_units = "mm", 
  size_error = df_final$size_error, 
  size_error_type = "CI", 
  time_rep = NA, 
  time_error = NA, 
  year = df_final$year, 
  year_error = NA, 
  year_error_type = NA, 
  sample_size = df_final$sample_size, 
  sample_size_units = "number of snails"
)

write.csv(dfMeta, "sbs_meta/output/dfMeta_Elahi2015.csv")
