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

