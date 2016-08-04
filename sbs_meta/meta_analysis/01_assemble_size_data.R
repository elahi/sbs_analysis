################################################################################
##' @title Assemble scraped size datasets
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-28
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

elahi <- read.csv("sbs_meta/output/dfMeta_Elahi2015.csv")
# Subset elahi dataset - I removed the lower 5% of the size distribution, in case we sampled the smallest snails more carefully in the modern dataset
elahi <- elahi %>% filter(studySub == "subset")

fisher <- read.csv("sbs_meta/output/dfMeta_Fisher2009.csv")
roy <- read.csv("sbs_meta/output/dfMeta_Roy2003.csv")

dat <- rbind(elahi, fisher, roy)
summary(dat)

##### CALCULATE DELTA SIZE PER YEAR #####

## remove CNM and field data from Roy
## This way, I compare museum specimens at two time points
## The field data in 2001 were separated by snails sampled in vs outside of 
## Cabrillo national monument

dat2 <- dat %>% filter(site != "CNM" &
                         site != "Field") %>%
  arrange(study, site, year)

# Get in wide format
dat3 <- dat2 %>% group_by(species, site) %>% 
  mutate(size_rep2 = lead(size_rep), 
         size_error2 = lead(size_error), 
         year2 = lead(year), 
         sample_size2 = lead(sample_size)) %>%
  ungroup() %>% filter(!is.na(size_rep2)) %>% 
  select(-c(X, size_error_type:time_error, size_units, 
            year_error:year_error_type, sample_size_units))

head(dat3)

dat4 <- dat3 %>% 
  mutate(delta_year = year2 - year, 
         delta_size = size_rep2 - size_rep, 
         delta_size_per = delta_size/size_rep * 100)

dat4 %>% 
  ggplot(aes(delta_year, delta_size_per, color = species, shape = study)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
