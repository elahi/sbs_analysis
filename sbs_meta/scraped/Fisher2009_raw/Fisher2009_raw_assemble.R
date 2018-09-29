################################################################################
##' @title Assembling scraped data, Fisher 2009
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2018-09-28
##' 
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

source("R/convert_histo_to_raw.R")

fig2a <- read_csv("sbs_meta/scraped/Fisher2009_raw/Fisher_2009_fig2a.csv") %>% 
  mutate(figure = "fig2a",
         n = ifelse(era == "past", 219, 120))

fig2b <- read_csv("sbs_meta/scraped/Fisher2009_raw/Fisher_2009_fig2b.csv") %>% 
  mutate(figure = "fig2b",
         n = ifelse(era == "past", 436, 256))

fig2c <- read_csv("sbs_meta/scraped/Fisher2009_raw/Fisher_2009_fig2c.csv") %>% 
  mutate(figure = "fig2c",
         n = ifelse(era == "past", 498, 231))

dat <- rbind(fig2a, fig2b, fig2c)
dat <- dat %>% 
  mutate(size1mm = round(size_bin, 0), 
         freq = round(proportion * n, 0))

head(dat)
dat %>% count(size1mm) %>% print(n = 40) # should not be > 6 (3 sites, 2 eras)

# Now sum up across size bins within eras
fisher <- dat %>% 
  group_by(era, size1mm) %>% 
  summarise(freq = sum(freq)) %>% 
  ungroup()

# Sanity check (values should be 24.94 and 31.73mm, for past and present respectively)
fisher_past <- dat %>% filter(era == "past" & figure == "fig2c")
fisher_pres <- dat %>% filter(era == "present" & figure == "fig2c")

# All data
fisher_past <- fisher %>% filter(era == "past")
fisher_pres <- fisher %>% filter(era == "present")

## Get raw sizes (use repeat sizes)
dat_present <- repeat_sizes(size_bin_vector = fisher_pres$size1mm, 
                            count_vector = fisher_pres$freq)
n_present <- length(dat_present)

dat_past <- repeat_sizes(size_bin_vector = fisher_past$size1mm, 
                         count_vector = fisher_past$freq)
n_past <- length(dat_past)

df_past <- data_frame(era = rep("past", n_past), size1mm = dat_past)
df_present <- data_frame(era = rep("present", n_present), size1mm = dat_present)

df <- rbind(df_past, df_present) %>% 
  mutate(year = ifelse(era == "past", 1918, 2007))

df %>% 
  group_by(era) %>% 
  summarise(mean(size1mm))

dat <- data.frame(
  study = "Fisher", 
  studySub = NA, 
  species = "Nucella lapillus", 
  sp = "NULP", 
  site = "Maine",
  era = df$era, 
  date = NA, 
  nest1 = NA, 
  nest2 = NA, 
  nest3 = NA, 
  nest3note = NA, 
  sampleUnit = NA, 
  size1mm = df$size1mm,  
  size1mm_rand = df$size1mm,
  habitat = NA, 
  tideHTm = NA, 
  lat = 44.2553654,   
  long = -68.36960568, 
  Shaw_hab = NA, 
  notes = "", 
  notes2 = "", 
  year = df$year
)

# Save this file
write.csv(dat, "sbs_meta/scraped/Fisher2009_raw/Fisher_raw.csv")



