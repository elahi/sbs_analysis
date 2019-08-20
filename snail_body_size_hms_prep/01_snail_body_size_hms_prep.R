#################################################
# Author: Robin Elahi
# Date: 190820

# Prepare the raw data for Littorina, Lottia, and Tegula sampled in 2014
# Prepare the raw data for Lottia sampled in 1950
#################################################

library(dplyr)
library(lubridate)

# load modern data, along with historic data for Lottia (Hexter)
mod <- read.csv("./data/SBSmaster_150717.csv", na.strings = c("NA"))
mod %>% distinct(era, date)

# Historic data for Lottia are already presented as individual values to the nearest 0.1 mm
mod %>% filter(sp == "LODI" & era == "past")

# Separate Lottia data and change year
hexPast <- mod %>% filter(era == "past") %>% 
  mutate(date = "7/1/1950") %>% 
  mutate(date = mdy(date), 
         year = lubridate::year(date), 
         tideHTm = NA)
hexPast %>% distinct(era, year, date)

hexPast$row <- seq(1:length(hexPast$sp))

##### SELECT MODERN DATA FOR MANUSCRIPT #####

# Separate the modern data and change year
mod <- mod %>% 
  filter(era == "present") %>% 
  mutate(date = mdy(date), 
         year = lubridate::year(date))
mod %>% distinct(era, year, sp, date)

# Subset the three species with historical size-frequency distributions 
mod <- filter(mod, sp == "LIKE" | 
                       sp == "CHFU" |
                       sp == "LODI")

mod <- droplevels(mod)
summary(mod)

# remove CHFU that were hermit crabs, brunnei (or empty; revised 2019)
unique(mod$notes)
with(mod, table(notes))

mod <- mod %>% filter(notes != "hermit crab" &
                        notes != "brunnei"&
                        notes != "(hc)" &
                        notes != "2 snails - calliostoma and brunnei" & 
                        notes != "empty")

mod <- droplevels(mod)
unique(mod$notes)
with(mod, table(notes))
mod$row <- seq(1:length(mod$sp))

# ##### WRITE THE MODERN AND LOTTIA DATA TO OUTPUT #####
# 
# # Write the modern and Lottia data to output
# write.csv(hexPast, "./data/hexter_raw.csv")
# write.csv(mod, './output/snail_body_size_hms_modern.csv')