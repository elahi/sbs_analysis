#################################################
# Author: Robin Elahi
# Date: 150803

# Analysis of historical and modern gastropod body sizes
# Hopkins Marine Station

# Create csv file of button GPS coordinates with sampling details
#################################################

rm(list=ls(all=TRUE)) 

# load packages
library(dplyr)
library(ggplot2)

# load sampling details
dat <- read.csv("./data/buttonDeployment.csv", na.strings = c("", "NA"))
# get one observation for each position (there are two, meant for each of the loggers
# in each zspar blob)
head(dat)
dat <- dat %>% distinct(position)
dat

# load gps coordinates for each logger position from etrex gpx file
gps <- read.csv("./data/ibuttons_gps_150801.csv", na.strings = c("", "NA"))
gps <- gps %>% select(lon, lat, ele, time, name) %>% rename(gps = name)
gps

# now join files
dat2 <- inner_join(dat, gps, by = "gps")
head(dat2)

########################################################
# How accurate were my stillwater estimates of tidal height?

# first, need to remove waraD and lottia area 3 - because Denny's 
# markers were not present in those areas

summary(dat2)
dat3 <- dat2 %>% filter(nest1 != "WaraD" & code != "LODI")

lodiSub <- dat2 %>% filter(code == "LODI") %>% filter(nest1 != "3")

dat4 <- rbind(dat3, lodiSub)

theme_set(theme_classic(base_size = 8))

qplot(tidalHT_old, tidalHT, color = code, data = dat4, 
      xlab = "Tidal height (m) - stillwater estimate", 
      ylab = "Tidal height (m) - Denny marker estimate") + 
  geom_smooth(method = "lm") + 
  geom_abline(intercept = 0, slope = 1) 

ggsave("./figs/tidalEstimates.pdf", width = 3.5, height = 3.5)
  
summary(lm(tidalHT ~ tidalHT_old, data = dat4))


# I tended to underestimate tidal height using still water for CHFU and LODI

########################################################
# get columns for logger retrieval

names(dat2)
loggerPositions <- dat2 %>% select(code, nest1, nest1note, nest2, nest2note, 
                       position, tidalHT, aspect, slope, gps, 
                       lon, lat, ele)
head(loggerPositions)
write.csv(loggerPositions, "./output/buttonPositions.csv")
