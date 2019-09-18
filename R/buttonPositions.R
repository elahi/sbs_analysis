################################################################################
##' @title Create csv file of button GPS coordinates with sampling details 
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-09-12
##' 
##' @log 
##' 2016-09-12: fixed tidal heights for logger positions
################################################################################

##' My original tidal height estimates of snail sampling locations, which are in the sbsMaster file, are based on stillwater estimates (2014-2015)
##' When I put temperature loggers out (2015), I used Mark Denny's pvc markers as reference points, which indicate 1.5m above mllw
##' In response to my first round of results, Luke Miller told me that at High Rock, Mark's pvc markers drifted to 1.6-1.7m above mllw (this is the end of Mark's intertidal transect). 
##' 
##' 

#rm(list=ls(all=TRUE)) 

# load packages
library(dplyr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))

##### ORIGINAL CODE, 2nd ROUND OF TIDAL HEIGHT ESTIMATES #####

# load sampling details
dat <- read.csv("./data/buttonDeployment.csv", na.strings = c("", "NA"))

# get one observation for each position (there are two, meant for each of the loggers
# in each zspar blob)
dat <- dat %>% distinct(position, .keep_all = TRUE)
head(dat)

# load gps coordinates for each logger position from etrex gpx file
gps <- read.csv("./data/ibuttons_gps_150801.csv", na.strings = c("", "NA"))
gps <- gps %>% select(lon, lat, ele, time, name) %>% rename(gps = name)
head(gps)

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

qplot(tidalHT_old, tidalHT, color = code, data = dat4, 
      xlab = "Tidal height (m) - stillwater estimate", 
      ylab = "Tidal height (m) - Denny marker estimate") + 
  geom_smooth(method = "lm") + 
  geom_abline(intercept = 0, slope = 1) 

#ggsave("./figs/tidalEstimates.pdf", width = 3.5, height = 3.5)
  
summary(lm(tidalHT ~ tidalHT_old, data = dat4))

# I tended to underestimate tidal height using still water for CHFU and LODI

########################################################
# get columns for logger retrieval

names(dat2)
loggerPositions <- dat2 %>% select(code, nest1, nest1note, nest2, nest2note, 
                       position, tidalHT, tidalHT_old, aspect, slope, gps, 
                       lon, lat, ele)
head(loggerPositions)
#write.csv(loggerPositions, "./output/buttonPositions.csv")

##### REVISED CODE, 3RD ROUND OF TIDAL HEIGHT ESTIMATES #####
##' 13 September 2016
##' 

##' So, for Littorina logger positions, I will add 0.15m to each tidal height
##' to account for the upward drift of Mark's loggers at High Rock

loggerPositions <- loggerPositions %>% 
  mutate(tidalHT_round2 = tidalHT, 
         tidalHT = ifelse(code == "LIKE", 
                          tidalHT + 0.15, tidalHT))

##' Let's plot comparison again
##' How accurate were my stillwater estimates of tidal height?

# first, need to remove waraD and lottia area 3 - because Denny's 
# markers were not present in those areas
head(loggerPositions)

loggerPositions2 <- loggerPositions %>% 
  filter(nest1 != "WaraD") %>% filter(nest1 != "3")

# Round 2
qplot(tidalHT_old, tidalHT_round2, color = code, data = loggerPositions2, 
      xlab = "Tidal height (m) - stillwater estimate", 
      ylab = "Tidal height (m) - Denny marker estimate") + 
  geom_smooth(method = "lm") + 
  geom_abline(intercept = 0, slope = 1) 

# Round 3
qplot(tidalHT_old, tidalHT, color = code, data = loggerPositions2, 
      xlab = "Tidal height (m) - stillwater estimate", 
      ylab = "Tidal height (m) - Denny marker estimate") + 
  geom_smooth(method = "lm") + 
  geom_abline(intercept = 0, slope = 1) 

# ggsave("./figs/tidalEstimates_160913.pdf", width = 3.5, height = 3.5)


##' Get correction factor for stillwater estimates of tidal height
##' I will apply this to the sbsmaster data file
##' 

mod1 <- lm(tidalHT ~ tidalHT_old, data = loggerPositions2)
mod1

mod2 <- lm(tidalHT ~ code * tidalHT_old, data = loggerPositions2)
anova(mod2)

##' There is no interaction between the stillwater estimate of tidal height and species, which means I can apply a global correction factor

##' If I only want to apply the correction to Chlorostoma and Lottia
loggerPositions3 <- loggerPositions2 %>% filter(code != "LIKE")
mod3 <- lm(tidalHT ~ tidalHT_old, data = loggerPositions3)
summary(mod3)

# Round 3
qplot(tidalHT_old, tidalHT, color = code, data = loggerPositions2, 
      xlab = "Tidal height (m) - stillwater estimate", 
      ylab = "Tidal height (m) - Denny marker estimate") + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  geom_smooth(method = "lm", aes(color = NULL), color = "blue")

qplot(tidalHT_old, tidalHT, color = code, data = loggerPositions3, 
      xlab = "Tidal height (m) - stillwater estimate", 
      ylab = "Tidal height (m) - Denny marker estimate") + 
  geom_smooth(method = "lm", aes(color = NULL)) + 
  geom_abline(intercept = 0, slope = 1) 


##### CORRECTIONS THAT NEED TO BE APPLIED #####

##' 1. sbsMaster
##' Chlorostoma: apply regression to get corrected tidal height 
##' Lottia: apply regression to get corrected tidal height 
##' Littorina: apply regression to get corrected tidal height, OR I choose the tidal heights to best match the logger positions (because two loggers were placed in each area). I will choose the former. 

##' 2. intertidal temperature loggers
##' Chlorostoma: apply regression to get corrected tidal height for waraD
##' Lottia: apply regression to get corrected tidal height for area 3 
##' (both of these areas did not have Mark Denny's tidal height markers, and thus were estimated using the stillwater method)

summary(mod3)
coef(mod3)

##' Apply the regression 

loggerPositions_fixed <- loggerPositions %>% 
  mutate(tidalHT_round3 = tidalHT, 
         tidalHT_mod3 = ifelse(nest1 == "WaraD" | nest1 == "3", 
                          tidalHT * coef(mod3)[2] + coef(mod3)[1], 
                          tidalHT), 
         tidalHT_mod1 = ifelse(nest1 == "WaraD" | nest1 == "3", 
                          tidalHT * coef(mod1)[2] + coef(mod1)[1], 
                          tidalHT))
loggerPositions_fixed

write.csv(loggerPositions_fixed, "output/buttonPositions.csv")

ggplot(data = loggerPositions2, aes(tidalHT_old, tidalHT, color = code)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(color = NULL)) + 
  geom_smooth(method = "lm", aes(color = NULL), data = loggerPositions3) + 
  geom_abline(intercept = 0, slope = 1) 

# Compare results from two different models
ggplot(data = loggerPositions_fixed, aes(tidalHT_mod1, tidalHT_mod3, color = code)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

