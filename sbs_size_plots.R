#################################################
# Author: Robin Elahi
# Date: 150718

# Temporal shifts in gastropod size-frequency distributions are
# consistent with climate warming

# Plots of historical and modern gastropod body sizes
# Hopkins Marine Station
#################################################

library(ggplot2)
theme_set(theme_classic(base_size = 8))
library(dplyr)

rm(list=ls(all=TRUE)) 

# plotting functions
source("./R/multiplotF.R")

# Set WD
setwd("~/github/sbs_analysis")

# load modern data
dat <- read.csv("./output/sbsMaster.csv", na.strings = "NA")
summary(dat)

# change ft to meters
range(dat$tideHTm, na.rm = TRUE)
dat$tideHTm <- dat$tideHTm/3.28084

# create numeric lat-long columns
dat$lat2 <- as.numeric(substr(dat$lat, 1, 8))
dat$long2 <- as.numeric(paste("-", substr(dat$long, 1, 9), sep = ""))
unique(dat$lat2)
unique(dat$long2)

dat$LL <- with(dat, paste(lat2, long2, sep = ","))
unique(dat$LL)

###############################################################
# Littorina keenae 
childsDF <- droplevels(filter(dat, sp == "LIKE"))
summary(childsDF)
unique(childsDF$LL)

childsPast <- childsDF %>% filter(era == "past")
childsPres <- childsDF %>% filter(era == "present")

# exclude zoneD
childsSub <- childsDF[childsDF$nest1 != "zoneD", ]

childsSubPres <- childsSub %>% filter(era == "present")
childsSubPast <- childsSub %>% filter(era == "past")

# lat longs
childsLL <- childsPres %>% group_by(site) %>% 
  summarise(species = first(species), nest1 = first(nest1), 
            meanLat = first(lat2), meanLong = first(long2), 
            LL = first(LL))
childsLL
###############################################################
# Chlorostoma funebralis
waraDF <- droplevels(filter(dat, sp == "CHFU"))
summary(waraDF)

# waraDF <- droplevels(waraDF[complete.cases(waraDF$size1mm), ])

waraPast <- waraDF %>% filter(era == "past")
waraPres <- waraDF %>% filter(era == "present")

unique(waraPres$LL)
unique(waraPres$nest1)

waraPres$transect <- with(waraPres, paste(site, nest1, sep = "_"))

# lat longs
waraLL <- waraPres %>% group_by(transect) %>% 
  summarise(site = first(site), species = first(species), 
            lat = first(lat2), long = first(long2), 
            LL = first(LL)) %>%
  arrange(lat, long)
  
waraLL
write.csv(waraLL, 'waraLL.csv')

###############################################################
# Lottia digitalis
hexDF <- droplevels(filter(dat, sp == "LODI"))
summary(hexDF)
unique(hexDF$LL)

# truncate to > 5.95mm
hexSub <- hexDF %>% filter(size1mm > 5.95)
with(hexSub, table(era, nest1, site))
with(hexSub, table(era, site))

# use subsetted data (with truncation)
hexPast <- hexSub %>% filter(era == "past")
hexPres <- hexSub %>% filter(era == "present")

####################################################
####################################################
####################################################
# Figure 1

range(childsPres$tideHTm, na.rm = TRUE)
range(waraPres$tideHTm, na.rm = TRUE)
range(hexPres$tideHTm, na.rm = TRUE)

ULClabel <- theme(plot.title = element_text(hjust = -0.08, vjust = 1, 
                                            size = rel(1)))
no_legend <- theme(legend.position = "none")

fig1aSub <- ggplot(childsSubPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  labs(title = "A") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Littorina keenae\n1947\nn = 650", 
           x = 18, y = 0.16, size = 2.2)

fig1a <- ggplot(childsPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  labs(title = "A") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Littorina keenae\n1947\nn = 682", 
           x = 18, y = 0.16, size = 2.2)

fig1bSub <- ggplot(childsSubPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  labs(title = "B") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Littorina keenae\n2014\nn = 683", 
           x = 18, y = 0.16, size = 2.2)

fig1b <- ggplot(childsPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  labs(title = "B") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Littorina keenae\n2014\nn = 733", 
           x = 18, y = 0.16, size = 2.2)

fig1c <- ggplot(hexPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(5, 25)) + 
  scale_y_continuous(limits = c(0, 0.20)) + 
  labs(title = "C") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Lottia digitalis\n1950\nn = 492", 
           x = 20, y = 0.15, size = 2.2)

fig1d <- ggplot(hexPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(5, 25)) + 
  scale_y_continuous(limits = c(0, 0.20)) + 
  labs(title = "D") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Lottia digitalis\n2015\nn = 587", 
           x = 20, y = 0.15, size = 2.2)


fig1e <- ggplot(waraPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 2, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  labs(title = "E") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Chlorostoma funebralis\n1963\nn = 817", 
           x = 25, y = 0.25, size = 2.2)

summary(waraPres)
fig1f <- ggplot(waraPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 2, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  labs(title = "F") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Chlorostoma funebralis\n2014\nn = 5995", 
           x = 25, y = 0.25, size = 2.2) 


###############################
# save as 7 x 3.5 pdf
pdf("./figs/sbs_fig1.pdf", 7, 3.5)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	

####################################################
####################################################
####################################################

# KS-tests: overall
ks.test(waraPast$size1mm, waraPres$size1mm)
ks.test(childsPast$size1mm, childsPres$size1mm)
ks.test(hexPast$size1mm, hexPres$size1mm)
