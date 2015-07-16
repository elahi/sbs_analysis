#################################################
# Author: Robin Elahi
# Date: 150716

# Analysis of historical and modern gastropod body sizes
# Hopkins Marine Station

# Change Log
# 150717: added data for Lottia digitalis, area A (Hexter 1950)
#################################################

library(ggplot2)
theme_set(theme_classic(base_size = 8))
library(dplyr)

rm(list=ls(all=TRUE)) 

# plotting functions
source("./R/multiplotF.R")

# load modern data
mod <- read.csv("./data/SBSmaster_150717.csv", na.strings = c("NA", ""))
glimpse(mod)
unique(mod$Shaw_hab)
summary(mod)
unique(mod$notes)
unique(mod$notes2)

# quick look at size-frequencies
qplot(size1mm, data = mod, geom = "density",  facets = ~ species, color = site)
qplot(size1mm, data = mod, geom = "histogram",  facets = ~ species, color = site)

# load historic data
childs <- read.csv("./data/childs_raw.csv", na.strings = c("NA", ""))
wara <- read.csv("./data/wara_raw.csv", na.strings = c("NA", ""))

# need to expand these tables
childs

# use package "mirt"
# frequency column needs to be rightmost
library(mirt)

#################################################
# in one line
childsA <- childs %>% select(length_mm, count_A) %>% expand.table()
childsB <- childs %>% select(length_mm, count_B) %>% expand.table()
childsC <- childs %>% select(length_mm, count_C) %>% expand.table()
childsD <- childs %>% select(length_mm, count_D) %>% expand.table()

mod %>% filter(sp == "LIKE") %>% glimpse()

childsPast <- data.frame(size1mm = c(childsA, childsB, childsC, childsD), 
                       nest1 = c(rep("zoneA", length(childsA)), 
                                 rep("zoneB", length(childsB)), 
                                 rep("zoneC", length(childsC)), 
                                 rep("zoneD", length(childsD))))
childsPast
qplot(size1mm, data = childsPast, geom = "density", color = nest1)
qplot(size1mm, data = childsPast, geom = "histogram")


childsPres <- mod %>% filter(sp == "LIKE") %>% select(size1mm, nest1)

childsPast$era <- "1947"
childsPres$era <- "2014"

childsDF <- rbind(childsPast, childsPres)

qplot(size1mm, data = childsDF,  geom = "density", color = era, facets = ~ nest1)

qplot(size1mm, data = childsDF,  geom = "density", color = era)
qplot(size1mm, data = childsDF,  geom = "histogram", facets = ~ era)

qplot(size1mm, data = childsDF[childsDF$nest1 != "zoneD", ],  
      geom = "density", color = era)
summary(childsDF)
#################################################
### now do Wara
waraB <- wara %>% select(length_mm, count_B) %>% expand.table()
waraD <- wara %>% select(length_mm, count_D) %>% expand.table()

mod %>% filter(sp == "CHFU") %>% glimpse()

waraPast <- data.frame(size1mm = c(waraB, waraD), 
                         site = c(rep("Wara.B", length(waraB)), 
                                   rep("Wara.D", length(waraD))))

waraPres <- mod %>% filter(sp == "CHFU") %>% select(size1mm, site)
unique(waraPres$site)

summary(waraPres)
# remove NA's for size
waraPres <- droplevels(waraPres[complete.cases(waraPres$size1mm), ])

waraPast$era <- "1963"
waraPres$era <- "2014"

waraDF <- rbind(waraPast, waraPres)
qplot(size1mm, data = waraDF,  geom = "density", color = era, facets = ~ site)

qplot(size1mm, data = waraDF,  geom = "density", color = era, 
      xlab = "Size (mm)")


#################################################
# Lottia digitalis; Hexter 1950
hexter <- mod %>% filter(sp == "LODI") %>% select(size1mm, nest1, date)
glimpse(hexter)

qplot(size1mm, data = hexter,  geom = "density", color = date, 
      xlab = "Size (mm)")

# truncate to > 5.95mm?
hexSub <- hexter %>% filter(size1mm > 5.95)
qplot(size1mm, data = hexSub,  geom = "density", color = date, 
      xlab = "Size (mm)")
qplot(size1mm, data = hexSub,  geom = "histogram", fill = date, 
      xlab = "Size (mm)")

summary(hexSub)

# use raw data (no truncation)
hexPast <- hexSub %>% filter(date == "7/1/50")
hexPres <- hexSub %>% filter(date == "7/16/15")

# Try plotting percentages
ggplot(hexPast,  aes(x = size1mm)) +
  geom_histogram()

ggplot(hexPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray")

ggplot(hexPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray")


ggplot(hexSub, aes(x = size1mm)) + 
  geom_histogram(aes(y = ..count../sum(..count..), fill = date))
  
# KS-tests: overall
ks.test(waraPast$size1mm, waraPres$size1mm)
ks.test(childsPast$size1mm, childsPres$size1mm)
ks.test(hexPast$size1mm, hexPres$size1mm)


####################################################
####################################################
####################################################
# Figure 1

# Childs data, exclude zoneD
childsSub <- childsDF[childsDF$nest1 != "zoneD", ]
childsSubPast <- childsSub %>% 
glimpse(childsSub)

childsSubPres <- childsSub %>% filter(era == "2014")
childsSubPast <- childsSub %>% filter(era == "1947")
  
ULClabel <- theme(plot.title = element_text(hjust = -0.08, vjust = 1, 
                                            size = rel(1)))
no_legend <- theme(legend.position = "none")
axisLabels1 <- xlab("Size (mm)") + ylab("Frequency (%)")

fig1a <- ggplot(childsSubPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  labs(title = "A") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Littorina keenae\n1947\nn = 650", 
           x = 18, y = 0.16, size = 2.2)
  
fig1b <- ggplot(childsSubPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  labs(title = "B") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Littorina keenae\n2014\nn = 683", 
           x = 18, y = 0.16, size = 2.2)

fig1c <- ggplot(hexPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(5, 19)) + 
  scale_y_continuous(limits = c(0, 0.32)) + 
  labs(title = "C") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Lottia digitalis\n1950\nn = 104", 
           x = 17, y = 0.25, size = 2.2)

fig1d <- ggplot(hexPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(5, 19)) + 
  scale_y_continuous(limits = c(0, 0.32)) + 
  labs(title = "D") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Lottia digitalis\n2015\nn = 209", 
           x = 17, y = 0.25, size = 2.2)


fig1e <- ggplot(waraPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 2, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  labs(title = "E") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Tegula funebralis\n1963\nn = 817", 
           x = 25, y = 0.25, size = 2.2)

summary(waraPres)
fig1f <- ggplot(waraPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 2, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  labs(title = "F") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "Tegula funebralis\n2014\nn = 5995", 
           x = 25, y = 0.25, size = 2.2)


# multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)

###############################
# save as 7 x 3.5 pdf
pdf("./figs/sbs_fig1.pdf", 7, 3.5)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	

