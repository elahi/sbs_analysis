#################################################
# Author: Robin Elahi
# Date: 150717

# Analysis of historical and modern gastropod body sizes
# Hopkins Marine Station

# Littorina keenae (Childs 1947)
#################################################


library(ggplot2)
theme_set(theme_classic(base_size = 8))
library(dplyr)
library(lme4)
library(lmerTest)

rm(list=ls(all=TRUE)) 

# plotting functions
source("./R/multiplotF.R")

# load prepped data
dat <- read.csv("./output/sbsMaster.csv", na.strings = c("NA"))
# change ft to meters
range(dat$tideHTm, na.rm = TRUE)
dat$tideHTm <- dat$tideHTm/3.28084

chil <- droplevels(filter(dat, sp == "LIKE"))
summary(chil)
unique(chil$nest2)

############################################
# create new factor with tidal heights
factorList <- unique(chil$nest1)
factorList
unique(chil$tideHTm)

factorList2 <- c("2.4m", "4.3m", "5.8m", "7.3m")
factorList2
tideHT <- plyr::mapvalues(chil$nest1, from = factorList, to = factorList2)

# reorder the levels
tideHT2 <- factor(tideHT, levels = c("7.3m", "5.8m", "4.3m", "2.4m"))
chil$tideHT <- tideHT2
############################################
### plot

# histogram
ggplot(chil,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  facet_grid(tideHT ~ era, scales = "free_y") + 
  xlab("Size (mm)") + ylab("Frequency (%)") 

# density
ggplot(chil,  aes(x = size1mm)) +
  geom_density(aes(color = era)) +
  facet_wrap(~ tideHT, scales = "fixed", nrow = 4) + 
  xlab("Size (mm)") + ylab("Probability density") +
  theme(legend.justification = c(1, 1), legend.position = c(0.97, 0.23),
        legend.text = element_text(size = 6), legend.title = element_text(size = 6)) +
  ggtitle("Littorina keenae")

ggsave("./figs/sbs_fig2.pdf", width = 3.5, height = 7)




############################################

# create random effect
names(chil)
head(chil)
randF <- function(x) return(as.factor(paste(x$era, x$nest1, x$nest2, sep="_")))

chil$rand1 <- randF(chil)
unique(chil$rand1)

### quick model
mod1 <- lmer(size1mm ~ era*nest1 + (era|rand1), 
             data = chil)
;mod1
anova(mod1)

mod1 <- lm(size1mm ~ era*nest1, 
             data = chil)
anova(mod1)



