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

chil <- droplevels(filter(dat, sp == "LIKE"))
summary(chil)
unique(chil$nest2)

# create random effect
names(chil)
randF <- function(x) return(as.factor(paste(x$nest1, x$nest2, sep="_")))

chil$rand1 <- randF(chil)
unique(chil$rand1)

### quick model
mod1 <- lmer(size1mm ~ era*nest1 + (1|rand1), 
             data = chil)
mod1
anova(mod1)

mod1 <- lm(size1mm ~ era*nest1, 
             data = chil)
anova(mod1)

### quick boxplot
qplot(era, size1mm, data = chil, facets = ~ nest1, geom = "boxplot") 



