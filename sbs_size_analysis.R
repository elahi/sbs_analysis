#################################################
# Author: Robin Elahi
# Date: 151113

# Temporal shifts in gastropod size-frequency distributions are
# consistent with climate warming

# Analysis of historical and modern gastropod body sizes
# Hopkins Marine Station
#################################################

library(ggplot2)
theme_set(theme_classic(base_size = 8))
library(dplyr)
library(lme4)
library(nlme)

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
### Mixed-model analysis of overall change in size
###############################################################

###############################
### Set up model structure
# Species is a fixed effect
# Era is a fixed effect
# Sampling area is a random effect

# Need to define 'sampling areas' for each species, 
# that were sampled in historic and modern studies

# Littorina keenae = nest1 (zoneA, zoneB, zoneC, zoneD)
dat %>% filter(sp == "LIKE") %>% distinct(nest1)

# Lottia digitalis = site (areaA, areaB, areaC)
dat %>% filter(sp == "LODI") %>% distinct(site)

# Chlorostoma funebralis = site (Wara.B, Wara.D)
dat %>% filter(sp == "CHFU") %>% distinct(site)

# Create new column called 'sampleArea' based on 
# the areas that were resampled in modern surveys
# This will be a random effect
dat$sampleArea <- as.factor(ifelse(dat$sp == "LIKE", 
                   paste(dat$nest1), paste(dat$site)))
unique(dat$sampleArea)
head(dat)
glimpse(dat)

###############################
### Mixed-models: Era x Species
mod1 <- lmer(size1mm ~ era * species + (1 | sampleArea), 
             data = dat)

mod2 <- lmer(size1mm ~ era * species + (era | sampleArea), 
             data = dat)

AIC(mod1, mod2)
summary(mod2)

lmeMod1 <- lme(size1mm ~ era * species, 
            random = ~ era | sampleArea, 
            na.action = na.omit, 
            data = dat)

round(summary(lmeMod1)$tTable, 3)
anova(lmeMod1)
plot(lmeMod1)

###############################
### Model selection: Era x Species
statDat <- dat
library(AICcmodavg)
# relevel prediction
lmeDat$Prediction <- relevel(lmeDat$Prediction, ref = "none")

# Set up candidate model list
Cand.mod <- list()

# final full model 
Cand.mod[[1]] <- lmer(size1mm ~ era * species + (era | sampleArea), 
                      data = statDat, REML = FALSE)

Cand.mod[[2]] <- lmer(size1mm ~ era + species + (era | sampleArea), 
                      data = statDat, REML = FALSE)

Cand.mod[[3]] <- lmer(size1mm ~ era + (era | sampleArea), 
                      data = statDat, REML = FALSE)

Cand.mod[[4]] <- lmer(size1mm ~ species + (era | sampleArea), 
                      data = statDat, REML = FALSE)

Cand.mod[[5]] <- lmer(size1mm ~ 1 + (era | sampleArea), 
                      data = statDat, REML = FALSE)

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

mod_text <- c("Era x Species", "Era + Species", "Era", 
              "Species", "Null model")

#generate AICc table with numbers
mod.aicctab <- aictab(cand.set= Cand.mod, modnames=mod_numbers, sort=TRUE, 
                      second.ord=FALSE) # second.ord =TRUE means AICc is used (not AIC)

print(mod.aicctab, digits=2, LL=TRUE)

#generate AICc table with names
mod.aicctab <- aictab(cand.set= Cand.mod, modnames= mod_text, sort=TRUE, 
                      second.ord=FALSE) # second.ord =TRUE means AICc is used (not AIC)

print(mod.aicctab, digits=2, LL=TRUE)

# Format the data frame for nicer printing
aic_table <- data.frame(cbind(data.frame(mod.aicctab)[1], 
                              round(data.frame(mod.aicctab)[2:8], 3)))

write.csv(aic_table, "./output/size_AIC.csv")

bestMod <- update(Cand.mod[[2]], REML = TRUE)
summary(bestMod)

# Summarize sizes for each species x era

summaryStats <- statDat %>% group_by(species, sp, era) %>% 
  summarise(meanSize = mean(size1mm, na.rm = TRUE), 
            sdSize = sd(size1mm, na.rm = TRUE), 
            medianSize = median(size1mm,  na.rm = TRUE))

write.csv(summaryStats, "./output/size_summaryStats.csv")

# Calculate percent change for each species
summaryStats

library(tidyr)
spread(summaryStats, era, c(meanSize, sdSize, medianSize) )

spread(summaryStats, era, meanSize)

# Summarize percent change
perChange_mean <- summaryStats %>% 
  select(species, sp, era, meanSize) %>%
  spread(key = era, value = meanSize) %>%
  mutate(perChange = (present - past)/past *100)

perChange_median <- summaryStats %>% 
  select(species, sp, era, medianSize) %>%
  spread(key = era, value = medianSize) %>%
  mutate(perChange = (present - past)/past *100)

perChange_mean
perChange_median

###############################################################
### Violin plots by species and era

# Relevel the species codes
statDat$sp <- factor(statDat$sp, levels = c('LIKE', 'LODI', 'CHFU'), 
                   ordered = TRUE)

p1 <- ggplot(data = statDat, aes(sp, size1mm, fill = era)) + 
  geom_violin(position = position_dodge(0.75)) + 
  geom_boxplot(width = 0.2, notch = TRUE, color = "black", 
               position = position_dodge(0.75)) + 
  scale_fill_manual(values = c("darkgray", "white")) + 
  ylab("Length (mm)") + 
  xlab("Species") + 
  # theme(legend.position = "none") + 
  scale_x_discrete("", 
                   labels = c("CHFU" = expression(italic(Chlorostoma)), 
                              "LODI" = expression(italic(Lottia)), 
                              "LIKE" = expression(italic(Littorina))))

###############################################################
