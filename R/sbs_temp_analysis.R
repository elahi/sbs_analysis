#################################################
# Author: Robin Elahi
# Date: 151116

# Temporal shifts in gastropod size-frequency distributions are
# consistent with climate warming

# Analyzing daily temperature data
#################################################

# Set working directory to main project folder
setwd("~/github/sbs_analysis")

library(ggplot2)
theme_set(theme_classic(base_size = 8))
library(dplyr)
library(nlme)
library(AICcmodavg)

source("./R/multiplotF.R")

###############################
# Load raw temperature data
rawDat <- read.csv("./output/temp_raw_iButton.csv", header = TRUE)
str(rawDat)
ggplot(data = rawDat, aes(tidalHT, tempC, color = code, shape = code)) + 
  geom_point(alpha = 0.1, size = 2)

###############################
# Load daily temperature data summarized by species
dat <- read.csv("./output/temp_daily_species.csv", header = TRUE)
head(dat)
dat$day <- as.Date(dat$day)
str(dat)

# Relevel the species codes
unique(dat$code)
dat$code <- relevel(dat$code, ref = "LIKE")

# Figure out how many daily records per species, 
# and mean tidal height
dat %>% group_by(code) %>% dplyr::summarise(totalN = n(), 
                                     meanHT = mean(tidalHT, na.rm = TRUE), 
                                     sdHT = sd(tidalHT, na.rm = TRUE))

# Get mean tidal heights for each of nested samples
dat %>% group_by(code, nest2) %>% 
  dplyr::summarise(totalN = n(), 
            meanHT = mean(tidalHT, na.rm = TRUE),
            sdHT = sd(tidalHT, na.rm = TRUE))

###############################################################
### Mixed-model analysis of temperature differences by species
###############################################################

###############################
### Set up model structure
# Fixed effects: species, tidalHT, aspect, slope
# Random intercepts: sampling area, logger position
# Random slopes: ?
# Each measurement is an observation
# Need autocorrelation (nlme)

head(dat)

# Need to define 'sampling areas' for each species, 
# that were sampled in historic and modern studies

# Rename code to sp (for consistency)
dat <- dat %>% dplyr::rename(sp = code)

# Littorina keenae = nest1 (zoneA, zoneB, zoneC, zoneD)
dat %>% filter(sp == "LIKE") %>% distinct(nest1)

# Lottia digitalis = nest1 (1, 2, 3)
dat %>% filter(sp == "LODI") %>% distinct(nest1)

# Chlorostoma funebralis = nest1 (Wara.B, Wara.D)
dat %>% filter(sp == "CHFU") %>% distinct(nest1)

### So, in these models, the sampling areas will be referred
# to as nest1 (random effect)
unique(dat$nest1)

##### Create list of 'sites' (i.e., logger positions) 
# for Luke to run 15 yr hindcasts
### Logger positions are as follows:
head(dat)
unique(dat$position)

positionDF <- dat %>% group_by(position) %>% distinct()
unique(positionDF$position)
write.csv(positionDF, "./output/positionDF.csv")

# Create new column called 'sampleArea' based on 
# the areas that were resampled in modern surveys
# This will be a random effect
# (rename nest1 to sampleArea)
dat$sampleArea <- dat$nest1
unique(dat$sampleArea)
head(dat)
glimpse(dat)

###############################
### Mixed-models: Era x Species
statDat <- dat
unique(statDat$nest2)

lmeMod1 <- lme(fixed = mean_tempC ~ sp, 
               random = list(~ 1 | sampleArea, ~ 1 | position), 
               na.action = na.omit, 
               correlation = corCAR1(), 
               data = dat)
round(summary(lmeMod1)$tTable, 3)

lmeMod2 <- lme(fixed = mean_tempC ~ sp + tidalHT + slope, 
               random = list(~ 1 | sampleArea), 
               na.action = na.omit, 
               correlation = corCAR1(), 
               data = dat)
summary(lmeMod2)

AIC(lmeMod1, lmeMod2)

summary(lmeMod1)
round(summary(lmeMod1)$tTable, 3)
anova(lmeMod1)
plot(lmeMod1)

###############################
### Model selection: Species + Tidal Height
# Set up candidate model list
Cand.mod <- list()

# final full model 
Cand.mod[[1]] <- lme(fixed = mean_tempC ~ sp + tidalHT, 
                     random = list(~ 1 | sampleArea), 
                     na.action = na.omit, method = "ML", 
                     correlation = corCAR1(), 
                     data = dat)
Cand.mod[[2]] <- update(Cand.mod[[1]], ~. - sp)
Cand.mod[[3]] <- update(Cand.mod[[1]], ~. - tidalHT)
Cand.mod[[4]] <- update(Cand.mod[[1]], ~. - tidalHT - sp)

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

mod_text <- c("Species + Tidal Height", "Tidal height", "Species", 
              "Null model")

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

write.csv(aic_table, "./output/temp_AIC.csv")

fullMod <- update(Cand.mod[[1]], method = "REML")
summary(fullMod)
plot(fullMod)

###############################
### Model selection:Species only
### MEAN SIZE
###############################
# Set up candidate model list
Cand.mod <- list()

# final full model 
Cand.mod[[1]] <- lme(fixed = mean_tempC ~ sp, 
                     random = list(~ 1 | sampleArea, ~ 1 | position), 
                     na.action = na.omit, method = "ML", 
                     correlation = corCAR1(), 
                     data = dat)
Cand.mod[[2]] <- update(Cand.mod[[1]], ~. - sp)

anova(Cand.mod[[1]], Cand.mod[[2]])

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

mod_text <- c("Species", "Null model")

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

write.csv(aic_table, "./output/temp_mean_AIC.csv")
plot(Cand.mod[[1]])









###############################
### Model selection:Species only
### MAX SIZE
###############################
# Set up candidate model list
Cand.mod <- list()

# final full model 
Cand.mod[[1]] <- lme(fixed = max_tempC ~ sp, 
                     random = list(~ 1 | sampleArea, ~ 1 | position), 
                     na.action = na.omit, method = "ML", 
                     correlation = corCAR1(), 
                     data = dat)
Cand.mod[[2]] <- update(Cand.mod[[1]], ~. - sp)

anova(Cand.mod[[1]], Cand.mod[[2]])

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

mod_text <- c("Species", "Null model")

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

write.csv(aic_table, "./output/temp_max_AIC.csv")
plot(Cand.mod[[1]])


###############################
### Model selection:Species only
### MIN SIZE
###############################
# Set up candidate model list
Cand.mod <- list()

# final full model 
Cand.mod[[1]] <- lme(fixed = min_tempC ~ sp, 
                     random = list(~ 1 | sampleArea, ~ 1 | position), 
                     na.action = na.omit, method = "ML", 
                     correlation = corCAR1(), 
                     data = dat)
Cand.mod[[2]] <- update(Cand.mod[[1]], ~. - sp)

anova(Cand.mod[[1]], Cand.mod[[2]])

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

mod_text <- c("Species", "Null model")

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

write.csv(aic_table, "./output/temp_min_AIC.csv")
plot(Cand.mod[[1]])

