################################################################################
##' @title Analysing snail size size data - present data only 
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-09
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

# Load data
source("05_summarise_size_data.R")

library(nlme)
library(AICcmodavg)
library(lme4)

##' For present day snails only:
##' Does habitat temperature predict variation in snail body size?
##' 
##' H1: Body size declines with increasing mean temperature 
##' (mechanism: temp-size theory, increased metabolism without a food consumption increase)
##' 
##' H2: Body size increases with increasing mean temperature 
##' (mechanism: increased metabolism accompanied by an increase in food consumption)
##' 
##' H3: Body size increases with increasing maximum temperature
##' (mechanism: dessication stress from extreme thermal events selects for larger individuals)

###############################################################
### Mixed-model analysis of size differences by species and temperature
###############################################################

### Set up model structure
# Fixed effects: species, temperature
# Random intercepts: sampling unit
# Random slopes: ?
# Each measurement is a single snail size

head(pres3)
unique(pres3$metric)
unique(pres3$sampleUnit)
unique(pres3$sampleArea)
names(pres3)

##### DAILY MEAN TEMPERATURE #####
dat_mean <- pres3 %>% filter(metric == "daily_mean")

lmeMod1 <- lme(fixed = size1mm ~ species * mean,
               random = list(~ 1 | sampleUnit), 
               na.action = na.omit, method = "REML", 
               data = dat_mean)

summary(lmeMod1)
round(summary(lmeMod1)$tTable, 3)
anova(lmeMod1)
plot(lmeMod1)

##### DAILY MEDIAN TEMPERATURE #####
dat_median <- pres3 %>% filter(metric == "daily_med")

lmeMod2 <- lme(fixed = size1mm ~ species * mean ,
               random = list(~ 1 | sampleUnit), 
               na.action = na.omit, method = "REML", 
               data = dat_median)

summary(lmeMod2)
round(summary(lmeMod2)$tTable, 3)
anova(lmeMod2)
plot(lmeMod2)

##### DAILY MAX TEMPERATURE #####
dat_max <- pres3 %>% filter(metric == "daily_max")

lmeMod3 <- lme(fixed = size1mm ~ species * mean ,
               random = list(~ 1 | sampleUnit), 
               na.action = na.omit, method = "REML", 
               data = dat_max)

summary(lmeMod3)
round(summary(lmeMod3)$tTable, 3)
anova(lmeMod3)
plot(lmeMod3)

##### DAILY MIN TEMPERATURE #####
dat_min <- pres3 %>% filter(metric == "daily_min")

lmeMod4 <- lme(fixed = size1mm ~ species * mean ,
               random = list(~ 1 | sampleUnit), 
               na.action = na.omit, method = "REML", 
               data = dat_min)

summary(lmeMod4)
round(summary(lmeMod4)$tTable, 3)
anova(lmeMod4)
plot(lmeMod4)

##### DAILY MEAN TEMPERATURE - MODEL SELECTION #####


###############################
### Model selection: Species x Temperature
# Set up candidate model list
Cand.mod <- list()

# final full model 
Cand.mod[[1]] <- lme(fixed = size1mm ~ species * mean ,
                     random = list(~ 1 | sampleUnit), 
                     na.action = na.omit, method = "ML",  
                     data = meanDat)
Cand.mod[[2]] <- update(Cand.mod[[1]], ~. - species:mean)
Cand.mod[[3]] <- update(Cand.mod[[1]], ~. - species)
Cand.mod[[4]] <- update(Cand.mod[[1]], ~. - mean)
Cand.mod[[5]] <- update(Cand.mod[[1]], ~. -species - mean)

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

mod_text <- c("Species x Temperature", "Species + Temperature", "Temperature", 
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

# write.csv(aic_table, "./output/temp_AIC.csv")

fullMod <- update(Cand.mod[[1]], method = "REML")
summary(fullMod)
plot(fullMod)
anova(fullMod)
