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

#rm(list=ls(all=TRUE)) 

# Load data
source("05_summarise_size_data.R")

library(nlme)
library(AICcmodavg)

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
### Mixed-model analysis of size differences by species and thermal habitat
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


meanDat <- pres3 %>% filter(metric == "daily_mean")

lmeMod1 <- lme(fixed = size1mm ~ species * mean ,
               random = list(~ 1 | sampleArea, ~ 1 | position), 
               na.action = na.omit, 
               data = meanDat)
round(summary(lmeMod1)$tTable, 3)


maxDat <- pres3 %>% filter(metric == "daily_max")

lmeMod1 <- lme(fixed = size1mm ~ species * mean,
               random = list(~ 1 | sampleArea, ~ 1 | position), 
               na.action = na.omit, 
               data = maxDat)
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

