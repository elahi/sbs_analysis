################################################################################
##' @title Analyze size-density results
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-02-10
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

# Load data
source("05_summarise_size_density.R")

glimpse(datMeans4)

library(nlme)
library(lme4)
library(AICcmodavg)

statDat <- datMeans4 
statDat
unique(statDat$site)

statDat <- statDat %>% 
  mutate(dens_log = log10(density_m2))

##' Test 1
##' Does density affect size, by species, using present data only?
##' (don't use past data because not enough samples for Lottia or Littorina)
##' 
##' Test 2
##' Test size ~ era x density x site (for Chlorostoma only)
##' 
##' Test 3
##' Size ~ era * species * density (all data)

##### TEST 1: SIZE-DENSITY-SPECIES - PRESENT ONLY #####

statPres <- statDat %>% filter(era == "present")
unique(statPres$site)

mod1 <- lme(fixed = size_mm ~ species * log10(density_m2), 
            random = list(~ 1 | site), 
            na.action = na.omit, method = "REML", 
            data = statDat)

summary(mod1)
plot(mod1)
data.frame(anova(mod1))
round(summary(mod1)$tTable, 3)

# Format the data frame for nicer printing
mod1_table <- data.frame(cbind(data.frame(anova(mod1)[1], 
                              round(data.frame(mod1)[2:3], 3))))

round(data.frame(anova(mod1))[1:4], 3)

mod1_table <- round(data.frame(anova(mod1))[1:4], 3)
mod1_table <- mod1_table[-1,]

rownames(mod1_table) <- c("Era", "Species", "Density", "Era x Species", 
                          "Era x Density", "Species x Density", "Era x Species x Density")
colnames(mod1_table) <- c("df_numerator", "df_denominator", "F", "P")
mod1_table
str(mod1_table)
# write.csv(mod1_table, "output/size_dens_anova.csv")

### Model selection

# Set up candidate model list

Cand.mod <- list()

# final full model 
Cand.mod[[1]] <- lme(fixed = size_mm ~ species * dens_log, 
                     random = list(~ 1 | site), 
                     na.action = na.omit, method = "ML", 
                     data = statPres)

Cand.mod[[2]] <- update(Cand.mod[[1]], size_mm ~ species + dens_log)
Cand.mod[[3]] <- update(Cand.mod[[1]], size_mm ~ dens_log)
Cand.mod[[4]] <- update(Cand.mod[[1]], size_mm ~ species)
Cand.mod[[5]] <- update(Cand.mod[[1]], size_mm ~ 1)

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

mod_text <- c("Species x Density", "Species + Density", "Density", "Species","Null model")

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

write.csv(aic_table, "output/size_dens_present_AIC.csv")

## Need to estimate slopes from the best model (species + density)
bestMod <- update(Cand.mod[[1]], method = "REML")
summary(bestMod)$tTable
plot(bestMod)
anova(bestMod)
bestMod$tT

## Predict values from this model, and then fit regression lines
##???
x_range <- range(statPres$dens_log)
x_min <- min(statPres$dens_log)
x_max <- max(statPres$dens_log)

#create data.frame with new values for predictors
#more than one predictor is possible
dens_log = seq(x_min, x_max, 0.01)
x_length = length(dens_log)

new.dat <- data.frame(dens_log = rep(dens_log, 3))
new.dat$species <- c(rep("Chlorostoma funebralis", 262), 
                     rep("Littorina keenae", 262), 
                     rep("Lottia digitalis", 262))

#predict response
new.dat$pred <- predict(bestMod, newdata = new.dat, level=0)
new.dat$dens <- 10 ^ new.dat$dens_log

new.dat %>% 
  ggplot(aes(dens, pred)) + 
  #geom_point(size = 0.1) + 
  facet_wrap(~ species) + 
  geom_smooth(method = "lm")


## Get linear models for each species

lm_fits <- statPres %>% group_by(species) %>% 
  do(fit = lm(size_mm ~ dens_log, data = .))
  
lm_fits %>% glance(fit)
lm_fits %>% tidy(fit)

## Only Chlorostoma is sig for present day data

##### TEST 2: SIZE-DENSITY-ERA - CHLOROSTOMA ONLY #####

statCH <- statDat %>% filter(sp == "CHFU")
statCH %>% group_by(era, site) %>% tally()

mod1 <- lm(size_mm ~ era * site * dens_log, 
   data = statCH)
plot(mod1)
anova(mod1)

mod2 <- lm(size_mm ~ era * site, data = statCH)
summary(mod2)
anova(mod2)
plot(mod2)

statCH %>% 
  ggplot(aes(density_m2, size_mm, color = era)) + 
  facet_wrap(~ site) + 
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method = "lm")

statCH %>% 
  ggplot(aes(era, size_mm, color = era)) + 
  geom_boxplot() + 
  facet_wrap(~ site)

##### TEST 3: SIZE ~ DENSITY-ERA-SPECIES - ALL DATA #####

library(lme4)
library(lmerTest)
unique(statDat$site)

lmMod <- lm(size_mm ~ era * species * dens_log, data = statDat)
anova(lmMod)
summary(lmMod)

mod1 <- lmer(size_mm ~ era * species + (1 | site), 
                 data = statDat)
mod1
plot(mod1)
anova(mod1)
rand(mod1)

mod2 <- lmer(dens_log ~ era * species * tideHTm + (1 | site), 
                 data = statDat)
plot(mod2)
anova(mod2)
rand(mod2)

lmerMod1
anova(lmerMod1)
rand(lmerMod1)
stepMod1 <- step(lmerMod1)
stepMod1
plot(stepMod1)
summary(lmerMod1)

coef(lmerMod1)

fullMod <- lme(fixed = size_mm ~ era * species * dens_log, 
                     random = list(~ 1 | site), 
                     na.action = na.omit, method = "ML", 
                     data = statDat)

summary(fullMod)$tTable
anova(fullMod)

### Model selection
statDat
unique(statDat$site)

# Set up candidate model list

Cand.mod <- list()

mod_text <- c("Era + Species + Density", 
              "Era x Species", "Era x Density", "Species x Density", 
              "Era x Species x Density", 
              "Era", "Species", "Density", "Null", 
              "All 2-way intx")

# three main effects
Cand.mod[[1]] <- lme(fixed = size_mm ~ era + species + dens_log, 
                     random = list(~ 1 | site), 
                     na.action = na.omit, method = "ML", 
                     data = statDat)

# each two way interaction
Cand.mod[[2]] <- update(Cand.mod[[1]], . ~ . + era:species)
Cand.mod[[3]] <- update(Cand.mod[[1]], . ~ . + era:dens_log)
Cand.mod[[4]] <- update(Cand.mod[[1]], . ~ . + species:dens_log)

# thre-way interaction
Cand.mod[[5]] <- update(Cand.mod[[1]], . ~ era * species * dens_log)

# single effects
Cand.mod[[6]] <- update(Cand.mod[[1]], . ~ era)
Cand.mod[[7]] <- update(Cand.mod[[1]], . ~ species)
Cand.mod[[8]] <- update(Cand.mod[[1]], . ~ dens_log)
Cand.mod[[9]] <- update(Cand.mod[[1]], . ~ 1)

# all 2 way intx
Cand.mod[[10]] <- update(Cand.mod[[5]], . ~ . - era : species : dens_log)


#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

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

write.csv(aic_table, "output/era_size_dens_AIC.csv")

## Need to estimate slopes from the best model (species + density)
bestMod <- update(Cand.mod[[3]], method = "REML")
summary(bestMod)$tTable
plot(bestMod)
anova(bestMod)

mod1 <- lmer(size_mm ~ era + species + dens_log + era:dens_log + (1 | site), 
             data = statDat)
anova(mod1)
summary(mod1)

##### FOR THE BEST MODEL ... #####

## Predict values from this model, and then fit regression lines
##???
x_range <- range(statDat$dens_log)
x_min <- min(statDat$dens_log)
x_max <- max(statDat$dens_log)

#create data.frame with new values for predictors
#more than one predictor is possible
dens_log = seq(x_min, x_max, 0.01)
x_length = length(dens_log)

new.dat <- data.frame(dens_log = rep(dens_log, 3))
new.dat$species <- c(rep("Chlorostoma funebralis", 262), 
                     rep("Littorina keenae", 262), 
                     rep("Lottia digitalis", 262))

new.dat$era <- c(rep("past", 786/2), 
                 rep("present", 786/2))

#predict response
new.dat$pred <- predict(bestMod, newdata = new.dat, level=0)
new.dat$dens <- 10 ^ new.dat$dens_log

new.dat %>% 
  ggplot(aes(dens, pred, color = era)) + 
  #geom_point(size = 0.1) + 
  facet_wrap(~ species) + 
  geom_smooth(method = "lm") + 
  scale_x_log10()


## Get linear models for each species

lm_fits <- statPres %>% group_by(species) %>% 
  do(fit = lm(size_mm ~ dens_log, data = .))

lm_fits %>% glance(fit)
lm_fits %>% tidy(fit)