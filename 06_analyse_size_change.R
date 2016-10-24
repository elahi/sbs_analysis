################################################################################
##' @title Analysing snail size size change data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-10
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

# Load data
source("05_summarise_size_data.R")

library(nlme)
library(lme4)
library(AICcmodavg)

##' For size change, I have two main questions:
##' 
##' Do intertidal snails display a reduction in body size over ~ 50 years?
##' 
##' Does tidal height mediate change in size over time?

##' H1: Body size declines over time
##' Mechanism 1: temp-size theory, increased metabolism without a food consumption increase; assumes warming

##' H2: Reductions in body size vary with tidal height
##' Mechanism 1: extreme temperatures select for larger body sizes - e.g., higher in the intertidal
##' Mechanism 2: depending on species thermal performance curve relative to tidal height, increases in minimum temperature increase size but increases in maximum temperature decrease size

###############################################################
### Mixed-model analysis: species x era x tidal height
###############################################################

### Set up model structure
# Fixed effects: era, species, tidal height
# Random intercepts: sampling area
# Each measurement is a single snail size

statDat <- dat5
unique(statDat$sample_area_tidal_ht)
glimpse(statDat)

### calculate the z-score for each species relative to past
spp_past_means <- statDat %>% filter(era == "past") %>% 
  group_by(species) %>% 
  summarise(past_size_mean = mean(size1mm), 
            past_size_sd = sd(size1mm)) 
spp_past_means

statDat2 <- inner_join(statDat, spp_past_means, by = "species")

statDat3 <- statDat2 %>% 
  mutate(size_centered = size1mm - past_size_mean,
         size_z = size_centered/past_size_sd)

statDat3 %>% 
  ggplot(aes(era, size_centered)) + 
  geom_boxplot() + 
  facet_wrap(~ species)

statDat3 %>% 
  ggplot(aes(era, size_z)) + 
  geom_boxplot() + 
  facet_wrap(~ species)

#
statDat %>% 
  group_by(species, era) %>% 
  summarise(size_mean = mean(size1mm)) %>% 
  spread(key = era, value = size_mean) %>% 
  mutate(size_diff = past - present, 
         size_per = size_diff/past * 100)

dat4 %>% 
  group_by(species, era) %>% 
  summarise(size_stat = max(size1mm, na.rm = TRUE)) %>% 
  spread(key = era, value = size_stat) %>% 
  mutate(size_diff = past - present, 
         size_per = size_diff/past * 100)


##### SIZE - lme #####
lmeMod1 <- lme(fixed = size1mm ~ era * species * sample_area_tidal_ht,
               random = list(~ 1 | sampleArea), 
               na.action = na.omit, method = "REML", 
               data = statDat)

summary(lmeMod1)
round(summary(lmeMod1)$tTable, 3)
anova(lmeMod1)
plot(lmeMod1)

##### SIZE - lme - MODEL SELECTION #####

# Set up candidate model list
Cand.mod <- list()

# final full model 
Cand.mod[[1]] <- lme(fixed = size1mm ~ era * species * sample_area_tidal_ht,
                     random = list(~ 1 | sampleArea), 
                     na.action = na.omit, method = "ML",  
                     data = statDat)

Cand.mod[[2]] <- update(Cand.mod[[1]], ~. - era:species:sample_area_tidal_ht)

Cand.mod[[3]] <- update(Cand.mod[[1]], ~ era * species)

Cand.mod[[4]] <- update(Cand.mod[[1]], ~ species * sample_area_tidal_ht)

Cand.mod[[5]] <- update(Cand.mod[[1]], ~ era * sample_area_tidal_ht)

Cand.mod[[6]] <- update(Cand.mod[[1]], ~ species)

Cand.mod[[7]] <- update(Cand.mod[[1]], ~ era)

Cand.mod[[8]] <- update(Cand.mod[[1]], ~ sample_area_tidal_ht)

Cand.mod[[9]] <- update(Cand.mod[[1]], ~ 1)

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

mod_text <- c("Era x Species x Tidal height", 
              "All three 2-way intx", 
              "Era x Species", "Species x Tidal height", "Era x Tidal height", 
              "Species", "Era", "Tidal height", "Null model")

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

write.csv(aic_table, "output/AIC_eraXspeciesXtidalht.csv")

fullMod <- update(Cand.mod[[1]], method = "REML")
summary(fullMod)
plot(fullMod)
anova(fullMod)

##### SIZE - lmer #####

## Test general effect of era
model_1 <- lmer(size1mm ~ era + 
                  (1 | species) + (1 | species:sampleArea), 
                data = statDat3, REML = FALSE)
model_1

model_2 <- lmer(size1mm ~ 1 + 
                  (1 | species) + (1 | species:sampleArea), 
                data = statDat3, REML = FALSE)

anova(model_1, model_2)

## Test era * tidal height
model_3 <- lmer(size1mm ~ era * sample_area_tidal_ht + 
                  (1 | species) + (1 | species:sampleArea), 
                data = statDat3, REML = FALSE)
model_3

model_4 <- lmer(size1mm ~ era + sample_area_tidal_ht + 
                  (1 | species) + (1 | species:sampleArea), 
                data = statDat3, REML = FALSE)

anova(model_3, model_4)


## Test 3 way interaction
model_5 <- lmer(size1mm ~ era * species * sample_area_tidal_ht + 
                   (1 | sampleArea), 
                 data = statDat3, REML = FALSE)

model_6  <- update(model_5, .~. - era : species : sample_area_tidal_ht)
model_6
anova(model_5, model_6)

## Test effect of era
model_7 <- lmer(size1mm ~ era + species + 
                  (1 | sampleArea), 
                data = statDat3, REML = FALSE)

model_8  <- update(model_7, ~ species + 
                     (1 | sampleArea))

anova(model_7, model_8)
plot(model_7)
model_8
model_7
AIC(model_7, model_8)

##### SIZE - lmer - MODEL SELECTION #####

# Set up candidate model list
Cand.mod <- list()

# final full model 
Cand.mod[[1]] <- lmer(size1mm ~ era * species * sample_area_tidal_ht + 
                        (1 | sampleArea), data = statDat, REML = "FALSE")

Cand.mod[[2]] <- update(Cand.mod[[1]], ~. - era:species:sample_area_tidal_ht)

Cand.mod[[3]] <- update(Cand.mod[[1]], ~ era * species + 
                          (1 | sampleArea))

Cand.mod[[4]] <- update(Cand.mod[[1]], ~ species * sample_area_tidal_ht + 
                          (1 | sampleArea))

Cand.mod[[5]] <- update(Cand.mod[[1]], ~ era * sample_area_tidal_ht + 
                          (1 | sampleArea))

Cand.mod[[6]] <- update(Cand.mod[[1]], ~ species + 
                          (1 | sampleArea))

Cand.mod[[7]] <- update(Cand.mod[[1]], ~ era + 
                          (1 | sampleArea))

Cand.mod[[8]] <- update(Cand.mod[[1]], ~ sample_area_tidal_ht + 
                          (1 | sampleArea))

Cand.mod[[9]] <- update(Cand.mod[[1]], ~ 1 + 
                          (1 | sampleArea))

summary(Cand.mod[[2]])

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	

mod_text <- c("Era x Species x Tidal height", 
              "All three 2-way interactions", 
              "Era x Species", "Species x Tidal height", "Era x Tidal height", 
              "Species", "Era", "Tidal height", "Null model")

#generate AICc table with numbers
mod.aicctab <- aictab(cand.set = Cand.mod, modnames=mod_numbers, sort=TRUE, 
                      second.ord = FALSE) # second.ord =TRUE means AICc is used (not AIC)

print(mod.aicctab, digits=2, LL=TRUE)

#generate AICc table with names
mod.aicctab <- aictab(cand.set= Cand.mod, modnames= mod_text, sort=TRUE, 
                      second.ord=FALSE) # second.ord =TRUE means AICc is used (not AIC)

print(mod.aicctab, digits=2, LL=TRUE)

# Format the data frame for nicer printing
aic_table <- data.frame(cbind(data.frame(mod.aicctab)[1], 
                              round(data.frame(mod.aicctab)[2:8], 3)))

write.csv(aic_table, "output/AIC_eraXspeciesXtidalht_lmer.csv")

fullMod <- update(Cand.mod[[1]], REML = "TRUE")
summary(fullMod)
plot(fullMod)
anova(fullMod)





