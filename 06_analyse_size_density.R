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
bestMod <- update(Cand.mod[[2]], method = "REML")
summary(bestMod)$tTable
plot(bestMod)
anova(bestMod)
bestMod$tT

## Predict values from this model, and then fit regression lines
##???


## Get linear models for each species

lm_fits <- statPres %>% group_by(species) %>% 
  do(fit = lm(size_mm ~ dens_log, data = .))
  
lm_fits %>% glance(fit)
lm_fits %>% tidy(fit)

## Only Chlorostoma is sig for present day data

##### TEST 2: SIZE-DENSITY-ERA - CHLOROSTOMA ONLY #####


mod_text <- c("Era x Species x Density", "2 way intx", "Era + Species + Density","Era", 
              "Species", "Density","Null model", "no Era x Species", "no Era x Density", 
              "no Species x Density")
