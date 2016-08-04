################################################################################
##' @title Assemble scraped datasets
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-28
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12))
library(lubridate)

elahi <- read.csv("sbs_meta/output/dfMeta_Elahi2015.csv")
# Subset elahi dataset
elahi <- elahi %>% filter(studySub == "subset")
fisher <- read.csv("sbs_meta/output/dfMeta_Fisher2009.csv")
roy <- read.csv("sbs_meta/output/dfMeta_Roy2003.csv")

dat <- rbind(elahi, fisher, roy)
summary(dat)

##### PLOT RAW DATA #####
head(dat)
summary(dat)
unique(dat$site)

dat %>% #filter(site != "CNM") %>%
  ggplot(aes(year, size_rep, color = site)) + 
  geom_point() + geom_line() + 
  facet_wrap(~ species + study, scales = "free_y") + 
  theme(legend.position = "none") + 
  geom_errorbar(aes(ymax = size_rep + size_error, 
                    ymin = size_rep - size_error), width = 3)

# ggsave("sbs_meta/meta_figs/size_change_by_spp.png", height = 7,width = 7)

##### CALCULATE DELTA SIZE PER YEAR #####

# remove CNM and field data from Roy
dat2 <- dat %>% filter(site != "CNM" &
                         site != "Field") %>%
  arrange(study, site, year)

# Get in wide format
dat3 <- dat2 %>% group_by(species, site) %>% 
  mutate(size_rep2 = lead(size_rep), 
         size_error2 = lead(size_error), 
         year2 = lead(year), 
         sample_size2 = lead(sample_size)) %>%
  ungroup() %>% filter(!is.na(size_rep2)) %>% 
  select(-c(X, size_error_type:time_error, size_units, 
            year_error:year_error_type, sample_size_units))

head(dat3)

dat4 <- dat3 %>% 
  mutate(delta_year = year2 - year, 
         delta_size = size_rep2 - size_rep, 
         delta_size_per = delta_size/size_rep * 100)

dat4 %>% 
  ggplot(aes(delta_year, delta_size_per, color = species, shape = study)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

##### JOIN DELTA TEMPERATURE WITH SIZE DATA #####
source("sbs_meta/meta_analysis/sbs_hadisst_process.R")

dat4

dat5 <- inner_join(dat4, dfEra3, by = "species")

dat6 <- dat5 %>% 
  mutate(delta_size_per_C = delta_size_per/delta_C)

dat6 %>% 
  ggplot(aes(delta_C, delta_size_per, color = species, shape = study)) + 
  theme_bw(base_size = 10) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  facet_wrap(~ metric) + 
  labs(x = "Change in temperature (C)", y = "Change in size (%)")

ggsave("sbs_meta/meta_figs/deltaSizePer_v_deltaC.png", height = 3.5, width = 7)

##### STATISTICAL ANALYSIS - LME4 #####
library(lme4)
library(nlme)

lmerDat <- dat6 %>% filter(metric == "mean_C")

## Delta size_per ~ delta C
mod1 <- lmer(delta_size_per ~ delta_C + (1 | study) + (1 | species), 
             lmerDat)
summary(mod1)

mod1 <- lme(delta_size_per ~ delta_C, 
            data = lmerDat, 
            random = ~ 1 | study)
summary(mod1)


mod2 <- lmer(delta_size_per ~ 1 + (1 | study) + (1 | species), 
             lmerDat)
anova(mod1, mod2)

AIC(mod1, mod2)

# Create prediction dataframe
tempRange <- with(lmerDat, range(delta_C))
newdat_x <- seq(tempRange[1], tempRange[2], by = 0.1)
newdat <- data.frame(delta_C = newdat_x)
newdat$delta_size_per <- predict(mod1, level = 0, newdata = newdat)

lmerDat %>% 
  ggplot(aes(delta_C, delta_size_per, color = species, shape = study)) + 
  theme_bw(base_size = 10) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  labs(x = "Change in mean annual temperature (C)", y = "Change in size (%)") + 
  theme(legend.justification = c(1, 0), legend.position = c(1, 0)) + 
  theme(legend.key.size = unit(0.2, "cm"), legend.title = element_blank()) + 
  guides(shape = FALSE)

# geom_line(aes(color = NULL, shape = NULL), data = newdat) 
ggsave("sbs_meta/meta_figs/deltaSizePer_v_meanDeltaC.png", height = 3.5, width = 3.5)
  
  
  
# Get average size change  
lmerDat2 <- lmerDat %>% 
  group_by(study, species) %>% 
  summarise(mean_delta_size_per = mean(delta_size_per), 
            sd_delta_size_per = sd(delta_size_per), 
            n_delta_size_per = n(), 
            delta_C = mean(delta_C))
  
lmerDat2 %>% 
  ggplot(aes(delta_C, mean_delta_size_per, color = species, shape = study)) + 
  theme_bw(base_size = 10) + 
  geom_point(aes(size = n_delta_size_per)) + 
  scale_size_continuous(range = c(3,6)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  labs(x = "Change in mean annual temperature (C)", y = "Change in size (%)") + 
  geom_errorbar(aes(ymin = mean_delta_size_per - sd_delta_size_per, 
                    ymax = mean_delta_size_per +  sd_delta_size_per)) + 
  theme(legend.justification = c(0, 1), legend.position = c(0, 1)) + 
  theme(legend.key.size = unit(0.3, "cm"), legend.title = element_blank()) + 
  guides(shape = FALSE, size = FALSE)
ggsave("sbs_meta/meta_figs/meanDeltaSizePer_v_meanDeltaC.png", height = 3.5, width = 3.5)


