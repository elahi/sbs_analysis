################################################################################
##' @title Link size and hadisst change data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-04
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE)) 

source("sbs_meta/meta_analysis/02_process_hadisst.R")

# Change in size by species
dat4
# Change in temperature by species
dfEra3

dat5 <- inner_join(dat4, dfEra3, by = "species")

dat6 <- dat5 %>% 
  mutate(delta_size_per_C = delta_size_per / delta_C)

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


