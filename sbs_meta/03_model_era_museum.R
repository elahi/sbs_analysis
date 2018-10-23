#################################################
# Author: Robin Elahi
# Date: 180929
# Analysis of historical and modern gastropod body sizes
# LMER for era x museum
#################################################

source("sbs_meta/01_assemble_raw_data.R")
library(nlme)
library(broom)
library(viridis)

library(lme4)
library(sjPlot)

###### ALL DATA #####

stat_dat <- df
stat_dat <- dfsub

stat_dat <- stat_dat %>% 
  filter(!is.na(size1mm)) %>% 
  mutate(size_log = log(size1mm), 
         species2 = paste(species, fig_legend, sep = "_"))

## Let intercepts vary by species nested within study
fit1 <- lme(size_log ~ era * museum,
            random = ~ 1 | species2 / study,
            data = stat_dat)

## Let intercepts vary by species 
fit2 <- lme(size_log ~ era * museum,
            random = ~ 1 | species2,
            data = stat_dat)

## Let intercepts vary by species, and slopes vary by era
fit3 <- lme(size_log ~ era * museum,
            random = ~ era | species2,
            data = stat_dat)

AIC(fit1, fit2, fit3)

my_fit <- fit1
my_fit <- fit2
my_fit <- fit3

summary(my_fit)
plot(my_fit, resid(., type = "p") ~ fitted(.), abline = 0)

plot(my_fit, species2 ~ resid(.), abline = 0)
plot(my_fit, species2 ~ resid(.) | era, abline = 0)
plot(my_fit, species2 ~ resid(.) | museum, abline = 0)

plot(my_fit, size_log ~ fitted(.), abline = c(0, 1))
plot(my_fit, size_log ~ fitted(.) | species2, abline = c(0, 1))
plot(my_fit, size_log ~ fitted(.) | era, abline = c(0, 1))
plot(my_fit, size_log ~ fitted(.) | museum, abline = c(0, 1))

##### PLOT GLMM EFFECTS #####

summary(my_fit)

## All data
effects_table_df <- summary(my_fit)$tTable %>% tbl_df() %>% mutate(dataset = "All data")
## Data subset (1/2 max)
effects_table_dfsub <- summary(my_fit)$tTable %>% tbl_df() %>% mutate(dataset = "Subset of data")

effects_table <- rbind(effects_table_df, effects_table_dfsub)

n_df <- length(df$study)
n_dfsub <- length(dfsub$study)

## Prepare table
effects_table <- effects_table %>% 
  mutate(upper = Value + Std.Error * 2, 
         lower = Value - Std.Error * 2, 
         Parameter = rep(rownames(summary(fit1)$tTable), 2), 
         Param = rep(c("Intercept", "Era (present)", "Museum", "Era x Museum"), 2), 
         n = ifelse(dataset == "All data", n_df, n_dfsub)) %>%
  mutate(Param = factor(Param, levels = rev(c("Intercept", "Era (present)", "Museum", "Era x Museum"))))

## Plot
my_dodge <- -0.5

theme_set(theme_bw(base_size = 14) + 
            theme(panel.grid.major.y = element_blank(), 
                  panel.grid.major.x = element_line(size = 0.25), 
                  panel.grid.minor = element_blank(), 
                  strip.background = element_blank()))

p1 <- effects_table %>% 
  filter(Param != "Intercept") %>% 
  ggplot(aes(Param, Value, fill = dataset)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = my_dodge)) + 
  geom_point(aes(size = n), position = position_dodge(width = my_dodge), pch = 21) + 
  coord_flip() + 
  labs(x = "", y = "Model coefficient") + 
  scale_fill_viridis_d(begin = 0.25, end = 0.75) + 
  theme(legend.title = element_blank(), 
        legend.position = c(0.95, 0.99), 
        legend.justification = c(0.95, 0.99)) + 
  scale_size_continuous(range = c(2,4), guide = FALSE) + 
  guides(fill = guide_legend(override.aes = list(size = c(4, 2)))) + 
  scale_y_continuous(breaks = seq(-0.25, 1.00, 0.25))

p1

ggsave("sbs_meta/meta_figs/meta_lme_coefs.pdf", height = 3.5, width = 5)

###### ONLY COMPLETE DATA ON THE WEST COAST #####

stat_dat <- df %>% 
  #filter(size_freq == "complete") %>% 
  #filter(study != "Sagarin") %>% 
  filter(study != "Fisher")

stat_dat <- stat_dat %>% 
  filter(!is.na(size1mm)) %>% 
  mutate(size_log = log(size1mm), 
         species2 = paste(species, fig_legend, sep = "_"), 
         lat_z = study_lat)

## Let intercepts vary by species nested within study
fit1 <- lme(size_log ~ era * lat_z, 
            random = ~ 1 | species / study,
            data = stat_dat)

## Let intercepts vary by species 
fit2 <- lme(size_log ~ era * lat_z,  
            random = ~ 1 | species2,
            data = stat_dat)

## Let intercepts vary by species, and slopes vary by era
fit3 <- lme(size_log ~ era * lat_z,  
            random = ~ era | species2,
            data = stat_dat)

AIC(fit1, fit2, fit3)

my_fit <- fit1
my_fit <- fit2
my_fit <- fit3

summary(my_fit)
plot(my_fit, resid(., type = "p") ~ fitted(.), abline = 0)

plot(my_fit, species2 ~ resid(.), abline = 0)
plot(my_fit, species2 ~ resid(.) | era, abline = 0)
plot(my_fit, species2 ~ resid(.) | museum, abline = 0)

plot(my_fit, size_log ~ fitted(.), abline = c(0, 1))
plot(my_fit, size_log ~ fitted(.) | species2, abline = c(0, 1))
plot(my_fit, size_log ~ fitted(.) | era, abline = c(0, 1))
plot(my_fit, size_log ~ fitted(.) | museum, abline = c(0, 1))

##### PLOT GLMM EFFECTS #####

summary(my_fit)

## All data
effects_table <- summary(my_fit)$tTable %>% tbl_df()

## Prepare table
effects_table <- effects_table %>% 
  mutate(upper = Value + Std.Error * 2, 
         lower = Value - Std.Error * 2, 
         Parameter = rownames(summary(fit1)$tTable), 
         Param = c("Intercept", "Era (present)")) %>%
  mutate(Param = factor(Param, levels = rev(c("Intercept", "Era (present)"))))

## Plot
theme_set(theme_bw(base_size = 14) + 
            theme(panel.grid.major.y = element_blank(), 
                  panel.grid.major.x = element_line(size = 0.25), 
                  panel.grid.minor = element_blank(), 
                  strip.background = element_blank()))

p1 <- effects_table %>% 
  filter(Param != "Intercept") %>% 
  ggplot(aes(Param, Value)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = my_dodge)) + 
  geom_point(aes(size = n), position = position_dodge(width = my_dodge), pch = 21) + 
  coord_flip() + 
  labs(x = "", y = "Model coefficient") + 
  scale_fill_viridis_d(begin = 0.25, end = 0.75) + 
  theme(legend.title = element_blank(), 
        legend.position = c(0.95, 0.99), 
        legend.justification = c(0.95, 0.99)) + 
  scale_size_continuous(range = c(2,4), guide = FALSE) + 
  guides(fill = guide_legend(override.aes = list(size = c(4, 2)))) + 
  scale_y_continuous(breaks = seq(-0.25, 1.00, 0.25))

p1

##### LMER AND SJPLOT #####

stat_dat <- df
stat_dat <- dfsub

stat_dat <- stat_dat %>% 
  filter(!is.na(size1mm)) %>% 
  mutate(size_log = log(size1mm), 
         species2 = paste(species, fig_legend, sep = "_"))

## Let intercepts vary by species nested within study
fit1 <- lmer(size_log ~ era * museum + (1 | species / study), 
             data = stat_dat)

## Let intercepts vary by species 
fit2 <- lmer(size_log ~ era * museum + (1 | species2), 
             data = stat_dat)

## Let intercepts vary by species, and slopes vary by era
fit3 <- lmer(size_log ~ era * museum + (era | species2), 
             data = stat_dat)

fit4 <- lmer(size_log ~ era * museum + (era | species / study), 
             data = stat_dat)

AIC(fit1, fit2, fit3, fit4)

my_fit <- fit3
summary(my_fit)

## These are the predicted coefficients, incorporating both fixed and random effects
coef(my_fit)$species2

coef(summary(my_fit))[ , "Estimate"]
confint(my_fit)
confint(my_fit, method = "Wald")

## Random effects (the adjustments to each from the estimated mean intercept and slope)
ranef(my_fit)$species2

colMeans(ranef(my_fit)$species2)

sjp.lmer(my_fit, type = "re", facet.grid = TRUE, sort.coef = "era")
sjp.lmer(my_fit, type = "fe", p.kr = FALSE)
