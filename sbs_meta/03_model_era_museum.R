#################################################
# Author: Robin Elahi
# Date: 180929
# Analysis of historical and modern gastropod body sizes
# LMER for era x museum
#################################################

source("sbs_meta/01_assemble_raw_data.R")
library(nlme)
library(broom)

theme_set(theme_bw(base_size = 14) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()))

library(viridis)

###### ALL DATA #####

stat_dat <- df
stat_dat <- dfsub

stat_dat <- stat_dat %>% 
  filter(!is.na(size1mm)) %>% 
  mutate(size_log = log(size1mm), 
         species2 = paste(species, fig_legend, sep = "_"))

## Let intercepts vary by species nested within study
fit1 <- lme(size_log ~ era * museum,
            random = ~ 1 | species / study,
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
        legend.position = c(0.95, 0.95), 
        legend.justification = c(0.95, 0.95)) + 
  scale_size_continuous(range = c(2,4), guide = FALSE) + 
  guides(fill = guide_legend(override.aes = list(size = c(4, 2)))) 

p1

ggsave("sbs_meta/meta_figs/meta_lme_coefs.pdf", height = 3.5, width = 5)


