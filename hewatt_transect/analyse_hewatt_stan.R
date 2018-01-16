################################################################################
##' @title Explore snail data
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2010-01-15
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####

library(dplyr)
options(dplyr.print_max = 100)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(lme4)
library(nlme)
library(rstan)
library(rstanarm)
library(ggplot2)
library(sjPlot)
library(bayesplot)
library(broom)


library(cowplot)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  panel.grid = element_blank()))

source("hewatt_transect/load_snail_size_data.R")

## Load all data for exploration
dat <- load_snail_size_data(remove_rare = FALSE)
str(dat)

##### PLOT ABUNDANCE #####

# ## Plot all snails by trophic level
# dat %>%
#   ggplot(aes(year, abundance + 1, color = trophic.level, 
#              shape = specialist)) +
#   geom_point(alpha = 0.5) +
#   geom_line(aes(group = quadrat), alpha = 0.5) +
#   facet_wrap(~ spp) +
#   scale_y_log10() +
#   geom_smooth(method = "lm", color = "black") + 
#   theme(legend.position = "bottom")
# 
# ggsave("figs/abund_year_raw.pdf", height = 10, width = 10)

##### REMOVE RARE SPECIES #####

# 19 quadrats per year, species must be present in > 5% of quads
n_quads <- length(unique(dat$quadrat))
n_yrs <- length(unique(dat$year))
quad_threshold <- 0.05 * n_quads * n_yrs # returns 13 species

# Get quad counts for each species
spp_quad_counts <- dat %>% 
  filter(abundance > 0) %>% 
  distinct(spp, year, quadrat) %>% 
  count(spp)

spp_keep <- spp_quad_counts %>% filter(n > quad_threshold)

dat2 <- dat[dat$spp %in% spp_keep$spp, ]
unique(dat2$spp)

# dat2 %>%
#   ggplot(aes(year, abundance + 1, color = trophic.level, 
#              shape = specialist)) +
#   geom_point(alpha = 0.5) +
#   geom_line(aes(group = quadrat), alpha = 0.5) +
#   facet_wrap(~ spp) +
#   scale_y_log10() +
#   geom_smooth(method = "lm", color = "black") + 
#   theme(legend.position = "bottom")
# 
# ggsave("figs/abund_year_raw_subset.pdf", height = 10, width = 10)

##### REMOVE QUADRATS #####

## Remove the quads where species were never observed
spp_quad_presence <- dat2 %>% 
  filter(abundance > 0) %>% 
  distinct(spp, quadrat_factor)

dat2 <- inner_join(spp_quad_presence, dat2, by = c("quadrat_factor", "spp"))
names(dat2)

##### PLOT LRR OVER TIME #####

dat2 <- dat2 %>% mutate(abund1 = abundance + 1)

dat_1931 <- dat2 %>% filter(year == 1931) %>% 
  mutate(abund_initial = abund1)

dat_modern <- dat2 %>% filter(year > 1931) %>% 
  mutate(abund_final = abund1)

datLRR <- dat_1931 %>% select(spp, quadrat, quadrat_factor, abund_initial) %>% 
  left_join(dat_modern, ., by = c("spp", "quadrat", "quadrat_factor")) %>% 
  mutate(lrr = log(abund_final/abund_initial))

## Plot lrr by quad over time
# datLRR %>%
#   ggplot(aes(year, lrr, color = trophic.level, 
#              shape = specialist)) +
#   geom_hline(yintercept = 0, linetype = "solid", color = 'black', size = 1) + 
#   geom_point(alpha = 0.5) +
#   geom_line(aes(group = quadrat), alpha = 0.5) +
#   facet_wrap(~ spp) +
#   #geom_smooth(method = "lm", color = "black") + 
#   theme(legend.position = "bottom") + 
#   scale_y_continuous(limits = c(-6.5, 6.5)) 
# 
# ggsave("figs/lrr_year_raw.pdf", height = 10, width = 10)

##### MODEL ALL SPECIES #####

##' LRR ~ year * size + (1|quadrat) + (1|species)
##' I want quadrat nested within species
##' (1|spp/quadrat)
##' (when i removed random effect of species the model sucked)

stat_dat <- datLRR
stat_dat %>% select(spp, year, quadrat, trophic.level, size1, lrr) %>% slice(1:100)

# Filter herbivores
stat_dat <- datLRR %>% filter(trophic.level == "Herbivore")

## Choose size metric and rename
stat_dat <- stat_dat %>% mutate(size_mm = size1, 
                      size_log = log(size_mm), 
                      year_z = as.numeric(scale(year, scale = TRUE)), 
                      size_z = as.numeric(scale(size_mm, scale = TRUE)))

stat_dat %>% distinct(trophic.level, spp)
stat_dat %>% distinct(spp, trophic.level) %>% count(trophic.level)

# fit1 <- lme(lrr ~ year_z * size_z, 
#             random = ~ year_z|spp/quadrat_factor, 
#             data = stat_dat, 
#             correlation = corCAR1())

## For herbivores only
## Intercept varies among species, and among quadrats within species
fit1 <- lme(lrr ~ year_z * size_z, 
            random = ~ 1|spp/quadrat_factor, 
            data = stat_dat, 
            correlation = corCAR1())


summary(fit1)
plot(fit1, resid(., type = "p") ~ fitted(.), abline = 0)
plot(fit1, quadrat_factor ~ resid(.) | spp, abline = 0)
plot(fit1, spp ~ resid(.), abline = 0)
plot(fit1, lrr ~ fitted(.), abline = c(0, 1))
plot(fit1, lrr ~ fitted(.) | quadrat_factor, abline = c(0, 1))
plot(fit1, lrr ~ fitted(.) | spp, abline = c(0, 1))

##### PLOT SIZE EFFECT #####
#https://stackoverflow.com/questions/14358811/extract-prediction-band-from-lme-fit

# newdat <- expand.grid(year_z = seq(from = min(stat_dat$year_z), 
#                           to = max(stat_dat$year_z), length.out = 100), 
#                       size_z = seq(from = min(stat_dat$size_z), 
#                           to = max(stat_dat$size_z), length.out = 100))
                      
newdat <- expand.grid(year_z = 0, 
                      size_z = seq(from = min(stat_dat$size_z), 
                                   to = max(stat_dat$size_z), length.out = 100))

size_mean <- mean(stat_dat$size_mm)
size_sd <- sd(stat_dat$size_mm)

newdat <- newdat %>% 
  mutate(lrr = predict(fit1, level = 0, newdata = newdat), 
         size1 = (size_z * size_sd) + size_mean)

#create design matrix
Designmat <- model.matrix(eval(eval(fit1$call$fixed)[-2]), newdat[-ncol(newdat)])

#compute standard error for predictions
predvar <- diag(Designmat %*% fit1$varFix %*% t(Designmat))
newdat$SE <- sqrt(predvar) 
newdat$SE2 <- sqrt(predvar + fit1$sigma^2)

## Jitter Tegula points
stat_dat %>% distinct(spp, size1) %>% arrange(size1)
stat_datJ <- stat_dat %>% mutate(size1 = ifelse(spp == "Tegula brunnea", size1 + 1, size1))

p1 <- stat_datJ %>% 
  filter(trophic.level == "Herbivore") %>% 
  ggplot(aes(size1, lrr)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = 'gray') + 
  geom_jitter(alpha = 0.1, width = 0) + 
  theme(legend.position = "none") +
  geom_ribbon(data = newdat, inherit.aes = F,
              aes(size1, ymin = lrr-2*SE2, ymax = lrr+2*SE2), alpha=0.2, fill="red") +
  geom_ribbon(data = newdat, inherit.aes = F, 
              aes(size1, ymin = lrr-2*SE, ymax = lrr+2*SE), alpha=0.2, fill="blue") + 
  geom_line(data = newdat, inherit.aes = F, 
            aes(size1, lrr), alpha = 1, color = "black") + 
  labs(x = "Size (mm)", 
       y = "Log change in abundance")

ggsave("figs/lrr_herb_size_glmm.pdf", height = 3.5, width = 3.5)

##### STAN #####

tidy_predictions <- function(mat_pred, df_data, obs_name = "observation",
                             prob_lwr = .025, prob_upr = .975) {
  # Get data-frame with one row per fitted value per posterior sample
  df_pred <- mat_pred %>% 
    as_data_frame %>% 
    setNames(seq_len(ncol(.))) %>% 
    tibble::rownames_to_column("posterior_sample") %>% 
    tidyr::gather_(obs_name, "fitted", setdiff(names(.), "posterior_sample"))
  df_pred
  
  # Helps with joining later
  class(df_pred[[obs_name]]) <- class(df_data[[obs_name]])
  
  # Summarise prediction interval for each observation
  df_pred %>% 
    group_by_(obs_name) %>% 
    summarise(median = median(fitted),
              lower = quantile(fitted, prob_lwr), 
              upper = quantile(fitted, prob_upr)) %>% 
    left_join(df_data, by = obs_name)
}

make_predict_vector <- function(my_vector, predict_length = 100){
  my_min <- min(my_vector)
  my_max <- max(my_vector)
  my_vector_pred <- seq(my_min, my_max, length.out = predict_length)
  return(my_vector_pred)
}

## Predictive dataframe
size_z_pred <- make_predict_vector(stat_dat$size_z, predict_length = 100)
year_z_pred <- c(0)
spp_pred <- unique(stat_dat$spp)
quadrat_factor_pred <- unique(stat_dat$quadrat_factor)

pred_df <- expand.grid(size_z_pred, year_z_pred) %>% 
  rename(size_z = Var1, year_z = Var2) %>% tbl_df() %>% 
  mutate(observation = seq_along(size_z))

pred_df <- expand.grid(size_z_pred, year_z_pred, spp_pred, quadrat_factor_pred) %>%
  rename(size_z = Var1, year_z = Var2,
         spp = Var3, quadrat_factor = Var4) %>% tbl_df() %>% 
  mutate(observation = seq_along(size_z))


CORES <- 4
SEED <- 101

stat_dat

## Intercept varies among species, and among quadrats within species
stan1 <- stan_lmer(lrr ~ year_z * size_z + (1|spp/quadrat_factor), 
              data = stat_dat, cores = CORES, seed = SEED)
summary(stan1)
plot(stan1, pars = c("(Intercept)", "size_z", "year_z", "year_z:size_z"))

## Make predictions
pred_lin <- posterior_linpred(stan1, newdata = pred_df)
pred_lin <- posterior_linpred(stan1, newdata = pred_df, re.form = NA)
str(pred_lin)

df_pred_lin <- tidy_predictions(pred_lin, pred_df)

df_pred_lin <- df_pred_lin %>% 
  mutate(spp_quad = paste(spp, quadrat_factor, sep = "-"))

ggplot(stat_dat) + 
  aes(x = size_z) + 
  geom_line(aes(y = median, group = spp_quad), 
            data = df_pred_lin, colour = "#3366FF", size = 1, alpha = 0.1) + 
  geom_line(aes(y = upper, group = spp_quad), 
            data = df_pred_lin, colour = "#3366FF", size = 1, alpha = 0.1) + 
  geom_line(aes(y = lower, group = spp_quad), 
            data = df_pred_lin, colour = "#3366FF", size = 1, alpha = 0.1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, group = spp_quad), 
              data = df_pred_lin, 
              alpha = 0.4, fill = "grey60") + 
  geom_point(aes(y = lrr)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")




pred_lin_quantiles <- pred_lin %>% 
  as_data_frame() %>% 
  gather() %>% 
  group_by(key) %>% 
  do(tidy(t(quantile(.$value, probs = c(0.025, 0.5, 0.975))))) %>% 
  ungroup()

pred_lin_quantiles <- pred_lin_quantiles %>% 
  mutate(key = as.numeric(key)) %>% 
  arrange(key) %>% 
  mutate(size_z = pred_df$size_z)

pred_lin_quantiles %>% 
  ggplot(aes(size_z, X50.)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = X5., ymax = X95.), 
                          alpha = 0.4, fill = "grey60") + 
  geom_point(aes(size_z, lrr), data = stat_dat)


plot(stat_dat$size_mm, stan1_linpred)

pred_post <- posterior_predict(stan1, newdata = pred_df, re.form = NA)
df_pred_post <- tidy_predictions(pred_post, pred_df)

ggplot(stat_dat) + 
  aes(x = size_z) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), data = df_pred_lin, 
              alpha = 0.4, fill = "grey20") + 
  geom_ribbon(aes(ymin = lower, ymax = upper), data = df_pred_post, 
              alpha = 0.4, fill = "grey60") + 
  geom_line(aes(y = median), data = df_pred_post, colour = "#3366FF", size = 1) + 
  geom_point(aes(y = lrr)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")


stan2 <- stan_lmer(lrr ~ year_z * size_z + (year_z|spp/quadrat_factor), 
             data = stat_dat, cores = CORES, seed = SEED)

##### PLOT GLMM EFFECTS #####

summary(fit1)
library(broom)

effects_table <- summary(fit1)$tTable %>% 
  tbl_df() %>% 
  mutate(upper = Value + Std.Error * 2, 
         lower = Value - Std.Error * 2, 
         Parameter = rownames(summary(fit1)$tTable), 
         Param = c("Intercept", "Year", "Size", "Size x Year")) %>%
  mutate(Param = factor(Param, levels = rev(c("Intercept", "Size", "Year", "Size x Year"))))

p2 <- effects_table %>% 
  ggplot(aes(Param, Value)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  coord_flip() + 
  labs(x = "", y = "Standardized coefficient")

ggsave("figs/lrr_herb_size_glmm_effects.pdf", height = 3.5, width = 3.5)

## plot two panels
plot2 <- plot_grid(p2, p1, labels = c("A", "B"), ncol = 2)
save_plot("figs_ms/lrr_herb_size_glmm_2panel.pdf", plot = plot2,
          ncol = 2, nrow = 1, base_height = 3.5, 
          base_aspect_ratio = 1.1)

## Plot the raw data that went into the analysis

dat2 %>%
  filter(trophic.level == "Herbivore") %>% 
  ggplot(aes(year, abundance + 1)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = quadrat), alpha = 0.5) +
  facet_wrap(~ spp, ncol = 3) +
  scale_y_log10() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Abundance")

ggsave("figs/abund_year_raw_subset_herb.pdf", height = 9, width = 7)

##### PLOT YEAR EFFECT #####
#https://stackoverflow.com/questions/14358811/extract-prediction-band-from-lme-fit

# newdat <- expand.grid(year_z = seq(from = min(stat_dat$year_z), 
#                           to = max(stat_dat$year_z), length.out = 100), 
#                       size_z = seq(from = min(stat_dat$size_z), 
#                           to = max(stat_dat$size_z), length.out = 100))

newdat <- expand.grid(size_z = 0, 
                      year_z = seq(from = min(stat_dat$year_z), 
                                   to = max(stat_dat$year_z), length.out = 100))

size_mean <- mean(stat_dat$size_mm)
size_sd <- sd(stat_dat$size_mm)
year_mean <- mean(stat_dat$year)
year_sd <- sd(stat_dat$year)

newdat <- newdat %>% 
  mutate(lrr = predict(fit1, level = 0, newdata = newdat), 
         year = (year_z * year_sd) + year_mean)

#create design matrix
Designmat <- model.matrix(eval(eval(fit1$call$fixed)[-2]), newdat[-ncol(newdat)])

#compute standard error for predictions
predvar <- diag(Designmat %*% fit1$varFix %*% t(Designmat))
newdat$SE <- sqrt(predvar) 
newdat$SE2 <- sqrt(predvar + fit1$sigma^2)

summary(newdat)

stat_dat %>% 
  #filter(trophic.level == "Herbivore") %>% 
  ggplot(aes(year, lrr, color = spp, group = quadrat_factor)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = 'gray') + 
  geom_point(alpha = 0.5) + 
  geom_line(aes(group = quadrat_factor)) + 
  theme(legend.position = "none") +
  geom_ribbon(data = newdat, inherit.aes = F,
              aes(year, ymin = lrr-2*SE2, ymax = lrr+2*SE2), alpha=0.2, fill="red") +
  geom_ribbon(data = newdat, inherit.aes = F, 
              aes(year, ymin = lrr-2*SE, ymax = lrr+2*SE), alpha=0.2, fill="blue") + 
  geom_line(data = newdat, inherit.aes = F, 
            aes(year, lrr), alpha = 1, color = "black") + 
  labs(x = "Size (mm)", 
       y = "Log change in abundance")

##### SUMMARISE LRR BY QUADRAT #####

lrr_df_quad <- datLRR %>% 
  group_by(spp, quadrat, trophic.level, specialist, size1) %>% 
  summarise(lrr_mean = mean(lrr), 
            lrr_sd = sd(lrr), 
            lrr_n = n()) %>% 
  ungroup()

## Plot lrr by species
lrr_df_quad %>%
  ggplot(aes(spp, lrr_mean, color = trophic.level, 
             shape = specialist)) +
  geom_hline(yintercept = 0, linetype = "solid", color = 'black', size = 1) + 
  geom_boxplot(alpha = 0.5) +
  theme(legend.position = "bottom") + 
  coord_flip()

##' LRR ~ size + (year|species)

stat_dat <- lrr_df_quad

# Filter herbivores
stat_dat <- stat_dat %>% filter(trophic.level == "Herbivore")

## Choose size metric and rename
stat_dat
stat_dat <- stat_dat %>% mutate(size_mm = size1, 
                                size_log = log10(size_mm), 
                                #year_z = as.numeric(scale(year, scale = TRUE)), 
                                size_z = as.numeric(scale(size_log, scale = TRUE)), 
                                quadrat_factor = as.factor(paste("q_", quadrat, sep = "")))

fit1 <- lme(lrr_mean ~ size_z, 
            random = ~ 1|spp/quadrat_factor, 
            data = stat_dat)

summary(fit1)
plot(fit1, resid(., type = "p") ~ fitted(.), abline = 0)
plot(fit1, spp ~ resid(.), abline = 0)
plot(fit1, lrr_mean ~ fitted(.), abline = c(0, 1))
plot(fit1, lrr_mean ~ fitted(.) | spp, abline = c(0, 1))





##### SUMMARISE LRR BY YEAR #####

lrr_df_yr <- datLRR %>% 
  group_by(spp, year, trophic.level, specialist, size1) %>% 
  summarise(lrr_mean = mean(lrr), 
            lrr_sd = sd(lrr), 
            lrr_n = n()) %>% 
  ungroup()

lrr_df_yr

## Plot lrr over time
lrr_df_yr %>%
  ggplot(aes(year, lrr_mean, color = trophic.level, 
             shape = specialist)) +
  geom_hline(yintercept = 0, linetype = "solid", color = 'black', size = 1) + 
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ spp) +
  #geom_smooth(method = "lm", color = "black") + 
  theme(legend.position = "bottom")


##' LRR ~ year * size + (year|species)

stat_dat <- lrr_df_yr

# Filter herbivores
stat_dat <- stat_dat %>% filter(trophic.level == "Herbivore")

## Choose size metric and rename
stat_dat
stat_dat <- stat_dat %>% mutate(size_mm = size1, 
                                size_log = log10(size_mm), 
                                year_z = as.numeric(scale(year, scale = TRUE)), 
                                size_z = as.numeric(scale(size_log, scale = TRUE)))

fit1 <- lme(lrr_mean ~ year_z * size_z, 
            random = ~ 1|spp, 
            data = stat_dat, 
            correlation = corCAR1())

fit2 <- lme(lrr_mean ~ year_z * size_z, 
            random = ~ year_z|spp, 
            data = stat_dat, 
            correlation = corCAR1())

AIC(fit1, fit2)

summary(fit1)
plot(fit1, resid(., type = "p") ~ fitted(.), abline = 0)
plot(fit1, spp ~ resid(.), abline = 0)
plot(fit1, lrr_mean ~ fitted(.), abline = c(0, 1))
plot(fit1, lrr_mean ~ fitted(.) | spp, abline = c(0, 1))


