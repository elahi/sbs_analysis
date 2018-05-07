################################################################################
##' @title Analyze log size - present data only
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2018-05-07
##' 
##' @log 2018-04-30: Model with density, tidal height, and 2-way interactions
################################################################################

##### PACKAGES, DATA #####
source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")
library(lme4)
library(rstan)
library(rstanarm)
library(ggplot2)
library(sjPlot)
library(bayesplot)
library(broom)

##### INSPECT DATA #####
##' x1 = era
##' x2 = density
##' x3 = tide height

## Plot raw data
statDat %>% 
  ggplot(aes(tideHTm, size_log)) + 
  geom_point() + 
  facet_grid(~ species, scales = "free") + 
  geom_smooth(method = "lm")

statDat %>% 
  ggplot(aes(density_m2, size_log, color = tideHTm)) + 
  geom_point(alpha = 0.2) + 
  facet_grid(~ species, scales = "free") + 
  geom_smooth(method = "lm")

statDat %>% 
  ggplot(aes(density_m2, size1mm, color = tideHTm)) + 
  geom_point(alpha = 0.2) + 
  facet_grid(~ species, scales = "free") + 
  geom_smooth(method = "lm")

## Get medians for heatmap
my_upper = 0.9
statDat_summary <- statDat %>% 
  filter(!is.na(size1mm)) %>% 
  group_by(species, sampleUnit, density_m2, tideHTm) %>% 
  summarise(size_median = median(size1mm), 
            size_median_log = log(size_median), 
            size_max = quantile(size1mm, probs = my_upper), 
            size_max_log = log(size_max), 
            n = n()) %>% 
  ungroup()

## Remove first 2 rows (Wara.B and Wara.D)
statDat_summary <- statDat_summary %>% slice(-c(1:2))
statDat_summary

statDat_summary %>% 
  ggplot(aes(density_m2, size_median, color = tideHTm)) +
  geom_point() + 
  facet_wrap(~ species, scales = "free_x") + 
  geom_smooth(method = "lm")

statDat_summary %>% 
  ggplot(aes(density_m2, size_median_log, color = tideHTm)) +
  geom_point() + 
  facet_wrap(~ species, scales = "free_x") + 
  geom_smooth(method = "lm")

statDat_summary %>% 
  ggplot(aes(density_m2, size_max, color = tideHTm, size = n)) +
  geom_point() + 
  facet_wrap(~ species, scales = "free_x") + 
  geom_smooth(method = "lm")

statDat_summary %>% 
  ggplot(aes(density_m2, size_max_log, color = tideHTm, size = n)) +
  geom_point() + 
  facet_wrap(~ species, scales = "free_x") + 
  geom_smooth(method = "lm")

#### PREP DATA ####

## My data
stat_dat <- childsDF %>% filter(era == "present") %>% 
  mutate(y = size_log)

## My quantile for size threshold
my_quantile <- 0

## Choose data
stat_dat <- truncate_data(stat_dat, era = "combined", quant = my_quantile, filter_data = TRUE)
stat_dat %>% count(sampleArea, era)

##### PREP DATA FOR REGRESSION ANALYSIS #####

## To use a prior t distribution for alpha and beta (Gelman et al. 2008)
## Binary inputs are shifted to have a mean of 0 and to differ by 1 in their lower and upper conditions
## Other inputs are shifted to have a mean of 0 and scaled to have a sd of 0.5
## "This scaling puts continuous variables on the same scale as symmetric binary inputs (which, taking on the values of +- 0.5, have sd = 0.5)

## Prepare dataset for stats
unique(stat_dat$sampleUnit)

stat_dat <- stat_dat %>% 
  mutate(x2z = scale_gelman(x2), 
         x3z = scale_gelman(x3), 
         group_j = as.integer(as.factor(sampleUnit)), 
         obs_id = seq(1:n()), 
         y = size_log) 

n_group_j = length(unique(stat_dat$group_j))

##### LMER #####

##' Pooled model
lm1 <- lm(y ~ x2z*x3z, data = stat_dat)
summary(lm1)
plot(lm1)

##' Partially pooled model (with random intercept for each group)
lmer1 <- lmer(y ~ x2z*x3z + (1 | group_j), data = stat_dat)
summary(lmer1)
sjp.lmer(lmer1, type = "re", p.kr = F)
sjp.lmer(lmer1, type = "fe", p.kr = F)
plot(lmer1)

##' Don't fit a model that has varying slope - because I don't expect that the effect of density should vary by quadrat (for simplicity)

##### RSTAN #####

CORES <- 4
SEED <- 101

## Pooled model
stan0 <- stan_lm(size_log ~ x2z*x3z, data = stat_dat, 
                 prior = R2(0.5, what = "median"),  
                 cores = CORES, seed = SEED, adapt_delta = 0.99)
plot(stan0, regex_pars = "x")

## Group level intercepts
stan1 <- stan_lmer(size_log ~ x2z*x3z + (1 | group_j), data = stat_dat, 
                   cores = CORES, seed = SEED, adapt_delta = 0.99) 
plot(stan1, regex_pars = "x")

##### SUMMARISE MCMC DRAWS #####

get_stan_summary <- function(fit, y_label, prob_lwr = .05, prob_upr = .95){
  
  ## Extract posterior draws as dataframe
  posterior_df <- as_data_frame(fit)
  names(posterior_df)[1] <- "Intercept"
  
  ## Get median and 90% credible intervals
  fit_summary <- posterior_df %>% 
    gather(key = parameter, value = estimate) %>% 
    group_by(parameter) %>% 
    summarise(median = median(estimate), 
              lower = quantile(estimate, probs = prob_lwr), 
              upper = quantile(estimate, probs = prob_upr)) %>% 
    mutate(y = y_label)
  
  return(fit_summary)
}

fit_summary_like <- get_stan_summary(stan0, y_label = "LIKE")

##### NEW DATA FOR PLOTTING #####

## For re-scaling
# x_scaled <- (x - x_mu)/(2*x_sd)
# Get means and sd of continuous variables
x2_mu <- mean(stat_dat$x2)
x2_sd <- sd(stat_dat$x2)
x3_mu <- mean(stat_dat$x3)
x3_sd <- sd(stat_dat$x3)

## Create new data for plotting relationship
x2_rng <- range(stat_dat$x2)
x3_rng <- range(stat_dat$x3)
x2z_rng <- range(stat_dat$x2z)
x3z_rng <- range(stat_dat$x3z)

x2_steps <- seq(x2_rng[1], x2_rng[2], length.out = 80)
x3_steps <- seq(x3_rng[1], x3_rng[2], length.out = 80)
x2z_steps <- seq(x2z_rng[1], x2z_rng[2], length.out = 80)
x3z_steps <- seq(x3z_rng[1], x3z_rng[2], length.out = 80)

##### LIKE - LMER predictions #####

fit <- lmer1

newdat <- expand.grid(x2z = 0, x3z = x3z_steps) 
newdat_x <- expand.grid(x2z = 0, x3z = x3_steps) 

newdat$x3 <- newdat_x$x3z
newdat$y <- predict(fit, newdat, re.form = NA)

mm <- model.matrix(terms(fit), newdat)

## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(fit), mm))
tvar1 <- pvar1 + VarCorr(fit)$group_j[1] ## must be adapted for more complex models

cmult <- 2 ## could use 1.96

newdat <- data.frame(
  newdat
  , plo = newdat$y - cmult*sqrt(pvar1)
  , phi = newdat$y + cmult*sqrt(pvar1)
  , tlo = newdat$y - cmult*sqrt(tvar1)
  , thi = newdat$y + cmult*sqrt(tvar1)
)

# plot confidence
g0 <- ggplot(newdat, aes(x = x3, y = y)) + geom_point()
g0
g0 + geom_pointrange(aes(ymin = plo, ymax = phi))+
  labs(title="CI based on fixed-effects uncertainty ONLY") + 
  geom_point(data = stat_dat, aes(tideHTm, size_log), color = "red")

#plot prediction
g0 + geom_pointrange(aes(ymin = tlo, ymax = thi))+
  labs(title="CI based on FE uncertainty + RE variance") + 
  geom_point(data = stat_dat, aes(tideHTm, size_log), color = "red")

##### LIKE - PREDICTIONS BY X3 (tidal height) #####

fit <- stan1
plot(fit)
stat_dat %>% count(x1)

## Hold x2z constant (density)

## Hold nation at Italy (i.e., x1 = 1)
new_data_x3 <- expand.grid(x2z = 0, x3z = x3_steps) 

## Combine
new_data <- rbind(new_data_x3) %>% 
  mutate(observation = seq_along(x2z))

## Get posterior predictions
pred_lin <- posterior_linpred(fit, newdata = new_data)

## Get tidy predictions in log-odds
df_pred_lin <- tidy_predictions(pred_lin, new_data) %>% 
  mutate(y = "Income")

## Convert to probabilities using plogis
## Re-scale x2 and x3
df_pred_lin <- df_pred_lin %>%
  mutate(x2raw = (x2 * 2 * x2_sd) + x2_mu, 
         x3raw = (x3 * 2 * x3_sd) + x3_mu) %>% 
  mutate_at(c("median", "lower", "upper"), plogis)

## Rename
df_pred_lin_income <- df_pred_lin

##### EFFECT - PREDICTIONS BY X1 (NATION) AND X2 (DISTANCE) #####

fit <- fit_effect

## Hold vessel size at mean value
new_data <- expand.grid(x1 = c(croatia_value, italy_value), x2 = x2_steps, x3 = 0) %>%
  mutate(Nation = ifelse(x1 == croatia_value, "Croatia", "Italy"))

## Combine
new_data <- new_data %>% mutate(observation = seq_along(x2))

## Get posterior predictions
pred_lin <- posterior_linpred(fit, newdata = new_data)

## Get tidy predictions in log-odds
df_pred_lin <- tidy_predictions(pred_lin, new_data) %>% 
  mutate(y = "Effect")

## Convert to probabilities using plogis
## Re-scale x2 and x3
df_pred_lin <- df_pred_lin %>%
  mutate(x2raw = (x2 * 2 * x2_sd) + x2_mu) %>% 
  mutate_at(c("median", "lower", "upper"), plogis)

## Rename
df_pred_lin_effect <- df_pred_lin

