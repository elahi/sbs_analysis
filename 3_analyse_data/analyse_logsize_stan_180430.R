################################################################################
##' @title Analyze log size
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2018-01-13
##' 
##' @log 2018-04-30: Model with density, tidal height, era, and 2-way interactions
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

##### SUBSET DATA #####
##' x1 = era
##' x2 = density
##' x3 = tide height

## My data
statDat <- childsDF
statDat <- hexDF
statDat <- waraDF

# # Subsample for Wara
# statDat <- waraDF %>% filter(sampleArea == "Wara.B" |
#                                era == "past" & sampleArea == "Wara.D")
# 
# pres_d <- waraDF %>% filter(sampleArea == "Wara.D" & era == "present") %>%
#   sample_n(size = 1444)
# 
# statDat <- rbind(statDat, pres_d)

## My quantile for size threshold
my_quantile <- 0.5

## Plot raw data with size threshold
statDat <- truncate_data(waraDF, era = "separate", quant = my_quantile, filter_data = FALSE)
statDat %>% 
  ggplot(aes(size1mm)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black") + 
  facet_grid(era ~ species) + 
  geom_vline(aes(xintercept = size_threshold), color = "red")

## Choose data
statDat <- truncate_data(childsDF, era = "separate_min", quant = my_quantile, filter_data = TRUE)
statDat %>% count(sampleArea, era)

##### PREP DATA FOR REGRESSION ANALYSIS #####

## To use a prior t distribution for alpha and beta (Gelman et al. 2008)
## Binary inputs are shifted to have a mean of 0 and to differ by 1 in their lower and upper conditions
## Other inputs are shifted to have a mean of 0 and scaled to have a sd of 0.5
## "This scaling puts continuous variables on the same scale as symmetric binary inputs (which, taking on the values of +- 0.5, have sd = 0.5)

total_n <- nrow(statDat)
past_n <- statDat %>% count(x1) %>% filter(x1 == 0) %>% select(n) %>% unlist(use.names = F)
past_prop <- past_n / total_n

## Prepare dataset for stats
statDat <- statDat %>% 
  mutate(#x1_z = ifelse(x1 == 0, -past_prop, 1 - past_prop), 
         x1z = ifelse(x1 == 0, -0.5, 0.5), 
         x2z = scale_gelman(x2), 
         x3z = scale_gelman(x3), 
         group_j = as.integer(as.factor(sampleArea)), 
         obs_id = seq(1:n())) 

n_group_j = length(unique(statDat$group_j))
unique(statDat$x1_z)

## Check gelman scaling
statDat %>% select(x1, x1z, x2z, x3z) %>% 
  summarise_if(is.numeric, mean) %>% t() %>% round(., 3)
statDat %>% select(x1, x1z, x2z, x3z) %>% 
  summarise_if(is.numeric, sd) %>% t() %>% round(., 3)

## Check histos
statDat %>% 
  ggplot(aes(size1mm, fill = era)) + 
  geom_density(alpha = 0.5)

statDat %>% 
  ggplot(aes(size_log, fill = era)) + 
  geom_density(alpha = 0.5)

statDat %>% 
  ggplot(aes(sampleArea, size_log, color = era)) + 
  geom_boxplot()

##### LMER #####

##' Pooled model
lm1 <- lm(size_log ~ era01*x2z + era01*x3z, data = statDat)
summary(lm1)
plot(lm1)

##' Partially pooled model (with random intercept for each group)
lmer1 <- lmer(size_log ~ era01*x2z + era01*x3z + (1 | group_j), data = statDat)
summary(lmer1)
sjp.lmer(lmer1, type = "re", p.kr = F)
sjp.lmer(lmer1, type = "fe", p.kr = F)

##' Partially pooled model (with random slope and intercept for each group)
lmer2 <- lmer(size_log ~ era01*x2z + era01*x3z + (era01 | group_j), data = statDat)
summary(lmer2)
coef(lmer2)
sjp.lmer(lmer2, type = "re", p.kr = F)
sjp.lmer(lmer2, type = "fe", p.kr = F)
plot(lmer2)

##### RSTAN #####

CORES <- 4
SEED <- 101

## Pooled model
stan0 <- stan_lm(size_log ~ era01*x2z + era01*x3z, data = statDat, 
                 prior = R2(0.5, what = "median"),  
                 cores = CORES, seed = SEED)
stan0
plot(stan0, regex_pars = c("era01", "x"))
pp_check(stan0, plotfun = "hist", nreps = 5)
pp_check(stan0, plotfun = "stat_2d", stat = c("mean", "sd"))

## LOO - model performance
loo_0 <- loo(stan0)
loo_0

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
x2_mu <- mean(statDat$x2)
x2_sd <- sd(statDat$x2)
x3_mu <- mean(statDat$x3)
x3_sd <- sd(statDat$x3)

## Create new data for plotting relationship
x2_rng <- range(statDat$x2)
x3_rng <- range(statDat$x3)

x1_steps <- c(0,1)
x2_steps <- seq(x2_rng[1], x2_rng[2], length.out = 80)
x3_steps <- seq(x3_rng[1], x3_rng[2], length.out = 80)

##### LIKE - PREDICTIONS BY X2 (DISTANCE) AND X3 (VESSEL SIZE) #####

fit <- stan_0
statDat %>% count(x1)
past_value <- unique(statDat$x1)[1]
present_value <- unique(statDat$x1)[2]

## Hold nation at Italy (i.e., x1 = 1)
new_data_x2 <- expand.grid(x1 = italy_value, x2 = x2_steps, x3 = 0) 

## Hold nation at Italy (i.e., x1 = 1)
new_data_x3 <- expand.grid(x1 = italy_value, x2 = 0, x3 = x3_steps) 

## Combine
new_data <- rbind(new_data_x2, new_data_x3) %>% 
  mutate(observation = seq_along(x2))

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

