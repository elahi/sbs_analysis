################################################################################
##' @title Analyze log size - LIKE
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2018-01-13
##' 
##' @log 
################################################################################

##' The goal of this script is to compare pooled vs hierarchical model for Littorina keenae
##' In this case, I will use 4 groups (which may be too few)

##### PACKAGES, DATA #####
source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")
library(lme4)
#library(rjags)
library(rstan)
library(rstanarm)
#library(brms)
library(ggplot2)
library(sjPlot)
library(bayesplot)
library(broom)

##### PREPARE DATA #####
##' x1 = era
##' x2 = density
##' x3 = tide height

## My data
statDat <- childsDF
statDat <- hexDF

# Subsample for Wara
statDat <- waraDF %>% filter(sampleArea == "Wara.B" | 
                               era == "past" & sampleArea == "Wara.D")

pres_d <- waraDF %>% filter(sampleArea == "Wara.D" & era == "present") %>% 
  sample_n(size = 1444)

statDat <- rbind(statDat, pres_d)

## My quantile for size threshold
my_quantile <- 0
statDat <- truncate_data(statDat, era = "past", quant = my_quantile)
statDat <- statDat %>% mutate(era01 = ifelse(era == "past", 0, 1))

## Create groups for multilevel model
unique(statDat$nest1)
unique(statDat$sampleArea)
statDat %>% count(sampleArea, era)

statDat <- statDat %>% mutate(group_j = as.integer(as.factor(sampleArea))) #!!! CHANGE
n_group_j = length(unique(statDat$group_j))

# ## My species
# my_species <- "LIKE"
# 
# ## My data type
# my_data <- "raw"

# Get means and sd of continuous variables
x2_mu <- mean(statDat$density_m2)
x2_sd <- sd(statDat$density_m2)
x3_mu <- mean(statDat$tideHTm)
x3_sd <- sd(statDat$tideHTm)

# Standardize continuous variables
statDat$x2z <- as.numeric(scale(statDat$density_m2))
statDat$x3z <- as.numeric(scale(statDat$tideHTm))

make_predict_vector <- function(my_vector, predict_length = 100){
  my_min <- min(my_vector)
  my_max <- max(my_vector)
  my_vector_pred <- seq(my_min, my_max, length.out = predict_length)
  return(my_vector_pred)
}

x2z_pred <- make_predict_vector(statDat$x2z, predict_length = 100)
x3z_pred <- make_predict_vector(statDat$x3z, predict_length = 100)

# For prediction
era_predict <- c(0,1)
pred_df <- expand.grid(x2z_pred, era_predict) %>% 
  rename(x2z = Var1, x1 = Var2) %>% tbl_df()
pred_df$x3z <- 0

statDat %>% 
  ggplot(aes(size1mm, fill = era)) + 
  geom_density(alpha = 0.5)

statDat %>% 
  ggplot(aes(size_log, fill = era)) + 
  geom_density(alpha = 0.5)

##### LMER #####

##' Pooled model
lm1 <- lm(size_log ~ era01, data = statDat)
summary(lm1)
plot(lm1)

##' Partially pooled model (with random intercept for each group)
lmer1 <- lmer(size_log ~ era01 + (1 | group_j), data = statDat)
summary(lmer1)
sjp.lmer(lmer1, type = "re", p.kr = F)
sjp.lmer(lmer1, type = "fe", p.kr = F)

##' Partially pooled model (with random intercept for each group)
lmer2 <- lmer(size_log ~ era01 + (era01 | group_j), data = statDat)
summary(lmer2)
coef(lmer2)
sjp.lmer(lmer2, type = "re", p.kr = F)
sjp.lmer(lmer2, type = "fe", p.kr = F)

##### RSTAN #####

CORES <- 4
SEED <- 101

## Pooled model
stan0 <- stan_lm(size_log ~ era01, data = statDat, 
                 prior = R2(0.5, what = "median"),  
                 cores = CORES, seed = SEED)
plot(stan0)
pp_check(stan0, plotfun = "hist", nreps = 5)
pp_check(stan0, plotfun = "stat_2d", stat = c("mean", "sd"))

## The coefficient
pred_change0 <- as.matrix(stan0, pars = "era01")

## Group-level intercepts
# Increase adapt_delta from 0.95 to 0.99 to prevent divergent transitions in this dataset
stan1 <- stan_lmer(size_log ~ era01 + (1 | group_j), data = statDat, 
                   cores = CORES, seed = SEED, adapt_delta = 0.99) 
plot(stan1)
pred_change1 <- as.matrix(stan1, pars = "era01")

## Group-level intercepts and slopes
stan2 <- stan_lmer(size_log ~ era01 + (era01 | group_j), data = statDat, 
                   cores = CORES, seed = SEED, adapt_delta = 0.99) 
plot(stan2)
pred_change2 <- as.matrix(stan2, pars = "era01")

## Combine pred_change
pred_df <- data.frame(pred_change0, pred_change1, pred_change2)
head(pred_df)
pred_long <- gather(pred_df) %>% mutate(value = value * 100)
head(pred_long)

pred_long_quantiles <- pred_long %>% 
  group_by(key) %>% 
  do(tidy(t(quantile(.$value, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))))) %>% 
  ungroup()

## Compare model performance
loo_0 <- loo(stan0)
loo_1 <- loo(stan1)
loo_2 <- loo(stan2)
compare_models(loo_0, loo_1, loo_2)

##### PLOT AND SAVE #####

# plq_like <- pred_long_quantiles %>% 
#   mutate(spp = "Littorina keenae")
# 
# plq_lodi <- pred_long_quantiles %>% 
#   mutate(spp = "Lottia digitalis")

plq_chfu <- pred_long_quantiles %>% 
  mutate(spp = "Chlorostoma funebralis")

plq <- rbind(plq_like, plq_lodi, plq_chfu)

write.csv(plq, "output/analyse_logsize_like_170113.csv")

plq %>% 
  ggplot(aes(key, X50.)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_errorbar(aes(ymin = X5., ymax = X95.), width = 0, size = 1) + 
  geom_errorbar(aes(ymin = X25., ymax = X75.), width = 0, size = 2) + 
  geom_point(size = 2, pch = 21, fill = "white") + 
  labs(x = "Model", 
       y = "Change in body size (%)") + 
  facet_wrap(~ spp)

ggsave("figs_ms/stan_models_like.pdf", height = 3.5, width = 7)
