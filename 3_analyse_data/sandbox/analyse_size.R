################################################################################
##' @title Analyze size raw
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-10-26
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

##### PACKAGES, DATA #####

library(broom)
library(ggplot2)
library(cowplot)

# Load data
source("2_summarise_data/summarise_size_density.R")
source("R/HighstatLibV6.R")

statDat <- dat_dens
# Subset data by species
dens_wara <- statDat %>% filter(sp == "CHFU")
dens_hex <- statDat %>% filter(sp == "LODI")
dens_childs <- statDat %>% filter(sp == "LIKE")

dens_wara %>% select(density_m2, tideHTm) %>% Mypairs()
dens_hex %>% select(density_m2, tideHTm) %>% Mypairs()
dens_childs %>% select(density_m2, tideHTm) %>% Mypairs()

##### PREPARE DATA FOR JAGS #####
library(rjags)

# Choose data to models
statDat <- dens_childs %>% filter(!is.na(dens_log) & !is.na(mass_log)) 
#statDat <- dens_wara %>% filter(!is.na(dens_log) & !is.na(mass_log))
#statDat <- dens_hex %>% filter(!is.na(dens_log) & !is.na(mass_log))

# Get era as 0 or 1
statDat <- statDat %>% mutate(era01 = ifelse(era == "past", 0, 1))

# Center and standardize
dens_mu <- mean(statDat$density_m2)
dens_sd <- sd(statDat$density_m2)
dens_cent <- statDat$density_m2 - dens_mu
statDat$dens_stand <- dens_cent/dens_sd
statDat$tideHTm_stand <- as.numeric(scale(statDat$tideHTm))

# For plotting predicted values
x_min <- min(statDat$density_m2)
x_max <- max(statDat$density_m2)
x_predict <- seq(x_min, x_max, length.out = 1000)
x_predict_stand <- (x_predict - dens_mu)/dens_sd

# For prediction
era_predict <- c(0,1)
pred_df <- expand.grid(x_predict_stand, era_predict) %>% 
  rename(x_predict_stand = Var1, era_predict = Var2) %>% tbl_df()
pred_df$x_predict <- x_predict
pred_df$tideHTm_stand <- 0

# Get data
data = list(
  N = nrow(statDat), 
  y = as.double(statDat$size1mm), # number of oasis detections
  era = as.double(statDat$era01), 
  x = as.double(statDat$dens_stand),
  tideHTm = as.double(statDat$tideHTm_stand), 
  x_predict = as.double(pred_df$x_predict_stand), 
  era_predict = as.double(pred_df$era_predict), 
  tide_predict = as.double(pred_df$tideHTm_stand)
)

##### ERA + SIZE + DENSITY + TIDE ####

#' size ~ b0 + b1*era 

# JAGS model
sink("3_analyse_data/bayes_models/model_size.R")
cat("
    model{
    # priors
    beta0 ~ dnorm(0, 1/10^2)
    beta1 ~ dnorm(0, 1/10^2) 
    sigma ~ dunif(0, 5)
    
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:N){
    mu[i] <- exp(beta0 + beta1*era[i])
    y[i] ~ dlnorm(log(mu[i]), tau) 
    y.new[i] ~ dlnorm(log(mu[i]), tau)
    sq.error.data[i] <- (y[i] - mu[i])^2
    sq.error.new[i] <- (y.new[i] - mu[i])^2
    }
    
    # bayesian p-values
    sd.data <- sd(y)
    sd.new <- sd(y.new)
    p.sd <- step(sd.new - sd.data)
    
    mean.data <- mean(y)
    mean.new  <- mean(y.new)
    p.mean <- step(mean.new - mean.data)
    
    discrep.data <- sum(sq.error.data)
    discrep.new <- sum(sq.error.new)
    p.discrep <- step(discrep.new - discrep.data)
    
    # Derived quantities
    for(j in 1:length(x_predict)){
    y_pred[j] <- exp(beta0 + beta1*era_predict[j])
    y_new[j] ~ dlnorm(log(y_pred[j]), tau)
    }
    
    }
    ", fill = TRUE)
sink()

inits = list(
  #list(beta0 = 1, beta1 = 0.5, beta2 = 0.1, beta3 = 1, sigma = 1), 
  #list(beta0 = -1, beta1 = -0.5, beta2 = -0.1, beta3 = 0, sigma = 0.2), 
  #list(beta0 = 2, beta1 = 0.1, beta2 = 0, beta3 = -0.01, sigma = 10), 
  list(beta0 = 0, beta1 = -0.1, beta2 = 0.4, beta3 = -4, sigma = 4))

# Number of iterations
n.adapt <- 1000
n.update <- 1000
n.iter <- 1000

## Run model
jm <- jags.model("3_analyse_data/bayes_models/model_size.R", 
                 data = data, 
                 inits = inits, n.chains = length(inits), 
                 n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("beta0", "beta1","sigma"), 
                  n.iter = n.iter, n.thin = 10)

zj = jags.samples(jm, variable.names = c("y_pred", "y_new", 
                                         "p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 10)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # intercept

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)
mean(zj$p.discrep)

##### FIGURES #####

# Credible intervals
y_predict <- summary(zj$y_pred, quantile, c(0.025, 0.5, 0.975))$stat
pred_df$y_median <- y_predict[2, ]
pred_df$y_lower <- y_predict[1, ]
pred_df$y_upper <- y_predict[3, ]

# Prediction intervals
y_new <- summary(zj$y_new, quantile, c(0.025, 0.5, 0.975))$stat
pred_df$y_median_pred <- y_new[2, ]
pred_df$y_lower_pred <- y_new[1, ]
pred_df$y_upper_pred <- y_new[3, ]

pred_df$era <- ifelse(pred_df$era_predict == 0, "past", "present")

with(pred_df, plot(x_predict, y_median))


pred_df %>% 
  ggplot(aes(era, y_median)) + 
  geom_point() + 
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper, fill = era, color = NULL), 
              alpha = 0.5) + 
  geom_ribbon(aes(ymin = y_lower_pred, ymax = y_upper_pred, fill = era, color = NULL), 
              alpha = 0.25) +
  geom_point(data = statDat, aes(density_m2, size1mm, color = era), alpha = 0.5) + 
  #geom_boxplot(data = statDat, aes(density_m2, size1mm, group = density_m2)) + 
  ggtitle("Bayesian model") + 
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01))
