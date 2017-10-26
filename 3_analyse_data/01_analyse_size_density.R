################################################################################
##' @title Analyze size-density results
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-02-10
##' 
##' @log 
##' 2017-10-26 Updating size data
################################################################################

# rm(list=ls(all=TRUE)) 
library(broom)
library(ggplot2)

# Load data
source("05_summarise_size_density.R")

statDat <- datMeans4
# Subset data by species
dens_wara <- statDat %>% filter(sp == "CHFU")
dens_hex <- statDat %>% filter(sp == "LODI")
dens_childs <- statDat %>% filter(sp == "LIKE")

##### LM #####

## Wara
statDat <- dens_wara
lm1 <- lm(mass_log ~ era*dens_log, data = statDat)
summary(lm1)

glm1 <- glm(mass_log ~ era*dens_log, data = statDat, 
            family = gaussian(link = "identity"))
summary(glm1)

## Hexter
statDat <- dens_hex
lm1 <- lm(mass_log ~ era*dens_log, data = statDat)
summary(lm1)

## Childs
statDat <- dens_childs
lm1 <- lm(mass_log ~ era*dens_log, data = statDat)
summary(lm1)

## Get linear models for each species x era
statDat <- datMeans4

lm_fits <- statDat %>% group_by(species, era) %>% 
  do(fit = lm(mass_log ~ dens_log, data = .))
  
lm_fits %>% glance(fit)
lm_fits %>% tidy(fit) %>% filter(term == "dens_log")

## Get linear models for each species
statDat <- datMeans4

lm_fits <- statDat %>% group_by(species) %>% 
  do(fit = lm(mass_log ~ era*dens_log, data = .))

lm_fits %>% glance(fit)
lm_fits %>% tidy(fit) %>% filter(term != "(Intercept)")

## Only Chlorostoma is sig for present day data

##### SIZE DENSITY FROM FRANK 1975 - PRIOR FOR BAYES #####

frank <- read.csv("data/Frank_1975_table1.csv")
frank <- frank %>% 
  mutate(dens_log = log10(Density_m2), 
         mass_mean_mg = chfu_mmTOmg(Width_mm), 
         mass_log = log10(mass_mean_mg))

frank %>% 
  ggplot(aes(dens_log, mass_log, color = State)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(color = NULL))

lm1 <- lm(mass_log ~ dens_log, data = frank)
summary(lm1)


##### PREPARE DATA FOR JAGS #####
library(rjags)

statDat <- dens_wara %>% filter(!is.na(dens_log) & !is.na(mass_log))

# Get era as 0 or 1
statDat <- statDat %>% mutate(era01 = ifelse(era == "past", 0, 1))

# For plotting predicted values
x_min <- min(statDat$dens_log)
x_max <- max(statDat$dens_log)
x_predict <- seq(x_min, x_max, length.out = 100)

# Number of iterations
n.adapt <- 1000
n.update <- 1000
n.iter <- 1000

# Get data
data = list(
  N = nrow(statDat), 
  y = as.double(statDat$mass_log), # number of oasis detections
  era = as.double(statDat$era01), 
  x = as.double(statDat$dens_log),
  x_predict = as.double(x_predict) 
)

##### MODEL 1: ERA X SIZE ####

# JAGS model
sink("3_analyse_data/bayes_models/model1.R")
cat("
    model{
    # priors
    p ~ dbeta(1, 1)
    for(i in 1:2){
    beta[i] ~ dnorm(0, 0.1)
    }
    
    # likelihood
    for(i in 1:N){
    logit(psi[i]) <- beta[1] + beta[2]*hpd[i]
    z[i] ~ dbern(psi[i])
    y[i] ~ dbin(z[i] * p, n[i])
    y.new[i] ~ dbin(z[i] * p, n[i])
    }
    
    # bayesian p-values
    sd.data <- sd(y)
    sd.new <- sd(y.new)
    p.sd <- step(sd.new - sd.data)
    
    mean.data <- mean(y)
    mean.new  <- mean(y.new)
    p.mean <- step(mean.new - mean.data)
    
    # derived quantities
    for(j in 1:length(hpd_predict_stand)){
    logit(psi_predict[j]) <- beta[1] + beta[2]*hpd_predict_stand[j]
    }
    
    }
    ", fill = TRUE)
sink()

# # Inits
# inits = list(
#   list(p = 0.2,
#        beta = runif(2, -2, 2)),
#   list(p = 0.1,
#        beta = runif(2, -2, 2))
# )

inits = list(
  list(z = rep(1, nrow(statDat)), p = 0.1, beta = runif(2, -2, 2)), 
  list(z = rep(1, nrow(statDat)), p = 0.1, beta = runif(2, -2, 2)), 
  list(z = rep(1, nrow(statDat)), p = 0.1, beta = runif(2, -2, 2)), 
  list(z = rep(1, nrow(statDat)), p = 0.1, beta = runif(2, -2, 2))
)

# Number of iterations
n.adapt <- 10000
n.update <- 10000
n.iter <- 10000

jm <- jags.model("bv_test/models/model2.R", data = data, inits = inits, n.chains = length(inits), 
                 n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("p", "beta"), 
                  n.iter = n.iter, n.thin = 1)

zj = jags.samples(jm, variable.names = c("p", "beta", "psi_predict", "p.mean", "p.sd"), n.iter = n.iter, n.thin = 1)

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

##### FIGURES #####

# Plot predicted psi as function of human population density
psi <- summary(zj$psi_predict, quantile, c(0.025, 0.5, 0.975))$stat

png("bv_test/bv_test_figs/psi_predict.png", height = 3.5, width = 3.5, units = "in", res = 300)
par(mar = c(5,5,1,1))
plot(hpd_predict/1e6, psi[2, ], type = "l", ylim = c(0, 1), 
     xlab = "Human population density\n(x1e6; within 50 km)", 
     ylab = "Posterior probability\nof a coral oasis", lwd = 2)
points(statDat$hpd/1e6, jitter(statDat$oasis_present, factor = 0.1), col = "gray")
lines(hpd_predict/1e6, psi[1, ], type = "l", ylim = c(0, 1), lty = "dashed", lwd = 2)
lines(hpd_predict/1e6, psi[3, ], type = "l", ylim = c(0, 1), lty = "dashed", lwd = 2)
dev.off()

# Plot probability density of p (probability of oasis detection)
png("bv_test/bv_test_figs/p_histo.png", height = 3.5, width = 3.5, units = "in", res = 300)
par(mar = c(5,5,1,1))
hist(zj$p, breaks = 100, main = "", 
     xlab = "Probability of detection", 
     ylab = "Posterior density", 
     freq = FALSE, col = "azure1")
abline(v = summary(zj$p, quantile, c(.975))$stat, lty = "dashed", lwd = 2)
abline(v = summary(zj$p, quantile, c(.5))$stat, lty = "solid", lwd = 2)
abline(v = summary(zj$p, quantile, c(.025))$stat, lty = "dashed", lwd = 2)
box()
dev.off()

# Plot beta coefficients
beta_coefs <- summary(zj$beta, quantile, c(0.025, 0.5, 0.975))$stat
t(beta_coefs)

beta_df <- as.data.frame(t(beta_coefs))
names(beta_df) <- c("lower", "median", "upper")

beta_df <- beta_df %>% 
  mutate(beta = c("beta0_intercept", "beta1_hpd"))

beta_df %>%
  ggplot(aes(beta, median)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  ylab("Standardized coefficient") + 
  xlab("")

ggsave("bv_test/bv_test_figs/beta_coefs.png", height = 3.5, width = 3.5)









