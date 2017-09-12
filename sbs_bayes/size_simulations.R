
library(broom)
library(dplyr)
library(tidyr)
library(ggplot2)

median_change <- function(dat){
  dat_summary <- dat %>% group_by(era) %>% 
    summarise(size_median = median(size1mm, na.rm = TRUE))
  test_stat <- 1 - (dat_summary$size_median[2]/dat_summary$size_median[1])
  return(test_stat)
}

# Function to truncate data
truncate_data <- function(dat, quant = 0.5, subtract_value = 1){
  
  my_quantile = quant
  size_threshold <- dat %>% filter(era == "past") %>% 
    summarise(size_threshold = quantile(size1mm, probs = my_quantile)) %>% unlist(use.names = FALSE)
  dat$size_threshold <- size_threshold
  
  dat <- dat %>% filter(size1mm >= size_threshold)
  
  # Need to subtract minimum size so that I can use a distribution appropriately
  dat <- dat %>%
    mutate(size_min = min(size1mm) - subtract_value, 
           size_centered = size1mm - size_min)
  
  return(dat)
  
}

# For prediction
era_predict <- c(0,1)

##### SIMULATE DATA #####

## 
past_mu <- 14
present_mu <- 10
past_sigma <- 1.25
present_sigma <- 1.25
pastN <- 300
presentN <- 200

## Add individuals that are larger than the past mean
present_mu2 <- 20
present_sigma2 <- 1.25
presentN2 <- 100

set.seed(151)
past <- rlnorm(pastN, log(past_mu), log(past_sigma))
present <- rlnorm(presentN, log(present_mu), log(present_sigma))
present2 <- rlnorm(presentN2, log(present_mu2), log(present_sigma2))
present <- c(present, present2)

dat <- data.frame(past = past, present = present) %>% tbl_df() %>% 
  gather(key = era, value = size1mm) %>% 
  mutate(eraJ = ifelse(era == "past", 0, 1))

dat %>% 
  ggplot(aes(size1mm, fill = era)) + geom_density(alpha = 0.5)

dat %>%
  group_by(era) %>%
  do(tidy(t(quantile(.$size1mm)))) %>% ungroup()

dat %>% group_by(era) %>% summarise(mean(size1mm))


##### MODEL DATA - LOGNORMAL - MEDIAN #####

# load jags
library(rjags)

## Get data
data = list(
  y = as.double(dat$size1mm),
  era = as.double(dat$eraJ), 
  k = as.double(length(dat$size1mm))
)

## Iterations
n.adapt = 1000
n.update = 1000

inits = list(
  list(
    alpha = runif(1, 0, 30), 
    beta = runif(1, -10, 10), 
    sigma = 1), 
  list(
    alpha = runif(1, 0, 30), 
    beta = runif(1, -10, 10), 
    sigma = 1))


## JAGS model
sink("sbs_bayes/models/model_lognormal.R")
cat(" 
    model{
    # priors
    alpha ~ dnorm(0, 1/100^2) 
    beta ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 100)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:k){
    mu[i] <- exp(alpha + beta * era[i])
    #y[i] ~ dlnorm(log(mu[i]) - 0.67*sigma, tau)
    #y.new[i] ~ dlnorm(log(mu[i]) - 0.67*sigma, tau)
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
    
    }
    ", fill = TRUE)
sink()
  
jm = jags.model("sbs_bayes/models/model_lognormal.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd"), 
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

# Compare with median values
dat %>% group_by(era) %>% summarise(median(size1mm))
median_change(dat)

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)

# Compared observed vs simulated
hist(dat$size1mm, breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary
coda_summary <- summary(zm)
summary_name <- "1-lognormal-median"
coda_quantile_1 <- data.frame(coda_summary$quantile) %>% 
  mutate(coda_quantile = summary_name, 
         param = rownames(coda_summary$quantile))



##### MODEL DATA - NORMAL - P METHOD - MEDIAN #####

dat$p <- 0.5

# load jags
library(rjags)

## Get data
data = list(
  y = as.double(log(dat$size1mm)),
  era = as.double(dat$eraJ), 
  k = as.double(length(dat$size1mm)), 
  p = as.double(dat$p[1])
)

## Iterations
n.adapt = 1000
n.update = 1000

inits = list(
  list(
    alpha = runif(1, 0, 5), 
    beta = runif(1, -10, 10), 
    sigma = 1), 
  list(
    alpha = runif(1, 0, 5), 
    beta = runif(1, -10, 10), 
    sigma = 1))


## JAGS model
sink("sbs_bayes/models/model_normal_quantile.R")
cat(" 
    model{
    # priors
    alpha ~ dnorm(0, 1/5^2) 
    beta ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 100)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:k){
    mu[i] <- alpha + beta * era[i]
    
    w[i]  ~ dexp(tau) # exponential distribution
    me[i] <- (1 - 2 * p) / (p * (1 - p)) * w[i] + mu[i] # if p = 0.5, then this just leaves mu[i]
    pe[i] <- (p * (1 - p) * tau) / (2 * w[i]) # 
    #y[i]  ~ dnorm(me[i], pe[i])
    
    y[i] ~ dnorm(me[i], pe[i])
    
    y.new[i] ~ dnorm(me[i], pe[i])
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
    
    }
    ", fill = TRUE)
sink()

jm = jags.model("sbs_bayes/models/model_normal_quantile.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd"), 
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

# Compare with median values
dat %>% group_by(era) %>% summarise(median(size1mm))
median_change(dat)

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)

# Compared observed vs simulated
hist(log(dat$size1mm), breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary
coda_summary <- summary(zm)
summary_name <- "2-normal-p"
coda_quantile_2 <- data.frame(coda_summary$quantile) %>% 
  mutate(coda_quantile = summary_name, 
         param = rownames(coda_summary$quantile))

##### MODEL DATA - NORMAL - MEDIAN #####

# load jags
library(rjags)

## Get data
data = list(
  y = as.double(log(dat$size1mm)),
  era = as.double(dat$eraJ), 
  k = as.double(length(dat$size1mm))
)

## Iterations
n.adapt = 1000
n.update = 1000

inits = list(
  list(
    alpha = runif(1, 0, 5), 
    beta = runif(1, -10, 10), 
    sigma = 1), 
  list(
    alpha = runif(1, 0, 5), 
    beta = runif(1, -10, 10), 
    sigma = 1))


## JAGS model
sink("sbs_bayes/models/model_normal.R")
cat(" 
    model{
    # priors
    alpha ~ dnorm(0, 1/5^2) 
    beta ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 100)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:k){
    mu[i] <- alpha + beta * era[i]
    y[i] ~ dnorm(mu[i], tau)
    y.new[i] ~ dnorm(mu[i], tau)      
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
    
    }
    ", fill = TRUE)
sink()

jm = jags.model("sbs_bayes/models/model_normal.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd"), 
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

# Compare with median values
dat %>% group_by(era) %>% summarise(median(size1mm))
median_change(dat)

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)

# Compared observed vs simulated
hist(log(dat$size1mm), breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary
coda_summary <- summary(zm)
summary_name <- "3-normal-median"
coda_quantile_3 <- data.frame(coda_summary$quantile) %>% 
  mutate(coda_quantile = summary_name, 
         param = rownames(coda_summary$quantile))


##### MODEL DATA - LOGNORMAL - MEDIAN #####

# load jags
library(rjags)

## Get data
data = list(
  y = as.double(dat$size1mm),
  era = as.double(dat$eraJ), 
  k = as.double(length(dat$size1mm))
)

## Iterations
n.adapt = 1000
n.update = 1000

inits = list(
  list(
    alpha = runif(1, 0, 30), 
    beta = runif(1, -10, 10), 
    sigma = 1), 
  list(
    alpha = runif(1, 0, 30), 
    beta = runif(1, -10, 10), 
    sigma = 1))


## JAGS model
sink("sbs_bayes/models/model_lognormal.R")
cat(" 
    model{
    # priors
    alpha ~ dnorm(0, 1/100^2) 
    beta ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 100)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:k){
    mu[i] <- exp(alpha + beta * era[i])
    y[i] ~ dlnorm(log(mu[i]) - 0.67*sigma, tau)
    y.new[i] ~ dlnorm(log(mu[i]) - 0.67*sigma, tau)
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
    
    }
    ", fill = TRUE)
sink()

jm = jags.model("sbs_bayes/models/model_lognormal.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd"), 
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

# Compare with median values
dat %>% group_by(era) %>% summarise(median(size1mm))
median_change(dat)

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)

# Compared observed vs simulated
hist(dat$size1mm, breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary
coda_summary <- summary(zm)
summary_name <- "4-lognormal-0.75"
coda_quantile_4 <- data.frame(coda_summary$quantile) %>% 
  mutate(coda_quantile = summary_name, 
         param = rownames(coda_summary$quantile))

##### MODEL DATA - NORMAL - P METHOD - MEDIAN #####

dat$p <- 0.75

# load jags
library(rjags)

## Get data
data = list(
  y = as.double(log(dat$size1mm)),
  era = as.double(dat$eraJ), 
  k = as.double(length(dat$size1mm)), 
  p = as.double(dat$p[1])
)

## Iterations
n.adapt = 1000
n.update = 1000

inits = list(
  list(
    alpha = runif(1, 0, 5), 
    beta = runif(1, -10, 10), 
    sigma = 1), 
  list(
    alpha = runif(1, 0, 5), 
    beta = runif(1, -10, 10), 
    sigma = 1))


## JAGS model
sink("sbs_bayes/models/model_normal_quantile.R")
cat(" 
    model{
    # priors
    alpha ~ dnorm(0, 1/5^2) 
    beta ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 100)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:k){
    mu[i] <- alpha + beta * era[i]
    
    w[i]  ~ dexp(tau) # exponential distribution
    me[i] <- (1 - 2 * p) / (p * (1 - p)) * w[i] + mu[i] # if p = 0.5, then this just leaves mu[i]
    pe[i] <- (p * (1 - p) * tau) / (2 * w[i]) # 
    #y[i]  ~ dnorm(me[i], pe[i])
    
    y[i] ~ dnorm(me[i], pe[i])
    
    y.new[i] ~ dnorm(me[i], pe[i])
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
    
    }
    ", fill = TRUE)
sink()

jm = jags.model("sbs_bayes/models/model_normal_quantile.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd"), 
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

# Compare with median values
dat %>% group_by(era) %>% summarise(median(size1mm))
median_change(dat)

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)

# Compared observed vs simulated
hist(log(dat$size1mm), breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary
coda_summary <- summary(zm)
summary_name <- "5-normal-p-0.75"
coda_quantile_5 <- data.frame(coda_summary$quantile) %>% 
  mutate(coda_quantile = summary_name, 
         param = rownames(coda_summary$quantile))

##### MODEL TRUNCATED DATA - MEDIAN #####

my_quantile = 0.5
subtract_value = 0.5
datT <- truncate_data(dat, quant = my_quantile, subtract_value = subtract_value)
summary(datT$size_centered)
hist(datT$size1mm, prob = TRUE)

## Get data
data = list(
  #y = as.double(datT$size_centered), 
  k = as.double(length(datT$size1mm)), 
  era = as.double(datT$eraJ), 
  size_threshold = as.double(datT$size_threshold), 
  subtract_value = subtract_value
)

inits = list(
  list(
    alpha = runif(1, 0, 10), 
    beta = runif(1, -10, 10), 
    sigma = 1), 
  list(
    alpha = runif(1, 0, 30), 
    beta = runif(1, -10, 10), 
    sigma = 1)
)

## JAGS model
sink("sbs_bayes/models/model_JAGS.R")
cat(" 
    model{
    # priors
    alpha ~ dnorm(0, 1/100^2) 
    beta ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 100)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:k){
    mu[i] <- exp(alpha + beta * era[i])
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
    
    }
    ", fill = TRUE)
sink()

jm = jags.model("sbs_bayes/models/model_JAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma", "beta_new"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", 
                                         "y.new", "p.mean", "p.sd"), 
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 
exp(summary(zm)$stat[1]) + datT$size_threshold + subtract_value
  
# Compare with median values
datT %>% group_by(era) %>% summarise(median(size1mm))
median_change(datT)

# Alpha values
zj$beta
alpha_pred <- zj$alpha[, , 1]



#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)

# Compared observed vs simulated
hist(datT$size_centered, breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

# ### Save coda summary
# coda_summary <- summary(zm)
# summary_name <- "5-normal-p-0.75"
# coda_quantile_5 <- data.frame(coda_summary$quantile) %>% 
#   mutate(coda_quantile = summary_name, 
#          param = rownames(coda_summary$quantile))


##### COMBINE SUMMARIES ####
## Combine quantile summaries
ls()
coda_list <- mget(ls(pattern = "coda_quantile_*"))
coda_df <- do.call("rbind", lapply(coda_list, data.frame))
coda_df %>% arrange(param)
