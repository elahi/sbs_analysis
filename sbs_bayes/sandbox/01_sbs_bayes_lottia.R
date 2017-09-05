################################################################################
##' @title Analyzing snail data with Bayes
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-08-21
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### PREP DATA #####

source("sbs_bayes/00_sbs_bayes_data.R")

# Choose data - Lottia
dat <- hexDF %>% filter(!is.na(size1mm))

# Get site groups
dat <- dat %>% 
  mutate(group_j = as.integer(as.factor(sampleArea)))
unique(dat$group_j)

# load jags
library(rjags)

median_change <- function(dat){
  dat_summary <- dat %>% group_by(era) %>% 
    summarise(size_median = median(size1mm, na.rm = TRUE))
  test_stat <- 1 - (dat_summary$size_median[2]/dat_summary$size_median[1])
  return(test_stat)
}

##### ERA - median #####

## Create model matrix
X <- model.matrix(~ eraJ, dat)
n_betas = ncol(X)
beta = as.vector(runif(n_betas, -1, 1))
beta

data = list(
  y = as.double(dat$size1mm),
  X = X,
  k = as.double(length(dat$size1mm)), 
  n_betas = as.double(n_betas), 
  thc = as.double(dat$thc), 
  era = as.double(dat$eraJ)
)

n_betas = ncol(X)
beta = as.vector(runif(n_betas, -1, 1))
beta

n.adapt = 2000
n.update = 2000
n.iter = 2000

inits = list(
  list(
    beta = as.vector(runif(n_betas, -10, 10)),
    sigma = 1)
  # ), 
  # list(
  #   beta = as.vector(runif(n_betas, -10, 10)),
  #   sigma = 1
  # )
)

inits

## JAGS model
sink("sbs_bayes/models/pooledJAGS.R")
cat(" 
    model{
    # priors
    for (i in 1:n_betas){
    beta[i] ~ dnorm(0, .00001)
    }
    sigma ~ dunif(0, 10)
    tau <- 1/sigma^2
    
    # likelihood
    mu_mat <- X %*% beta # can't exponentiate a vector
    for (i in 1:k){
    mu[i] <- exp(mu_mat[i])
    y[i] ~ dlnorm(log(mu[i]), tau)
    y.new[i] ~ dlnorm(log(mu[i]), tau)
    sq.error.data[i] <- (y[i] - mu[i])^2
    sq.error.sim[i] <- (y.new[i] - mu[i])^2
    }
    
    # bayesian p-values
    sd.data <- sd(y)
    sd.sim <- sd(y.new)
    p.sd <- step(sd.sim - sd.data)
    
    mean.data <- mean(y)
    mean.sim  <- mean(y.new)
    p.mean <- step(mean.sim - mean.data)
    
    discrep.data <- sum(sq.error.data)
    discrep.sim <- sum(sq.error.sim)
    p.discrep <- step(discrep.sim - discrep.data)
    
    }
    ", fill = TRUE)
sink()

start_time <- proc.time()
jm = jags.model("sbs_bayes/models/pooledJAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)
zm = coda.samples(jm, variable.names = c("beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("beta", "sigma", "y.new", "p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)
end_time <- proc.time()
end_time - start_time 

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
mean(zj$p.discrep)

# Compared observed vs simulated
hist(data$y, breaks = 30, freq=FALSE) 
lines(density(zj$y.new), col="red")



#### ERA - with sample area groups #####

unique(dat$group_j)
y.n.sites = length(unique(dat$group_j))

# Create a group index
data = list(
  y = dat$size1mm,
  era = dat$eraJ, 
  thc = dat$thc, 
  y.group = dat$group_j, 
  y.n.sites = length(unique(dat$group_j))
)

inits = list(
  list(
    alpha1 = rep(15, y.n.sites),
    alpha2 = 0,
    sigma = 40, 
    mu.alpha1 = 2, 
    sigma.alpha1 = 4
  ),
  list(
    alpha1 = rep(15, y.n.sites),
    alpha2 = 0.7,
    sigma = 40, 
    mu.alpha1 = 0, 
    sigma.alpha1 = 2
  )
)

n.adapt = 2000
n.update = 2000
n.iter = 2000

## JAGS model
sink("sbs_bayes/models/hier1_JAGS.R")
cat(" 
    model{
    
    # priors for within site model (process)
    alpha2 ~ dnorm(0, 5) 
    sigma ~ dunif(0, 50)
    tau <- 1/sigma^2
    
    ### priors for intercept model
    mu.alpha1 ~ dnorm(15, 30)
    sigma.alpha1 ~ dunif(0, 50) #notated varsigma in model documentation
    tau.alpha1 <- 1/sigma.alpha1^2
    
    ### priors
    for(j in 1:y.n.sites){
    alpha1[j] ~ dnorm(mu.alpha1, tau.alpha1)
    }
    
    # likelihood
    for(i in 1:length(y)){
    y_mu[i] <- exp(alpha1[y.group[i]] + alpha2*era[i])
    y[i] ~ dlnorm(log(y_mu[i]), tau)
    
    # Simulated data for posterior predictive checks
    y.sim[i] ~ dlnorm(log(y_mu[i]), tau)
    sq.error.data[i] <- (y[i] - y_mu[i])^2
    sq.error.sim[i] <- (y.sim[i] - y_mu[i])^2
    }
    
    #Bayesian P values
    sd.data <- sd(y)
    sd.sim <- sd(y.sim)
    p.sd <- step(sd.sim - sd.data)
    
    mean.data <- mean(y)
    mean.sim  <- mean(y.sim)
    p.mean <- step(mean.sim - mean.data)
    
    discrep.data <- sum(sq.error.data)
    discrep.sim <- sum(sq.error.sim)
    p.discrep <- step(discrep.sim - discrep.data)
    
    }
    ", fill = TRUE)
sink()


jm = jags.model("sbs_bayes/models/hier1_JAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", "p.sd", 
                                         "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

summary(zm)
heidel.diag(zm)

zj = jags.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", 
                                         "y.sim", "p.sd", "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

#Produce trace plots of the chains for model parameters. 
traceplot(zm)
densplot(zm)

#Produce a summary table for the parameters. 
summary(zm)

#Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
zj$p.sd
zj$p.mean
zj$p.discrep

# Compared observed vs simulated
hist(data$y, breaks = 20, freq=FALSE)
lines(density(zj$y.sim), col="red")

#### ERA - varying intercepts and slopes for sample area #####

# I need to use the multivariate normal so I will log transform body size first

unique(dat$group_j)
y.n.sites = length(unique(dat$group_j))

# Create a group index
data = list(
  #y = dat$size1mm,
  y = log(dat$size1mm), 
  era = dat$eraJ, 
  thc = dat$thc, 
  y.group = dat$group_j, 
  y.n.sites = length(unique(dat$group_j))
)

inits = list(
  list(
    alpha = rep(3, y.n.sites),
    beta = 0,
    sigma = 40, 
    mu.alpha = 2, 
    sigma.alpha = 4
  # ),
  # list(
  #   alpha = rep(15, y.n.sites),
  #   beta = 0.7,
  #   sigma = 40, 
  #   mu.alpha1 = 0, 
  #   sigma.alpha1 = 2
  )
)

n.adapt = 1000
n.update = 1000
n.iter = 1000

## JAGS model
sink("sbs_bayes/models/hier2_JAGS.R")
cat(" 
    model{
    
    # priors for within site model (process)
    alpha ~ dnorm(0, 5) 
    sigma ~ dunif(0, 50)
    tau <- 1/sigma^2
    
    ### priors for intercept model
    mu.alpha ~ dnorm(5, 10)
    sigma.alpha ~ dunif(0, 25) #notated varsigma in model documentation
    tau.alpha <- 1/sigma.alpha^2
    
    ### priors
    for(j in 1:y.n.sites){
    alpha[j] ~ dnorm(mu.alpha, tau.alpha)
    }

    # likelihood
    for(i in 1:length(y)){
    y_mu[i] <- alpha[y.group[i]] + beta*era[i]
    y[i] ~ dnorm(y_mu[i], tau)
    
    # # Simulated data for posterior predictive checks
    # y.sim[i] ~ dnorm(y_mu[i], tau)
    # sq.error.data[i] <- (y[i] - y_mu[i])^2
    # sq.error.sim[i] <- (y.sim[i] - y_mu[i])^2
    }
    
    # #Bayesian P values
    # sd.data <- sd(y)
    # sd.sim <- sd(y.sim)
    # p.sd <- step(sd.sim - sd.data)
    # 
    # mean.data <- mean(y)
    # mean.sim  <- mean(y.sim)
    # p.mean <- step(mean.sim - mean.data)
    # 
    # discrep.data <- sum(sq.error.data)
    # discrep.sim <- sum(sq.error.sim)
    # p.discrep <- step(discrep.sim - discrep.data)
    
    }
    ", fill = TRUE)
sink()


jm_hier2 = jags.model("sbs_bayes/models/hier2_JAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", "p.sd", 
                                         "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

summary(zm)
heidel.diag(zm)

zj = jags.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", 
                                         "y.sim", "p.sd", "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

#Produce trace plots of the chains for model parameters. 
traceplot(zm)
densplot(zm)

#Produce a summary table for the parameters. 
summary(zm)

#Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
zj$p.sd
zj$p.mean
zj$p.discrep

# Compared observed vs simulated
hist(data$y, breaks = 20, freq=FALSE)
lines(density(zj$y.sim), col="red")
