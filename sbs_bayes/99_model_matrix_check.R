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

# Choose data - Littorina
dat <- childsDF

# Get groups for sample areas
# Note that thc is centered relative to all three species, not just Littorina
dat <- dat %>% group_by(sampleArea) %>% 
  mutate(group_j = as.integer(as.factor(sampleArea)))

# load jags
library(rjags)

##### POOLED MODEL - model matrix #####
head(dat)

## Create model matrix
X <- model.matrix(~ eraJ, dat)
n_betas = ncol(X)
beta = as.vector(runif(n_betas, -1, 1))
beta

data = list(
  y = as.double(dat$size1mm),
  X = X,
  k = as.double(length(dat$size1mm)), 
  group = as.double(dat$group_j), 
  n_betas = as.double(n_betas), 
  thc = as.double(dat$thc), 
  era = as.double(dat$eraJ)
)

n_betas = ncol(X)
beta = as.vector(runif(n_betas, -1, 1))
beta

n.adapt = 1000
n.update = 1000
n.iter = 1000

inits = list(
  list(
    beta = as.vector(runif(n_betas, -1, 1)),
    sigma = 1
  ), 
  list(
    beta = as.vector(runif(n_betas, -1, 1)),
    sigma = 1
  )
)

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
    }
    }
    ", fill = TRUE)
sink()

start_time <- proc.time()
jm = jags.model("sbs_bayes/models/pooledJAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)
zm = coda.samples(jm, variable.names = c("beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
end_time <- proc.time()
end_time - start_time # 47.17 seconds

#Produce a summary table for the parameters. 
summary(zm)
exp(2.3681)
exp(2.3681 - 0.3477)
# Compare with mean values
dat %>% group_by(era) %>% summarise(mean = mean(size1mm))

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)


##### POOLED MODEL - by hand #####
head(dat)

## Create model matrix
X <- model.matrix(~ eraJ, dat)
n_betas = ncol(X)
beta = as.vector(runif(n_betas, -1, 1))
beta

data = list(
  y = as.double(dat$size1mm),
  X = X,
  k = as.double(length(dat$size1mm)), 
  group = as.double(dat$group_j), 
  n_betas = as.double(n_betas), 
  thc = as.double(dat$thc), 
  era = as.double(dat$eraJ)
)

n_betas = ncol(X)
beta = as.vector(runif(n_betas, -1, 1))
beta

n.adapt = 1000
n.update = 1000
n.iter = 1000

inits = list(
  list(
    beta1 = as.vector(runif(1, -1, 1)),
    beta2 = as.vector(runif(1, -1, 1)), 
    sigma = 1
  ), 
  list(
    beta1 = as.vector(runif(1, -1, 1)),
    beta2 = as.vector(runif(1, -1, 1)),     
    sigma = 1
  )
)

## JAGS model
sink("sbs_bayes/models/pooledJAGS.R")
cat(" 
    model{
    # priors
    beta1 ~ dnorm(0, .00001) 
    beta2 ~ dnorm(0, .00001)
    sigma ~ dunif(0, 10)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:k){
    mu[i] <- exp(beta1 + beta2 * era[i])
    y[i] ~ dlnorm(log(mu[i]), tau)
    }
    }
    ", fill = TRUE)
sink()

start_time <- proc.time()
jm2 = jags.model("sbs_bayes/models/pooledJAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm2, n.iter = n.update)
zm2 = coda.samples(jm2, variable.names = c("beta1", "beta2", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
end_time <- proc.time()
end_time - start_time # 16.5 seconds

#Produce a summary table for the parameters. 
summary(zm)
summary(zm2)
exp(2.3694)
exp(2.3694 - 0.3480)
# Compare with mean values
dat %>% group_by(era) %>% summarise(mean = mean(size1mm))

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)
