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

# Choose data - Chlorostoma
dat <- waraDF %>% filter(!is.na(size1mm))
unique(dat$site)
dat1 <- waraDF %>% filter(site == "Wara.B") %>% filter(!is.na(size1mm))
dat2 <- waraDF %>% filter(site == "Wara.D") %>% filter(!is.na(size1mm))

# load jags
library(rjags)

median_change <- function(dat){
  dat_summary <- dat %>% group_by(era) %>% 
    summarise(size_median = median(size1mm, na.rm = TRUE))
  test_stat <- 1 - (dat_summary$size_median[2]/dat_summary$size_median[1])
  return(test_stat)
}

##### ERA - median #####
dat <- waraDF

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
median_change(dat2)

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)
mean(zj$p.discrep)

# Compared observed vs simulated
hist(data$y, breaks = 30, freq=FALSE) #note that this is the log transformed data
lines(density(zj$y.new), col="red")

