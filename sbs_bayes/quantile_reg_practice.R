#https://github.com/brendan-r/quantile_regression/blob/master/quantile_regression.Rmd

#### GENERATE DATA #####

# Set-up the data generating mechainsm
set.seed(666)
N     <- 500
x     <- runif(N, max=10)
alpha <- 1
beta  <- 2
y     <- alpha + beta * x + rnorm(N, sd = .6 * x)
p     <- 0.95

# The dataset to be used for estiamtion
data_set   <- list(y = y, x = x, p = p)
data_frame <- as.data.frame(data_set)[-3]

#### USE QUANTREG #####

library(quantreg)
qreg_fit <- rq(y ~ x, data = data_frame, tau = p)
# (setting ci = TRUE seems to choke it for some reason)

# See results
summary(qreg_fit)

#### IN JAGS #####

# Adapted from http://stats.stackexchange.com/q/17672
library(rjags)

jags_code <- "
model{
for(i in 1:length(y)){
mu[i] <- alpha + beta * x[i]
w[i]  ~ dexp(tau) # exponential distribution
me[i] <- (1 - 2 * p) / (p * (1 - p)) * w[i] + mu[i] # if p = 0.5, then this just leaves mu[i]
pe[i] <- (p * (1 - p) * tau) / (2 * w[i]) # 
y[i]  ~ dnorm(me[i], pe[i])
}

# Regression Priors
alpha ~ dnorm(0, 1E-6)
beta  ~ dnorm(0, 1E-6)

lsigma ~ dunif(-5, 15)
sigma  <- exp(lsigma / 2)
tau    <- pow(sigma, -2)
}
"

# Init the model
n_iter <- 10000
jags_model <- jags.model(file = textConnection(jags_code), data = data_set, 
                         n.chains = 4, n.adapt = n_iter / 2)

# Run some MCMC iterations
params <- c("alpha", "beta", "sigma")
jags_samples <- coda.samples(jags_model, params, n.iter = n_iter)

# Results
t(apply(
  data.frame(do.call(rbind, jags_samples)), 2, function(x)
    c(mean = mean(x), quantile(x, c(0.005, 0.25, 0.5, 0.75, 0.95)))
))
