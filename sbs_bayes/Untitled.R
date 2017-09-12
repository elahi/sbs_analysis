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


jags_code <- "
  model{
    for(i in 1:length(y)){
      mu[i] <- alpha + beta * x[i]
      w[i]  ~ dexp(tau)
      me[i] <- (1 - 2 * p) / (p * (1 - p)) * w[i] + mu[i]
      pe[i] <- (p * (1 - p) * tau) / (2 * w[i])
      y[i]  ~ dnorm(me[i], pe[i])
      y.new[i] ~ dnorm(me[i], pe[i])
    }
    
    # Regression Priors
    alpha ~ dnorm(0, 1E-6)
    beta  ~ dnorm(0, 1E-6)
    
    lsigma ~ dunif(-5, 15)
    sigma  <- exp(lsigma / 2)
    tau    <- pow(sigma, -2)

    # bayesian p-values
    sd.data <- sd(y)
    sd.new <- sd(y.new)
    p.sd <- step(sd.new - sd.data)
    
    mean.data <- mean(y)
    mean.new  <- mean(y.new)
    p.mean <- step(mean.new - mean.data)
  }
"

# Init the model
n_iter <- 1000
jags_model <- jags.model(file = textConnection(jags_code), data = data_set, 
                         n.chains = 4, n.adapt = n_iter / 2)

# Run some MCMC iterations
params <- c("alpha", "beta", "sigma", "y.new", "p.sd", "p.mean")
zj <- coda.samples(jags_model, params, n.iter = n_iter)

# Results
t(apply(
  data.frame(do.call(rbind, zj)), 2, function(x)
    c(mean = mean(x), quantile(x, c(0.005, 0.25, 0.5, 0.75, 0.95)))
))

zj$p.mean
c(p.mean = mean(zj$p.mean), p.sd = mean(zj$p.sd))

# Compared observed vs simulated
hist(y, breaks = 20, freq=FALSE)
lines(density(zj$y.new), col="red")
