 
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
    
