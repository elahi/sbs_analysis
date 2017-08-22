 
    model{
    # priors
    alpha1 ~ dnorm(15, 30)
    alpha2 ~ dnorm(-2, 2) 
    sigma ~ dunif(0, 50)
    tau <- 1/sigma^2
    beta1 ~ dnorm(0, 10)
    beta2 ~ dnorm(0, 5)
    #beta3 ~ dnorm(0, 5)
    
    # likelihood
    for(i in 1:length(y)){
    y_mu[i] <- exp(alpha1 + alpha2*era[i] + beta1*thc[i] + beta2*thc[i]*era[i])
    y[i] ~ dnorm(y_mu[i], tau)
    }

    }
    
