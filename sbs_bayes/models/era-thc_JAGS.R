 
    model{
    # priors
    alpha1 ~ dnorm(15, 30)
    alpha2 ~ dnorm(0, 30) 
    sigma ~ dunif(0, 50)
    tau <- 1/sigma^2
    beta1 ~ dnorm(0, 20)
    beta2 ~ dnorm(0, 20)

    # likelihood
    for(i in 1:length(y)){
    y_mu[i] <- exp(alpha1 + alpha2*era[i] + beta1*thc[i] + beta2*thc[i]*era[i])
    y[i] ~ dnorm(y_mu[i], tau)

    # Simulated data for posterior predictive checks
    y.sim[i] ~ dnorm(y_mu[i], tau)
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
    
    max.data <- max(y)
    max.sim <- max(y.sim)
    p.max <-step(max.sim - max.data)

    }
    
