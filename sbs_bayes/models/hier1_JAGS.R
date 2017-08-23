 
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
    
