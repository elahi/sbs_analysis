 
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
    
