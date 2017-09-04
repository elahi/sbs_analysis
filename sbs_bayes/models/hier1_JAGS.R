 
    model{
    
    # priors for within site model (process)
    beta2 ~ dnorm(0, 5) 
    sigma ~ dunif(0, 50)
    tau <- 1/sigma^2
    
    ### priors for intercept model
    mu.beta1 ~ dnorm(15, 30)
    sigma.beta1 ~ dunif(0, 50) #notated varsigma in model documentation
    tau.beta1 <- 1/sigma.beta1^2
    
    ### priors
    for(j in 1:y.n.sites){
    beta1[j] ~ dnorm(mu.beta1, tau.beta1)
    }
    
    # likelihood
    for(i in 1:length(y)){
    y_mu[i] <- exp(beta1[y.group[i]] + beta2*era[i])
    y[i] ~ dlnorm(log(y_mu[i]), tau)
    
    # Simulated data for posterior predictive checks
    y.new[i] ~ dlnorm(log(y_mu[i]), tau)
    sq.error.data[i] <- (y[i] - y_mu[i])^2
    sq.error.sim[i] <- (y.new[i] - y_mu[i])^2
    }
    
    #Bayesian P values
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
    
