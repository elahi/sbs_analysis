 
    model{
    # priors
    alpha ~ dnorm(0, 1/5^2) 
    beta ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 100)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:k){
    mu[i] <- alpha + beta * era[i]
    
    w[i]  ~ dexp(tau) # exponential distribution
    me[i] <- (1 - 2 * p) / (p * (1 - p)) * w[i] + mu[i] # if p = 0.5, then this just leaves mu[i]
    pe[i] <- (p * (1 - p) * tau) / (2 * w[i]) # 
    #y[i]  ~ dnorm(me[i], pe[i])
    
    y[i] ~ dnorm(me[i], pe[i])
    
    y.new[i] ~ dnorm(me[i], pe[i])
    sq.error.data[i] <- (y[i] - mu[i])^2
    sq.error.new[i] <- (y.new[i] - mu[i])^2
    }
    
    # bayesian p-values
    sd.data <- sd(y)
    sd.new <- sd(y.new)
    p.sd <- step(sd.new - sd.data)
    
    mean.data <- mean(y)
    mean.new  <- mean(y.new)
    p.mean <- step(mean.new - mean.data)
    
    }
    
