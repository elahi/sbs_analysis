
    model{
    # priors
    for(i in 1:p) {  
    b[i] ~ dnorm(0, 0.01)
    }

    sigma ~ dunif(0, 5)
    tau <- 1/sigma^2
    
    # likelihood
    z <- X %*% b
    for (i in 1:N){
    mu[i] <- z[i]
    y[i] ~ dnorm(mu[i], tau) 
    y.new[i] ~ dnorm(mu[i], tau)
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
    
    discrep.data <- sum(sq.error.data)
    discrep.new <- sum(sq.error.new)
    p.discrep <- step(discrep.new - discrep.data)

    }
    
