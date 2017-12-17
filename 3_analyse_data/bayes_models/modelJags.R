
    model{
    # priors
    b0 ~ dnorm(0, 1/10^2)
    b1 ~ dnorm(0, 1/10^2) 
    b2 ~ dnorm(0, 1/10^2) 
    b3 ~ dnorm(0, 1/10^2)
    b4 ~ dnorm(0, 1/10^2)
    b5 ~ dnorm(0, 1/10^2)
    b6 ~ dnorm(0, 1/10^2)
    b7 ~ dnorm(0, 1/10^2)

    sigma ~ dunif(0, 5)
    
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:N){
    mu[i] <- b0 + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x1[i]*x2[i] + b5*x1[i]*x3[i] + b6*x2[i]*x3[i] + b7*x1[i]*x2[i]*x3[i]
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
    
