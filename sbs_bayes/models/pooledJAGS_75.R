 
    model{
    # priors
    for (i in 1:n_betas){
    beta[i] ~ dnorm(0, .00001)
    }
    sigma ~ dunif(0, 10)
    tau <- 1/sigma^2
    
    # likelihood
    mu_mat <- X %*% beta # can't exponentiate a vector
    for (i in 1:k){
    mu[i] <- exp(mu_mat[i])
    y[i] ~ dlnorm(log(mu[i]) - 0.67*sigma, tau)
    y.new[i] ~ dlnorm(log(mu[i]) - 0.67*sigma, tau)
    sq.error.data[i] <- (y[i] - mu[i])^2
    sq.error.sim[i] <- (y.new[i] - mu[i])^2
    }
    
    # bayesian p-values
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
    
