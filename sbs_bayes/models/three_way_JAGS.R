
    model{
    # priors
    for (i in 1:n_betas){
    beta[i] ~ dnorm(0, .00001)
    }
    sigma ~ dunif(0, 10)
    tau <- 1/sigma^2
    
    # likelihood
    mu_mat <- X %*% beta
    for (i in 1:k){
    mu[i] <- exp(mu_mat[i])
    #mu[i] <- exp(beta[1] + beta[2]*X[i,2]) # my old way, would suck for lots of betas
    y[i] ~ dlnorm(log(mu[i]), tau)
    y.new[i] ~ dlnorm(log(mu[i]), tau)
    epsilon[i] <- y[i] - mu[i]
    }
    
    # derived quantities
    cv.y <- sd(y[]) / mean(y[])
    cv.y.new <- sd(y.new[]) / mean(y.new[])
    pvalue.cv <- step(cv.y.new - cv.y)   
    
    }
    
