
    model{
    # priors
    beta0 ~ dnorm(0, 1/10^2)
    beta1 ~ dnorm(0, 1/10^2) 
    beta2 ~ dnorm(0, 1/10^2) 
    beta3 ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 5)
    
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:N){
    mu[i] <- beta0 + beta1*era[i] + beta2*x[i] + beta3*era[i]*x[i]
    
    # Regression (mean)
    #y[i] ~ dnorm(mu[i], tau) 
    #y.new[i] ~ dnorm(mu[i], tau)
    
    # Quantile regression
    w[i]  ~ dexp(tau)
    me[i] <- (1 - 2 * p) / (p * (1 - p)) * w[i] + mu[i]
    pe[i] <- (p * (1 - p) * tau) / (2 * w[i])
    y[i]  ~ dnorm(me[i], pe[i])
    y.new[i] ~ dnorm(me[i], pe[i])
    
    sq.error.data[i] <- (y[i] - mu[i])^2
    sq.error.new[i] <- (y.new[i] - mu[i])^2
    }
    
    
  #   jags_code <- "
  # model{
  #   for(i in 1:length(y)){
  #   mu[i] <- alpha + beta * x[i]
  #   w[i]  ~ dexp(tau)
  #   me[i] <- (1 - 2 * p) / (p * (1 - p)) * w[i] + mu[i]
  #   pe[i] <- (p * (1 - p) * tau) / (2 * w[i])
  #   y[i]  ~ dnorm(me[i], pe[i])
  #   }
  #   
  #   # Regression Priors
  #   alpha ~ dnorm(0, 1E-6)
  #   beta  ~ dnorm(0, 1E-6)
  #   
  #   lsigma ~ dunif(-5, 15)
  #   sigma  <- exp(lsigma / 2)
  #   tau    <- pow(sigma, -2)
  #   }
  #   "
    
    
    
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
    
    # Derived quantities
    for(j in 1:length(x_predict)){
    y_pred[j] <- beta0 + beta1*era_predict[j] + beta2*x_predict[j] + beta3*tide_predict[j]
    y_new[j] ~ dnorm(y_pred[j], tau)
    }
    
    }
    
