
    model{
    # priors
    beta0 ~ dnorm(0, 1/10^2)
    beta1 ~ dnorm(0, 1/10^2) 
    beta2 ~ dnorm(0, 1/10^2) 
    beta3 ~ dnorm(0, 1/10^2)
    beta4 ~ dnorm(0, 1/10^2) 
    beta5 ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 5)
    
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:N){
    mu[i] <- beta0 + beta1*era[i] + beta2*x[i] + beta3*tideHTm[i] + beta4*era[i]*x[i] + beta5*era[i]*tideHTm[i] 
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
    
    # Derived quantities
    for(j in 1:length(x_predict)){
    y_pred_tide0[j] <- beta0 + beta1*era_predict[j] + beta2*x_predict[j] + beta3*tide_predict0[j] + beta4*era_predict[j]*x_predict[j] + beta5*era[j]*tide_predict0[j] 
    
    y_pred_dens0[j] <- beta0 + beta1*era_predict[j] + beta2*x_predict0[j] + beta3*tide_predict[j] + beta4*era_predict[j]*x_predict0[j] + beta5*era[j]*tide_predict[j] 
    
    #y_new[j] ~ dnorm(y_pred[j], tau)
    }
    
    }
    
