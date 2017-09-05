 
      model{
      
      # priors for within site model (process)
      beta ~ dnorm(0, 1/10^2)
      sigma ~ dunif(0, 100)
      tau <- 1/sigma^2
      
      ### priors for intercept model
      mu.alpha ~ dnorm(0, 1/100^2)
      sigma.alpha ~ dunif(0, 100)
      tau.alpha <- 1/sigma.alpha^2
      
      ### priors
      for(j in 1:y.n.sites){
      alpha[j] ~ dnorm(mu.alpha, tau.alpha)
      }
      
      # likelihood
      for(i in 1:length(y)){
      mu[i] <- exp(alpha[y.group[i]] + beta*era[i])
      y[i] ~ dlnorm(log(mu[i]), tau)
      
      # Simulated data for posterior predictive checks
      y.new[i] ~ dlnorm(log(mu[i]), tau)
      sq.error.data[i] <- (y[i] - mu[i])^2
      sq.error.new[i] <- (y.new[i] - mu[i])^2
      }
      
      #Bayesian P values
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
      
