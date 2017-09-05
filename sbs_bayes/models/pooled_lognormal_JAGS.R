 
      model{
      # priors
      alpha ~ dnorm(0, 100) 
      beta ~ dnorm(0, 10)
      sigma ~ dunif(0, 100)
      tau <- 1/sigma^2
      
      # likelihood
      for (i in 1:k){
      mu[i] <- exp(alpha + beta * era[i])
      y[i] ~ dlnorm(log(mu[i]), tau)
      y.new[i] ~ dlnorm(log(mu[i]), tau)
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
      
      # Check sink
      
      }
      
