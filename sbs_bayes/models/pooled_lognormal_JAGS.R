 
      model{
      # priors
      alpha ~ dnorm(0, 1/100^2) 
      beta ~ dnorm(0, 1/10^2)
      sigma ~ dunif(0, 100)
      tau <- 1/sigma^2
      
      # likelihood
      for (i in 1:k){
      mu[i] <- exp(alpha + beta * era[i])
      y[i] ~ dlnorm(log(mu[i]), tau)
      y.new[i] ~ dlnorm(log(mu[i]), tau)
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
      
      # Check sink
      
      }
      
