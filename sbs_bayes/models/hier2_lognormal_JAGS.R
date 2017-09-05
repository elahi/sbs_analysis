 
      model{
      
      # priors for within site model (process)
      sigma ~ dunif(0, 100)
      tau <- 1/sigma^2
      
      ### priors for intercept and slope model
      for(j in 1:y.n.sites){
      alpha[j] <- B[j, 1] # group level intercept
      beta[j] <- B[j, 2] # group level slope
      B[j, 1:2] ~ dmlnorm(B.hat[j, 1:2], Tau.B)
      B.hat[j, 1] <- mu.alpha # required by JAGS syntax
      B.hat[j, 2] <- mu.beta # required by JAGS syntax
      }

      mu.alpha ~ dnorm(0, 1/100^2)
      mu.beta ~ dnorm(0, 1/10^2)
      # Inverse of covariance matrix required by JAGS
      Tau.B[1:2, 1:2] <- inverse(Sigma.B[1:2, 1:2])
      # Elements of covariance matrix
      Sigma.B[1,1] <- sigma.alpha^2
      sigma.alpha ~ dunif(0, 100)
      Sigma.B[2,2] <- sigma.beta^2
      sigma.beta ~ dunif(0, 100)      
      Sigma.B[1,2] <- rho * sigma.alpha * sigma.beta
      Sigma.B[2,1] <- Sigma.B[1,2]
      rho ~ dunif(-1, 1)
      
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
      
