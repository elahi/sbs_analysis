################################################################################
##' @title Hierarchical intercept and slope model - lognormal distribution
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-09-04
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 


hier2_model <- function(dat, iter_adapt, iter_update, n_chains){
  
  # load jags
  library(rjags)
  
  # Create sample area groups
  dat <- dat %>% mutate(group_j = as.integer(as.factor(sampleArea)))
  y.n.sites = length(unique(dat$group_j))
  
  # Create matrix for group level intercept and slope
  B = matrix(nrow = y.n.sites, ncol = 2)
  B[, 1] = 0
  B[, 2] = 1.5
  
  ## Get data
  data = list(
    y = as.double(log(dat$size1mm)),
    k = as.double(length(dat$size1mm)), 
    thc = as.double(dat$thc), 
    era = as.double(dat$eraJ), 
    y.group = dat$group_j, 
    y.n.sites = length(unique(dat$group_j))
  )
  
  ## Iterations
  n.adapt = iter_adapt
  n.update = iter_update

  ## Inits
  if(n_chains == 1){
    inits = list(
      B = B, 
      sigma = 1, 
      mu.alpha = runif(1, 0, 5), 
      mu.beta = runif(1, -10, 10), 
      sigma.alpha = 1, 
      sigma.beta = 1, 
      rho = -0.5)
  }
  
  if(n_chains == 2){
    inits = list(
      list(
        B = B, 
        sigma = 1, 
        mu.alpha = runif(1, 0, 5), 
        mu.beta = runif(1, -10, 10), 
        sigma.alpha = 1, 
        sigma.beta = 1, 
        rho = -0.5), 
    list(
      B = B*0.5, 
      sigma = 10, 
      mu.alpha = runif(1, 0, 5), 
      mu.beta = runif(1, -10, 10), 
      sigma.alpha = 5, 
      sigma.beta = 5, 
      rho = 0.5)
    )
  }
  
  ## JAGS model
  sink("sbs_bayes/models/hier2_normal_JAGS.R")
  cat(" 
      model{
      
      # priors for within site model (process)
      sigma ~ dunif(0, 100)
      tau <- 1/sigma^2
      
      ### priors for intercept and slope model
      for(j in 1:y.n.sites){
      alpha[j] <- B[j, 1] # group level intercept
      beta[j] <- B[j, 2] # group level slope
      B[j, 1:2] ~ dmnorm(B.hat[j, 1:2], Tau.B)
      B.hat[j, 1] <- mu.alpha # required by JAGS syntax
      B.hat[j, 2] <- mu.beta # required by JAGS syntax
      }

      mu.alpha ~ dnorm(0, 1/5^2)
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
      mu[i] <- alpha[y.group[i]] + beta[y.group[i]]*era[i]
      y[i] ~ dnorm(mu[i], tau)
      
      # Simulated data for posterior predictive checks
      y.new[i] ~ dnorm(mu[i], tau)
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
      ", fill = TRUE)
  sink()
  
  
  jm = jags.model("sbs_bayes/models/hier2_normal_JAGS.R", data = data, inits = inits, 
                  n.chains = length(inits), n.adapt = n.adapt)
  update(jm, n.iter = n.update)
  
  return(jm)

}


