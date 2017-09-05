################################################################################
##' @title Hierarchical intercepts model - lognormal distribution
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-09-04
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 


hier1_model <- function(dat, iter_adapt, iter_update, n_chains){
  
  # load jags
  library(rjags)
  
  # Create sample area groups
  dat <- dat %>% mutate(group_j = as.integer(as.factor(sampleArea)))
  y.n.sites = length(unique(dat$group_j))
  
  ## Get data
  data = list(
    y = as.double(dat$size1mm),
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
      alpha = rep(runif(1, 0, 30), y.n.sites), 
      beta = runif(1, -10, 10), 
      sigma = 1, 
      mu.alpha = runif(1, 0, 30), 
      sigma.alpha = 1)
  }
  
  if(n_chains == 2){
    inits = list(
      list(
      alpha = runif(1, 0, 30), 
      beta = runif(1, -10, 10), 
      sigma = 1), 
    list(
      alpha = runif(1, 0, 30), 
      beta = runif(1, -10, 10), 
      sigma = 1)
    )
  }
  
  ## JAGS model
  sink("sbs_bayes/models/hier1_lognormal_JAGS.R")
  cat(" 
      model{
      
      # priors for within site model (process)
      beta ~ dnorm(0, 10)
      sigma ~ dunif(0, 100)
      tau <- 1/sigma^2
      
      ### priors for intercept model
      mu.alpha ~ dnorm(0, 100)
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
      ", fill = TRUE)
  sink()
  
  
  jm = jags.model("sbs_bayes/models/hier1_lognormal_JAGS.R", data = data, inits = inits, 
                  n.chains = length(inits), n.adapt = n.adapt)
  update(jm, n.iter = n.update)
  
  return(jm)

}


