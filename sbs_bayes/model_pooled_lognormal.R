################################################################################
##' @title Pooled model - lognormal distribution
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-09-04
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

pooled_model <- function(dat, iter_adapt, iter_update, n_chains){
  
  # load jags
  library(rjags)
  
  ## Get data
  data = list(
    y = as.double(dat$size1mm),
    k = as.double(length(dat$size1mm)), 
    thc = as.double(dat$thc), 
    era = as.double(dat$eraJ)
  )
  
  ## Iterations
  n.adapt = iter_adapt
  n.update = iter_update

  ## Inits
  if(n_chains == 1){
    inits = list(
      alpha = runif(1, 0, 30), 
      beta = runif(1, -10, 10), 
      sigma = 1)
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
  sink("sbs_bayes/models/pooled_lognormal_JAGS.R")
  cat(" 
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
      ", fill = TRUE)
  sink()
  
  jm = jags.model("sbs_bayes/models/pooled_lognormal_JAGS.R", data = data, inits = inits, 
                  n.chains = length(inits), n.adapt = n.adapt)
  update(jm, n.iter = n.update)
  
  return(jm)

}


