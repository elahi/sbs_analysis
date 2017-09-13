################################################################################
##' @title Pooled model -  normal distribution - truncated data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-09-04
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

## NORMAL DISTRIBUTION
model_jags <- function(dat, iter_adapt, iter_update, n_chains){
  
  # load jags
  library(rjags)
  
  ## Get data
  data = list(
    y = as.double(dat$size_log), 
    k = as.double(length(dat$size1mm)), 
    thc = as.double(dat$thc), 
    era = as.double(dat$eraJ), 
    thc_predict = as.double(pred_df$thc_predict), 
    era_predict = as.double(pred_df$era_predict)
  )
  
  ## Iterations
  n.adapt = iter_adapt
  n.update = iter_update
  
  ## Inits
  if(n_chains == 1){
    inits = list(
      alpha = runif(1, 0, 5), 
      beta = runif(1, -10, 10), 
      sigma = 1)
  }
  
  if(n_chains == 2){
    inits = list(
      list(
        alpha = runif(1, 0, 5), 
        beta = runif(1, -10, 10), 
        sigma = 1), 
      list(
        alpha = runif(1, 0, 5), 
        beta = runif(1, -10, 10), 
        sigma = 1)
    )
  }
  
  ## JAGS model
  sink("sbs_bayes/models/model_jags.R")
  cat(" 
      model{
      # priors
      alpha ~ dnorm(0, 1/5^2) 
      beta ~ dnorm(0, 1/10^2)
      sigma ~ dunif(0, 100)
      tau <- 1/sigma^2
      
      # likelihood
      for (i in 1:k){
      mu[i] <- alpha + beta * era[i]
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
      
      }
      ", fill = TRUE)
  sink()
  
  jm = jags.model("sbs_bayes/models/model_jags.R", data = data, inits = inits, 
                  n.chains = length(inits), n.adapt = n.adapt)
  update(jm, n.iter = n.update)
  
  return(jm)

}

