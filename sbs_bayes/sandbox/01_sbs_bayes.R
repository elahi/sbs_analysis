################################################################################
##' @title Analyzing snail data with Bayes
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-08-21
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### PREP DATA #####

# load data
source("03_identify_size_cutoff.R")

### I need to recode some things for JAGS
# era
unique(dat4$era)

## Recode era: past = 0, present = 1
## thc = scaled tidal height
datJ <- dat4 %>% 
  mutate(eraJ = ifelse(era == "past", 0, 1), 
         thc = as.numeric(scale(sample_area_tidal_ht, scale = F)))
head(datJ)
summary(datJ)

## Analyze species separately
## Use group level effects for Littorina and Lottia
## For Chlorostoma, analyze sites separately

## Make separate dataframes
childsDF <- droplevels(filter(datJ, sp == "LIKE"))
waraDF <- droplevels(filter(datJ, sp == "CHFU"))
hexDF <- droplevels(filter(datJ, sp == "LODI"))

##### THREE WAY MODEL WITH FIXED EFFECTS #####

## Create a model matrix for a 3-way model
## Era, Species, Tidal height

# Create site and species groups
datJ <- datJ %>% group_by(sampleArea) %>% 
  mutate(group_j = as.integer(as.factor(sampleArea))) %>%
  ungroup() %>% group_by(species) %>% 
  mutate(species_k = as.integer(as.factor(species))) %>% ungroup()

unique(datJ$sampleArea)
unique(datJ$group_j)
unique(datJ$species)
unique(datJ$species_k)

## Create model matrix
X <- model.matrix(~ eraJ*species_k*thc, datJ)
head(X)
summary(X)
is.matrix(X)
str(X)

n_betas = ncol(X)
beta = as.vector(runif(n_betas, -1, 1))
beta

n.adapt = 500
n.update = 1000
n.iter = 1000

inits = list(
  list(
    beta = as.vector(runif(n_betas, -1, 1)),
    sigma = 1
  )
)

data = list(
  y = as.double(datJ$size1mm),
  X = X,
  k = length(datJ$size1mm), 
  group = datJ$group_j, 
  species = datJ$species_k, 
  n_betas = n_betas
  )

## JAGS model
sink("sbs_bayes/models/three_way_JAGS.R")
cat("
    model{
    # priors
    for (i in 1:n_betas){
    beta[i] ~ dnorm(0, .00001)
    }
    sigma ~ dunif(0, 10)
    tau <- 1/sigma^2
    
    # likelihood
    mu_mat <- X %*% beta
    for (i in 1:k){
    mu[i] <- exp(mu_mat[i])
    #mu[i] <- exp(beta[1] + beta[2]*X[i,2]) # my old way, would suck for lots of betas
    y[i] ~ dlnorm(log(mu[i]), tau)
    y.new[i] ~ dlnorm(log(mu[i]), tau)
    epsilon[i] <- y[i] - mu[i]
    }
    
    # derived quantities
    cv.y <- sd(y[]) / mean(y[])
    cv.y.new <- sd(y.new[]) / mean(y.new[])
    pvalue.cv <- step(cv.y.new - cv.y)   
    
    }
    ", fill = TRUE)
sink()

jm = jags.model("sbs_bayes/models/three_way_JAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)

summary(zm)

zj = jags.samples(jm, variable.names = c("beta", "sigma", "pvalue.cv"), 
                  n.iter = n.iter, n.thin = 1)

#Produce trace plots of the chains for model parameters. 
traceplot(zm)
densplot(zm)

#Produce a summary table for the parameters. 
summary(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$pvalue.cv)

## Compare with lm
names(datJ)
summary(datJ)
lm1 <- lm(size1mm ~ eraJ * species_k * thc, data = datJ)
lm1
summary(lm1)
head(X)
tail(X)
summary(zm)$stat
summary(zm)$quantile[,3] # median
head(X)

##### POOLED MODEL FOR EACH SPECIES, IGNORING SPATIAL STRUCTURE & TIDAL HEIGHT #####

# load jags
library(rjags)

# Choose data - Littorina
dat <- childsDF

data = list(
  y = dat$size1mm,
  era = dat$eraJ
)

inits = list(
  list(
    alpha1 = 15,
    alpha2 = 0,
    sigma = 40
  ),
  list(
    alpha1 = 5,
    alpha2 = 0.7,
    sigma = 20
  )
)

n.adapt = 2000
n.update = 2000
n.iter = 2000

## JAGS model
sink("sbs_bayes/models/pooledJAGS.R")
cat(" 
    model{
    # priors
    alpha1 ~ dnorm(15, 30)
    alpha2 ~ dnorm(0, 5) 
    sigma ~ dunif(0, 50)
    tau <- 1/sigma^2
    
    # likelihood
    for(i in 1:length(y)){
    y_mu[i] <- exp(alpha1 + alpha2 * era[i])
    y[i] ~ dlnorm(log(y_mu[i]), tau)
    #y[i] ~ dlnorm(log(y_mu[i]) - 0.67*sigma, tau)
    
    # Simulated data for posterior predictive checks
    y.sim[i] ~ dlnorm(log(y_mu[i]), tau)
    #y.sim[i] ~ dlnorm(log(y_mu[i]) - 0.67*sigma, tau)
    sq.error.data[i] <- (y[i] - y_mu[i])^2
    sq.error.sim[i] <- (y.sim[i] - y_mu[i])^2
    }
    
    #Bayesian P values
    sd.data <- sd(y)
    sd.sim <- sd(y.sim)
    p.sd <- step(sd.sim - sd.data)

    mean.data <- mean(y)
    mean.sim  <- mean(y.sim)
    p.mean <- step(mean.sim - mean.data)
    
    discrep.data <- sum(sq.error.data)
    discrep.sim <- sum(sq.error.sim)
    p.discrep <- step(discrep.sim - discrep.data)

    }
    ", fill = TRUE)
sink()

jm = jags.model("sbs_bayes/models/pooledJAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", "p.sd", 
                                         "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

summary(zm)

zj = jags.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", 
                                         "y.sim", "p.sd", "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

#Produce trace plots of the chains for model parameters. 
traceplot(zm)
densplot(zm)

#Produce a summary table for the parameters. 
summary(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
zj$p.sd
zj$p.mean
zj$p.discrep

# Compared observed vs simulated
hist(data$y, breaks = 30, freq=FALSE) #note that this is the log transformed data
lines(density(zj$y.sim), col="red")

##### POOLED MODEL FOR EACH SPECIES & TIDAL HEIGHT, WITH SITE GROUPS #####

# load jags
library(rjags)

# Choose data - Littorina
dat <- childsDF

# Create site groups
dat <- dat %>% group_by(sampleArea) %>% 
  mutate(group_j = as.integer(as.factor(sampleArea)))

unique(dat$group_j)
y.n.sites = length(unique(dat$group_j))

# Create a group index
data = list(
  y = dat$size1mm,
  era = dat$eraJ, 
  thc = dat$thc, 
  y.group = dat$group_j, 
  y.n.sites = length(unique(dat$group_j))
)

inits = list(
  list(
    alpha1 = rep(15, y.n.sites),
    alpha2 = 0,
    sigma = 40, 
    mu.alpha1 = 2, 
    sigma.alpha1 = 4
  ),
  list(
    alpha1 = rep(15, y.n.sites),
    alpha2 = 0.7,
    sigma = 40, 
    mu.alpha1 = 0, 
    sigma.alpha1 = 2
  )
)

n.adapt = 2000
n.update = 2000
n.iter = 2000

## JAGS model
sink("sbs_bayes/models/hier1_JAGS.R")
cat(" 
    model{

    # priors for within site model (process)
    alpha2 ~ dnorm(0, 5) 
    sigma ~ dunif(0, 50)
    tau <- 1/sigma^2
    
    ### priors for intercept model
    mu.alpha1 ~ dnorm(15, 30)
    sigma.alpha1 ~ dunif(0, 50) #notated varsigma in model documentation
    tau.alpha1 <- 1/sigma.alpha1^2

    ### priors
    for(j in 1:y.n.sites){
    alpha1[j] ~ dnorm(mu.alpha1, tau.alpha1)
    }

    # likelihood
    for(i in 1:length(y)){
    y_mu[i] <- exp(alpha1[y.group[i]] + alpha2*era[i])
    y[i] ~ dlnorm(log(y_mu[i]), tau)
    
    # Simulated data for posterior predictive checks
    y.sim[i] ~ dlnorm(log(y_mu[i]), tau)
    sq.error.data[i] <- (y[i] - y_mu[i])^2
    sq.error.sim[i] <- (y.sim[i] - y_mu[i])^2
    }
    
    #Bayesian P values
    sd.data <- sd(y)
    sd.sim <- sd(y.sim)
    p.sd <- step(sd.sim - sd.data)
    
    mean.data <- mean(y)
    mean.sim  <- mean(y.sim)
    p.mean <- step(mean.sim - mean.data)
    
    discrep.data <- sum(sq.error.data)
    discrep.sim <- sum(sq.error.sim)
    p.discrep <- step(discrep.sim - discrep.data)
    
    }
    ", fill = TRUE)
sink()


jm = jags.model("sbs_bayes/models/hier1_JAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", "p.sd", 
                                         "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

summary(zm)
heidel.diag(zm)

zj = jags.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", 
                                         "y.sim", "p.sd", "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

#Produce trace plots of the chains for model parameters. 
traceplot(zm)
densplot(zm)

#Produce a summary table for the parameters. 
summary(zm)

#Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
zj$p.sd
zj$p.mean
zj$p.discrep

# Compared observed vs simulated
hist(data$y, breaks = 30, freq=FALSE)
lines(density(zj$y.sim), col="red")

##### POOLED MODEL FOR EACH SPECIES & TIDAL HEIGHT, IGNORING SPATIAL STRUCTURE #####

# load jags
library(rjags)

# Choose data - Littorina
dat <- childsDF
#dat <- waraDF
#dat <- hexDF

data = list(
  y = dat$size1mm,
  era = dat$eraJ, 
  thc = dat$thc
)

inits = list(
  list(
    alpha1 = 15,
    alpha2 = 0,
    sigma = 40, 
    beta1 = -0.9, 
    beta2 = -0.2
  ),
  list(
    alpha1 = 5,
    alpha2 = 0.7,
    sigma = 20, 
    beta1 = 9, 
    beta2 = 0.5
  )
)

n.adapt = 1000
n.update = 1000
n.iter = 1000

## JAGS model
sink("sbs_bayes/models/era-thc_JAGS.R")
cat(" 
    model{
    # priors
    alpha1 ~ dnorm(15, 30)
    alpha2 ~ dnorm(0, 30) 
    sigma ~ dunif(0, 50)
    tau <- 1/sigma^2
    beta1 ~ dnorm(0, 20)
    beta2 ~ dnorm(0, 20)
    
    # likelihood
    for(i in 1:length(y)){
    y_mu[i] <- exp(alpha1 + alpha2*era[i] + beta1*thc[i] + beta2*thc[i]*era[i])
    y[i] ~ dlnorm(log(y_mu[i]), tau)
    
    # Simulated data for posterior predictive checks
    y.sim[i] ~ dlnorm(log(y_mu[i]), tau)
    sq.error.data[i] <- (y[i] - y_mu[i])^2
    sq.error.sim[i] <- (y.sim[i] - y_mu[i])^2
    }
    
    #Bayesian P values
    sd.data <- sd(y)
    sd.sim <- sd(y.sim)
    p.sd <- step(sd.sim - sd.data)
    
    mean.data <- mean(y)
    mean.sim  <- mean(y.sim)
    p.mean <- step(mean.sim - mean.data)
    
    discrep.data <- sum(sq.error.data)
    discrep.sim <- sum(sq.error.sim)
    p.discrep <- step(discrep.sim - discrep.data)
    
    }
    ", fill = TRUE)
sink()


jm = jags.model("sbs_bayes/models/era-thc_JAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", 
                                         "beta1", "beta2","p.sd", 
                                         "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)
summary(zm)
head(zm[[1]])

zj = jags.samples(jm, variable.names = c("alpha1", "alpha2", "sigma", "beta1", "beta2", 
                                         "y.sim", "p.sd", "p.mean", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

#Produce trace plots of the chains for model parameters. 
traceplot(zm)
densplot(zm)
#Produce a summary table for the parameters. 
summary(zm)

#Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
zj$p.sd
zj$p.mean
zj$p.max
zj$p.discrep

# Compared observed vs simulated
hist(data$y, breaks = 30, freq=FALSE) #note that this is the log transformed data
lines(density(zj$y.sim), col="red")

lm2 <- lm(size1mm ~ era * thc, data = dat)
summary(lm2)
lm2

waraDF %>% group_by(era) %>% 
  summarise(mean = mean(size1mm, na.rm = TRUE),
            sd = sd(size1mm, na.rm = TRUE))
exp(2.45)
11/8.5
8.5/11.1
11.1 * -0.3
