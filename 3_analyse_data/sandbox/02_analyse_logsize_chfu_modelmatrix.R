################################################################################
##' @title Analyze logsize - CHFU
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-12-17
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

##### PACKAGES, DATA #####
source("3_analyse_data/01_sbs_bayes_data.R")
source("3_analyse_data/bayes_R/bayes_functions_general.R")

##### SET UP JAGS MODEL #####
# load jags
library(rjags)

## Iterations (5000 may not be enough for Wara)
n.adapt = 500
n.update = 500
n.iter = 500

# JAGS model
sink("3_analyse_data/bayes_models/modelJags.R")
cat("
    model{
    # priors
    for(i in 1:p) {  
    b[i] ~ dnorm(0, 0.01)
    }

    sigma ~ dunif(0, 5)
    tau <- 1/sigma^2
    
    # likelihood
    z <- X %*% b
    for (i in 1:N){
    mu[i] <- z[i]
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

    }
    ", fill = TRUE)
sink()

## Choose the model
my_model = "modelJags.R"

## My data
dat <- waraDF
dat <- dat %>% mutate(era01 = ifelse(era == "past", 0, 1))

## My species
my_species <- "Chlorostoma funebralis"

## My data type
my_data <- "individual size"

# Prepare data for JAGS
nData <- nrow(dat)

predictedName = "size_log"
predictorNames <- "era01"
zx <- get_jags_predictors(dat, predictorNames, standardize = F)
my_col_names <- colnames(zx)

y <- as.matrix(dat[, predictedName])
x <- as.matrix(dat[, predictorNames])

# Get model matrix
my_formula = ~ zx[,1] 
X <- model.matrix(my_formula)
p <- dim(X)[2]

# Data for JAGS
data = list(
  X = X,
  y = as.double(as.vector(y)), 
  N = nData, 
  p = p
)

# Get inits
inits <- list(
  list(b = c(mean(log(y)), rep(0, p-1))),
  list(b = c(0.2, rnorm(n = p-1, 0, 0.1))))


jm = jags.model(paste("3_analyse_data/bayes_models/", my_model, sep = ""), 
                data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("b","sigma"), 
                  n.iter = n.iter, n.thin = 1)

zj = jags.samples(jm, variable.names = c("b", "p.sd", "p.mean", "p.discrep"),
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
b1_estimate <- summary(zm)$stat[1] # b1
b2_estimate <- summary(zm)$stat[2] # b2
past_estimate <- 10^(b1_estimate)
present_estimate <- 10^(b1_estimate + b2_estimate)
(present_estimate - past_estimate)/past_estimate

#Produce trace plots of the chains for model parameters. 
traceplot(zm)
densplot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Inspect zj
str(zj$b)

# Check Bayesian pvals
pvals <- c(p.mean = mean(zj$p.mean), p.sd = mean(zj$p.sd), p.discrep = mean(zj$p.discrep))
pvals

# Get proportional change
zj_b0 <- zj$b[1,,]
head(zj_b0)
zj_b1 <- zj$b[2,,]
past_size <- 10^zj_b0
present_size <- 10^(zj_b0 + zj_b1)
prop_change <- (present_size - past_size)/past_size
prop_change_vec <- as.numeric(prop_change)
prop_change_quantile <- t(quantile(prop_change_vec, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
rownames(prop_change_quantile) <- "prop_change"

# Save coda summary
coda_summary <- summary(zm)
coda_quantile <- data.frame(rbind(coda_summary$quantile, prop_change_quantile))
params <- rownames(coda_quantile)

coda_quantile <- coda_quantile %>%
  mutate(species = my_species,
         data = my_data, 
         param = params)

# Calculate on raw data
dat %>% group_by(era) %>% 
  summarise(mean = mean(size1mm)) %>% 
  spread(key = era, value = mean) %>% 
  mutate(prop_change = (present-past)/past)

##### SAVE OUTPUT #####
write.csv(x = coda_quantile, file = "3_analyse_data/bayes_output/by_species/chfu_logsize.csv")
