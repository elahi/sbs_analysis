################################################################################
##' @title Analyze logsize-density-tide
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-12-15
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

##### PACKAGES, DATA #####

source("3_analyse_data/01_sbs_bayes_data.R")

library(broom)
library(ggplot2)
library(cowplot)

statDat <- dat_dens 

statDat %>% 
  distinct(sp, site, era, tideHTm, density_m2, dens_log) %>% 
  ggplot(aes(density_m2, fill = era)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~sp, scales = "free")

statDat %>% 
  distinct(sp, site, era, tideHTm, density_m2, dens_log) %>% 
  ggplot(aes(dens_log, fill = era)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~sp, scales = "free")

# Subset data by species
dens_wara <- statDat %>% filter(sp == "CHFU")
dens_hex <- statDat %>% filter(sp == "LODI")
dens_childs <- statDat %>% filter(sp == "LIKE")

dens_wara %>% select(density_m2, tideHTm, size1mm) %>% Mypairs()
dens_hex %>% select(density_m2, tideHTm, size1mm) %>% Mypairs()
dens_childs %>% select(density_m2, tideHTm, size1mm) %>% Mypairs()
wara_means %>% select(density_m2, tideHTm, size_mm) %>% Mypairs()

##### PREPARE DATA FOR JAGS #####
library(rjags)

# Choose data to models
statDat <- dens_childs %>% filter(!is.na(dens_log) & !is.na(mass_log))
statDat <- dens_wara %>% filter(!is.na(dens_log) & !is.na(mass_log))
statDat <- dens_hex %>% filter(!is.na(dens_log) & !is.na(mass_log))

statDat <- wara_means %>% filter(!is.na(dens_log) & !is.na(mass_log))

# Get era as 0 or 1
statDat <- statDat %>% mutate(era01 = ifelse(era == "past", 0, 1))

predictedName <- "size_log"
predictorNames <- c("era01", "density_m2", "tideHTm")
nData <- nrow(statDat)
y <- as.matrix(statDat[, predictedName])
x <- as.matrix(statDat[, predictorNames])
nPredictors <- ncol(x)

# Standardize
zx <- apply(x, MARGIN = 2, FUN = scale) 

# # Data for JAGS
# data = list(
#   x = zx,
#   y = as.double(as.vector(y)), 
#   N = nData, 
#   nPredictors = nPredictors
# )
# 
# inits = list(
#   list(b0 = 0, b = rep(0, nPredictors), sigma = 2, bx12 = 0, bx13 = 0), 
#   list(b0 = 1, b = rep(-0.5, nPredictors), sigma = 2, bx12 = 0.2, bx13 = -0.2))
# 
# # JAGS model
# sink("3_analyse_data/bayes_models/modelJags.R")
# cat("
#     model{
#     # priors
#     b0 ~ dnorm(0, 1/10^2)
#     bx12 ~ dnorm(0, 1/10^2)
#     bx13 ~ dnorm(0, 1/10^2)
# 
#     for(j in 1:nPredictors){
#       b[j] ~ dnorm(0, 1/10^2)
#     }
#     sigma ~ dunif(0, 5)
#     tau <- 1/sigma^2
#     
#     # likelihood
#     for (i in 1:N){
#     mu[i] <- b0 + inprod(b[], x[i,]) + bx12*x[i,]*x[i,] + bx13*x[1,]*x[3,]
#     y[i] ~ dnorm(mu[i], tau) 
#     }
# 
#     }
#     ", fill = TRUE)
# sink()

## Different method

X <- model.matrix(~ zx[,1] * zx[,2] + zx[,1] * zx[,3])
X
p <- dim(X)[2]
p

# Data for JAGS
data = list(
  X = X,
  y = as.double(as.vector(y)), 
  N = nData, 
  p = p
)

inits <- list(
  list(b = c(mean(log(y)), rep(0, p-1))),
  list(b = c(0.2, rnorm(n = p-1, 0, 0.1))))
inits           

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

# Number of iterations
n.adapt <- 1000
n.update <- 1000
n.iter <- 1000

## Run model
jm <- jags.model("3_analyse_data/bayes_models/modelJags.R", 
                 data = data, 
                 inits = inits, n.chains = length(inits), 
                 n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("b","sigma"), 
                  n.iter = n.iter, n.thin = 10)

zj = jags.samples(jm, variable.names = c("b", "p.sd", "p.mean", "p.discrep"),
                  n.iter = n.iter, n.thin = 10)

#Produce a summary table for the parameters. 
summary(zm)
10^(summary(zm)$stat[1]) # intercept

#Produce trace plots of the chains for model parameters. 
traceplot(zm)
densplot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
pvals <- c(p.mean = mean(zj$p.mean), p.sd = mean(zj$p.sd), p.discrep = mean(zj$p.discrep))
pvals

##### FIGURES #####

zmq <- summary(zm)$quantile
rownames(zmq)

zmq_df <- as_data_frame(zmq) %>% 
  mutate(param = rownames(zmq))
zmq_df

zmq_df %>% 
  filter(!param %in% c("sigma", "b[1]")) %>% 
  ggplot(aes(param, `50%`)) + 
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`)) + 
  geom_point() + 
  coord_flip() + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") + 
  labs(x = "Parameter", 
       y = "Standardized estimate")
