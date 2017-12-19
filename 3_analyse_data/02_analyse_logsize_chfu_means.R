################################################################################
##' @title Analyze log size - CHFU means
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-12-17
##' 
##' @log 
################################################################################

##### PACKAGES, DATA #####
source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")
library(rjags)

##### PREPARE DATA FOR JAGS #####
##' x1 = era
##' x2 = density
##' x3 = tide height

## My data
statDat <- wara_means

## My quantile for size threshold
my_quantile <- 0
statDat <- truncate_data(statDat, era = "past", quant = my_quantile)
statDat <- statDat %>% mutate(era01 = ifelse(era == "past", 0, 1))

## My species
my_species <- "CHFU"

## My data type
my_data <- "means"

# Get means and sd of continuous variables
x2_mu <- mean(statDat$density_m2)
x2_sd <- sd(statDat$density_m2)
x3_mu <- mean(statDat$tideHTm)
x3_sd <- sd(statDat$tideHTm)

# Standardize continuous variables
statDat$x2z <- as.numeric(scale(statDat$density_m2))
statDat$x3z <- as.numeric(scale(statDat$tideHTm))

make_predict_vector <- function(my_vector, predict_length = 100){
  my_min <- min(my_vector)
  my_max <- max(my_vector)
  my_vector_pred <- seq(my_min, my_max, length.out = predict_length)
  return(my_vector_pred)
}

x2z_pred <- make_predict_vector(statDat$x2z, predict_length = 100)
x3z_pred <- make_predict_vector(statDat$x3z, predict_length = 100)

# For prediction
era_predict <- c(0,1)
pred_df <- expand.grid(x2z_pred, era_predict) %>% 
  rename(x2z = Var1, x1 = Var2) %>% tbl_df()
pred_df$x3z <- 0

# Get data
data = list(
  N = nrow(statDat), 
  y = as.double(statDat$size_log),
  x1 = as.double(statDat$era01), 
  x2 = as.double(statDat$x2z),
  x3 = as.double(statDat$x3z), 
  x1_pred = as.double(pred_df$x1), 
  x2_pred = as.double(pred_df$x2z), 
  x3_pred = as.double(pred_df$x3z)
)

##### MODEL 1: ERA X DENSITY x TIDE ####

my_model <- "eraXdensityXtide"
output_location <- "3_analyse_data/bayes_output/by_species/"

# JAGS model
sink("3_analyse_data/bayes_models/modelJags.R")
cat("
    model{
    # priors
    b0 ~ dnorm(0, 1/10^2)
    b1 ~ dnorm(0, 1/10^2) 
    b2 ~ dnorm(0, 1/10^2) 
    b3 ~ dnorm(0, 1/10^2)
    b4 ~ dnorm(0, 1/10^2)
    b5 ~ dnorm(0, 1/10^2)
    b6 ~ dnorm(0, 1/10^2)
    b7 ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 5)
    
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:N){
    mu[i] <- b0 + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x1[i]*x2[i] + b5*x1[i]*x3[i] + b6*x2[i]*x3[i] + b7*x1[i]*x2[i]*x3[i]
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

inits = list(
  list(b0 = 1, b1 = 0, b2 = 0, b3 = 0, 
       b4 = 0, b5 = 0, b6 = 0, b7 = 0, sigma = 4), 
  list(b0 = 0.5, b1 = 0.1, b2 = -0.1, b3 = 0.5, 
       b4 = -0.05, b5 = 0.2, b6 = 0, b7 = 0, sigma = 2), 
  list(b0 = 2, b1 = -0.2, b2 = 0.3, b3 = -0.1, 
       b4 = 0.1, b5 = 0.8, b6 = -0.5, b7 = 0.3, sigma = 1))

# Number of iterations
n.adapt <- 1000
n.update <- 1000
n.iter <- 1000

## Run model
jm <- jags.model("3_analyse_data/bayes_models/modelJags.R", data = data, 
                 inits = inits, n.chains = length(inits), 
                 n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("b0", 
                                         "b1", "b2", "b3", "b4", "b5", "b6", "b7", 
                                         "sigma"), 
                  n.iter = n.iter, n.thin = 10)

zj = jags.samples(jm, variable.names = c("b0", "b1","p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 10)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # intercept

# Save trace plots
trace_file <-  paste(output_location, my_species, my_data, 
                     my_model, my_quantile, "trace", sep = "_")
png(filename = paste(trace_file, "png", sep = "."), 
    height = 5, width = 5, units = "in", res = 150)
par(mfrow = c(3,3))
traceplot(zm)
dev.off()

# Save density plots
density_file <-  paste(output_location, my_species, my_data, 
                       my_model, my_quantile, "dens", sep = "_")
png(filename = paste(density_file, "png", sep = "."), 
    height = 5, width = 5, units = "in", res = 150)
par(mfrow = c(3,3))
densplot(zm)
dev.off()

# Test for convergence using the Gelman diagnostic.
gd <- gelman.diag(zm, multivariate = F)[[1]]

# Check Bayesian pvals
pvals <- c(p.mean = mean(zj$p.mean), p.sd = mean(zj$p.sd), 
           p.discrep = mean(zj$p.discrep))

# Get proportional change [assumes all other variable are at mean]
str(zj)
zj_b0 <- zj$b0
head(zj_b0)
zj_b1 <- zj$b1
past_size <- 10^zj_b0
present_size <- 10^(zj_b0 + zj_b1)
prop_change <- (present_size - past_size)/past_size
prop_change_vec <- as.numeric(prop_change)
prop_change_quantile <- t(quantile(prop_change_vec, 
                                   probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
rownames(prop_change_quantile) <- "prop_change"

# Save coda summary
coda_summary <- summary(zm)
coda_quantile <- data.frame(rbind(coda_summary$quantile, prop_change_quantile))
params <- rownames(coda_quantile)

coda_quantile <- coda_quantile %>%
  mutate(spp = my_species,
         data = my_data, 
         model = my_model, 
         param = params)

##### SAVE OUTPUT #####
my_file <-  paste(output_location, my_species, my_data, my_model, my_quantile, sep = "_")

## Coda quantile summary
write.csv(x = coda_quantile, file = paste(my_file, "csv", sep = "."))

## Pvals
write.csv(x = pvals, file = paste(my_file, "pvals",  "csv", sep = "."))

## Gelman
write.csv(x = gd, file = paste(my_file, "gd", "csv", sep = "."))

