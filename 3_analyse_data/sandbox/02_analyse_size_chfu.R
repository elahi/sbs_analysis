################################################################################
##' @title Analyze size - CHFU
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
source("R/truncate_data.R")
library(rjags)

##### PREPARE DATA FOR JAGS #####
statDat <- waraDF
statDat <- truncate_data(statDat, era = "past", quant = 0.05)
#statDat <- statDat %>% filter(site == "Wara.D")
#statDat <- statDat %>% filter(nest1 == "zoneD")

statDat %>% 
  ggplot(aes(era, size1mm)) + 
  geom_violin()

statDat %>% 
  ggplot(aes(era, size_log)) + 
  geom_violin()

# Get era as 0 or 1
statDat <- statDat %>% mutate(era01 = ifelse(era == "past", 0, 1))

# Center and standardize
dens_mu <- mean(statDat$density_m2)
dens_sd <- sd(statDat$density_m2)
dens_cent <- statDat$density_m2 - dens_mu
statDat$dens_stand <- dens_cent/dens_sd

# For plotting predicted values
x_min <- min(statDat$density_m2)
x_max <- max(statDat$density_m2)
x_predict <- seq(x_min, x_max, length.out = 100)
x_predict_stand <- (x_predict - dens_mu)/dens_sd

# For prediction
era_predict <- c(0,1)
pred_df <- expand.grid(x_predict_stand, era_predict) %>% 
  rename(x_predict_stand = Var1, era_predict = Var2) %>% tbl_df()
pred_df$x_predict <- x_predict

# Get data
data = list(
  N = nrow(statDat), 
  y = as.double(statDat$size1mm), 
  era = as.double(statDat$era01), 
  x = as.double(statDat$dens_stand),
  x_predict = as.double(pred_df$x_predict_stand), 
  era_predict = as.double(pred_df$era_predict) 
)

##### MODEL 1: ERA X SIZE ####

#' size ~ b0 + b1*era 

# JAGS model
sink("3_analyse_data/bayes_models/modelJags.R")
cat("
    model{
    # priors
    beta0 ~ dnorm(0, 1/10^2)
    beta1 ~ dnorm(0, 1/10^2) 
    sigma ~ dunif(0, 5)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:N){
    mu[i] <- exp(beta0 + beta1*era[i])
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
    
    }
    ", fill = TRUE)
sink()

inits = list(
  list(beta0 = 1, beta1 = 0.5, sigma = 1), 
  list(beta0 = 0.5, beta1 = -0.1, sigma = 4))

# Number of iterations
my_iterations <- 100
n.adapt <- my_iterations
n.update <- my_iterations
n.iter <- my_iterations

## Run model
jm <- jags.model("3_analyse_data/bayes_models/modelJags.R", data = data, 
                 inits = inits, n.chains = length(inits), 
                 n.adapt = n.adapt)

update(jm, n.iter = n.update)

zm = coda.samples(jm, variable.names = c("beta0", "beta1", "sigma"), 
                  n.iter = n.iter, n.thin = 10)

zj = jags.samples(jm, variable.names = c("p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 10)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # intercept
exp(2.55)
#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)
mean(zj$p.discrep)

##### FIGURES #####

# Plot prediction 
y_predict <- summary(zj$y_pred, quantile, c(0.025, 0.5, 0.975))$stat

pred_df$y_median <- y_predict[2, ]
pred_df$y_lower <- y_predict[1, ]
pred_df$y_upper <- y_predict[3, ]

pred_df$era <- ifelse(pred_df$era_predict == 0, "past", "present")

pred_df %>% 
  ggplot(aes(x_predict, y_median, color = era)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper, fill = era, color = NULL), 
              alpha = 0.5) + 
  #geom_point(data = statDat, aes(density_m2, size1mm, color = era), alpha = 0.5) + 
  geom_boxplot(data = statDat, aes(density_m2, size1mm, group = density_m2)) + 
  ggtitle("Bayesian model") + 
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01))



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
zx <- get_jags_predictors(dat, predictorNames, standardize = T)
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

10^b2_estimate
10^-b2_estimate


past_estimate <- (b1_estimate)
present_estimate <- (b1_estimate + b2_estimate)
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
