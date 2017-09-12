################################################################################
##' @title Run pooled model - lognormal distribution - quantile
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-09-04
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

source("sbs_bayes/00_sbs_bayes_data.R")
source("sbs_bayes/model_pooled_lognormal_quantile.R")

median_change <- function(dat){
  dat_summary <- dat %>% group_by(era) %>% 
    summarise(size_median = median(size1mm, na.rm = TRUE))
  test_stat <- 1 - (dat_summary$size_median[2]/dat_summary$size_median[1])
  return(test_stat)
}

# Number of iterations
n.adapt <- 3000
n.update <- 3000
n.iter <- 3000

# Number of chains
n_chains <- 2

##### LOTTIA #####
dat <- hexDF 
dat$p <- 0.5

start_time <- proc.time()
jm = pooled_model(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)
end_time <- proc.time()
end_time - start_time 

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

# median, orginal: alpha = 2.54; beta = -0.25
# 0.5, p-method: alpha = 2.7; beta = -0.24 # not the same!!!


# Compare with median values
dat %>% group_by(era) %>% summarise(median(size1mm))
median_change(dat)

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)
mean(zj$p.discrep)

# Compared observed vs simulated
hist(log(dat$size1mm), breaks = 20, freq=FALSE) 
hist(dat$size1mm, breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Lottia
coda_summary <- summary(zm)
hex_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "LODI", 
         param = rownames(coda_summary$quantile))

##### LITTORINA #####
dat <- childsDF #%>% filter(sampleArea == "HighRock_zoneD")

# Choose quantile
dat$p <- 0.75

start_time <- proc.time()
jm = pooled_model(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)
end_time <- proc.time()
end_time - start_time 

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

## Pmethod (0.25): alpha = 2.27; beta = -0.6
## Pmethod (0.5): alpha = 2.45; beta = -0.51
## Pmethod (0.75): alpha = 2.64; beta = -0.35
## Pmethod (0.9): alpha = 2.62; beta = 0.16

# Compare with median values
dat %>% group_by(era) %>% summarise(median(size1mm))
median_change(dat)

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)
mean(zj$p.discrep)

# Compared observed vs simulated
hist(log(dat$size1mm), breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Littorina
coda_summary <- summary(zm)
childs_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "LIKE", 
         param = rownames(coda_summary$quantile))

##### CHLOROSTOMA #####
dat <- waraDF

start_time <- proc.time()
jm = pooled_model(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)
end_time <- proc.time()
end_time - start_time 

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

# Compare with median values
dat %>% group_by(era) %>% summarise(median(size1mm))
median_change(dat)

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)
mean(zj$p.discrep)

# Compared observed vs simulated
hist(dat$size1mm, breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Chlorostoma
wara_coda_summary <- summary(zm)
wara_coda_quantile <- data.frame(wara_coda_summary$quantile) %>% 
  mutate(sp = "CHFU", 
         param = c("alpha", "beta", "sigma"))

coda_summary <- summary(zm)
wara_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "CHFU", 
         param = rownames(coda_summary$quantile))

##### SAVE CODA SUMMARIES #####

### Compile and save
coda_quantile <- rbind(hex_coda_quantile, childs_coda_quantile, wara_coda_quantile)
write.csv(coda_quantile, "sbs_bayes/bayes_output/coda_quantile_pooled_lognormal_median.csv")

library(ggplot2)
coda_quantile %>%
  filter(param == "beta") %>%
  ggplot(aes(sp, X50.)) +
  geom_point() +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) +
  geom_hline(aes(yintercept = 0), color = "gray", linetype = "dashed")


