################################################################################
##' @title Run hierarchical intercept and slope model - normal distribution
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
source("sbs_bayes/model_hier2_normal.R")

median_change <- function(dat){
  dat_summary <- dat %>% group_by(era) %>% 
    summarise(size_median = median(size1mm, na.rm = TRUE))
  test_stat <- 1 - (dat_summary$size_median[2]/dat_summary$size_median[1])
  return(test_stat)
}

# Number of chains
n_chains <- 2

##### LOTTIA #####
# Number of iterations
n.adapt <- 10000
n.update <- 10000
n.iter <- 10000

dat <- hexDF 

start_time <- proc.time()
jm = hier2_model(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma", "mu.alpha", "mu.beta"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "mu.alpha", "mu.beta", 
                                         "y.new", "p.mean", "p.sd", "p.discrep"), 
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
hist(log(dat$size1mm), breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Lottia
coda_summary <- summary(zm)
hex_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "LODI", 
         param = rownames(coda_summary$quantile))

### Save coda and jags objects - Littorina
hex_hier2_normal <- list(dat, zm, zj)
path_to_sd <- "/Volumes/sdxc1/sbs_bayes_output/"
save(hex_hier2_normal, file = paste(path_to_sd, "hex_hier2_normal.RData", sep = ""))

##### LITTORINA #####
# Number of iterations
n.adapt <- 10000
n.update <- 10000
n.iter <- 10000

dat <- childsDF

start_time <- proc.time()
jm = hier2_model(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma", "mu.alpha", "mu.beta"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "mu.alpha", "mu.beta", 
                                         "y.new", "p.mean", "p.sd", "p.discrep"), 
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
hist(log(dat$size1mm), breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Littorina
coda_summary <- summary(zm)
childs_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "LIKE", 
         param = rownames(coda_summary$quantile))

### Save coda and jags objects - Littorina
childs_hier2_normal <- list(dat, zm, zj)
path_to_sd <- "/Volumes/sdxc1/sbs_bayes_output/"
save(childs_hier2_normal, file = paste(path_to_sd, "childs_hier2_normal.RData", sep = ""))

##### CHLOROSTOMA #####

## not run, only two sample areas

##### SAVE CODA SUMMARIES #####

### Compile and save
coda_quantile <- rbind(hex_coda_quantile, childs_coda_quantile)
write.csv(coda_quantile, "sbs_bayes/bayes_output/coda_quantile_hier2_normal.csv")

library(ggplot2)
coda_quantile %>%
  filter(param == "mu.beta") %>%
  ggplot(aes(sp, X50.)) +
  geom_point() +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) +
  geom_hline(aes(yintercept = 0), color = "gray", linetype = "dashed")

