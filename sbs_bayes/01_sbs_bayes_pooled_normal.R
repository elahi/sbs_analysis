################################################################################
##' @title Run pooled model - normal distribution - for each sample area separately, Chlorostoma
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
source("sbs_bayes/model_pooled_normal.R")

median_change <- function(dat){
  dat_summary <- dat %>% group_by(era) %>% 
    summarise(size_median = median(size1mm, na.rm = TRUE))
  test_stat <- 1 - (dat_summary$size_median[2]/dat_summary$size_median[1])
  return(test_stat)
}

# Number of chains
n_chains <- 2

##### CHLOROSTOMA - WARA B #####
# Number of iterations
n.adapt <- 10000
n.update <- 10000
n.iter <- 10000
dat <- waraDF %>% filter(site == "Wara.B")

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
hist(log(dat$size1mm), breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Chlorostoma
coda_summary <- summary(zm)
waraB_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "CHFU", 
         param = rownames(coda_summary$quantile), 
         site = "Wara.B")

### Save coda and jags objects - Chlorostoma B
waraB_pooled_normal <- list(dat, zm, zj)
path_to_sd <- "/Volumes/sdxc1/sbs_bayes_output/"
save(waraB_pooled_normal, file = paste(path_to_sd, "waraB_pooled_normal.RData", sep = ""))

##### CHLOROSTOMA - WARA D #####
# Number of iterations
n.adapt <- 10000
n.update <- 10000
n.iter <- 10000
dat <- waraDF %>% filter(site == "Wara.D")

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
hist(log(dat$size1mm), breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Chlorostoma
coda_summary <- summary(zm)
waraD_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "CHFU", 
         param = rownames(coda_summary$quantile), 
         site = "Wara.D")

### Save coda and jags objects - Chlorostoma B
waraD_pooled_normal <- list(dat, zm, zj)
path_to_sd <- "/Volumes/sdxc1/sbs_bayes_output/"
save(waraD_pooled_normal, file = paste(path_to_sd, "waraD_pooled_normal.RData", sep = ""))

##### SAVE CODA SUMMARIES #####

### Compile and save
coda_quantile <- rbind(waraB_coda_quantile, waraD_coda_quantile)
write.csv(coda_quantile, "sbs_bayes/bayes_output/coda_quantile_pooled_normal_wara.csv")
