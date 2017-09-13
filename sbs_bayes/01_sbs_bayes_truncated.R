################################################################################
##' @title Run pooled model - truncated size distribution
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

## Choose model
source("sbs_bayes/model_pooled_truncated.R")

median_change <- function(dat){
  dat_summary <- dat %>% group_by(era) %>% 
    summarise(size_median = median(size1mm, na.rm = TRUE))
  test_stat <- 1 - (dat_summary$size_median[2]/dat_summary$size_median[1])
  return(test_stat)
}

# Number of iterations
n.adapt <- 1000
n.update <- 1000
n.iter <- 1000

# Number of chains
n_chains <- 2

# For prediction
era_predict <- c(0,1)
pred_df <- expand.grid(thc_predict, era_predict) %>% 
  rename(thc_predict = Var1, era_predict = Var2) %>% tbl_df()

# Function to truncate data
truncate_data <- function(dat, quant = 0.5, subtract_value = 1){
  
  my_quantile = quant
  size_threshold <- dat %>% filter(era == "past") %>% 
    summarise(size_threshold = quantile(size1mm, probs = my_quantile)) %>% unlist(use.names = FALSE)
  dat$size_threshold <- size_threshold
  
  dat <- dat %>% filter(size1mm >= size_threshold)
  
  # Need to subtract minimum size so that I can use a distribution appropriately
  dat <- dat %>%
    mutate(size_min = min(size1mm) - subtract_value, 
           size_centered = size1mm - size_min)
  
  return(dat)
  
}

# What quantile will I use for this run?
my_quantile = 0.5

##### LOTTIA #####
dat <- hexDF 
subtract_value = 0.5
dat <- truncate_data(dat, quant = my_quantile, subtract_value = subtract_value)
summary(dat$size_centered)

jm = model_jags(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", 
                                         "p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)
mean(zj$p.discrep)

# Compared observed vs simulated
hist(dat$size_centered, breaks = 10, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Lottia
coda_summary <- summary(zm)
hex_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "LODI", 
         param = rownames(coda_summary$quantile))

##### LITTORINA #####
dat <- childsDF
subtract_value = 0.1
dat <- truncate_data(dat, quant = my_quantile, subtract_value = subtract_value)
summary(dat$size_centered)

jm = model_jags(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", 
                                         "p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)
mean(zj$p.discrep)

# Compared observed vs simulated
hist(dat$size_centered, breaks = 15, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Littorina
coda_summary <- summary(zm)
childs_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "LIKE", 
         param = rownames(coda_summary$quantile))

##### CHLOROSTOMA #####
dat <- waraDF 
subtract_value = 2
dat <- truncate_data(dat, quant = my_quantile, subtract_value = subtract_value)

jm = model_jags(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", 
                                         "p.mean", "p.sd", "p.discrep"), 
                  n.iter = n.iter, n.thin = 1)

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

#Produce trace plots of the chains for model parameters. 
plot(zm)

# Test for convergence using the Gelman diagnostic.
gelman.diag(zm, multivariate = F)

# Check Bayesian pvals
mean(zj$p.mean)
mean(zj$p.sd)
mean(zj$p.discrep)

# Compared observed vs simulated
hist(dat$size_centered, breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Chlorostoma
coda_summary <- summary(zm)
childs_coda_quantile <- data.frame(coda_summary$quantile) %>% 
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


