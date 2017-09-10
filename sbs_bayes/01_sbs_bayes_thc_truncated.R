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
source("sbs_bayes/model_pooled_thc_truncated.R")

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

# For prediction
era_predict <- c(0,1)
pred_df <- expand.grid(thc_predict, era_predict) %>% 
  rename(thc_predict = Var1, era_predict = Var2) %>% tbl_df()

# Function to truncate data
truncate_data <- function(dat, quant = 0.5, subtract_value = 1){
  
  my_quantile = quant
  size0.5 <- dat %>% filter(era == "past") %>% 
    summarise(size0.5 = quantile(size1mm, probs = my_quantile)) %>% unlist(use.names = FALSE)
  dat$size0.5 <- size0.5
  
  dat <- dat %>% filter(size1mm >= size0.5)
  
  # Need to subtract minimum size so that I can use a distribution appropriately
  dat <- dat %>%
    mutate(size_min = min(size1mm) - subtract_value, 
           size_centered = size1mm - size_min)
}

##### LOTTIA #####
dat <- hexDF 
dat %>% ggplot(aes(size1mm)) + geom_histogram(binwidth = 1) + facet_wrap(~ era) 
dat <- truncate_data(dat)
dat %>% ggplot(aes(size1mm)) + geom_histogram(binwidth = 1) + facet_wrap(~ era) 
dat %>% count(sampleArea, era)
dat %>% count(era)

start_time <- proc.time()
jm = model_jags(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma", "eta", "kappa"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd", "p.discrep", 
                                         "y_pred", "beta_pred"), 
                  n.iter = n.iter, n.thin = 1)
end_time <- proc.time()
end_time - start_time 

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

# Compare with median values
dat %>% group_by(era) %>% summarise(median(size_centered))
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
hist(dat$size_centered, breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Lottia
coda_summary <- summary(zm)
hex_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "LODI", 
         param = rownames(coda_summary$quantile))


## Plot predictions
y <- summary(zj$y_pred, quantile, c(.025, .5, .975))$stat
y <- data.frame(t(y))
xy <- cbind(pred_df, y) %>% 
  mutate(era = ifelse(era_predict == 0, "past", "present"), 
         th = thc_predict + mu_th, 
         size_median = exp(X50.), 
         size_lower = exp(X2.5.), 
         size_upper = exp(X97.5.))

xy %>% 
  filter(th > 1.5 & th < 3) %>% 
  ggplot(aes(x = th, y = X50., color = era)) +
  geom_line() + 
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., color = NULL, group = era), 
              fill = "gray", alpha = 0.5) + 
  geom_jitter(data = dat, aes(sample_area_tidal_ht, size_centered, color = era), alpha = 0.5)

## Plot - beta
y <- summary(zj$beta_pred, quantile, c(.025, .5, .975))$stat
y <- data.frame(t(y))
xy <- cbind(pred_df, y) %>% 
  mutate(era = ifelse(era_predict == 0, "past", "present"), 
         th = thc_predict + mu_th, 
         size_median = exp(X50.), 
         size_lower = exp(X2.5.), 
         size_upper = exp(X97.5.))

xy %>% 
  filter(era == "present") %>%
  filter(th > 1.5 & th < 2.75) %>% 
  ggplot(aes(x = th, y = X50., color = era)) +
  geom_line() + 
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., color = NULL, group = era), 
              fill = "gray", alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")


##### LITTORINA #####
dat <- childsDF
dat <- dat %>% group_by(era) %>% mutate(era0.5 = median(size1mm)) %>% ungroup() %>% 
  filter(size1mm >= era0.5)
dat %>% ggplot(aes(size1mm, color = era)) + geom_histogram(binwidth = 1) 

# Need to subtract minimum size so that I can use a distribution appropriately
dat <- dat %>%
  mutate(size_min = min(size1mm) - 1, 
         size_centered = size1mm - size_min)
dat %>% ggplot(aes(size_centered, color = era)) + geom_histogram(binwidth = 1) 
summary(dat$size_centered)


dat <- childsDF 
dat <- truncate_data(dat)


my_quantile = 0.5
size0.5 <- dat %>% filter(era == "past") %>% 
  summarise(size0.5 = quantile(size1mm, probs = my_quantile)) %>% unlist(use.names = FALSE)
dat$size0.5 <- size0.5

dat %>%
  ggplot(aes(size1mm)) + geom_histogram(binwidth = 1) + facet_wrap(~ era) +
  geom_vline(aes(xintercept = size0.5), color = "red", linetype = "dashed")

dat <- dat %>% filter(size1mm >= size0.5)
dat %>% count(sampleArea, era)
dat %>% count(era)

dat %>%
  ggplot(aes(size1mm)) + geom_histogram(binwidth = 1) + facet_wrap(~ era)

# Need to subtract minimum size so that I can use a distribution appropriately
dat <- dat %>%
  mutate(size_min = min(size1mm) - 1, 
         size_centered = size1mm - size_min)

start_time <- proc.time()
jm = model_jags(dat = dat, iter_adapt = n.adapt, iter_update = n.update, n_chains = n_chains)
zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma", "eta", "kappa"), 
                  n.iter = n.iter, n.thin = 1)
zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd", "p.discrep", 
                                         "y_pred", "beta_pred"), 
                  n.iter = n.iter, n.thin = 1)
end_time <- proc.time()
end_time - start_time 

#Produce a summary table for the parameters. 
summary(zm)
exp(summary(zm)$stat[1]) # size intercept (past)
summary(zm)$stat[2] 

# Compare with median values
dat %>% group_by(era) %>% summarise(median(size_centered))
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
hist(dat$size_centered, breaks = 20, freq=FALSE) 
lines(density(zj$y.new), col="red")

### Save coda summary - Littorina
coda_summary <- summary(zm)
childs_coda_quantile <- data.frame(coda_summary$quantile) %>% 
  mutate(sp = "LIKE", 
         param = rownames(coda_summary$quantile))

## Plot predictions
y <- summary(zj$y_pred, quantile, c(.025, .5, .975))$stat
y <- data.frame(t(y))
xy <- cbind(pred_df, y) %>% 
  mutate(era = ifelse(era_predict == 0, "past", "present"), 
         th = thc_predict + mu_th, 
         size_median = exp(X50.), 
         size_lower = exp(X2.5.), 
         size_upper = exp(X97.5.))

xy %>% 
  filter(th > 2 & th < 8) %>% 
  ggplot(aes(x = th, y = X50., color = era)) +
  geom_line() + 
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., color = NULL, group = era), 
              fill = "gray", alpha = 0.5) + 
  geom_jitter(data = dat, aes(sample_area_tidal_ht, size_centered, color = era), alpha = 0.5)

## Plot - beta
y <- summary(zj$beta_pred, quantile, c(.025, .5, .975))$stat
y <- data.frame(t(y))
xy <- cbind(pred_df, y) %>% 
  mutate(era = ifelse(era_predict == 0, "past", "present"), 
         th = thc_predict + mu_th, 
         size_median = exp(X50.), 
         size_lower = exp(X2.5.), 
         size_upper = exp(X97.5.))

xy %>% 
  filter(era == "present") %>%
  filter(th > 1.5 & th < 2.75) %>% 
  ggplot(aes(x = th, y = X50., color = era)) +
  geom_line() + 
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., color = NULL, group = era), 
              fill = "gray", alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

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


