################################################################################
##' @title Run pooled model - quantiles, normal distribution
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-09-04
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### GET DATA #####

library(broom)
library(dplyr)
library(tidyr)
library(ggplot2)

source("sbs_bayes/00_sbs_bayes_data.R")

# For prediction
era_predict <- c(0,1)
pred_df <- expand.grid(thc_predict, era_predict) %>% 
  rename(thc_predict = Var1, era_predict = Var2) %>% tbl_df()

##### SET UP JAGS MODEL #####
# load jags
library(rjags)

inits = list(
  list(
    alpha = runif(1, 0, 5), 
    beta = runif(1, -10, 10), 
    sigma = 1), 
  list(
    alpha = runif(1, 0, 5), 
    beta = runif(1, -10, 10), 
    sigma = 1))

sink("sbs_bayes/models/model_jags.R")
cat(" 
    model{
    # priors
    alpha ~ dnorm(0, 1/5^2) 
    beta ~ dnorm(0, 1/10^2)
    sigma ~ dunif(0, 100)
    tau <- 1/sigma^2
    
    # likelihood
    for (i in 1:k){
    mu[i] <- alpha + beta * era[i]
    
    w[i]  ~ dexp(tau) # exponential distribution
    me[i] <- (1 - 2 * p) / (p * (1 - p)) * w[i] + mu[i] # if p = 0.5, then this just leaves mu[i]
    pe[i] <- (p * (1 - p) * tau) / (2 * w[i]) # 
    
    y[i] ~ dnorm(me[i], pe[i])
    y.new[i] ~ dnorm(me[i], pe[i])
    }
    
    # bayesian p-values
    sd.data <- sd(y)
    sd.new <- sd(y.new)
    p.sd <- step(sd.new - sd.data)
    
    mean.data <- mean(y)
    mean.new  <- mean(y.new)
    p.mean <- step(mean.new - mean.data)
    
    }
    ", fill = TRUE)
sink()

## Iterations
n.adapt = 1000
n.update = 1000
n.iter = 1000
p_vector = c(0.1, 0.25, 0.5, 0.75, 0.9)
p_vector = c(0.5, 0.75)

##### FUNCTION TO RUN LOOP FOR EACH SPECIES #####

loop_coda_list <- function(dat, species_abbrev){
  
  coda_list <- list()
  dat <- dat %>% mutate(size_log = log(size1mm))

  for(i in 1:length(p_vector)){
    
    my_p = p_vector[i]
    
    ## Data
    data = list(
      y = as.double(dat$size_log),
      era = as.double(dat$eraJ),
      k = as.double(length(dat$size1mm)),
      p = as.double(my_p)
    )
    
    jm = jags.model("sbs_bayes/models/model_JAGS.R", data = data, inits = inits, 
                    n.chains = length(inits), n.adapt = n.adapt)
    update(jm, n.iter = n.update)
    zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"),
                      n.iter = n.iter, n.thin = 1)
    zj = jags.samples(jm, variable.names = c("y.new", "p.mean", "p.sd"),
                      n.iter = n.iter, n.thin = 1)
    
    # Save diagnostic plots
    png(filename = paste("sbs_bayes/bayes_figs/diagnostic_quantile_normal/", 
                         "convergence", species, my_p, ".png", sep = "_"), 
        height = 5, width = 5, units = "in", res = 150)
    plot(zm)
    dev.off()
    
    # Model fit - compare observed vs simulated
    png(filename = paste("sbs_bayes/bayes_figs/diagnostic_quantile_normal/", 
                         "model_fit", species, my_p, ".png", sep = "_"), 
        height = 5, width = 5, units = "in", res = 150)
    hist(dat$size_log, breaks = 15, freq=FALSE)
    lines(density(zj$y.new), col="red")
    dev.off()
    
    # Test for convergence using the Gelman diagnostic.
    gd <- gelman.diag(zm, multivariate = F)[[1]]
    
    # Check Bayesian pvals
    pvals <- c(p.mean = mean(zj$p.mean), p.sd = mean(zj$p.sd))
    
    # Save coda summary
    coda_summary <- summary(zm)
    summary_name <- paste("quantile", my_p, sep = "_")
    coda_quantile <- data.frame(coda_summary$quantile) %>%
      mutate(coda_quantile = summary_name,
             param = rownames(coda_summary$quantile))
    
    # Create list of all desired objects
    coda_list.i <- list(coda_quantile, gd, pvals)
    coda_list[[i]] <- coda_list.i
    
  }
  
  return(coda_list)
}

loop_coda_df <- function(coda_list, species_abbrev){
  
  # Rbind results
  coda_df <- coda_list[[1]][[1]]
  for(i in 2:length(p_vector)){
    coda_df <- rbind(coda_df, coda_list[[i]][[1]])
  }
  
  cc       <- strsplit(coda_df$coda_quantile, split = "_")
  part1    <- unlist(cc)[2*(1:length(coda_df$coda_quantile))-1]
  part2    <- unlist(cc)[2*(1:length(coda_df$coda_quantile))  ]
  coda_df <- coda_df %>% mutate(quant = part2, sp = species_abbrev)
  
  return(coda_df)
  
}

loop_gelman_df <- function(coda_list, species_abbrev){
  
  # Rbind results
  coda_df <- coda_list[[1]][[2]]
  for(i in 2:length(p_vector)){
    coda_df <- rbind(coda_df, coda_list[[i]][[2]])
  }
  gelman_df <- as_data_frame(coda_df) %>% 
    mutate(param = rownames(coda_df))
  names(gelman_df)[1:2] <- c("point_estimate", "upper_ci")
  n_rows <- dim(coda_list[[1]][[2]])[1]
  gelman_df$quant <- sort(rep(p_vector, n_rows))
  gelman_df$sp <- species_abbrev
  
  return(gelman_df)
  
}

loop_pval_df <- function(coda_list, species_abbrev){
  # Rbind results
  coda_df <- data.frame(t(coda_list[[1]][[3]]))
  for(i in 2:length(p_vector)){
    coda_df <- rbind(coda_df, data.frame(t(coda_list[[i]][[3]])))
  }
  pval_df<- coda_df %>% mutate(quant = p_vector, sp = species_abbrev)
  return(pval_df)
}

##### RUN MODELS - LOTTIA #####
my_sp <- "LODI"

# Run models and save diagnostic plots
coda_list <- loop_coda_list(hexDF, species_abbrev = my_sp)
coda_list

# Create and rename diagnostic summary
gelman_df_hex <- loop_gelman_df(coda_list, species_abbrev = my_sp)
gelman_df_hex

# Create and rename pvalue summary
pval_df_hex <- loop_pval_df(coda_list, species_abbrev = my_sp)
pval_df_hex

# Create and rename coda summary
coda_df_hex <- loop_coda_df(coda_list, species_abbrev = my_sp)
coda_df_hex

##### RUN MODELS - LITTORINA #####
coda_list <- list()
dat <- childsDF %>% mutate(size_log = log(size1mm))

for(i in 1:length(p_vector)){
  
  my_p = p_vector[i]
  
  ## Data
  data = list(
    y = as.double(dat$size_log),
    era = as.double(dat$eraJ),
    k = as.double(length(dat$size1mm)),
    p = as.double(my_p)
  )
  
  jm = jags.model("sbs_bayes/models/model_JAGS.R", data = data, inits = inits, 
                  n.chains = length(inits), n.adapt = n.adapt)
  update(jm, n.iter = n.update)
  zm = coda.samples(jm, variable.names = c("alpha", "beta", "sigma"),
                    n.iter = n.iter, n.thin = 1)
  zj = jags.samples(jm, variable.names = c("alpha", "beta", "sigma", "y.new", "p.mean", "p.sd"),
                    n.iter = n.iter, n.thin = 1)
  
  # Test for convergence using the Gelman diagnostic.
  gd <- gelman.diag(zm, multivariate = F)[[1]]
  
  # Check Bayesian pvals
  pvals <- c(p.mean = mean(zj$p.mean), p.sd = mean(zj$p.sd))
  
  # Save coda summary
  coda_summary <- summary(zm)
  summary_name <- paste("quantile", my_p, sep = "_")
  coda_quantile <- data.frame(coda_summary$quantile) %>%
    mutate(coda_quantile = summary_name,
           param = rownames(coda_summary$quantile))
  
  # Create list of all desired objects
  coda_list.i <- list(coda_quantile, gd, pvals)
  coda_list[[i]] <- coda_list.i
  
}

# Compared observed vs simulated
hist(dat$size_log, breaks = 15, freq=FALSE, ylim = c(0,5))
lines(density(zj$y.new), col="red")

# Create and rename coda summary
coda_df <- loop_coda_df(coda_list)
coda_df_hex <- coda_df

##### RUN MODELS - CHLOROSTOMA #####
my_sp <- "CHFU"

# Run models and save diagnostic plots
coda_list <- loop_coda_list(waraDF, species_abbrev = my_sp)

# Create and rename diagnostic summary
gelman_df_wara <- loop_gelman_df(coda_list, species_abbrev = my_sp)

# Create and rename pvalue summary
pval_df_wara <- loop_pval_df(coda_list, species_abbrev = my_sp)

# Create and rename coda summary
coda_df_wara <- loop_coda_df(coda_list, species_abbrev = my_sp)


##### PLOT BETAS #####
coda_df_childs$sp <- "LIKE"
coda_df_hex$sp <- "LODI"
coda_df_wara$sp <- "CHFU"
coda_df_all <- rbind(coda_df_wara, coda_df_hex, coda_df_childs)
write.csv(coda_df_all, "sbs_bayes/bayes_output/truncated_normal_coda_df_all.csv")

coda_df %>% 
  filter(param == "beta") %>% 
  ggplot(aes(as.numeric(quant), X50.)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  ylab("Proportional change in size (mm)") + 
  xlab("Quantile")

# jitter points a bit
jitter_value = 0.01
coda_df_all <- coda_df_all %>% 
  mutate(quant = as.numeric(quant), 
         quant2 = ifelse(sp == "CHFU", quant - jitter_value, 
                         ifelse(sp == "LIKE", quant + jitter_value, quant)))

coda_df_all %>% 
  filter(param == "beta") %>% 
  ggplot(aes(quant2, X50., shape = sp)) + 
  geom_point() + 
  geom_line(aes(linetype = sp)) + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  ylab("Proportional change in size (mm)") + 
  xlab("Size threshold (quantile of past size)") + 
  theme(legend.position = "top")

ggsave("sbs_bayes/bayes_figs/truncated_normal_beta_quantile_plot.png", height = 3.5, width = 3.5)

