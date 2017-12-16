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
source("R/truncate_data.R")
source("3_analyse_data/bayes_R/bayes_functions_general.R")

library(broom)
library(ggplot2)
library(cowplot)

##### SET UP JAGS MODEL #####
# load jags
library(rjags)

## Iterations (5000 may not be enough for Wara)
n.adapt = 1000
n.update = 1000
n.iter = 1000
p_vector = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)

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

## Choose the folder to output diagnostic plots
output_location = "3_analyse_data/bayes_output/logsize_density_tide/"

##### LODI #####
my_sp <- "LODI"

# Run models and save diagnostic plots
coda_list <- loop_coda_list(hexDF, species_abbrev = my_sp, my_model = my_model, 
                            figs_location = output_location, 
                            my_formula = ~ zx[,1] + zx[,2] + zx[,3])

# Create and rename diagnostic summary
gelman_df_hex <- loop_gelman_df(coda_list, species_abbrev = my_sp)

# Create and rename pvalue summary
pval_df_hex <- loop_pval_df(coda_list, species_abbrev = my_sp)

# Create and rename coda summary
coda_df_hex <- loop_coda_df(coda_list, species_abbrev = my_sp)

# Create and rename prediction dataframe
#pred_df_hex <- loop_pred_df(coda_list, species_abbrev = my_sp)


##### FIGURES #####

coda_df_hex %>% 
  filter(!param %in% c("sigma", "b[1]")) %>% 
  ggplot(aes(param, X50.)) + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) + 
  geom_point() + 
  coord_flip() + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") + 
  labs(x = "Parameter", 
       y = "Standardized estimate") + 
  facet_wrap(~ quant)
ggsave("3_analyse_data/bayes_figs/caterpillar1.pdf")
