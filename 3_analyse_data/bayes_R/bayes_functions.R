################################################################################
##' @title Functions to run jags models per species and quantile removed
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-10-30
##' 
##' @log 
################################################################################



##### FUNCTIONS TO RUN LOOP FOR EACH SPECIES #####

# my_sp = "LIKE"
# species_abbrev = my_sp
# dat = dens_childs
# i = 1
# my_model = "model_logsize_density.R"
# figs_location = "3_analyse_data/bayes_figs/diagnostic_logsize_density"

loop_coda_list <- function(dat, species_abbrev, my_model, figs_location){
  
  coda_list <- list()
  
  for(i in 1:length(p_vector)){
    
    my_p = p_vector[i]
    
    ## Truncate data
    dat <- truncate_data(dat, quant = my_p)
    
    # Standardize predictors
    dat <- dat %>% mutate(era01 = ifelse(era == "past", 0, 1))
    
    # Center and standardize
    dens_mu <- mean(dat$density_m2)
    dens_sd <- sd(dat$density_m2)
    dens_cent <- dat$density_m2 - dens_mu
    dat$dens_stand <- dens_cent/dens_sd
    
    # For plotting predicted values
    x_min <- min(dat$density_m2)
    x_max <- max(dat$density_m2)
    x_predict <- seq(x_min, x_max, length.out = 100)
    x_predict_stand <- (x_predict - dens_mu)/dens_sd
    
    # For prediction
    era_predict <- c(0,1)
    pred_df <- expand.grid(x_predict_stand, era_predict) %>% 
      rename(x_predict_stand = Var1, era_predict = Var2) %>% tbl_df()
    pred_df$x_predict <- x_predict
    
    # Get data
    data = list(
      N = nrow(dat), 
      y = as.double(dat$size_log), 
      era = as.double(dat$era01), 
      x = as.double(dat$dens_stand),
      x_predict = as.double(pred_df$x_predict_stand), 
      era_predict = as.double(pred_df$era_predict)
    )
    
    jm = jags.model(paste("3_analyse_data/bayes_models/", my_model, sep = ""), 
                    data = data, inits = inits, 
                    n.chains = length(inits), n.adapt = n.adapt)
    
    update(jm, n.iter = n.update)
    
    zm = coda.samples(jm, variable.names = c("beta0", "beta1", "beta2", "beta3","sigma"), 
                      n.iter = n.iter, n.thin = 10)
    
    zj = jags.samples(jm, variable.names = c("y_pred", "y_new", "beta0", "beta1", 
                                             "p.mean", "p.sd", "p.discrep"), 
                      n.iter = n.iter, n.thin = 10)
    
    # Save trace plots
    png(filename = paste(figs_location, "traceplot", species_abbrev, my_p, ".png", sep = "_"), 
        height = 5, width = 5, units = "in", res = 150)
    par(mfrow = c(3,2))
    traceplot(zm)
    dev.off()
    
    # Save density plots
    png(filename = paste(figs_location, "densplot", species_abbrev, my_p, ".png", sep = "_"), 
        height = 5, width = 5, units = "in", res = 150)
    par(mfrow = c(3,2))
    densplot(zm)
    dev.off()
    
    # Model fit - compare observed vs simulated
    png(filename = paste(figs_location, "model_fit", species_abbrev, my_p, ".png", sep = "_"), 
        height = 5, width = 5, units = "in", res = 150)
    hist(dat$size_log, breaks = 15, freq=FALSE)
    lines(density(zj$y_new), col="red")
    dev.off()
    
    # Test for convergence using the Gelman diagnostic.
    gd <- gelman.diag(zm, multivariate = F)[[1]]
    
    # Check Bayesian pvals
    pvals <- c(p.mean = mean(zj$p.mean), p.sd = mean(zj$p.sd), p.discrep = mean(zj$p.discrep))
    
    # Get proportional change
    zj_b0 <- zj$beta0
    zj_b1 <- zj$beta1
    past_size <- 10^zj_b0
    present_size <- 10^(zj_b0 + zj_b1)
    prop_change <- (present_size - past_size)/past_size
    prop_change_quantile <- t(summary(prop_change, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))$stat)
    rownames(prop_change_quantile) <- "prop_change"
    
    # Save coda summary
    coda_summary <- summary(zm)
    
    coda_quantile <- data.frame(rbind(coda_summary$quantile, prop_change_quantile))
    params <- rownames(coda_quantile)
    summary_name <- paste("quantile", my_p, sep = "_")
    
    coda_quantile <- coda_quantile %>%
      mutate(coda_quantile = summary_name, 
             param = params)
    
    # Make prediction dataframe
    # Credible intervals
    y_predict <- summary(zj$y_pred, quantile, c(0.025, 0.5, 0.975))$stat
    pred_df$y_median <- y_predict[2, ]
    pred_df$y_lower <- y_predict[1, ]
    pred_df$y_upper <- y_predict[3, ]
    
    # Prediction intervals
    y_new <- summary(zj$y_new, quantile, c(0.025, 0.5, 0.975))$stat
    pred_df$y_median_pred <- y_new[2, ]
    pred_df$y_lower_pred <- y_new[1, ]
    pred_df$y_upper_pred <- y_new[3, ]
    pred_df$era <- ifelse(pred_df$era_predict == 0, "past", "present")
    
    # Create list of all desired objects
    coda_list.i <- list(coda_quantile, gd, pvals, pred_df)
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

loop_pred_df <- function(coda_list, species_abbrev){
  
  # Rbind results
  coda_df <- data.frame(coda_list[[1]][[4]]) %>% 
    mutate(quant = p_vector[1], sp = species_abbrev)
  head(coda_df)
  
  for(i in 2:length(p_vector)){
    coda_df.i <- data.frame(coda_list[[i]][[4]]) %>% 
      mutate(quant = p_vector[i], sp = species_abbrev)
    coda_df <- rbind(coda_df, coda_df.i)
  }
  
  pred_df <- coda_df 
  return(pred_df)
  
}
