################################################################################
##' @title Functions to run jags models per species and quantile removed
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-12-15
##' 
##' @log 
################################################################################

##### FUNCTIONS TO RUN LOOP FOR EACH SPECIES #####

# my_sp = "LIKE"
# species_abbrev = my_sp
# dat = childsDF
# i = 1
# my_model = "modelJags.R"
# figs_location = "3_analyse_data/bayes_output/logsize_density_tide/"
# predictedName = "size_log"
# predictorNames = c("era01", "density_m2", "tideHTm")
# my_formula = ~ zx[,1] + zx[,2] + zx[,3]
# = ~ zx[,1] * zx[,2] + zx[,1] * zx[,3]

get_jags_predictors <- function(dat, predictorNames, standardize = TRUE){
  
  # Change era to 0,1
  dat <- dat %>% mutate(era01 = ifelse(era == "past", 0, 1))

  x <- as.matrix(dat[, predictorNames])
  
  if(standardize == T){
    zx <- apply(x, MARGIN = 2, FUN = scale) 
    my_col_names <- colnames(zx)
    pred_mat <- zx
  }
  
  if(standardize == F){
    pred_mat <- x
  }
  return(pred_mat)
}


loop_coda_list <- function(dat, species_abbrev, my_model, figs_location, 
                           predictedName = "size_log",
                           predictorNames = c("era01", "density_m2", "tideHTm"), 
                           my_formula){
  
  coda_list <- list()
  
  for(i in 1:length(p_vector)){
    
    my_p = p_vector[i]
    
    ## Truncate data
    dat <- truncate_data(dat, quant = my_p)
    
    # Standardize predictors
    dat <- dat %>% mutate(era01 = ifelse(era == "past", 0, 1))
    
    nData <- nrow(dat)
    y <- as.matrix(dat[, predictedName])
    x <- as.matrix(dat[, predictorNames])

    # Standardize
    zx <- apply(x, MARGIN = 2, FUN = scale) 
    my_col_names <- colnames(zx)
    
    # Get model matrix
    X <- model.matrix(my_formula)
    p <- dim(X)[2]
    
    # Get inits
    inits <- list(
      list(b = c(mean(log(y)), rep(0, p-1))),
      list(b = c(0.2, rnorm(n = p-1, 0, 0.1))))
    
    # Data for JAGS
    data = list(
      X = X,
      y = as.double(as.vector(y)), 
      N = nData, 
      p = p
    )
    
    # # Center and standardize
    # dens_mu <- mean(dat$density_m2)
    # dens_sd <- sd(dat$density_m2)
    # dens_cent <- dat$density_m2 - dens_mu
    # dat$dens_stand <- dens_cent/dens_sd
    # 
    # # For plotting predicted values of continuous variables
    # my_prediction_length = 100
    # pred_mat <- matrix(nrow = my_prediction_length, ncol = length(my_col_names))
    # for(j in 2:length(my_col_names)){
    #   my_minZ <- min(zx[,j])
    #   my_maxZ <- max(zx[,j])
    #   my_vectorZ <- seq(my_minZ, my_maxZ, length.out = 100)
    #   pred_mat[, j] <- my_vectorZ
    # }
    # colnames(pred_mat) <- my_col_names
    # 
    # # For prediction
    # era_predict <- c(0,1)
    # pred_df2 <- expand.grid(era_predict, pred_mat[, 2]) %>% tbl_df()
    # colnames(pred_df2)[2] <- my_col_names[2]
    # 
    # pred_df3 <- expand.grid(era_predict, pred_mat[, 3]) %>% tbl_df()
    # colnames(pred_df3)[2] <- my_col_names[3]
    
    jm = jags.model(paste("3_analyse_data/bayes_models/", my_model, sep = ""), 
                    data = data, inits = inits, 
                    n.chains = length(inits), n.adapt = n.adapt)
    
    update(jm, n.iter = n.update)
    
    zm = coda.samples(jm, variable.names = c("b","sigma"), 
                      n.iter = n.iter, n.thin = 2)
    
    zj = jags.samples(jm, variable.names = c("b", "p.sd", "p.mean", "p.discrep", "y.new"),
                      n.iter = n.iter, n.thin = 2)
    
    # Save trace plots
    png(filename = paste(figs_location, "traceplot", species_abbrev, my_p, ".png", sep = "_"), 
        height = 9, width = 7, units = "in", res = 150)
    par(mfrow = c(4,2))
    traceplot(zm)
    dev.off()
    
    # Save density plots
    png(filename = paste(figs_location, "densplot", species_abbrev, my_p, ".png", sep = "_"), 
        height = 9, width = 7, units = "in", res = 150)
    par(mfrow = c(4,2))
    densplot(zm)
    dev.off()
    
    # Model fit - compare observed vs simulated
    png(filename = paste(figs_location, "model_fit", species_abbrev, my_p, ".png", sep = "_"), 
        height = 5, width = 5, units = "in", res = 150)
    hist(dat$size_log, breaks = 15, freq=FALSE)
    lines(density(zj$y.new), col="red")
    dev.off()
    
    # Test for convergence using the Gelman diagnostic.
    gd <- gelman.diag(zm, multivariate = F)[[1]]
    
    # Check Bayesian pvals
    pvals <- c(p.mean = mean(zj$p.mean), p.sd = mean(zj$p.sd), p.discrep = mean(zj$p.discrep))
    
    # # Get proportional change
    # zj_b0 <- zj$beta0
    # zj_b1 <- zj$beta1
    # past_size <- 10^zj_b0
    # present_size <- 10^(zj_b0 + zj_b1)
    # prop_change <- (present_size - past_size)/past_size
    # prop_change_quantile <- t(summary(prop_change, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))$stat)
    # rownames(prop_change_quantile) <- "prop_change"
    #
    
    # Save coda summary
    coda_summary <- summary(zm)
    coda_quantile <- data.frame(coda_summary$quantile)
    #coda_quantile <- rbind(coda_quantile, prop_change_quantile)
    params <- rownames(coda_quantile)
    summary_name <- paste("quantile", my_p, sep = "_")
    
    coda_quantile <- coda_quantile %>%
      mutate(coda_quantile = summary_name, 
             param = params)
    
    # # Make prediction dataframe
    # # Credible intervals
    # y_predict <- summary(zj$y_pred, quantile, c(0.025, 0.5, 0.975))$stat
    # pred_df$y_median <- y_predict[2, ]
    # pred_df$y_lower <- y_predict[1, ]
    # pred_df$y_upper <- y_predict[3, ]
    # 
    # # Prediction intervals
    # y_new <- summary(zj$y_new, quantile, c(0.025, 0.5, 0.975))$stat
    # pred_df$y_median_pred <- y_new[2, ]
    # pred_df$y_lower_pred <- y_new[1, ]
    # pred_df$y_upper_pred <- y_new[3, ]
    # pred_df$era <- ifelse(pred_df$era_predict == 0, "past", "present")
    
    # Create list of all desired objects
    coda_list.i <- list(coda_quantile, gd, pvals) #, pred_df)
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
