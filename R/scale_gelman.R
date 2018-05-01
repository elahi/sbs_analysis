
## Function to standardize continuous predictors - Gelman
scale_gelman <- function(x){
  x_mu <- mean(x, na.rm = TRUE)
  x_sd <- sd(x, na.rm = TRUE)
  x_scaled <- (x - x_mu)/(2*x_sd)
}