################################################################################
##' @title Estimating percent decline across species
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-08-30
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### PREP DATA #####

source("sbs_bayes/00_sbs_bayes_data.R")

# Choose data - Littorina
dat <- childsPast %>% filter(!is.na(size1mm))
#dat <- childsPres %>% filter(!is.na(size1mm))

# Get mean, standard deviation, and n
mu <- mean(dat$size1mm)
sgm <- sd(dat$size1mm)
N <- length(dat$size1mm)

#### GENERATE SIZE DATA USING PROBABILITY DISTRIBUTIONS #####

# Use gamma distribution

sim_gamma <- function(N){
  rgamma(n = N, shape = mu**2/sgm**2, rate = mu/sgm**2)}

x <- replicate(1000, sim_gamma(N))
dim(x)
sim_means <- apply(x, MARGIN = 1, FUN = mean)
grand_mean <- mean(sim_means)
sim_sds <- apply(x, 1, sd)
grand_sd <- sd(sim_sds)
grand_sd         
         
x <- rgamma(n = N, shape = mu**2/sgm**2, rate = mu/sgm**2)
par(mfrow = c(1,2))
hist(dat$size1mm, freq = F)
hist(x, freq = F)






# Use beta distribution
# First I need to standardize sizes by max (to get between 0 and 1)
size_prop <- dat$size1mm/(max(dat$size1mm))
summary(size_prop)

mu * ((mu * (1 - mu))/sgm**2 - 1)

(mu**2 - mu**3 - mu*sgm**2)/sgm**2


(mu - 2*mu**2 + mu**3 - sgm**2 + mu*sgm**2)/sgm**2


x <- rbeta(n = N, shape1 = (mu**2 - mu**3 - mu*sgm**2)/sgm**2, 
           shape2 = (mu - 2*mu**2 + mu**3 - sgm**2 + mu*sgm**2)/sgm**2)
par(mfrow = c(1,2))
hist(dat$size1mm, freq = F)
hist(x, freq = F)


x <- seq(0, 1, length = 100)
plot(x, dbeta(x, 0.5, 0.5), type = "l")
plot(x, dbeta(x, 5, 2), type = "l")
curve(dbeta(x,8,4),xlim=c(0,1))

dbeta(x, 1, 1)
pbeta(x, 1, 1)

## Visualization, including limit cases:
pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = sprintf("[dpq]beta(x, a=%g, b=%g)", a,b))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}
pl.beta(3,1)

pl.beta(2, 4)
pl.beta(3, 7)
pl.beta(3, 7, asp=1)
pl.beta(0.5, 0.35)

pl.beta(0, 0)   ## point masses at  {0, 1}

pl.beta(0, 2)   ## point mass at 0 ; the same as
pl.beta(1, Inf)

pl.beta(Inf, 2) ## point mass at 1 ; the same as
pl.beta(3, 0)

pl.beta(Inf, Inf)# point mass at 1/2




