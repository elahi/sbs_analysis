################################################################################
##' @title Paine 1971 - Tegula respiration
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-10-20
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

library(dplyr)
library(ggplot2)
library(tidyr)

source("metabolic_consequences/metabolicTheory.R")

# Import data extracted from Figure 1 - Paine 1917 - Ecology
dat <- read.csv("metabolic_consequences/metabolic_data/Paine_Fig1_data.csv")
dat

# Create size data
size_vec <- c(1, seq(5, 1000, by = 5))

# Make predictions 
resp_df <- expand.grid(size_vec, dat$tempC) %>% tbl_df()
names(resp_df) <- c("M", "tempC")
resp_df

resp_df <- dat %>% select(tempC, slope, slope_calc, y1, n) %>% 
  left_join(resp_df, ., by = "tempC")

resp_df <- resp_df %>% 
  mutate(resp = y1 * M^slope, 
         resp2 = y1 * M^slope_calc)

# Plot Paine's printed slope value (from text on figure 1)
resp_df %>% 
  ggplot(aes(M, resp, color = as.factor(tempC))) + 
  geom_line() + 
  scale_y_log10() + scale_x_log10()

# Plot using my calculate slope value (estimated from Figure 1)
# This matches the Figure (not surprisingly, since I used the calculated slopes and extrapolated y intercepts)
# More importantly, using these values, I match Paine's predicted respiration rate for an average-sized individual
resp_df %>% 
  ggplot(aes(M, resp2, color = as.factor(tempC))) + 
  geom_line() + 
  scale_y_log10() + scale_x_log10()

# Average sized individual
mean_weight <- 211 # mg

dat$mean_weight <- mean_weight
dat$Paines_resp <- c(104, 50, 39) # respiration

dat <- dat %>% 
  mutate(mean_resp_calc = y1 * mean_weight^slope_calc)

dat %>% 
  ggplot(aes(Paines_resp, mean_resp_calc)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)

# Can I estimate Q10 from my calculated predictions?
resp_df %>% filter(M == 210)
# Paine measured q10 between 6.5 and 17C
# Paine's q10 value
# Dell et al. 2011, Table S2 - Paine's calculated Q10 value is ~ Ea = 0.65eV
q10_paine <- 2.64

respQ10 <- resp_df %>% filter(tempC != 10)
respQ10 %>% arrange(M)

# Use my calculated values
my_temp_diff = 17-6.5
q10_mine <- respQ10 %>% select(M, tempC, resp2) %>% 
  spread(key = tempC, value = resp2) %>%
  mutate(q10 = (`17`/`6.5`) ^ (10/my_temp_diff))

q10_mine %>% 
  ggplot(aes(q10)) + 
  geom_density() + 
  geom_vline(xintercept = q10_paine, color = 'red')

q10_mine %>% filter(M == 210) # for an average-sized individual, I get 2.38, Paine got 2.64
resp_df %>% filter(M == 210)

## Calculate Ea 
resp_df

# 0 Celsius = 273.15 K
# Rearrange metabolic equation to solve for Ea
# mass must be in grams
# respiration must be in Joules per second
# Respiratory oxygen consumption was converted to energetic cost using the oxy-joule equivalent of 19.63 J ml O2−1 (Elliot and Davison, 1975), assuming that lipid is the main respiratory substrate (Patton et al., 1977).

resp_df <- resp_df %>% 
  mutate(tempK = tempC + 273.15, 
         M_g = M * 1000, 
         resp_J_s = resp2 * 1/60 * 1/1000 * 19.63) # convert hrs to seconds; ul to ml; Oxygen to joules 

resp_df <- resp_df %>% 
  mutate(Ea = (k * tempK)*(slope_calc*log(M_g) + log(y1) - log(resp_J_s)))

resp_df %>% 
  ggplot(aes(as.factor(tempC), Ea)) + 
  geom_boxplot()

unique(round(resp_df$Ea, 4))

## The next things to do are:
#' Convert length-frequency distribution to mass-frequency
#' Get average density for a quadrat (past and present)
#' Sample sizes randomly from probability distribution
#' Get average temperature for each month
#' Use the metabolic equation to get metabolism for each individual for each month
#' Assume Ea = 0.65 eV (consistent with Paine)
#' 
#' 

# Get a random sample of snails (from Paine, 412 individuals per m2; mean size = 211mg or 16.9 mm)
my_snail_sizes <- rnorm(n = 400, mean = 17, sd = 5)
my_snail_sizes <- my_snail_sizes[my_snail_sizes > 2]
hist(my_snail_sizes)

# Convert to biomass using Paine's combined equation (returns g dry weight)
mmTOg_tegula <- function(x) exp(-5.016 + 3.670 * log(x)) / 1000
my_snail_grams <- mmTOg_tegula(my_snail_sizes)
sum(my_snail_grams)

# Estimate respiration (in Joules) for each snail
# R = aM^bexp(-E/kT)
# ln(R) = b*ln(M) - E(1/kT) + log(a)

b = 0.86
a = 0.2
Ea = 0.65
tempK = 17 + 273.15
x = my_snail_grams

get_resp_Js <- function(x, b, a, Ea, tempK) {
  b * log(x) - Ea*(1/k*tempK) + log(a)
}

my_snail_resp <- get_resp_Js(x = x, b = b, a = a, Ea = Ea, tempK = tempK)
my_snail_resp

sum(my_snail_resp)

# 
kelvin_vec <- seq(0, 20, by = 1) + 273.15
k * kelvin_vec * ln(1)

##### TRY TO DUPLICATE PAINES KCAL ESTIMATES USING Q10 #####


