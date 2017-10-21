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
