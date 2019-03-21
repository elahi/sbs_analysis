#################################################
# Author: Robin Elahi
# Date: 190320
# Analysis of historical and modern gastropod body sizes
# Rethinking, rstan
#################################################

source("sbs_meta/01_assemble_raw_data.R")
library(rethinking)


###### ALL DATA #####

df <- df %>% 
  filter(!is.na(size1mm)) %>% 
  mutate(size_log = log(size1mm), 
         species2 = paste(species, fig_legend, sep = "_"))

df %>% count(study, species)

min_samples <- df %>% count(species2, era) %>% arrange(n) %>% 
  slice(1) %>% .$n

stat_dat <- df
stat_dat <- dfsub

## Subsample 74 observations from each species2 x era combination
stat_dat <- df %>% 
  group_by(species2, era) %>% 
  sample_n(min_samples) %>% 
  ungroup()

## Format for Stan
stat_dat <- as.data.frame(stat_dat)
stat_dat <- stat_dat %>% 
  mutate(species_index = coerce_index(species2), 
         era_01 = ifelse(era == "past", 0, 1))
stat_dat %>% count(species_index, era_01)

species_index_df <- stat_dat %>% distinct(species2, species_index)

###### RETHINKING #####

m1 <- alist(
  size_log ~ dnorm(mu, sigma), 
  mu <- alpha[species_index] + beta * era_01, 
  alpha[species_index] ~ dnorm(2, 2), 
  beta ~ dnorm(0, 1), 
  sigma ~ dcauchy(0, 2)
)

m1_map <- map(m1, data = stat_dat)
plot(precis(m1_map, depth = 2))

m2 <- alist(
  size_log ~ dnorm(mu, sigma), 
  mu <- alpha[species_index] + beta[species_index] * era_01, 
  c(alpha, beta)[species_index] ~ dmvnorm2( c(a, b), sigma_species, Rho), 
  a ~ dnorm(2, 2), 
  b ~ dnorm(0, 1), 
  sigma_species ~ dcauchy(0, 1), 
  sigma ~ dcauchy(0, 1), 
  Rho ~ dlkjcorr(2)
)

m2_stan <- map2stan(m2, data = stat_dat, 
                    iter = 1000, chains = 1, cores = 2)

precis(m2_stan)
plot(precis(m2_stan, depth = 2))
plot(m2_stan)
stancode(m2_stan)

##### TRY UNCORRELATED SLOPES AND INTERCEPTS #####

m3 <- alist(
  size_log ~ dnorm(mu, sigma), 
  mu <- alpha[species_index] + beta[species_index] * era_01, 
  alpha[species_index] ~ dnorm( a, sigma_a), 
  beta[species_index] ~ dnorm( b, sigma_b), 
  a ~ dnorm(2, 2), 
  b ~ dnorm(0, 1), 
  c(sigma_a, sigma_b) ~ dcauchy(0, 1), 
  sigma ~ dcauchy(0, 1)
)

m3_stan <- map2stan(m3, data = stat_dat, 
                    iter = 1000, chains = 1, cores = 2)

compare(m2_stan, m3_stan)
plot(precis(m3_stan))

##### PLOT SPECIES BETAS #####

## Extract samples
post <- extract.samples(m2_stan)
str(post)

beta_species <- post$beta
head(beta_species)
betas_median <- apply(beta_species, 2, median)
betas_hpdi <- apply(beta_species, 2, HPDI)

beta_df <- rbind(betas_median, betas_hpdi)
beta_df <- as.data.frame(t(beta_df)) 
beta_df
names(beta_df) <- c("median", "lower", "upper")
beta_df$species_index <- seq(1:13)

beta_df <- left_join(beta_df, species_index_df)

my_bar_width <- 0

beta_df %>% 
  ggplot(aes(species2, median)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = my_bar_width, size = 0.25) + 
  coord_flip()
  

