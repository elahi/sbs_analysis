################################################################################
##' @title Assembling scraped data, Roy 2003
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2018-02-18
##' 
##' @log Add a log here
################################################################################

##### LOAD PACKAGES #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

df <- read_csv("sbs_meta/scraped/Roy2003/Roy2003_processed.csv")
df

df %>%
  ggplot(aes(year, length_mean, color = temporalBin)) + geom_point() + 
  facet_wrap(~ species, scales = "free_y") + 
  geom_errorbar(aes(ymin = length_mean - diffMean, 
                    ymax = length_mean + diffMean), 
                width = 0.5)

##### BRIEF INTERLUDE - BACK TRANSFORMATION FOR LOG ERRORS #####

## Illustrating the problem
## https://stats.stackexchange.com/questions/123514/calculating-standard-error-after-a-log-transform

x <- rnorm(n = 1000, mean = 10, sd = 1)
se <- function(x) {sd(x) / sqrt(length(x))}
mean(x)
sd(x)
se(x)
hist(x)

z <- log(x)
mean(z)
sd(z)
se(z)

exp(mean(z)) # close to 10
exp(se(z)) # 1 - not close to 0.03!

# Instead, I need to do this:
log_mean <- mean(z)
log_sd <- sd(z)
exp(log_mean) # ok
exp(log_mean) * log_sd # ok!

exp(log_mean) * log_sd

# For the mean:
exp(log_mean) * (1 + (log_sd^2)/2)

##### BACK TRANSFORM TO MM #####

#' Roy reports confidence intervals
#' Change to standard error by dividing by 1.96
#' Change to standard deviation by multiplying SE by the square root of N
#' Exponentiate as above

df %>% distinct(site, temporalBin,  temporalBin2)

## I need the sample sizes for each mean
## From Roy 2003 Table 1
n_spp <- df %>% distinct(species) %>% 
  mutate(n_museum_field = c(1675, 322, 570, 610), 
         Museum = c(1456, 238, 91, 497), 
         Baseline_1 = Museum / 2, # Assumes equal N for two museum periods
         Baseline_2 = Museum / 2, # Assumes equal N for two museum periods 
         Present = n_museum_field - Museum, 
         Present_CNM = rep(100, 4), 
         size_threshold = c(20, 20, 50, 20))
n_spp 

n_spp_long <- n_spp %>% select(-c(n_museum_field, Museum)) %>% 
  gather(key = temporalBin2, value = sample_size, Baseline_1:Present_CNM)
n_spp_long

## Join with df
df <- inner_join(df, n_spp_long, by = c("species", "temporalBin2"))
df

## Back transform
exp(log_mean) * log_sd # sd
exp(log_mean) * (1 + (log_sd^2)/2) # mean

names(df)
df <- df %>%
  mutate(logsize_se = diffMean/2, 
         logsize_sd = logsize_se * sqrt(sample_size), 
         size_sd = exp(length_mean) * logsize_sd, 
         size_rep = exp(length_mean) * (1 + (logsize_sd^2)/2) , 
         size_upper = size_rep + size_sd, 
         size_lower = size_rep - size_sd)

df %>%
  ggplot(aes(year, size_rep, color = site)) + geom_point() + 
  facet_wrap(~ species, scales = "free_y") + 
  geom_errorbar(aes(ymin = size_lower, 
                    ymax = size_upper))

##### FORMAT TABLE FOR META-ANALYSIS #####

df_final <- df %>% 
  filter(!is.na(length_mean)) %>% 
  arrange(species, year, site) %>%
  mutate(size_original = "mean", 
         sample_size_units = "Total number of snails", 
         lat = 33.555, 
         long = -117.816, 
         era = ifelse(year < 2000, "past", "present"), 
         museum = TRUE, 
         size_error = size_sd)

names(df_final)

dfMeta <- data.frame(
  study = "Roy 2003", 
  studySub = df_final$temporalBin2, 
  fig_table = "Figure_2", 
  species = df_final$species, 
  site = df_final$site, 
  size_original = df_final$size_original, 
  size_rep = df_final$size_rep, 
  size_units = "mm", 
  size_error = df_final$size_error, 
  size_error_type = "SD", 
  time_rep = df_final$time_rep, 
  time_error = NA, 
  era = df_final$era, 
  year = df_final$year, 
  year_error = NA, 
  year_error_type = NA, 
  sample_size = df_final$sample_size, 
  sample_size_units = df_final$sample_size_units, 
  museum = df_final$museum, 
  size_threshold_mm = df_final$size_threshold, 
  latitude = df_final$lat, 
  longitude = df_final$long
)

unique(dfMeta$species)

write.csv(dfMeta, "sbs_meta/output/dfMeta_Roy2003.csv")
