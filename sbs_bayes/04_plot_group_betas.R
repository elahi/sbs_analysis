################################################################################
##' @title Plot group betas
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-09-07
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD DATA #####

### Read raw data for tidal height info per sample area
source("sbs_bayes/00_sbs_bayes_data.R")

### Read bayes objects
# Get Littorina and Lottia
hier2 <- read_csv("sbs_bayes/bayes_output/coda_quantile_hier2_normal.csv")

hier2 <- hier2 %>% rename(param_j = param) %>% # rename param to reflect groups
  mutate(param = unlist(strsplit(param_j, "[", fixed = TRUE))[1], # get new param
         group_j = gsub("[^0-9]", "", param_j), 
         sampleArea = NA) # get group j

# Get Chlorostoma
pooled <- read_csv("sbs_bayes/bayes_output/coda_quantile_pooled_normal_wara.csv")
pooled <- pooled %>% 
  mutate(group_j = as.integer(as.factor(site)), 
         param_j = NA) %>% 
  rename(sampleArea = site)

# Combine datasets
coda_quantile <- rbind(hier2, pooled)

##### PLOT #####
