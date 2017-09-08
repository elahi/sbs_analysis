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

rm(list=ls(all=TRUE)) 

##### LOAD DATA #####

### Read raw data for tidal height info per sample area
source("sbs_bayes/00_sbs_bayes_data.R")
wara_groups <- waraDF %>% distinct(sp, species, sampleArea, sample_area_tidal_ht) %>% 
  mutate(group_j = as.integer(as.factor(sampleArea))) 
childs_groups <- childsDF %>% distinct(sp, species, sampleArea, sample_area_tidal_ht) %>% 
  mutate(group_j = as.integer(as.factor(sampleArea))) 
hex_groups <- hexDF %>% distinct(sp, species, sampleArea, sample_area_tidal_ht) %>% 
  mutate(group_j = as.integer(as.factor(sampleArea))) 
groups_df <- rbind(wara_groups, childs_groups, hex_groups) %>% 
  mutate(group_j = as.character(group_j))

### Read bayes objects
# Get Littorina and Lottia
hier2 <- read_csv("sbs_bayes/bayes_output/coda_quantile_hier2_normal.csv")

param_j = hier2$param

i = 1
param <- strsplit(param_j[i], "[", fixed = TRUE)[[i]][1]
for(i in 2:length(param_j)){
  param.i <- strsplit(param_j[i], "[", fixed = TRUE)[[1]][1]
  param <- c(param, param.i)
}
param

hier2 <- hier2 %>% rename(param_j = param) %>% # rename param to reflect groups
  mutate(param = param, # get new param
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
coda_quantile <- coda_quantile %>% select(-sampleArea) %>%
  left_join(., groups_df, by = c("sp", "group_j"))

##### PLOT #####

# Rename to match old code
names(coda_quantile)
coda_quantile <- coda_quantile %>% rename(tidalHeight = sample_area_tidal_ht)

coda_quantile_beta <- coda_quantile %>% filter(!is.na(tidalHeight) & param == "beta")

coda_quantile_beta %>% 
  ggplot(aes(tidalHeight, X50., shape = species)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_errorbar(aes(ymax = X97.5., 
                    ymin = X2.5.), width = 0.1, alpha = 0.5) + 
  labs(x = "Tidal height (m)", y = "Size (mm)") + 
  theme(strip.background = element_blank()) + 
  theme(legend.justification=c(1, 1), legend.position=c(0.9, 1)) + 
  theme(legend.box = "horizontal") + 
  theme(legend.box.just = "top") + 
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) + 
  guides(shape = guide_legend(title = "SPECIES",
                              title.position = "top", 
                              title.hjust = 0, 
                              title.theme = element_text(size = 8, face = "bold", angle = 0), 
                              label.theme = element_text(face = "italic", angle = 0, size = 6), 
                              direction = "vertical")) + 
  guides(color = guide_legend(title = "ERA",
                              title.position = "top", 
                              title.hjust = 0, 
                              title.theme = element_text(size = 8, face = "bold", angle = 0), 
                              label.theme = element_text(angle = 0, size = 6), 
                              direction = "vertical")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")


datJ %>% 
  ggplot(aes(sample_area_tidal_ht, size1mm, color = era)) + 
  geom_jitter(alpha = 0.25) + 
  facet_wrap(~ species)

