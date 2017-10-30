################################################################################
##' @title Analyze logsize-density raw
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-10-30
##' 
##' @log 
################################################################################

##### PACKAGES, DATA #####

source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")
source("3_analyse_data/bayes_R/bayes_functions.R")

library(broom)
library(ggplot2)
library(cowplot)

##### SET UP JAGS MODEL #####
# load jags
library(rjags)

inits = list(
  list(beta0 = 1, beta1 = 0.25, beta2 = 0.1, beta3 = 1, sigma = 2), 
  list(beta0 = 1.5, beta1 = -0.25, beta2 = -0.1, beta3 = 0, sigma = 0.2), 
  list(beta0 = 0.5, beta1 = -0.1, beta2 = -0.4, beta3 = -1, sigma = 0.01))

## Iterations (5000 may not be enough for Wara)
n.adapt = 3000
n.update = 3000
n.iter = 3000
p_vector = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)

#p_vector = c(0, 0.5)

## Choose the model
my_model = "model_logsize_density.R"
## Choose the folder to output diagnostic plots
output_location = "3_analyse_data/bayes_output/logsize_density/"

##### RUN MODELS - LOTTIA #####
my_sp <- "LODI"

# Run models and save diagnostic plots
coda_list <- loop_coda_list(hexDF, species_abbrev = my_sp, my_model = my_model, figs_location = output_location)

# Create and rename diagnostic summary
gelman_df_hex <- loop_gelman_df(coda_list, species_abbrev = my_sp)

# Create and rename pvalue summary
pval_df_hex <- loop_pval_df(coda_list, species_abbrev = my_sp)

# Create and rename coda summary
coda_df_hex <- loop_coda_df(coda_list, species_abbrev = my_sp)

# Create and rename prediction dataframe
pred_df_hex <- loop_pred_df(coda_list, species_abbrev = my_sp)

##### RUN MODELS - LITTORINA #####
my_sp <- "LIKE"

# Run models and save diagnostic plots
coda_list <- loop_coda_list(childsDF, species_abbrev = my_sp, my_model = my_model, figs_location = output_location)

# Create and rename diagnostic summary
gelman_df_childs <- loop_gelman_df(coda_list, species_abbrev = my_sp)

# Create and rename pvalue summary
pval_df_childs <- loop_pval_df(coda_list, species_abbrev = my_sp)

# Create and rename coda summary
coda_df_childs <- loop_coda_df(coda_list, species_abbrev = my_sp)

# Create and rename prediction dataframe
pred_df_childs <- loop_pred_df(coda_list, species_abbrev = my_sp)

##### RUN MODELS - CHLOROSTOMA #####
my_sp <- "CHFU"

# Run models and save diagnostic plots
coda_list <- loop_coda_list(waraDF, species_abbrev = my_sp, my_model = my_model, figs_location = output_location)

# Create and rename diagnostic summary
gelman_df_wara <- loop_gelman_df(coda_list, species_abbrev = my_sp)

# Create and rename pvalue summary
pval_df_wara <- loop_pval_df(coda_list, species_abbrev = my_sp)

# Create and rename coda summary
coda_df_wara <- loop_coda_df(coda_list, species_abbrev = my_sp)

# Create and rename prediction dataframe
pred_df_wara <- loop_pred_df(coda_list, species_abbrev = my_sp)

##### RUN MODELS - CHLOROSTOMA - means #####
my_sp <- "CHFU_means"

# Run models and save diagnostic plots
coda_list <- loop_coda_list(wara_means, species_abbrev = my_sp, my_model = my_model, figs_location = output_location)

# Create and rename diagnostic summary
gelman_df_wara_means <- loop_gelman_df(coda_list, species_abbrev = my_sp)

# Create and rename pvalue summary
pval_df_wara_means <- loop_pval_df(coda_list, species_abbrev = my_sp)

# Create and rename coda summary
coda_df_wara_means <- loop_coda_df(coda_list, species_abbrev = my_sp)

# Create and rename prediction dataframe
pred_df_wara_means <- loop_pred_df(coda_list, species_abbrev = my_sp)

##### SAVE OUTPUT #####
coda_df_childs$sp <- "LIKE"
coda_df_hex$sp <- "LODI"
coda_df_wara$sp <- "CHFU"
coda_df_wara_means$sp <- "CHFU_means"

coda_df_all <- rbind(coda_df_wara, coda_df_wara_means, coda_df_hex, coda_df_childs)
write.csv(coda_df_all, paste(output_location, "coda_df_all.csv", sep = ""))

gelman_df_all <- rbind(gelman_df_wara, gelman_df_wara_means, gelman_df_hex, gelman_df_childs)
write.csv(gelman_df_all, paste(output_location, "gelman_df_all.csv", sep = ""))

pval_df_all <- rbind(pval_df_wara, pval_df_wara_means, pval_df_hex, pval_df_childs)
write.csv(pval_df_all, paste(output_location, "pval_df_all.csv", sep = ""))

pred_df_all <- rbind(pred_df_wara, pred_df_wara_means, pred_df_hex, pred_df_childs)
write.csv(pred_df_all, paste(output_location, "pred_df_all.csv", sep = ""))

##### PRELIM PLOTS #####
# jitter points a bit
jitter_value = 0.01
coda_df_all <- coda_df_all %>% 
  mutate(quant = as.numeric(quant), 
         quant2 = ifelse(sp == "CHFU", quant - jitter_value, 
                         ifelse(sp == "LIKE", quant + jitter_value, quant)))

coda_df_all %>% 
  filter(param == "prop_change") %>% 
  filter(sp != "CHFU_means") %>% 
  ggplot(aes(quant2, X50., shape = sp)) + 
  geom_point() + 
  geom_line(aes(linetype = sp)) + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  ylab("Proportional change in size (mm)") + 
  xlab("Size threshold (quantile of past size)") + 
  theme(legend.position = "top") + 
  coord_cartesian(ylim = c(-0.4, 0.4))

ggsave("3_analyse_data/bayes_figs/logsize_density_prop_change.png", height = 3.5, width = 3.5)
# 
# pred_df_all %>% 
#   ggplot(aes(x_predict, y_median, color = era)) + 
#   geom_line() + 
#   geom_ribbon(aes(ymin = y_lower, ymax = y_upper, fill = era, color = NULL), 
#               alpha = 0.5) + 
#   geom_ribbon(aes(ymin = y_lower_pred, ymax = y_upper_pred, fill = era, color = NULL), 
#               alpha = 0.25) +
#   facet_grid(quant ~ sp, scales = "free") + 
#   theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01))
# 
