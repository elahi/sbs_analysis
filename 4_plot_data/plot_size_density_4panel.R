################################################################################
##' @title Plot size-density 4 panel figure with bayesian intervals
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-10-30
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####
source("3_analyse_data/01_sbs_bayes_data.R")

library(cowplot)
options(tibble.print_max = 50, tibble.print_min = 10)

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "italic"), 
                  panel.grid = element_blank())) 

names(dat_dens)

# Observed data
wara_means
waraDF
childsDF
hexDF

## Load bayesian analysis results
pred_df <- read.csv("3_analyse_data/bayes_output/logsize_density/pred_df_all.csv")
head(pred_df)

# Prediction and credible intervals
pred_chfu_means <- pred_df %>% filter(sp == "CHFU_means" & quant == 0)
pred_chfu <- pred_df %>% filter(sp == "CHFU" & quant == 0)
pred_lodi <- pred_df %>% filter(sp == "LODI" & quant == 0)
pred_like <- pred_df %>% filter(sp == "LIKE" & quant == 0)

##### PLOT LOG SIZE VS DENSITY #####

plot_logsize_density <- function(my_data, my_pred, past_color = "darkgray", present_color = "black"){
  
  # Get density limits for plotting the bayesian predictions
  era_density_limits <- my_data %>% group_by(era) %>% 
    summarise_at(.vars = c("density_m2"), 
                 .funs = funs(min_density = min, max_density = max))
  
  # Filter prediction df
  my_pred <- inner_join(my_pred, era_density_limits, by = "era") %>% 
    filter(x_predict >= min_density & x_predict <= max_density)
  
  # Plot data
  my_data %>% 
    ggplot(aes(density_m2, size_log, shape = era, color = era)) + 
    geom_point(alpha = 0.75, size = 1) + 
    # theme(legend.position = c(0.99, 0.99), legend.justification = c(0.99, 0.99)) + 
    # theme(legend.title = element_blank()) +
    # theme(legend.key = element_rect(fill = "white")) + 
    theme(legend.position = "none") + 
    xlab(expression(paste("Density (no. ", m^-2, ")"))) + 
    ylab("Log size (mm)") + 
    guides(shape = FALSE) + 
    # geom_smooth(method = "lm") + 
    geom_line(data = my_pred, aes(x_predict, y_median, color = era)) +
    geom_ribbon(data = my_pred, aes(x_predict, y = NULL, ymin = y_lower, ymax = y_upper, fill = era, color = NULL),
                alpha = 0.5) +
    # geom_ribbon(data = my_pred, aes(x_predict, y = NULL, ymin = y_lower_pred, ymax = y_upper_pred, fill = era, color = NULL),
    #             alpha = 0.25) +
    scale_color_manual(values = c(past_color, present_color)) + 
    scale_fill_manual(values = c(past_color, present_color)) + 
    scale_shape_manual(values = c(1, 1)) + 
    facet_wrap(~ species, scales = "free_x") + 
    geom_point(data = subset(my_data, era == "past"), alpha = 0.75, size = 1) + 
    scale_y_continuous(limits = c(0, 1.6))
  
}

add_my_legend <-  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) 

p1 <- plot_logsize_density(my_data = wara_means, my_pred = pred_chfu_means, past_color = "red")
p1
p2 <- plot_logsize_density(my_data = waraDF, my_pred = pred_chfu, past_color = "red")
p3 <- plot_logsize_density(my_data = hexDF, my_pred = pred_lodi, past_color = "red")
p4 <- plot_logsize_density(my_data = childsDF, my_pred = pred_like, past_color = "red")

plot_logsize_density_4panel <- plot_grid(p1 + add_my_legend, 
                                         p2, p3, p4, 
                                         labels = c("A", "B", "C", "D", ncol = 2))

save_plot("3_analyse_data/bayes_figs/plot_logsize_density_4panel.png", plot_logsize_density_4panel, 
          ncol = 2, nrow = 2, base_height = 3.5, base_width = 3.5)
