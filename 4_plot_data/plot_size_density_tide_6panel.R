################################################################################
##' @title Plot size-density 3 panel figure for density and tide
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-12-18
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

##### PLOT SIZE VS DENSITY AND TIDE #####

my_data = wara_means
past_color = "red"
present_color = "black"
my_letter = "A"

past_col <- "#ef8a62"
present_col <- "#67a9cf"

plot_size <- function(my_data, x = "density_m2", y = "size1mm", 
                      past_color = past_col, present_color = present_col, my_letter = "A"){
  
  # Plot data
  my_data %>% 
    ggplot(aes_string(x, y, shape = "era", color = "era")) + 
    geom_smooth(method = "lm", aes(shape = NULL, color = NULL), color = "darkgray") + 
    #geom_smooth(method = "lm", aes(shape = NULL), linetype = "dashed") + 
    geom_point(alpha = 0.75, size = 1) + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c(past_color, present_color)) + 
    scale_fill_manual(values = c(past_color, present_color)) + 
    scale_shape_manual(values = c(1, 1)) + 
    facet_wrap(~ species, scales = "free") + 
    geom_point(data = subset(my_data, era == "past"), alpha = 0.75, size = 1) + 
    ylab("Size (mm)")
  
}

density_axis_X <- xlab(expression(paste("Density (no. ", m^-2, ")"))) 
tide_axis_X <- xlab("Tidal height (m)")

p1 <- plot_size(my_data = wara_means, my_letter = "A") + density_axis_X
p2 <- plot_size(my_data = hexDF, my_letter = "B") + density_axis_X
p3 <- plot_size(my_data = childsDF, my_letter = "C") + density_axis_X

p4 <- plot_size(my_data = wara_means, my_letter = "D", x = "tideHTm") + tide_axis_X
p5 <- plot_size(my_data = hexDF, my_letter = "E", x = "tideHTm_offset") + tide_axis_X
p6 <- plot_size(my_data = childsDF, my_letter = "F", x = "tideHTm_offset") + tide_axis_X

add_my_legend <-  theme(legend.position = c(0.95, 0.95), legend.justification = c(0.95, 0.95)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key = element_rect(fill = "white")) 

plot_size_density_6panel <- plot_grid(p1 + add_my_legend, p2, p3, p4, p5, p6, ncol = 3)

save_plot("figs_ms/plot_size_density_tide_6panel.pdf", plot_size_density_6panel, 
          ncol = 3, nrow = 2, base_height = 3.5, base_width = 3.5)


