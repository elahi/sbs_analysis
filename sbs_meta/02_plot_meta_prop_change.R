#################################################
# Author: Robin Elahi
# Date: 180227
# Plot for paper
#################################################

source("sbs_meta/01_assemble_metafor_data.R")

##### PLOT #####

### Create new variables for plotting

## Size category - mean or max size
datM <- datM %>% 
  mutate(size_cat = ifelse(studySub == "threshold_none", "mean size", "max size"), 
         size_cat = ifelse(is.na(studySub) & museum == FALSE, "mean size", size_cat), 
         size_cat = ifelse(is.na(size_cat), "max size", size_cat))

## Subset Roy protected
datM_royCNM <- datM %>% filter(study == "Roy_2003-Protected")
datM_sub <- datM %>% filter(study != "Roy_2003-Protected")

## Plotting deets
theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())) 
my_dodge <- 0
my_point_size <- 2
my_bar_width <- 0

datM_sub %>% 
  #filter(!species %in% c("Lottia persona", "Lottia scutum")) %>%
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = 0.25) + 
  geom_point(position = position_dodge(my_dodge), size = my_point_size) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Proportional change in body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  # theme(legend.position = c(0.95, 0.05), 
  #       legend.justification = c(0.95, 0.05)) + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.box.spacing = unit(x = -0, units = "lines"), 
        legend.text = element_text(size = 8)) + 
  #scale_x_discrete("", labels = rev(my_axis_labels)) + 
  theme(axis.text.y = element_text(face = "italic")) + 
  geom_errorbar(data = datM_royCNM, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = 0.25) +
  geom_point(data = datM_royCNM, position = position_dodge(my_dodge), 
             size = my_point_size, fill = "white") + 
  guides(shape = guide_legend(title = "", direction = "vertical", 
                              override.aes = list(fill = 1)), 
         fill = guide_legend(title = "", direction = "vertical", 
                             override.aes = list(pch = 21), reverse = TRUE))

#ggsave("sbs_meta/meta_figs/meta_lrr.pdf", height = 4.5, width = 4.5)


# # Code to override clipping
# library(grid)
# gt <- ggplot_gtable(ggplot_build(p))
# gt$layout$clip[gt$layout$name == "panel"] <- "off"
# grid.draw(gt)
