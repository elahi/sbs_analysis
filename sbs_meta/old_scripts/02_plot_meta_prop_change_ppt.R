#################################################
# Author: Robin Elahi
# Date: 180227
# Plot for paper
#################################################

source("sbs_meta/01_assemble_metafor_data.R")

### Create new variables for plotting

## Size category - mean or max size
datM <- datM %>% 
  mutate(size_cat = ifelse(studySub == "threshold_none", "mean size", "max size"), 
         size_cat = ifelse(is.na(studySub) & museum == FALSE, "mean size", size_cat), 
         size_cat = ifelse(is.na(size_cat), "max size", size_cat))

## Subset Elahi
datM_Elahi <- datM %>% filter(study == "Elahi_2015")

## Subset Galloway
datM_Galloway <- datM %>% filter(study == "Galloway_2017")

## Subset Hayford
datM_Hayford <- datM %>% filter(study == "Hayford_2017")

## Subset field vs museum
datM_field <- datM %>% filter(museum == "field")
datM_museum <- datM %>% filter(museum == "museum")

## Subset Roy protected
datM_royCNM <- datM %>% filter(study == "Roy_2003-Protected")
datM_sub <- datM %>% filter(study != "Roy_2003-Protected")

## Plotting deets
theme_set(theme_bw(base_size = 24) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()) + 
            theme(legend.position = "bottom", 
                  legend.title = element_blank(), 
                  #legend.text = element_text(size = 20), 
                  legend.box.spacing = unit(x = 1, units = "lines")) + 
            theme(axis.text.y = element_text(face = "italic"))) 

my_dodge <- 0
my_point_size <- 5
my_bar_width <- 0
my_error_thickness <- 1

##### PLOT #####

## 0. Set up blank plot
datM_sub %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Proportional change in body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  guides(shape = guide_legend(title = "", direction = "vertical", 
                              override.aes = list(fill = 1)), 
         fill = guide_legend(title = "", direction = "vertical", 
                             override.aes = list(pch = 21), reverse = TRUE)) + 
  geom_blank() + 
  scale_y_continuous(limits = c(-0.5, 0.5))

## 1. Add Elahi data points
datM_sub %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Proportional change in body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  guides(shape = guide_legend(title = "", direction = "vertical", 
                              override.aes = list(fill = 1)), 
         fill = guide_legend(title = "", direction = "vertical", 
                             override.aes = list(pch = 21), reverse = TRUE)) + 
  geom_blank() + 
  scale_y_continuous(limits = c(-0.5, 0.5)) + 
  geom_errorbar(data = datM_Elahi, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = my_error_thickness) +
  geom_point(data = datM_Elahi, position = position_dodge(my_dodge), 
             size = my_point_size)

ggsave("sbs_meta/meta_figs/meta_lrr_ppt_01.pdf", height = 8, width = 10)

## 2. Add Galloway data points
datM_sub %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Proportional change in body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  guides(shape = guide_legend(title = "", direction = "vertical", 
                              override.aes = list(fill = 1)), 
         fill = guide_legend(title = "", direction = "vertical", 
                             override.aes = list(pch = 21), reverse = TRUE)) + 
  geom_blank() + 
  scale_y_continuous(limits = c(-0.5, 0.5)) + 
  geom_errorbar(data = datM_Elahi, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = my_error_thickness) +
  geom_point(data = datM_Elahi, position = position_dodge(my_dodge), 
             size = my_point_size) + 
  geom_errorbar(data = datM_Galloway, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = my_error_thickness) +
  geom_point(data = datM_Galloway, position = position_dodge(my_dodge), 
             size = my_point_size)

ggsave("sbs_meta/meta_figs/meta_lrr_ppt_02.pdf", height = 8, width = 10)

## 3. Add Hayford data points (All field points)
datM_sub %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Proportional change in body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  guides(shape = guide_legend(title = "", direction = "vertical", 
                              override.aes = list(fill = 1)), 
         fill = guide_legend(title = "", direction = "vertical", 
                             override.aes = list(pch = 21), reverse = TRUE)) + 
  geom_blank() + 
  scale_y_continuous(limits = c(-0.5, 0.5)) + 
  geom_errorbar(data = datM_field, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = my_error_thickness) +
  geom_point(data = datM_field, position = position_dodge(my_dodge), 
             size = my_point_size)

ggsave("sbs_meta/meta_figs/meta_lrr_ppt_03.pdf", height = 8, width = 10)

## 4. Add museum points
datM_sub %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Proportional change in body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  guides(shape = guide_legend(title = "", direction = "vertical", 
                              override.aes = list(fill = 1)), 
         fill = guide_legend(title = "", direction = "vertical", 
                             override.aes = list(pch = 21), reverse = TRUE)) + 
  geom_blank() + 
  scale_y_continuous(limits = c(-0.5, 0.5)) + 
  geom_errorbar(data = datM_sub, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = my_error_thickness) +
  geom_point(data = datM_sub, position = position_dodge(my_dodge), 
             size = my_point_size)

ggsave("sbs_meta/meta_figs/meta_lrr_ppt_04.pdf", height = 8, width = 10)

## 5. Add CNM points
datM_sub %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Proportional change in body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  guides(shape = guide_legend(title = "", direction = "vertical", 
                              override.aes = list(fill = 1)), 
         fill = guide_legend(title = "", direction = "vertical", 
                             override.aes = list(pch = 21), reverse = TRUE)) + 
  geom_blank() + 
  scale_y_continuous(limits = c(-0.5, 0.5)) + 
  geom_errorbar(data = datM_sub, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = my_error_thickness) +
  geom_point(data = datM_sub, position = position_dodge(my_dodge), 
             size = my_point_size) + 
  geom_errorbar(data = datM_royCNM, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = my_error_thickness) +
  geom_point(data = datM_royCNM, position = position_dodge(my_dodge), 
             size = my_point_size, fill = "white")

ggsave("sbs_meta/meta_figs/meta_lrr_ppt_05.pdf", height = 8, width = 10)



##OLD
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
