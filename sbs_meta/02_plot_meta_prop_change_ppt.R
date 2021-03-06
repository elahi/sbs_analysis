#################################################
# Author: Robin Elahi
# Date: 181026
# Plot for paper
#################################################

source("sbs_meta/01_assemble_metafor_data.R")

### Create new variables for plotting

## Size category - mean or max size
datM <- datM %>% 
  mutate(size_cat = ifelse(threshold == "no", "mean size", "max size"))

## Subset Elahi
datM_Elahi <- datM %>% filter(study == "Elahi_2015")

## Subset Galloway
datM_Galloway <- datM %>% filter(study == "Galloway_2017")

## Subset Hayford
datM_Hayford <- datM %>% filter(study == "Hayford_2017")

## Subset field vs museum
datM_field <- datM %>% filter(museum == "field")
datM_museum <- datM %>% filter(museum == "museum")

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
datM %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Log ratio of body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  guides(shape = guide_legend(title = "", direction = "vertical", 
                              override.aes = list(fill = 1)), 
         fill = guide_legend(title = "", direction = "vertical", 
                             override.aes = list(pch = 21), reverse = TRUE)) + 
  geom_blank() + 
  scale_y_continuous(limits = c(-0.5, 0.5))

## 1. Add Elahi data points
datM %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Log ratio of body size", 
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
datM %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Log ratio of body size", 
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

## 3. Add Nucella lamellosa (Hayford-King; Hayford-Elahi) data points (All field points)
datM %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Log ratio of body size", 
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
datM %>% 
  ggplot(aes(species2, yi, fill = size_cat, shape = museum)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1) + 
  labs(y = "Log ratio of body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  guides(shape = guide_legend(title = "", direction = "vertical", 
                              override.aes = list(fill = 1)), 
         fill = guide_legend(title = "", direction = "vertical", 
                             override.aes = list(pch = 21), reverse = TRUE)) + 
  geom_blank() + 
  scale_y_continuous(limits = c(-0.5, 0.5)) + 
  geom_errorbar(data = datM, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = my_error_thickness) +
  geom_point(data = datM, position = position_dodge(my_dodge), 
             size = my_point_size)

ggsave("sbs_meta/meta_figs/meta_lrr_ppt_04.pdf", height = 8, width = 10)

##### Overall meta results
res_df
res_df_plot <- res_df %>% slice(1:2) %>% 
  mutate(xlabel = c("All data, as is", "All data, max size"), 
         xlabel = as.factor(xlabel))

## Plotting deets
theme_set(theme_bw(base_size = 24) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()) + 
            theme(legend.position = "bottom", 
                  legend.title = element_blank(), 
                  legend.box.spacing = unit(x = 1, units = "lines"))) 

res_df_plot %>% 
  ggplot(aes(xlabel, yi, fill = size_cat)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = my_error_thickness) + 
  coord_flip() + 
  scale_fill_viridis_d(begin = 0.5, end = 1, na.value = "black") + 
  labs(y = "Log ratio of body size", 
       x = "") + 
  scale_y_continuous(limits = c(-0.5, 0.5)) + 
  geom_errorbar(data = res_df_plot, aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = my_bar_width, size = my_error_thickness) +
  geom_point(data = res_df_plot, position = position_dodge(my_dodge), 
             size = my_point_size, pch = 23) + 
  theme(legend.position = "none") + 
  scale_x_discrete(limits = rev(levels(res_df_plot$xlabel)))

ggsave("sbs_meta/meta_figs/meta_lrr_ppt_overall.pdf", height = 3, width = 10)

