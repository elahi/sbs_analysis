#################################################
# Author: Robin Elahi
# Date: 180929
# Analysis of historical and modern gastropod body sizes
# Plot gastropod size-freq distributions
#################################################

source("sbs_meta/01_assemble_raw_data.R")

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()))

library(viridis)

##### HISTO PLOTS #####

df3 %>% 
  ggplot(aes(size1mm_rand, fill = era)) + 
  geom_rect(data = subset(df3, museum == "museum"), fill = "gray90", 
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.3) +
  geom_histogram(alpha = 0.75, center = 0,  bins = 25,
                 aes(y = ..density..), 
                 color = 'black', size = 0.2, 
                 position = "identity") + 
  geom_vline(data = df_size_threshold, aes(xintercept = size_threshold), linetype = "solid") + 
  facet_wrap(fig_legend ~ species, ncol = 3, scales = "free",  
             labeller = label_wrap_gen(multi_line = FALSE)) + 
  xlab("Size (mm)") + 
  ylab("Probability density") + 
  theme(legend.justification  = c(0.5, 0), 
        legend.position = c(0.5, 0), 
        legend.title = element_blank(), 
        strip.text = element_text(size = 8)) + 
  scale_fill_viridis_d()

ggsave("sbs_meta/meta_figs/meta_hist_era.pdf", height = 7, width = 5)

