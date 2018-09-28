#################################################
# Author: Robin Elahi
# Date: 180218
# Assemble data for meta-analysis
#################################################

library(dplyr)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())) 
library(viridis)

elahi <- read_csv("sbs_meta/output/dfMeta_Elahi2015.csv")
fisher <- read_csv("sbs_meta/output/dfMeta_Fisher2009_original.csv")
gall_frank <- read_csv("sbs_meta/output/dfMeta_Galloway-Frank_2017.csv")
gall_tren <- read_csv("sbs_meta/output/dfMeta_Galloway-Treneman_2017.csv")
hay_king <- read_csv("sbs_meta/output/dfMeta_Hayford-King_2017.csv")
roy <- read_csv("sbs_meta/output/dfMeta_Roy2003.csv")

dat <- rbind(elahi, fisher, gall_frank, gall_tren, hay_king, roy) 

## Remove CNM, then rename Site
dat <- dat %>% 
  filter(site != "CNM") %>%
  mutate(site = ifelse(study == "Roy_2003", "SoCal", site)) %>% 
  mutate(group = paste(study, species, site, sep = "-"), 
         museum = ifelse(museum == TRUE, "museum", "field"), 
         sample_size = as.numeric(sample_size))

## Change SD to CI
str(dat)
dat <- dat %>% 
  mutate(size_CI = ifelse(study == "Roy_2003", size_error, 2 * (size_error / sqrt(sample_size))))

## Plot untransformed data
dat %>% 
  #filter(species != "Lottia gigantea") %>% 
  ggplot(aes(year, size_rep, color = species, shape = study, group = group)) + 
  geom_point(size = 3, alpha = 0.75) + 
  geom_line(aes(group = group), size = 0.5, alpha = 0.75) + 
  #geom_errorbar(aes(ymax = size_rep + size_CI, ymin = size_rep - size_CI), size = 1, width = 5) + 
  labs(x = "Year", 
       y = "Mean size (mm)") + 
  scale_color_viridis_d() + 
  facet_wrap(~ museum) + 
  theme(legend.position = "right")
ggsave("sbs_meta/meta_figs/plot_meta_size_year.pdf", height = 6, width = 9)

