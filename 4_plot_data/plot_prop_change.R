################################################################################
##' @title Plot proportional change in size
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

library(cowplot)
library(dplyr)

options(tibble.print_max = 50, tibble.print_min = 10)

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "italic"), 
                  panel.grid = element_blank())) 


## Load bayesian analysis results
coda_df_logsize_density <- read.csv("3_analyse_data/bayes_output/logsize_density/coda_df_all.csv") %>% 
  mutate(model = "With density")
coda_df_logsize <- read.csv("3_analyse_data/bayes_output/logsize/coda_df_all.csv") %>% 
  mutate(model = "Without density")

names(coda_df_logsize)
names(coda_df_logsize_density)

coda_df_all <- rbind(coda_df_logsize, coda_df_logsize_density) 
species_df <- data.frame(sp = c("CHFU", "LODI", "LIKE"), 
                         species = c("Chlorostoma funebralis", "Lottia digitalis", "Littorina keenae"))
coda_df_all <- left_join(coda_df_all, species_df, by = "sp")

# Reorder levels
species <- factor(coda_df_all$species, levels = rev(c("Littorina keenae",
                                            "Lottia digitalis", 
                                            "Chlorostoma funebralis")))
coda_df_all$species <- species

##### 3 PANEL PLOT OF PROPORTIONAL CHANGE #####

facet_panels <- coda_df_all %>% 
  filter(!is.na(species)) %>% 
  select(species) %>% distinct() %>%
  arrange(species)

facet_panels

facet_panels <- facet_panels %>% 
  mutate(facet_labels = paste(LETTERS)[1:3]) %>% 
  arrange(facet_labels)

coda_df_all <- coda_df_all %>% 
  inner_join(., facet_panels, by = c("species"))

coda_df_all %>% 
  filter(param == "prop_change") %>% 
  filter(sp != "CHFU_means") %>% 
  ggplot(aes(quant, X50., color = model)) + 
  geom_point() + 
  geom_line(aes(linetype = model)) + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), width = 0.025) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  ylab("Proportional change in size (mm)") + 
  xlab("Size threshold (quantile of past size)") + 
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.99, 0.01), legend.justification = c(0.99, 0.01)) + 
  #coord_cartesian(ylim = c(-0.4, 0.4)) + 
  facet_wrap(~ species) + 
  scale_color_manual(values = c("darkgray", "black")) + 
  geom_text(data = facet_panels, aes(0, 0.18, label = facet_labels), 
            inherit.aes = FALSE, size = 6, nudge_x = 0, nudge_y = 0.00) 

ggsave("figs_ms/plot_prop_change.pdf", height = 3.5, width = 7)

