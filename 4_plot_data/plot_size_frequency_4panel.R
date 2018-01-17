################################################################################
##' @title Plot snail size-frequency distributions
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-01-16
##' 
##' @log 
##' 2017-10-24 Wrote functions to get data and choose size thresholds for better automation
################################################################################

##### LOAD PACKAGES, DATA #####

source("3_analyse_data/analyse_logsize_lmer.R")
library(patchwork)
library(cowplot)

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  panel.grid = element_blank(),
                  strip.text = element_text(face = "italic")))
dat_dens

##### DENSITY PLOT #####

n_df <- dat_dens %>% filter(!is.na(size1mm)) %>%
  group_by(species, era) %>% summarise(n = n()) %>% 
  ungroup()
n_df

yr_df <- dat_dens %>% filter(!is.na(size1mm)) %>%
  group_by(species, era) %>% distinct(year) %>% ungroup()

facet_panels <- dat_dens %>% select(species) %>% distinct() %>%
  arrange(species)

facet_panels

facet_panels <- facet_panels %>% 
  mutate(facet_labels = paste(LETTERS)[1:3]) %>% 
  arrange(facet_labels) %>% 
  mutate(past_text = c("1963; n=817", "1950; n=493", "1947; n=682"), 
         present_text = c("2014; n=5646", "2015; n=605", "2014; n=733"))

dat_dens <- dat_dens %>% 
  inner_join(., facet_panels, by = c("species"))

dat_dens %>% 
  ggplot(aes(size1mm, fill = era)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap( ~ species) + 
  scale_fill_manual(values = c("red", "black")) + 
  xlab("Size (mm)") + 
  ylab("Probability density") + 
  theme(legend.position = "none") +
  geom_text(data = facet_panels, aes(0, 0.15, label = facet_labels), 
            inherit.aes = FALSE, size = 6, nudge_x = 1, nudge_y = 0.01) + 
  geom_text(data = facet_panels, aes(15, 0.115, label = past_text), 
            inherit.aes = FALSE, size = 4, nudge_x = 1, nudge_y = 0.01, color = "red", hjust = 0) + 
  geom_text(data = facet_panels, aes(15, 0.1, label = present_text), 
            inherit.aes = FALSE, size = 4, nudge_x = 1, nudge_y = 0.01, color = "black", hjust = 0) 


##### SEPARATE DENSITY PLOTS #####

## Make separate dataframes
# Littorina keenae 
childsDF <- droplevels(filter(dat_dens, sp == "LIKE"))
# Chlorostoma funebralis
waraDF <- droplevels(filter(dat_dens, sp == "CHFU"))
# Lottia digitalis
hexDF <- droplevels(filter(dat_dens, sp == "LODI"))

# Basic function to plot density for any subset of data
plot_density_panel <- function(df, facet_panels, 
                               past_color = "black", present_color = "#e79f00") {
  
  df %>% 
    ggplot(aes(size1mm, fill = era)) + 
    geom_density(alpha = 0.5) + 
    facet_wrap( ~ species) + 
    scale_fill_manual(values = c(past_color, present_color)) + 
    xlab("Size (mm)") + ylab("Probability density") + 
    theme(legend.position = "none") +  
    geom_text(data = facet_panels, aes(15, 0.145, label = past_text), 
              inherit.aes = FALSE, size = 4, nudge_x = 1, nudge_y = 0.01, color = past_color, hjust = 0) + 
    geom_text(data = facet_panels, aes(15, 0.13, label = present_text), 
              inherit.aes = FALSE, size = 4, nudge_x = 1, nudge_y = 0.01, color = present_color, hjust = 0) + 
    scale_x_continuous(limits = c(0,32))
  
}

p1 <- plot_density_panel(waraDF, facet_panels = subset(facet_panels, species == "Chlorostoma funebralis"))
p2 <- plot_density_panel(hexDF, facet_panels = subset(facet_panels, species == "Lottia digitalis"))
p3 <- plot_density_panel(childsDF, facet_panels = subset(facet_panels, species == "Littorina keenae"))

##### PLOT MODEL RESULTS - SIZE CHANGE #####

plot_dat <- tidyfits %>% 
  filter(term == "erapresent") %>% 
  mutate(sample_order = c(1.9, 2.1, 3, 4), 
         sample_size = c(1, 1, 8, 4))

## colors from
# http://dr-k-lo.blogspot.com/2013/07/a-color-blind-friendly-palette-for-r.html

p4 <- plot_dat %>% 
  ggplot(aes(sample_order, estimate, color = model)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = 'darkgray') + 
  labs(x = "", 
       y = "Proportional change in size") + 
  theme(legend.position = "none") + 
  facet_wrap(~ term, labeller = label_bquote(rows = "")) + 
  scale_x_continuous(breaks = c(2,3,4), limits = c(1.5, 4.5), 
                     labels = rep(c("Chlorostoma", "Lottia", "Littorina"))) + 
  theme(axis.text.x = element_text(face = "italic")) + 
  geom_text(aes(x = sample_order, y = lower - 0.025, label = sample_size, color = NULL)) + 
  scale_color_manual(values = c("#009E73", "#CC79A7"))
p4

##### PLOT 4 PANELS TOGETHER #####

## With patchwork
#p1 + p2 + p3 + p4

## With cowplot
plot4 <- plot_grid(p1, p2, p3, p4, labels = c("A", "B", "C", "D"))

save_plot("figs_ms/plot_size_frequncy_density_4panel.pdf", plot4, 
          ncol = 2, nrow = 2, 
          base_height = 3.5, 
          base_aspect_ratio = 1)

