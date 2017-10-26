################################################################################
##' @title Plot intertidal temperatures
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-09-13
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

library(grid)
library(cowplot)

source("R/multiplotF.R")
library(grid)
theme_set(theme_bw(base_size = 12))

# Load temperature logger data (6 weeks of empirical data)
source("05_summarise_intertidal_temps.R")
tempMeans

# Load limpet hindcasts (15 yrs of modeled body temperatures)
source("05_summarise_limpet_temps.R")
monthly_extremes

names(tempMeans)
names(monthly_extremes)

##' Combine these two datasets
# species, mean, CI, tidalHT, metric, 

temp_logger <- tempMeans %>% 
  filter(metric != "daily_cv" & metric != "daily_mean") %>% 
  select(species, position, microhabitat, tidalHT, 
         metric, mean, sd, n, se, CI) %>% 
  mutate(month = NA, 
         year = NA,
         dataset = "Empirical")

# Rename levels of metric
metric <- temp_logger$metric
metric <- gsub("daily_max", "maximum", metric)
metric <- gsub("daily_med", "median", metric)
metric <- gsub("daily_min", "minimum", metric)

temp_logger$metric <- metric

temp_model <- monthly_extremes %>% ungroup() %>% 
  select(species, position, tidalHT, month, year, 
         metric, mean, sd, n, se, CI) %>% 
  mutate(microhabitat = NA, 
         dataset = "Model")

tempDF <- rbind(temp_logger, temp_model)
tempDF

##### PLOTS ######
theme_set(theme_bw(base_size = 12))

# theme_set(theme_bw(base_size = 12) + 
#             theme(panel.grid = element_blank()))

dataset_description <- c(
  Model = "Predicted body temperature", 
  Empirical = "Empirical rock temperature"
)

intertidal_text_df <- data.frame(x = c(rep(3.5, 3)), 
                      y = c(9.25, 14, 19), 
                      text1 = c("Min", "Median", "Max"), 
                      dataset = rep("Model", 3), 
                      metric = c("minimum", "median", "maximum"))

facet_label_text <- tempDF %>% group_by(dataset) %>% 
  summarise(minValue = min(mean), 
            maxValue = max(mean)) %>%  
  mutate(x = 0, 
         text1 = c("A", "B"), 
         dataset = c("Empirical", "Model"), 
         y = c(38.5, 20.5))


tempDF %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_line(aes(group = interaction(species, metric), color = NULL), 
            alpha = 1, size = 0.3) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  geom_point(data = subset(tempDF, microhabitat == "crevice" & metric == "maximum"), 
             aes(tidalHT, mean, shape = species, color = NULL), 
             alpha = 0.6, size = 2) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(color = FALSE) + 
  theme(legend.position = "top") + 
  #theme(legend.title = element_blank()) + 
  facet_wrap(~ dataset, ncol = 2, scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  guides(shape = 
           guide_legend(title = "SAMPLING AREA", title.position = "top", 
                        title.hjust = 0.5, 
                        title.theme = 
                          element_text(size = 10, face = "bold", angle = 0), 
                        label.theme = 
                          element_text(angle = 0, size = 8, face =  "italic"))) + 
  geom_text(aes(x, y, label = text1, shape = NULL), 
            data = intertidal_text_df, size = 3, 
            hjust = 0, fontface = "bold") + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = facet_label_text, size = 5, hjust = 0.25, vjust = 0.75, 
            show.legend = FALSE) 
  
ggsave("figs/elahi_temp_body_rock.png", height = 4.5, width = 7)

##### FRIENDLY PLOT FOR POWERPOINT - 2 PANEL#####

head(tempDF)
range(tempDF$mean)

# set colors
library(RColorBrewer)

### SPECIES
unique(tempDF$species)
cb_pal_species <- c("#999999", "#E69F00", "#56B4E9")
names(cb_pal_species) <- c("Chlorostoma funebralis", "Lottia digitalis ", "Littorina keenae")
col_scale_species <- scale_colour_manual(name = "Species",values = cb_pal_species)
cbPalette <- c("#999999", "#E69F00", "#56B4E9")

# plot1
tempDF %>% filter(metric == "maximum") %>% 
  filter(species == "Chlorostoma funebralis") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = species)) + 
  geom_point(alpha = 1, size = 2) + 
  geom_line(aes(group = interaction(species, metric), color = species), 
            alpha = 1, size = 0.5) + 
  scale_colour_manual(values=cbPalette) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.3, alpha = 1) + 
  ylab(expression(paste("Maximum temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  theme(legend.position = "none") + 
  facet_wrap(~ dataset, ncol = 2, #scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  scale_y_continuous(limits = c(16, 39)) + 
  scale_x_continuous(limits = c(-0.1, 8))

ggsave("figs/elahi_temp_body_rock_ppt1.png", height = 3.5, width = 7)


# plot2
tempDF %>% filter(metric == "maximum") %>% 
  filter(species != "Littorina keenae") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = species)) + 
  geom_point(alpha = 1, size = 2) + 
  geom_line(aes(group = interaction(species, metric), color = species), 
            alpha = 1, size = 0.5) + 
  scale_colour_manual(values=cbPalette) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.3, alpha = 1) + 
  ylab(expression(paste("Maximum temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  theme(legend.position = "none") + 
  facet_wrap(~ dataset, ncol = 2, #scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  scale_y_continuous(limits = c(16, 39)) + 
  scale_x_continuous(limits = c(-0.1, 8))

ggsave("figs/elahi_temp_body_rock_ppt2.png", height = 3.5, width = 7)

# plot3
tempDF %>% filter(metric == "maximum") %>% 
  #filter(species != "Littorina keenae") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = species)) + 
  geom_point(alpha = 1, size = 2) + 
  geom_line(aes(group = interaction(species, metric), color = species), 
            alpha = 1, size = 0.5) + 
  scale_colour_manual(values=cbPalette) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.3, alpha = 1) + 
  ylab(expression(paste("Maximum temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  theme(legend.position = "none") + 
  facet_wrap(~ dataset, ncol = 2, #scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  scale_y_continuous(limits = c(16, 39)) + 
  scale_x_continuous(limits = c(-0.1, 8))

ggsave("figs/elahi_temp_body_rock_ppt3.png", height = 3.5, width = 7)


##### FRIENDLY PLOT FOR POWERPOINT - HINDCASTS ONLY #####
head(tempDF)
theme_set(theme_bw(base_size = 8) + theme(panel.grid.minor=element_blank(),
                                          panel.grid.major=element_blank()))

  

# plot1
tempDF %>% filter(metric == "maximum") %>% filter(dataset == "Model") %>% 
  filter(species == "Chlorostoma funebralis") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = species)) + 
  geom_point(alpha = 1, size = 1.5) + 
  geom_line(aes(group = interaction(species, metric), color = species), 
            alpha = 1, size = 0.5) + 
  scale_colour_manual(values=cbPalette) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.1, alpha = 1) + 
  ylab(expression(paste("Max temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  theme(legend.position = "none") + 
  facet_wrap(~ dataset, ncol = 2, #scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  scale_y_continuous(limits = c(16, 21)) + 
  scale_x_continuous(limits = c(-0.1, 8))

ggsave("figs/elahi_temp_body_rock_ppt1_hindcast.png", height = 1.75, width = 3.5)


# plot2
tempDF %>% filter(metric == "maximum") %>% filter(dataset == "Model") %>% 
  filter(species != "Littorina keenae") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = species)) + 
  geom_point(alpha = 1, size = 1.5) + 
  geom_line(aes(group = interaction(species, metric), color = species), 
            alpha = 1, size = 0.5) + 
  scale_colour_manual(values=cbPalette) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.1, alpha = 1) + 
  ylab(expression(paste("Max temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  theme(legend.position = "none") + 
  facet_wrap(~ dataset, ncol = 2, #scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  scale_y_continuous(limits = c(16, 21)) + 
  scale_x_continuous(limits = c(-0.1, 8))

ggsave("figs/elahi_temp_body_rock_ppt2_hindcast.png", height = 1.75, width = 3.5)

# plot3
tempDF %>% filter(metric == "maximum") %>% filter(dataset == "Model") %>% 
  #filter(species != "Littorina keenae") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = species)) + 
  geom_point(alpha = 1, size = 1.5) + 
  geom_line(aes(group = interaction(species, metric), color = species), 
            alpha = 1, size = 0.5) + 
  scale_colour_manual(values=cbPalette) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.1, alpha = 1) + 
  ylab(expression(paste("Max temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  theme(legend.position = "none") + 
  facet_wrap(~ dataset, ncol = 2, #scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  scale_y_continuous(limits = c(16, 21)) + 
  scale_x_continuous(limits = c(-0.1, 8))

ggsave("figs/elahi_temp_body_rock_ppt3_hindcast.png", height = 1.75, width = 3.5)

##### 3 PANEL PLOT WITH AIR EXPOSURE ######
theme_set(theme_bw(base_size = 12))

tempDF$dataset

facet_label_text <- tempDF %>% group_by(dataset) %>% 
  summarise(minValue = min(mean), 
            maxValue = max(mean)) %>%  
  mutate(x = 0, 
         text1 = c("A", "B"), 
         dataset = c("Empirical", "Model"), 
         y = c(38.5, 20.5))

facet_label_text

panelAB <- tempDF %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_line(aes(group = interaction(species, metric), color = NULL), 
            alpha = 1, size = 0.3) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  geom_point(data = subset(tempDF, microhabitat == "crevice" & metric == "maximum"), 
             aes(tidalHT, mean, shape = species, color = NULL), 
             alpha = 0.6, size = 2) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(color = FALSE) + 
  theme(legend.position = "none") + 
  #theme(legend.title = element_blank()) + 
  facet_wrap(~ dataset, ncol = 1, scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  guides(shape = 
           guide_legend(title = "SAMPLING AREA", title.position = "top", 
                        title.hjust = 0.5, 
                        title.theme = 
                          element_text(size = 10, face = "bold", angle = 0), 
                        label.theme = 
                          element_text(angle = 0, size = 8, face =  "italic"))) + 
  geom_text(aes(x, y, label = text1, shape = NULL), 
            data = intertidal_text_df, size = 3, 
            hjust = 0, fontface = "bold") + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = facet_label_text, size = 5, hjust = 0.25, vjust = 0.75, 
            show.legend = FALSE) 



## Panel A - Empirical rock temps
panelA <- tempDF %>% 
  filter(dataset == "Empirical") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  guides(shape = 
           guide_legend(title = "SAMPLING AREA", title.position = "top", 
                        title.hjust = 0.5, 
                        title.theme = 
                          element_text(size = 8, face = "bold", angle = 0), 
                        keywidth = 0.1,
                        keyheight = 0.1,
                        default.unit="inch", 
                        label.theme = 
                          element_text(angle = 0, size = 8, face =  "italic"))) + 
  geom_point(alpha = 0.8, size = 2) + 
  geom_line(aes(group = interaction(species, metric), color = NULL), 
            alpha = 1, size = 0.3) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.8) + 
  geom_point(data = subset(tempDF, microhabitat == "crevice" & metric == "maximum"), 
             aes(tidalHT, mean, shape = species, color = NULL), 
             alpha = 0.6, size = 2) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(color = FALSE) + 
  facet_wrap(~ dataset, ncol = 1, scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  annotate(geom = "text", x = 8, y = 38.5, label = "A") + 
  theme(legend.position = c(0.05, 0.95), legend.justification = c(0.05, 0.95)) + 
  theme(legend.title = element_blank()) + 
  theme(legend.background = element_blank())
  
panelA

panelB <- tempDF %>% 
  filter(dataset == "Model") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.8, size = 2) + 
  geom_line(aes(group = interaction(species, metric), color = NULL), 
            alpha = 1, size = 0.3) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.8) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(color = FALSE) + 
  theme(legend.position = "none") +
  facet_wrap(~ dataset, ncol = 1, scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  geom_text(aes(x, y, label = text1, shape = NULL), 
            data = intertidal_text_df, size = 3, 
            hjust = 0, fontface = "bold") + 
  annotate(geom = "text", x = 0, y = 20.5, label = "B")

panelB


panelC <- df %>%
  ggplot(aes(tidal_height, air_exposure_proportion * 100, color)) +
  geom_line() + 
  labs(x = "Tidal height (m)", y = "Air exposure (%)") + 
  # scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), 
  #                    labels = c("0", "20", "40", "60", "80", "")) + 
  annotate(geom = "text", x = 0, y = 90, label = "C")

layout1 <- matrix(c(1, 1, 2, 2, 3), nrow = 5, byrow = TRUE)
png("figs/elahi_temp_body_rock_3panel.png", width = 3.5, height = 7, units = "in", res = 300)
multiplot(panelA, panelB, panelC, layout = layout1)
dev.off()	


