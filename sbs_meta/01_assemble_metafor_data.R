#################################################
# Author: Robin Elahi
# Date: 180218
# Assemble data for meta-analysis
# In preparation for use with metafor
#################################################

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw(base_size = 16) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())) 
library(viridis)

## Field data
elahi <- read_csv("sbs_meta/output/dfMeta_Elahi2015_species.csv") # averaged across sample areas
elahi <- elahi %>% 
  mutate(species = gsub(x = species, pattern = "\\.", replacement = " ")) %>% 
  mutate(species = ifelse(species == "Chlorostoma funebralis", "Tegula funebralis", species))

gall_frank <- read_csv("sbs_meta/output/dfMeta_Galloway-Frank_2017.csv")
gall_tren <- read_csv("sbs_meta/output/dfMeta_Galloway-Treneman_2017.csv")
hay_king <- read_csv("sbs_meta/output/dfMeta_Hayford-King_2017.csv")

## Museum data

## Fisher summary data is based on the mean sizes from 19 sites on Mount Desert Island
fisher <- read_csv("sbs_meta/output/dfMeta_Fisher2009_MDI.csv")

## Wilson-Brodie 2017
wilson <- read_csv("sbs_meta/output/dfMeta_Wilson-Brodie_2017.csv")

## Select first museum sample and field for Roy
roy <- read_csv("sbs_meta/output/dfMeta_Roy2003.csv") %>% filter(studySub != "Baseline_2")
roy

roy_present <- roy %>% filter(studySub != "Present_CNM") %>% 
  mutate(study = "Roy 2003 - Unprotected")
roy_presentCNM <- roy %>% filter(studySub != "Present") %>% 
  mutate(study = "Roy 2003 - Protected")

## Compile
dat <- rbind(elahi, gall_frank, gall_tren, hay_king, fisher, wilson, roy_present, roy_presentCNM) 
### Add UK to lapillus
dat <- dat %>% 
  mutate(species2 = ifelse(study == "Wilson-Brodie 2017", 
                           paste(species, "(UK)", sep = " "), species), 
         species2 = ifelse(study == "Galloway-Frank_2017", 
                           paste(species, "(OR)", sep = " "), species2))

## For metafor, I want the data in wide format
names(dat)
dat2 <- dat %>% 
  select(study, species, species2, museum, latitude, era, size_rep, size_error, sample_size) %>% 
  rename(size_n = sample_size)
dat2

datM <- dat2 %>% 
  gather(key = temp, value = value, starts_with("size")) %>% 
  unite(col = temp1, era, temp, sep = "_") %>% 
  spread(temp1, value) %>% 
  arrange(desc(latitude))
datM

### Re-order by latitude
datM <- datM %>% 
  mutate(species2 = reorder(species2, latitude))

## Try metafor
library(metafor)
names(datM)
names(datM) <- c("study", "species", "species2", "museum", "latitude", 
                 "sd2i", "n2i", "m2i", "sd1i", "n1i", "m1i")
datM

### calculate log ratio of means and corresponding sampling variances
datM <- escalc(measure = "ROM", m1i = m1i, sd1i = sd1i, n1i = n1i, 
               m2i = m2i, sd2i = sd2i, n2i = n2i, data = datM)
datM

### Get standard error (following Vuorre)
datM <- datM %>% 
  mutate(sei = as.numeric(sqrt(vi)), 
         upper = yi + 2*sei, 
         lower = yi - 2*sei)

### Plot this
my_dodge <- 0

spp_df <- datM %>% distinct(species2, study)
spp_df
spp_df <- spp_df %>% 
  mutate(my_text = c(rep("Central California", 3), 
                     seq(1:13)))
glimpse(spp_df)

my_axis_labels <- paste(datM$species)
my_axis_labels
rev(my_axis_labels)

p <- datM %>% 
  ggplot(aes(species2, yi, fill = study, shape = museum)) + 
  geom_point(position = position_dodge(my_dodge), size = 3) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(my_dodge), 
                width = 0) + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  labs(y = "Proportional change in body size", 
       x = "") + 
  scale_shape_manual(values = c(21, 22)) + 
  theme(legend.position = "none") + 
  #scale_x_discrete("", labels = rev(my_axis_labels)) + 
  theme(axis.text.y = element_text(face = "italic")) 
p

# # Code to override clipping
# library(grid)
# gt <- ggplot_gtable(ggplot_build(p))
# gt$layout$clip[gt$layout$name == "panel"] <- "off"
# grid.draw(gt)

ggsave("sbs_meta/meta_figs/meta_lrr.pdf", height = 5, width = 7)

### meta-analysis of log ratio of means using a random-effects model
res <- rma(yi, vi, method = "DL", data = datM)
res
forest(res)

