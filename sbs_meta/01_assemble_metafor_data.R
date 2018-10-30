#################################################
# Author: Robin Elahi
# Date: 180218
# Assemble data for meta-analysis
# In preparation for use with metafor
#################################################

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(readr)
library(tidyr)
library(metafor)
library(ggplot2)

### Field data
elahi <- read_csv("sbs_meta/output/dfMeta_Elahi2015_species.csv") 

elahi <- elahi %>% 
  mutate(species = gsub(x = species, pattern = "\\.", replacement = " ")) %>% 
  mutate(species = ifelse(species == "Chlorostoma funebralis", "Tegula funebralis", species)) %>% 
  mutate(museum = "field")

gall_frank <- read_csv("sbs_meta/output/dfMeta_Galloway-Frank_2017.csv") %>% 
  mutate(museum = "field")

gall_tren <- read_csv("sbs_meta/output/dfMeta_Galloway-Treneman_2017.csv") %>%
  mutate(museum = "field", studySub = "threshold_none")

hay_king <- read_csv("sbs_meta/output/dfMeta_Hayford-King_2017.csv") %>% 
  mutate(museum = "field")

hay_elahi <- read_csv("sbs_meta/output/dfMeta_Hayford-Elahi_2018.csv") %>% 
  mutate(museum = "field")

### Museum data

## Fisher summary data is based on raw data extracted from histograms
fisher <- read_csv("sbs_meta/output/dfMeta_Fisher2009_raw.csv") %>% 
  mutate(museum = "museum")

## Wilson-Brodie 2017
wilson <- read_csv("sbs_meta/output/dfMeta_Wilson-Brodie_2017.csv") %>% 
  mutate(museum = "museum", studySub = "threshold_half_max")

## Sagarin 2010
sagarin <- read_csv("sbs_meta/output/dfMeta_Sagarin_2010.csv") %>% 
  mutate(museum = "museum") 

## Select first museum sample and field for Roy (using raw data now)
roy <- read_csv("sbs_meta/output/dfMeta_Roy2003_raw.csv") %>% 
  mutate(museum = "museum", studySub = "threshold_half_max")

## Compile
dat <- rbind(elahi, gall_frank, hay_king, hay_elahi, #gall_tren, 
             fisher, wilson, roy, sagarin) 

### Modify species names
dat <- dat %>% 
  mutate(species2 = ifelse(study == "WilsonBrodie_2017", 
                           paste(species, "(UK)", sep = " "), species), 
         species2 = ifelse(study == "Galloway_2017" & species == "Tegula funebralis", 
                           paste(species, "(OR)", sep = " "), species2), 
         species2 = ifelse(study == "Hayford-Elahi_2018", 
                           paste(species, "(SC)", sep = " "), species2), 
         species2 = ifelse(study == "Hayford_2017", 
                           paste(species, "(FB)", sep = " "), species2))

dat <- dat %>% 
  mutate(threshold = ifelse(studySub == "threshold_none", "no", "yes"))

##### PREP FOR METAFOR #####
## For metafor, I want the data in wide format
names(dat)

dat2 <- dat %>% 
  select(study, studySub, threshold, species, species2, museum, latitude, era, 
         size_rep, size_error, sample_size) %>% 
  rename(size_n = sample_size)
dat2
names(dat2)

datM <- dat2 %>% 
  gather(key = temp, value = value, starts_with("size")) %>% 
  unite(col = temp1, era, temp, sep = "_") %>%
  spread(temp1, value) %>% 
  arrange(desc(latitude))
datM

### Re-order by latitude
datM <- datM %>% 
  mutate(species2 = reorder(species2, latitude)) %>% 
  arrange(desc(latitude))
datM

## Column names for metafor
names(datM)
names(datM) <- c("study", "studySub", "threshold", "species", "species2", "museum", "latitude", 
                 "sd2i", "n2i", "m2i", "sd1i", "n1i", "m1i")
datM

### calculate log ratio of means and corresponding sampling variances
datM <- escalc(measure = "ROM", m1i = m1i, sd1i = sd1i, n1i = n1i, 
               m2i = m2i, sd2i = sd2i, n2i = n2i, data = datM)
datM

### Get standard error and CI (following Vuorre)
datM <- datM %>% 
  mutate(sei = as.numeric(sqrt(vi)), 
         upper = yi + 2*sei, 
         lower = yi - 2*sei)

datM <- datM %>% 
  mutate(museum01 = ifelse(museum == "museum", 0, 1))

#### CALCULATE LOG RATIO OF MEANS BY STUDIES #####

## For mean size
datM_mean <- datM %>% 
  filter(threshold == "no")
res <- rma(yi, vi, method = "DL", data = datM_mean, slab = datM_mean$species2)
forest(res)
# Create results dataframe
res_df_mean <- data.frame(yi = res$b, 
                     upper = res$ci.ub, 
                     lower = res$ci.lb, 
                     size_cat = "mean",
                     data = "complete size-freq only", 
                     n = dim(datM_mean)[1])

## For max size
datM_max <- datM %>% 
  filter(threshold == "yes")
res <- rma(yi, vi, method = "DL", data = datM_max, slab = datM_max$species2)
forest(res)
ranef(res)
funnel(res)

## Studies as random effect
res <- rma.mv(yi = yi, V = vi, 
              data = datM_max, 
              slab = datM_max$species2, 
              random = ~ 1 | study)

pdf(file = "sbs_meta/meta_figs/forest_max.pdf", width = 7, height = 7)
forest(res)
dev.off()

# Create results dataframe
res_df_max <- data.frame(yi = res$b,
                         upper = res$ci.ub, 
                         lower = res$ci.lb, 
                         size_cat = "max", 
                         data = "all data", 
                         n = dim(datM_max)[1])

## Results, as is
datM_asis <- datM %>% 
  filter(study == "Roy_2003" | study == "WilsonBrodie_2017")
datM_asis <- rbind(datM_asis, datM_mean) %>% 
  arrange(desc(latitude))
res <- rma(yi, vi, method = "REML", data = datM_asis, slab = datM_asis$species2)
res <- rma.mv(yi = yi, V = vi, 
              data = datM_asis, 
              slab = datM_asis$species2, 
              random = ~ 1 | study)
res
forest(res, alim = c(-0.5, 0.5))
funnel(res)
ranef(res)

pdf(file = "sbs_meta/meta_figs/forest_asis.pdf", width = 7, height = 7)
forest(res)
dev.off()

# Create results dataframe
res_df_asis <- data.frame(yi = res$b, 
                         upper = res$ci.ub, 
                         lower = res$ci.lb, 
                         size_cat = NA, 
                         data = "all data", 
                         n = dim(datM_asis)[1])

## Our unpublished results only (field)
datM_field <- datM %>% 
  filter(museum == "field" & threshold == "no")
datM_field
res <- rma(yi, vi, method = "DL", data = datM_field, slab = datM_field$species2)
res
forest(res)

# Create results dataframe
res_df_field <- data.frame(yi = res$b, 
                          upper = res$ci.ub, 
                          lower = res$ci.lb, 
                          size_cat = "mean", 
                          data = "field", 
                          n = dim(datM_field)[1])

## Published results only (museum); as is
datM_museum <- datM_asis %>% 
  filter(museum == "museum")
datM_museum 
res <- rma(yi, vi, method = "DL", data = datM_museum, slab = datM_museum$species2)
res
forest(res)
# Create results dataframe
res_df_museum <- data.frame(yi = res$b, 
                           upper = res$ci.ub, 
                           lower = res$ci.lb, 
                           size_cat = NA, 
                           data = "museum", 
                           n = dim(datM_museum)[1])

##### COMBINE RESULTS ####
res_df <- rbind(res_df_asis, res_df_max, res_df_mean, res_df_field, res_df_museum) 
res_df
