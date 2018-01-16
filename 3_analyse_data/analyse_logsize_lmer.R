################################################################################
##' @title Analyze log size
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2018-01-15
##' 
##' @log 
################################################################################

##### PACKAGES, DATA #####
source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")
library(lme4)
library(ggplot2)
theme_set(theme_bw() + 
            theme(panel.grid = element_blank()))
library(sjPlot)
library(broom)

statDat <- childsDF
statDat <- hexDF
statDat <- waraDF %>% filter(sampleArea == "Wara.B")
statDat <- waraDF %>% filter(sampleArea == "Wara.D")

## My quantile for size threshold
my_quantile <- 0.0

##### CHFU - B #####

## Select data
statDat <- waraDF %>% filter(sampleArea == "Wara.B")
## My species
my_species <- "CHFU"
## My model
my_model <- "pooled"

## Run analysis
statDat <- truncate_data(statDat, era = "past", quant = my_quantile)

lm1 <- lm(size_log ~ era, data = statDat)

lm1_tidyfit <- tidy(lm1) %>% 
  mutate(upper = estimate + 1.96 * std.error,
         lower = estimate - 1.96 * std.error, 
         spp = my_species, 
         model = my_model, 
         sampleArea = ifelse(my_model == "pooled", 
                             unique(as.character(statDat$sampleArea)), NA))

lm1_tidyfit

tidyfits <- lm1_tidyfit

##### CHFU - D #####

## Select data
statDat <- waraDF %>% filter(sampleArea == "Wara.D")
## My species
my_species <- "CHFU"
## My model
my_model <- "pooled"

## Run analysis
statDat <- truncate_data(statDat, era = "past", quant = my_quantile)

lm1 <- lm(size_log ~ era, data = statDat)

lm1_tidyfit <- tidy(lm1) %>% 
  mutate(upper = estimate + 1.96 * std.error,
         lower = estimate - 1.96 * std.error, 
         spp = my_species, 
         model = my_model, 
         sampleArea = ifelse(my_model == "pooled", 
                             unique(as.character(statDat$sampleArea)), NA))

lm1_tidyfit

tidyfits <- rbind(tidyfits, lm1_tidyfit)

##### LODI #####

## Select data
statDat <- hexDF
## My species
my_species <- "LODI"
## My model
my_model <- "hierarchical"

## Run analysis
statDat <- truncate_data(statDat, era = "past", quant = my_quantile)

##' Hierarchical model
lmer1 <- lmer(size_log ~ era + (era | sampleArea), data = statDat)

lmer1_tidyfit <- tidy(lmer1) %>% 
  mutate(upper = estimate + 1.96 * std.error,
         lower = estimate - 1.96 * std.error, 
         spp = my_species, 
         model = my_model, 
         sampleArea = ifelse(my_model == "pooled", 
                             unique(as.character(statDat$sampleArea)), NA), 
         p.value = NA) %>% 
  select(-group)

tidyfits <- rbind(tidyfits, lmer1_tidyfit)

##### LIKE #####

## Select data
statDat <- childsDF
## My species
my_species <- "LIKE"
## My model
my_model <- "hierarchical"

## Run analysis
statDat <- truncate_data(statDat, era = "past", quant = my_quantile)

##' Hierarchical model
lmer1 <- lmer(size_log ~ era + (era | sampleArea), data = statDat)

lmer1_tidyfit <- tidy(lmer1) %>% 
  mutate(upper = estimate + 1.96 * std.error,
         lower = estimate - 1.96 * std.error, 
         spp = my_species, 
         model = my_model, 
         sampleArea = ifelse(my_model == "pooled", 
                             unique(as.character(statDat$sampleArea)), NA), 
         p.value = NA) %>% 
  select(-group)

tidyfits <- rbind(tidyfits, lmer1_tidyfit)

##### PLOT #####
names(tidyfits)

plot_dat <- tidyfits %>% 
  filter(term == "erapresent") %>% 
  mutate(sample_order = c(1.9, 2.1, 3, 4), 
         sample_size = c(1, 1, 8, 4))

plot_dat %>% 
  ggplot(aes(sample_order, estimate, color = model, 
             size = sample_size)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper, size = NULL), width = 0.1) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = 'gray') + 
  labs(x = "", 
       y = "Proportional change in size") + 
  theme(legend.position = "none") +
  coord_flip() + 
  theme(#axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave("figs_ms/plot_size_change_lmer.pdf", height = 3.5, width = 3.5)
