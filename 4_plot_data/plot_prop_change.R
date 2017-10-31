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

rm(list=ls(all=TRUE)) 

##### LOAD PACKAGES, DATA #####

library(cowplot)
options(tibble.print_max = 50, tibble.print_min = 10)

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "plain"), 
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

# jitter points a bit
jitter_value = 0.01
coda_df_all <- coda_df_all %>% 
  mutate(quant = as.numeric(quant), 
         quant2 = ifelse(sp == "CHFU", quant - jitter_value, 
                         ifelse(sp == "LIKE", quant + jitter_value, quant)))

coda_df_all %>% 
  filter(param == "prop_change") %>% 
  filter(sp != "CHFU_means") %>% 
  ggplot(aes(quant2, X50., shape = sp)) + 
  geom_point() + 
  geom_line(aes(linetype = sp)) + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  ylab("Proportional change in size (mm)") + 
  xlab("Size threshold (quantile of past size)") + 
  theme(legend.position = "top") + 
  coord_cartesian(ylim = c(-0.4, 0.4)) + 
  facet_wrap(~ model)

coda_df_all %>% 
  filter(param == "prop_change") %>% 
  filter(sp != "CHFU_means") %>% 
  ggplot(aes(quant2, X50., shape = sp, color = model)) + 
  geom_point() + 
  geom_line(aes(linetype = sp)) + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  ylab("Proportional change in size (mm)") + 
  xlab("Size threshold (quantile of past size)") + 
  theme(legend.position = "top") + 
  coord_cartesian(ylim = c(-0.4, 0.4)) 


theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "italic"), 
                  panel.grid = element_blank())) 
coda_df_all %>% 
  filter(param == "prop_change") %>% 
  filter(sp != "CHFU_means") %>% 
  ggplot(aes(quant, X50., color = model)) + 
  geom_point() + 
  geom_line(aes(linetype = model)) + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  ylab("Proportional change in size (mm)") + 
  xlab("Size threshold (quantile of past size)") + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.99, 0.99), legend.justification = c(0.99, 0.99)) + 
  coord_cartesian(ylim = c(-0.4, 0.4)) + 
  facet_wrap(~ species)
ggsave("3_analyse_data/bayes_figs/logsize_prop_change_2panel.png", height = 3.5, width = 7)

