################################################################################
##' @title Plot seawater and air temperature time-series together
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2016-09-13
##' @log 
################################################################################

## Temperature data
source("2_summarise_data/summarise_temperature_annual.R")
annual_df_long

## Years of Elahi snail samples
source("R/choose_size_data.R")
# load data - approximated sizes
dat <- choose_size_data(method = "normal")
# Get relevant years for each species
spYrs <- dat %>% ungroup() %>% select(species, year) %>% distinct() %>% 
  arrange(species, year) 
gen_sp <- spYrs$species
genus <- vector(length = length(gen_sp))
for(i in 1:length(gen_sp)){
  genus[i] <- unlist(strsplit(gen_sp[i], split = ".", fixed = TRUE))[1]
}
spYrs$genus <- genus
spYrs$genus <- factor(spYrs$genus, levels = c("Chlorostoma", "Lottia", "Littorina"))

## Years of Hewatt-Sagarin snail samples
hew <- read_csv("workspace/hewatt_sagarin_data_snail.csv")
hewYrs <- hew %>% distinct(year) %>% filter(year < 2016)
hewYrs

##### TWO-PANEL PLOT #####

air_means <- annual_df_long %>% 
  group_by(dataset, metric) %>% 
  summarise(mean = mean(tempC, na.rm = TRUE)) %>% 
  filter(dataset == "Air") %>% arrange(mean)
air_means

text_df <- data.frame(x = c(rep(1938, 3)), 
                      y = air_means$mean, 
                      text1 = c("Min", "Mean", "Max"), 
                      dataset = rep("Air", 3), 
                      metric = c("minimum", "mean", "maximum"))

facet_label_text <- data.frame(x = rep(1922, 2), 
                               y = rep(24, 2), 
                               text1 = c("A", "B"), 
                               dataset = c("Air", "Seawater"))

theme_set(theme_bw() + 
            theme(panel.grid  = element_blank(), 
                  strip.background = element_blank()))

annual_df_long %>% 
  ggplot(aes(year, tempC, color = metric)) + 
  geom_line(alpha = 1) +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") + 
  #guides(color = FALSE, shape = NULL) + 
  facet_wrap(~ dataset) + 
  geom_text(aes(x, y, label = text1), data = text_df, size = 3, 
            hjust = 0, fontface = "bold") + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = facet_label_text, size = 5, hjust = 0.25, vjust = 0.75, 
            show.legend = FALSE) +
  geom_point(aes(x = year, y = 4.5, shape = species, color = NULL), data = spYrs, alpha = 1, size = 2) + 
  geom_point(aes(x = year, y = 3.5, shape = NULL, color = NULL), 
             data = hew_yrs, alpha = 1, size = 1.5, shape = 8) + 
  scale_shape_manual(values = c(1, 2, 0))
  
ggsave("figs/plot_temp_timeseries.pdf", height = 3.5, width = 7)
  