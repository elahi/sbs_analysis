################################################################################
##' @title Plot seawater and air temperature time-series together
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-09-13
##' 
##' @log 
################################################################################

source("2_summarise_data/summarise_temperature.R")
annual_df_long

##### GET YEARS OF SNAIL SAMPLES #####
head(dat4)

spYrs <- dat4 %>% ungroup() %>% select(species, year) %>% distinct() %>% 
  arrange(species, year) %>% 
  group_by(species) %>% 
  mutate(genus = unlist(strsplit(as.character(species), split = " +"))[1]) %>%
  ungroup()

spYrs

spYrs$genus <- factor(spYrs$genus, levels = c("Chlorostoma", "Lottia", "Littorina"))

##### TWO-PANEL PLOT #####

sort(unique(lim_annual_seasons_long2$tidalHT))

lim_for_sst <- lim_annual_seasons_long2 %>% 
  filter(tidalHT == 1.1) %>% 
  mutate(dataset = "Seawater")

lim_for_air <- lim_annual_seasons_long2 %>% 
  filter(tidalHT == 7.55) %>% 
  mutate(dataset = "Air")

text_df <- data.frame(x = c(rep(1940, 3)), 
                      y = c(6.5, 13, 21), 
                      text1 = c("Min", "Median", "Max"), 
                      dataset = rep("Air", 3), 
                      metric = c("minimum", "median", "maximum"))

facet_label_text <- data.frame(x = rep(1938, 2), 
                               y = rep(23.3, 2), 
                               text1 = c("A", "B"), 
                               dataset = c("Air", "Seawater"))

temp_records %>% 
  ggplot(aes(year, tempC, color = metric)) + 
  geom_smooth(data = subset(temp_records, dataset == "Seawater" & 
                              metric == "maximum"), method = "lm", 
              color = "darkgray", linetype = "dashed") + 
  # geom_smooth(data = subset(temp_records, dataset == "Air" & 
  #                             metric == "maximum"), method = "lm", 
  #             color = "darkgray", linetype = "dashed") + 
  geom_smooth(data = subset(temp_records, dataset == "Air" & 
                              metric == "minimum"), method = "lm", 
              color = "darkgray", linetype = "dashed") + 
  geom_line(alpha = 1) +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  guides(color = FALSE) + 
  guides(shape = 
           guide_legend(title = "SAMPLING EVENT", title.position = "top", 
                        title.hjust = 0.5, 
                        title.theme = 
                                element_text(size = 10, face = "bold", angle = 0), 
                        direction = "horizontal", 
                        label.theme = element_text(angle = 0, size = 8, 
                                                   face =  "italic"))) + 
  facet_wrap(~ dataset) + 
  theme(strip.background = element_blank()) + 
  geom_line(data = lim_for_sst, 
             aes(year, tempC), alpha = 0.25, size = 1.5) + 
  geom_line(data = lim_for_air, 
            aes(year, tempC), alpha = 0.25, size = 1.5) + 
  geom_line(data = lim_for_sst, 
            aes(year, tempC, group = metric), alpha = 1, size = 0.25, color = "black") + 
  geom_line(data = lim_for_air, 
            aes(year, tempC, group = metric), alpha = 1, size = 0.25, color = "black") +
  geom_text(aes(x, y, label = text1), data = text_df, size = 3, 
            hjust = 0, fontface = "bold") + 
  geom_point(aes(x = year, y = 5, shape = species, color = NULL), data = spYrs, 
             alpha = 0.6, size = 2) + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = facet_label_text, size = 5, hjust = 0.25, vjust = 0.75, 
            show.legend = FALSE) 
  

ggsave("figs/temp_timeseries.png", height = 4.5, width = 7)
  