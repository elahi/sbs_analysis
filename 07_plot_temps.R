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
         dataset = "Rock")

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
         dataset = "Body")

tempDF <- rbind(temp_logger, temp_model)
tempDF

##### PLOTS ######

tempDF %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(color = FALSE) + 
  #theme(legend.position = "top") + 
  #theme(legend.title = element_blank()) + 
  facet_wrap(~ dataset, ncol = 2, scales = "free_y") + 
  # guides(shape = guide_legend(title = "Habitat", 
  #                             title.position = "top"))
  guides(shape = guide_legend(title = "Habitat", 
                              direction = "horizontal", title.position = "top",
                              label.position="bottom", label.hjust = 0.5, 
                              label.vjust = 0.1, label.theme = element_text(angle = 90)))
  

tempDF %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(color = FALSE) + 
  theme(legend.position = "bottom") + 
  #theme(legend.title = element_blank()) + 
  facet_wrap(~ dataset, ncol = 2, scales = "free_y") + 
  guides(shape = guide_legend(title = "Habitat",
                              title.position = "top", 
                              title.hjust = 0.5))


ggsave("figs/elahi_temp_body_rock.png", height = 3.5, width = 7)
