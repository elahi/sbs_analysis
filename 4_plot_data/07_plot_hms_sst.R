################################################################################
##' @title Plot SST data from HMS website
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-04
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

##### GET DATA #####

source("05_summarise_hms_sst.R")

# wide format
species_sst_annual
# long format
sp_sst_ann_L

##### PLOT ANNUAL TIME SERIES #####
unique(sp_sst_ann_L$metric) 
ggDat <- sp_sst_ann_L

## All three species
ggDat %>%
  ggplot(aes(year, tempC, color = metric)) + 
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("red", "black", "blue")) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") +
  facet_wrap(~ species, nrow = 3) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  geom_smooth(data = subset(ggDat,metric == "max_C"), 
              aes(year, tempC), method = "lm") +
  geom_point(aes(x = year, y = 10.5, shape = species, color = NULL), data = spYrs, 
             alpha = 0.5, size = 3) + 
  guides(color = FALSE, shape = FALSE)

## Just Littorina
ggDat %>% filter(species == "Littorina keenae") %>% 
  ggplot(aes(year, tempC, color = metric)) + 
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("red", "black", "blue")) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  geom_smooth(data = subset(ggDat,metric == "max_C"), 
              aes(year, tempC), method = "lm") +
  geom_point(aes(x = year, y = 10.5, shape = genus, color = NULL), data = spYrs, 
             alpha = 0.5, size = 3) + 
  guides(color = FALSE)


##### PLOT ANNUAL TIME SERIES #####

ggDat <- sstL %>% filter(metric != "cv_C" & metric != "mean_C")

ggDat %>%
  ggplot(aes(year, tempC, color = metric)) + 
  geom_point(alpha = 0.5) + 
  #geom_smooth(method = "lm") + 
  scale_color_manual(values = c("red", "black", "blue")) + 
  ylab(expression(paste("Sea surface temperature (", degree, "C)"))) + 
  xlab("Year") + 
  geom_point(aes(x = year, y = 10.5, shape = genus, color = NULL), data = spYrs, 
             alpha = 0.5, size = 3) + 
  guides(color = FALSE) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  geom_smooth(data = subset(ggDat,  metric == "max_C"), 
              aes(year, tempC), 
              method = "lm")

ggsave("figs/hms_sst.png", height = 3.5, width = 3.5)


##### GET TEMPERATURE BY ERA #####

temp_period_dat <- get_hms_two_periods(size_data = dat4)

##### PLOT TEMPERATURE BY ERA #####

names(temp_period_dat)

dodge <- position_dodge(width = 0.9)

# Daily temperature comparison
temp_period_dat %>% 
  ggplot(aes(species, tempC, fill = era)) +
  geom_boxplot(position = dodge, notch = TRUE) 

## Get monthly values
temp_monthly <- temp_period_dat %>% group_by(species, era, year, month) %>% 
  summarise(monthly_mean = mean(tempC), 
            monthly_max = max(tempC), 
            monthly_min = min(tempC), 
            monthly_sd = sd(tempC), 
            monthly_cv = monthly_sd/monthly_mean * 100) %>%
  mutate(dateR = ymd(paste(year, month, 15, sep = "-"))) %>% 
  ungroup()

temp_monthly

temp_monthlyL <- temp_monthly %>% gather(key = metric, value = tempC, 
                                         monthly_mean:monthly_min)
temp_monthlyL

temp_monthlyL %>% 
  ggplot(aes(metric, tempC, fill = era)) +
  geom_boxplot(position = dodge, notch = TRUE) + 
  facet_wrap(~ species)

ggsave("figs/hms_temp_era.png", height = 3.5, width = 7)


## Get annual values

temp_annual <- temp_monthly %>% 
  group_by(species, era, year) %>% 
  summarise(mean_C = mean(monthly_mean), 
            max_C = max(monthly_mean), 
            min_C = min(monthly_mean), 
            cv_C = mean(monthly_cv))

# Get in long format
temp_annualL <- temp_annual %>% gather(key = metric, value = tempC, mean_C:min_C)

temp_annualL %>% 
  ggplot(aes(metric, tempC, fill = era)) +
  geom_boxplot(position = dodge, notch = FALSE) + 
  facet_wrap(~ species)

