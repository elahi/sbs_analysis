################################################################################
##' @title Plot limpet temperatures
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-16
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

source("05_summarise_limpet_temps.R")

##### LOAD PACKAGES, DATA #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
theme_set(theme_bw(base_size = 12))
library(lubridate)

# Load the file that I sent to Luke for the hindcasts
positionDF <- read_csv("output/positionDF_edit.csv")[-1] %>% 
  select(position, sp, nest1:tidalHT, aspect, slope_normal, lon, lat, run_id) %>%
  rename(azimuth = aspect, aspect = slope_normal)
positionDF

# Load daily summary data (what was the max, median, min temp each day?)
# source("process_limpet_temps.R")
lim_daily <- read_csv("output/limpet_daily_temps.csv")[-1]
lim_daily
tail(lim_daily)

lim_dailyL <- lim_daily %>% select(-c(mean:cv)) %>% 
  gather(., key = "metric", value = "tempC", maximum:minimum)

lim_dailyL <- lim_dailyL %>% 
  mutate(month = month(dateR), 
         jDay = yday(dateR)) 

##### SUMMARISE DAILY #####

# Get daily summary stats 
lim_daily_summary <- lim_dailyL %>% 
  group_by(run_id, metric) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se, 
            max = max(tempC)) %>% 
  ungroup()

# Join with position data
lds2 <- inner_join(lim_daily_summary, positionDF, by = "run_id")

lds3 <- inner_join(lds2, spCodes, by = "sp")
unique(lds3$metric)

##### SUMMARISE AUG 1 - SEPT 12 #####

##' For the temperature logger data, I calculated the average daily maximum, median, and minimum temperature across 6 weeks (August1 - September 12, 2015)
##' The most relevant comparison would be to calculate a monthly mean for daily max, median, min etc., then select the hottest months across the entire 13 years

### Select the relevant dates (213-255 julian days)
head(lim_dailyL)
ldlSub <- lim_dailyL %>% filter(jDay <= 255 & jDay >=213)

### Now for each year, calculate the summary stats

ldlSub_summary <- ldlSub %>% group_by(run_id, metric, year) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se, 
            max = max(tempC)) %>% 
  ungroup() 

# Join with position data
ldlSub_summary2 <- inner_join(ldlSub_summary, positionDF, by = "run_id")

ldlSub_summary3 <- inner_join(ldlSub_summary2, spCodes, by = "sp")

##### SUMMARISE MONTHLY #####
unique(lim_dailyL$metric)

lim_monthly <- lim_dailyL %>% group_by(run_id, metric, month, year) %>% 
  summarise(monthly_mean = mean(tempC)) %>% 
  ungroup() %>% 
  mutate(dateR = as.Date(paste(year, month, "15", sep = "-")))

lim_monthly_summary <- lim_dailyL %>% group_by(run_id, metric, month, year) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se, 
            max = max(tempC)) %>% 
  ungroup() %>% 
  mutate(dateR = as.Date(paste(year, month, "15", sep = "-")))

# Join with position data
lms2 <- inner_join(lim_monthly_summary, positionDF, by = "run_id")

lms3 <- inner_join(lms2, spCodes, by = "sp")
unique(lms3$metric)

head(lms3)

# Extract the row with the maximum monthly 
monthly_max <- lms3 %>% filter(metric == "maximum") %>% 
  group_by(run_id) %>% arrange(desc(mean)) %>% ungroup() %>%
  group_by(run_id) %>% slice(1)

# Extract the row with the minimum monthly 
monthly_min <- lms3 %>% filter(metric == "minimum") %>% 
  group_by(run_id) %>% arrange(mean) %>% ungroup() %>%
  group_by(run_id) %>% slice(1)

# Extract the row with the median median? 
# 168 observations for each run_id metric 
# (18 runs x 14 years x 12 months * 3 metrics = 9072)
monthly_med <- lms3 %>% filter(metric == "median") %>% 
  group_by(run_id) %>% arrange(desc(mean)) %>% ungroup() %>% 
  group_by(run_id) %>% slice(168/2)

global_monthly_values <- rbind(monthly_max, monthly_med, monthly_min)

##### SUMMARISE ANNUAL #####

lim_annual <- lim_monthly %>% group_by(run_id, metric, year) %>% 
  summarise(maximum = max(monthly_mean), 
            median = median(monthly_mean), 
            minimum = min(monthly_mean)) %>% 
  ungroup()

la_max <- lim_annual %>% filter(metric == "maximum") %>% 
  select(run_id, year, maximum) 
la_med <- lim_annual %>% filter(metric == "median") %>% 
  select(run_id, year, median) 
la_min <- lim_annual %>% filter(metric == "minimum") %>% 
  select(run_id, year, minimum) 

la_summary <- inner_join(la_max, la_med, by = c("run_id", "year")) %>% 
  inner_join(., la_min, by = c("run_id", "year"))
la_summL <- gather(la_summary, key = "metric", value = "tempC", maximum:minimum)

la_summL2 <- la_summL %>% 
  inner_join(., positionDF, by = "run_id") %>% 
  inner_join(., spCodes, by = "sp")

annual_summary <- la_summL2 %>% 
  group_by(position, tidalHT, azimuth, aspect, species, metric) %>% 
  summarise(mean = mean(tempC), 
            sd = sd(tempC),
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()

##### PLOTS ######

## Plot means of daily median, max and min
lds3 %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  labs(x = "Tidal height (m)", y = "Mean temperature (C)") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  geom_point(data = subset(lds3, metric == "maximum"), 
             aes(tidalHT, max, color = NULL), alpha = 0.6, size = 2)

##### PLOT ANNUAL MAX, MEDIAN, MIN BY TIDAL HEIGHT ######

annual_summary %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  ylab(expression(paste("Predicted body temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

ggsave("figs/elahi_predicted_temp_tidal.png", height = 3.5, width = 7)

##### PLOT GLOBAL MONTHLY VALUES ######
global_monthly_values

global_monthly_values %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_point(alpha = 0.6, size = 2) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.2, alpha = 0.6) + 
  ylab(expression(paste("Predicted temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~ species)
  
ggsave("figs/elahi_predicted_temp_tidal_global.png", height = 3.5, width = 7)

##### PLOT ANNUAL MAX, MEDIAN, MIN TIME SERIES ######

la_summL2 %>% 
  ggplot(aes(year, tempC, color = metric)) +
  facet_wrap(~ species, ncol = 5) + 
  geom_point(alpha = 0.6) + geom_smooth(method = "lm") + 
  ylab(expression(paste("Predicted body temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(strip.background = element_blank()) + 
  facet_wrap(~ species) + 
  guides(shape = FALSE) + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) 

