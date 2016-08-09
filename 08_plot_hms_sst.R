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

# Get hms temperature data
source("R/get_hms_sst.R")

# Get hms temperature functions
source("R/hms_sst_functions.R")

# Get snail size data
source("03_identify_size_cutoff.R")

# Original data were single, daily measurements
glimpse(sst_hms)

# These are mean values of monthly mean, max, and min
glimpse(sst_annual)

##### GET ANNUAL TIME SERIES #####

# I want a time-series starting 10 years prior to the first historic sampling year
# Childs sampled Littorina in 1947
# So, start year is 1938

elahi_sst_monthly <- sst_monthly %>% filter(year > 1937) %>% filter(hmsTemp == "corrected") %>%
  select(-monthly_sd)

elahi_sst <- elahi_sst_monthly %>% filter(year > 1937) %>% 
  group_by(year) %>% 
  summarise(mean_C = mean(monthly_mean), 
            max_C = max(monthly_mean), 
            min_C = min(monthly_mean), 
            cv_C = mean(monthly_cv))

# Get in long format
monthly_sstL <- elahi_sst_monthly %>% gather(key = metric, value = tempC, monthly_mean:monthly_cv)
sstL <- elahi_sst %>% gather(key = metric, value = tempC, mean_C:cv_C)

# Get median, and upper and lower values per year
head(sst_hms)
annual_quants <- sst_hms %>%  filter(year > 1937) %>% group_by(year) %>% 
  summarise(quantile_0.1 = quantile(tempC, 0.1), 
            quantile_0.5 = quantile(tempC, 0.5), 
            quantile_0.9 = quantile(tempC, 0.9)) %>% 
  ungroup()

aql <- annual_quants %>% gather(key = quantileValue, value = tempC, quantile_0.1:quantile_0.9)

##### GET YEARS OF SNAIL SAMPLES #####
head(dat4)

spYrs <- dat4 %>% select(species, year) %>% distinct() %>% 
  arrange(species, year) %>% 
  group_by(species) %>% 
  mutate(genus = unlist(strsplit(as.character(species), split = " +"))[1]) %>%
  ungroup()

spYrs$genus <- factor(spYrs$genus, levels = c("Chlorostoma", "Lottia", "Littorina"))

##### PLOT ANNUAL TIME SERIES #####

sstL %>% filter(metric != "cv_C") %>% 
  ggplot(aes(year, tempC, color = metric)) + 
  geom_line(alpha = 1) + 
  # theme_bw(base_size = 10) + 
  scale_color_manual(values = c("red", "black", "blue")) + 
  # geom_smooth(method = "lm")  + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  geom_point(aes(x = year, y = 10, shape = genus, color = NULL), data = spYrs, 
             alpha = 0.5, size = 3) + 
  guides(color = FALSE) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") 
  #theme(legend.key.size = unit(0.2, "cm")) + 
  #theme(legend.text = element_text(size = 8))

ggsave("figs/hms_temperature.png", height = 3.5, width = 3.5)

aql %>% 
  ggplot(aes(year, tempC, color = quantileValue)) + 
  geom_line(alpha = 1) + 
  #geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("blue", "black", "red")) + 
  # geom_smooth(method = "lm")  + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  # theme(legend.position = "none") + 
  geom_point(aes(x = year, y = 10, shape = genus, color = NULL), data = spYrs, 
             alpha = 0.5, size = 3) + 
  guides(color = FALSE) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top")
  # theme(legend.position = c(0,1), legend.justification = c(0,1)) + 
  # theme(legend.key.size = unit(0.25, "cm"))

ggsave("figs/hms_temp_quantiles.png", height = 3.5, width = 3.5)

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

