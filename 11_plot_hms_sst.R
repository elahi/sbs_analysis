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

source("R/get_hms_sst.R")

# Original data were single, daily measurements
glimpse(sst_hms)

# These are mean values of monthly mean, max, and min
glimpse(sst_annual)

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
source("02_sbs_size_dataPrep2.R")
head(dat2)
spYrs <- dat2 %>% select(species, year) %>% distinct() %>% arrange(species, year) %>% 
  group_by(species) %>% 
  mutate(genus = unlist(strsplit(species, split = "\\."))[1]) %>%
  ungroup()
spYrs

##### FINAL PLOTS #####

sstL %>% filter(metric != "cv_C") %>% 
  ggplot(aes(year, tempC, color = metric)) + 
  geom_line(alpha = 1) + 
  scale_color_manual(values = c("red", "black", "blue")) + 
  # geom_smooth(method = "lm")  + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  geom_point(aes(x = year, y = 10, shape = genus, color = NULL), data = spYrs, 
             alpha = 0.5, size = 3) + 
  guides(color = FALSE) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") 

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

