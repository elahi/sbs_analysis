################################################################################
##' @title Plot residuals of temperature time series in context of 18y lunar cycles
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-12-05
##' 
##' @log 
################################################################################

#rm(list=ls(all=TRUE)) 

# Get temperature regression results
source("06_analyse_temp_time_series.R")

air_annual_long <- air_annual_long %>% 
  mutate(dataset = "Air")

sst_annual_long <- sst_annual_long %>% 
  mutate(dataset = "Seawater")

### Combine data
temp_records <- rbind(sst_annual_long, air_annual_long)
unique(temp_records$dataset)

gls_time_series_sst

library(cowplot)
library(broom)
library(zoo)

theme_set(theme_bw(base_size = 12))

##### CALCULATE RESIDUALS #####

 ## Seawater
gls_annual_summary <- sst_annual_long %>% group_by(metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()

sst_equation <- gls_annual_summary %>% select(metric, value, coef_name) %>% 
  spread(key = coef_name, value = value) %>% 
  mutate(dataset = "Seawater")

## Air
gls_annual_summary <- air_annual_long %>% group_by(metric) %>% 
  do(get_gls_results(df = ., dep_var = "tempC", ind_var = "year")) %>%
  ungroup()

air_equation <- gls_annual_summary %>% select(metric, value, coef_name) %>% 
  spread(key = coef_name, value = value) %>% 
  mutate(dataset = "Air")

equation_df <- rbind(air_equation, sst_equation)

## Join with time series data
temp_records2 <- inner_join(temp_records, equation_df, by = c("metric", "dataset")) %>% 
  mutate(resid_val = tempC- (slope * year + intercept))

temp_records2 %>% 
  ggplot(aes(year, resid_val, color = metric)) + 
  geom_line() + 
  facet_grid(dataset ~ metric) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray")

##### GET YEARS OF SNAIL SAMPLES #####
head(dat4)

spYrs <- dat4 %>% ungroup() %>% select(species, year) %>% distinct() %>% 
  arrange(species, year) %>% 
  group_by(species) %>% 
  mutate(genus = unlist(strsplit(as.character(species), split = " +"))[1]) %>%
  ungroup()

spYrs

spYrs$genus <- factor(spYrs$genus, levels = c("Chlorostoma", "Lottia", "Littorina"))

##### GET MOVING AVERAGES #####
temp_records2

temp_records3 <- temp_records2 %>% 
  arrange(dataset, metric, year) %>% 
  group_by(dataset, metric) %>% 
  mutate(temp_ma = rollmean(x = resid_val, 3, align = "center", fill = NA)) %>% 
  ungroup()
temp_records3

##### GET LUNAR CYCLES #####

# moons 18.61 yr cycle peaked in 2006 and 2024-25
# 18.61 
# 9.305
# sinusoidal

inclin_max <- seq(2024.6, 1940, by = -18.6)
inclin_min

inclin_min <- seq((2024.6 - 9.305), 1940, by = -18.6)
inclin_max

moon_df <- data.frame("inclin_min" = inclin_min, "inclin_max" = inclin_max) %>% 
  gather(key = inclination, value = value) %>%
  mutate(y_position = c(rep(1, 5), rep(-1, 5))) %>% 
  filter(value < 2016)
moon_df

##### SIX-PANEL PLOT OF RESIDUALS #####

plot_resids <- temp_records3 %>% 
  ggplot(aes(year, resid_val, color = metric)) + 
  geom_point(aes(value, y_position, color = NULL), alpha = 0.3, size = 3, data = moon_df) + 
  geom_line(alpha = 0.5, size = 0.5) + 
  geom_point(alpha = 0.5, size = 1) + 
  facet_grid(dataset ~ metric, switch = "y") + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray") +
  ylab(expression(paste("Deviation from mean temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") + 
  geom_point(aes(x = year, y = -2, shape = species, color = NULL), data = spYrs, 
             alpha = 0.6, size = 2) + 
  guides(shape = FALSE) + 
  geom_line(aes(year, temp_ma, color = metric), size = 1) + 
  theme(strip.background = element_blank()) 
plot_resids
ggsave("figs/temp_timeseries_resids.png", height = 5, width = 7)

##### SPECTRAL ANALYSIS #####
library(astsa)

##' See pages 122 - 126 of tsaEZ.pdf (Shumway 2016 - Time Series Analysis and Applications)
##' Padded automatically to 80
##' No taper

get_spectrum <- function(df){
  
  x = df$tempC
  x.per = mvspec(x, log = "no", plot = FALSE)
  df_per <- data.frame(freq = x.per$freq, 
                       spec = x.per$spec)
  return(df_per)
  
}

spectra_df <- temp_records3 %>% 
  group_by(dataset, metric) %>% 
  do(get_spectrum(df = .)) %>% ungroup() %>%
  mutate(period = 1/spec)
spectra_df

plot_spectra <- spectra_df %>% 
  ggplot(aes(freq, spec, color = metric))  + 
  geom_vline(aes(xintercept = 1/18.6), linetype = "solid", color = "gray", alpha = 0.5, 
             size = 2) + 
  geom_line() + geom_point() + 
  xlab("Frequency (1/yr)") + ylab("Spectrum") + 
  facet_grid(dataset ~ metric, switch = "y") + 
  theme(strip.background = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none")
plot_spectra  

ggsave("figs/temp_timeseries_spectra.png", height = 5, width = 7)

plot_resids_spectra <- plot_grid(plot_resids, plot_spectra, labels = c("A", "B"), nrow = 2, align = "v")

save_plot("figs/plot_resids_spectra.png", plot_resids_spectra,
          ncol = 1, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3, 
          base_width = 7
)

##### SEAWATER MINIMUM ONLY #####

plot_resids_min <- temp_records3 %>% 
  filter(dataset == "Seawater" & metric == "minimum") %>% 
  ggplot(aes(year, resid_val)) + 
  geom_point(aes(value, y_position, color = NULL), alpha = 0.3, size = 5, data = moon_df) + 
  geom_line(alpha = 0.5, size = 0.5) + 
  geom_point(alpha = 0.5, size = 1) + 
  #facet_grid(dataset ~ metric, switch = "y") + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray") +
  #ylab(expression(paste("Deviation from fitted temperature (", degree, "C)"))) + 
  ylab(expression(atop("Deviation from fitted", paste("temperature (", degree, "C)")))) + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") + 
  geom_point(aes(x = year, y = -2, shape = species, color = NULL), data = spYrs, 
             alpha = 0.6, size = 2) + 
  guides(shape = FALSE) + 
  geom_line(aes(year, temp_ma, color = NULL), size = 1) + 
  theme(strip.background = element_blank()) 
plot_resids_min

plot_spectra_min <- spectra_df %>% 
  filter(dataset == "Seawater" & metric == "minimum") %>% 
  ggplot(aes(freq, spec, color = NULL))  + 
  geom_vline(aes(xintercept = 1/18.6), linetype = "solid", color = "gray", alpha = 0.5, 
             size = 2) + 
  geom_line() + geom_point() + 
  xlab("Frequency (1/yr)") + ylab("Spectrum") + 
  #facet_grid(dataset ~ metric, switch = "y") + 
  theme(strip.background = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none")
plot_spectra_min


plot_resids_spectra_min <- plot_grid(plot_resids_min, plot_spectra_min, labels = c("A", "B"), 
                                     nrow = 1, align = "h")

save_plot("figs/plot_resids_spectra_min.png", plot_resids_spectra_min,
          ncol = 2, nrow = 1,
          base_aspect_ratio = 1.2, base_height = 3.5
)


##### TO DO: ADD SECOND X-AXIS TO SHOW PERIOD ON SPECTRAL PLOTS #####

plot_spectra_period <- spectra_df %>% 
  ggplot(aes(period, spec, color = metric))  + 
  geom_vline(aes(xintercept = 18.6), linetype = "solid", color = "gray", alpha = 0.5, 
             size = 2) + 
  geom_line() + geom_point() + 
  xlab("Frequency (1/yr)") + ylab("Spectrum") + 
  facet_grid(dataset ~ metric, switch = "y") + 
  theme(strip.background = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none")

plot_spectra_period 



#######################
##############################################
#######################
x.per = mvspec(x, log = "no") 
abline(v = 1/18.6, lty = "dotted")

par(mfrow=c(1,2))
x = sst_annual$minimum
n = length(x)
Per = Mod(fft(x - mean(x)))^2/n
Freq = (1:n - 1)/n
Freq
plot(Freq[1:50], Per[1:50], type='l', lwd=3, ylab="Periodogram",
     xlab="Frequency")

x.per = mvspec(x, log = "no", plot = F); 
abline(v = 1/18.6, lty = "dotted")
x.per

plot(x.per$freq, x.per$spec, type = "l")


par(mfrow=c(1,1))
x = air_annual$minimum
n = length(x)
Per = Mod(fft(x - mean(x)))^2/n
Freq = (1:n - 1)/n
Freq
plot(Freq[1:50], Per[1:50], type='h', lwd=3, ylab="Periodogram",
     xlab="Frequency")


\