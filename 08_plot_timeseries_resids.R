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
##' 
##' Also see:
##' http://rstudio-pubs-static.s3.amazonaws.com/9428_1197bd003ebd43c49b429f22ea4f36e5.html

## practice
df <- temp_records3 %>% 
  filter(dataset == "Seawater" & metric == "minimum")
df

?mvspec()
?spec.pgram()

mvspec(df$tempC, taper = 0)
spec.pgram(df$tempC, taper = 0)

mvspec(df$tempC, taper = 0, log = "no")
spec.pgram(df$tempC, taper = 0, log = "no")

x = df$tempC
x.per <- mvspec(x, log = "no", plot = FALSE)
df_per <- data.frame(freq = x.per$freq, 
                     spec = x.per$spec)
df_per

# Create a vector of periods to label on the graph, units are in years
yrs.period <- rev(c(1/6, 1/5, 1/4, 1/3, 0.5, 1, 3, 5, 10, 100))
yrs.labels <- rev(c("1/6", "1/5", "1/4", "1/3", "1/2", "1", "3", "5", "10", 
                    "100"))
yrs.freqs <- 1/yrs.period * 1/12  #Convert annual period to annual freq, and then to monthly freq
df_per$period <- 1/df_per$freq

ggplot(df_per) + 
  geom_line(aes(x = freq, y = spec))

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec))

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec))

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec)) + 
  scale_x_log10("Period (years)", breaks = yrs.period, labels = yrs.labels) 

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec)) + 
  scale_x_log10("Period (years)", breaks = rev(yrs.period), labels = rev(yrs.labels)) 

ggplot(df_per) + 
  geom_line(aes(x = freq, y = spec)) + 
  scale_x_continuous("Frequency", breaks = yrs.freqs, labels = yrs.labels) + 
  scale_y_continuous()

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec)) + 
  scale_x_log10("Period (years)")


ggplot(df_per) + 
  geom_line(aes(x = period, y = spec)) + 
  scale_x_log10("Period (years)", breaks = yrs.period, labels = yrs.labels)

ggplot(df_per) + 
  geom_line(aes(x = freq, y = spec)) + 
  scale_x_log10("Period (years)", breaks = yrs.freqs, labels = yrs.labels) 

yrs.period <- rev(c(1, 2, 4, 8, 16, 32, 64))
yrs.labels <- rev(c("1", "2", "4", "8", "16", "32", "64"))

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec)) + 
  scale_x_log10("Period (years)", breaks = rev(yrs.period), labels = rev(yrs.labels)) 

##### RUN FOR ALL TEMPERATURE METRICS #####

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
  mutate(period = 1/freq)

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

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec)) + 
  scale_x_log10("Period (years)", breaks = rev(yrs.period), labels = rev(yrs.labels)) 


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

##### SPECTRAL PLOTS - PERIOD ONLY #####

plot_spectra_period <- spectra_df %>% 
  ggplot(aes(period, spec, color = metric))  + 
  geom_vline(aes(xintercept = 18.6), linetype = "solid", color = "gray", alpha = 0.5, 
             size = 2) + 
  geom_line() + geom_point() + 
  scale_x_log10("Period (years)", breaks = rev(yrs.period), 
                labels = rev(yrs.labels)) +
  ylab("Spectrum") + 
  facet_grid(dataset ~ metric, switch = "y") + 
  theme(strip.background = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none")

plot_spectra_period  

ggsave("figs/temp_timeseries_spectra_period.png", height = 5, width = 7)

plot_resids_spectra_period <- plot_grid(plot_resids, plot_spectra_period, 
                                        labels = c("A", "B"), nrow = 2, align = "v")

save_plot("figs/plot_resids_spectra_period.png", plot_resids_spectra_period,
          ncol = 1, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3, 
          base_width = 7
)


plot_spectra_min_period <- spectra_df %>% 
  filter(dataset == "Seawater" & metric == "minimum") %>% 
  ggplot(aes(period, spec, color = NULL))  + 
  geom_vline(aes(xintercept = 18.6), linetype = "solid", color = "gray", alpha = 0.5, 
             size = 2) + 
  geom_line() + geom_point() + 
  scale_x_log10("Period (years)", breaks = rev(yrs.period), 
                labels = rev(yrs.labels)) +  ylab("Spectrum") + 
  #facet_grid(dataset ~ metric, switch = "y") + 
  theme(strip.background = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none")
plot_spectra_min_period


plot_resids_spectra_min_period <- plot_grid(plot_resids_min, plot_spectra_min_period, 
                                            labels = c("A", "B"), 
                                     nrow = 1, align = "h")

save_plot("figs/plot_resids_spectra_min_period.png", plot_resids_spectra_min_period,
          ncol = 2, nrow = 1,
          base_aspect_ratio = 1.2, base_height = 3.5
)

##### TO DO: ADD SECOND X-AXIS TO SHOW PERIOD ON SPECTRAL PLOTS #####

## Base R solution

yrs.period <- rev(c(2, 4, 8, 16, 32, 64))
yrs.labels <- rev(c("2", "4", "8", "16", "32", "64"))

df_per

png('figs/plot_spectra_min_period_base.png', height = 3.5, width = 3.5, res = 300, units = "in")

with(df_per, plot(freq, spec, type = "l", col = "black", las = 1, lwd = 0,
                  ylab = "Spectrum", 
                  xlab = "Frequency (1/years)"))


axis(side = 3, at = 1/yrs.period, labels = yrs.labels)
mtext(side = 3, line = 3, "Period (years)")
abline(v = 1/18.6, col = "darkgray", lwd = 4)

with(df_per, lines(freq, spec, lwd = 2, pch = 16))
with(df_per, points(freq, spec, lwd = 2, pch = 16))

dev.off()

## 2panel plot

par(mfrow = c(1,2))
with(df_per, plot(freq, spec, type = "l", col = "black", las = 1, lwd = 0,
                  ylab = "Spectrum", 
                  xlab = "Frequency (1/years)"))


axis(side = 3, at = 1/yrs.period, labels = yrs.labels)
mtext(side = 3, line = 3, "Period (years)")
abline(v = 1/18.6, col = "darkgray", lwd = 4)

with(df_per, lines(freq, spec, lwd = 2, pch = 16))
with(df_per, points(freq, spec, lwd = 2, pch = 16))

plot.new()              ## suggested by @Josh
vps <- baseViewports()
vps
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot

vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values 
vp1 <-plotViewport(c(1,1,0,1)) ## create new vp with margins, you play with this values 



print(plot_resids_min, vp = vp1) 


#######################
# https://gist.github.com/Niknafs/ff92e0356a3a6d6ed58b
# say I would like to have my regular R graphic in top-right quadrant
library(grid)
library(gridBase)

par(mfrow = c(1,2), mar=c(0,0,0,0), oma=c(0,0,0,0))

# leave top-left quadrant empty!
plot.new()

# plot regular R graphic in top-right quadrant
plot(seq(1:10), seq(1:10), pch = 20)

# now add the first ggplot element which is going to take
# up the left two quadrants
vp <- viewport(height = unit(1,"npc"), width=unit(0.5, "npc"), 
              just = c("left","top"), y = 1, x = 0.5)

print(my.first.ggplot.obj, vp = vp)
                        # add the second ggplot element in the bottom right quadrant
                        vp <- viewport(height = unit(1,"npc"), width=unit(0.5, "npc"), 
                        just = c("left","top),
               y = 0.5, x = 0.5)
print(my.second.ggplot.obj, vp = vp)


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