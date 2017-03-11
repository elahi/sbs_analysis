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

source("07_plot_timeseries_resids.R")

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec)) + 
  scale_x_log10("Period (years)", breaks = rev(yrs.period), labels = rev(yrs.labels)) 

yrs.period <- c(2, 4, 8, 16, 32, 64)

df_per
freq = df_per$freq

ggplot(df_per) + 
  geom_line(aes(x = freq, y = spec)) + 
  scale_x_continuous("Frequency (1/year)", 
                     sec.axis = sec_axis(trans = ~ ., 
                                         name = "Period (years)"))


##### SIX-PANEL PLOT OF SPECTRA #####

plot_spectra <- spectra_df %>% 
  ggplot(aes(freq, spec, color = metric))  + 
  geom_vline(aes(xintercept = 1/18.6), linetype = "solid", color = "gray", alpha = 0.5, 
             size = 2) + 
  geom_line() + geom_point() + 
  xlab("Frequency (1/yr)") + ylab("Spectrum") + 
  facet_grid(dataset ~ metric, switch = "y") + 
  theme(strip.background = element_blank(), 
        strip.placement = "outside") + 
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
yrs.period <- rev(c(2, 4, 8, 16, 32, 64))

plot_spectra_period <- spectra_df %>% 
  ggplot(aes(period, spec, color = metric))  + 
  geom_vline(aes(xintercept = 18.6), linetype = "solid", color = "gray", alpha = 0.5, 
             size = 2) + 
  geom_line() + geom_point() + 
  scale_x_log10("Period (years)", breaks = yrs.period, 
                labels = yrs.labels) +
  ylab("Spectrum") + 
  facet_grid(dataset ~ metric, switch = "y") + 
  theme(strip.background = element_blank(), 
        strip.placement = "outside") + 
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

##### 2 PANEL FIGURE WITH GGPLOT2 AND BASE R PLOTS #####

## Use ggplot for resids plot
plot_resids_min2 <- temp_records3 %>% 
  filter(dataset == "Seawater" & metric == "minimum") %>% 
  ggplot(aes(year, resid_val)) + 
  geom_point(aes(value, y_position, color = NULL), alpha = 0.3, size = 5, data = moon_df) + 
  geom_line(alpha = 0.5, size = 0.5) + 
  geom_point(alpha = 0.5, size = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray") +
  ylab(expression(paste("Temperature deviation (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") + 
  geom_point(aes(x = year, y = -1.75, shape = species, color = NULL), data = spYrs, 
             alpha = 0.6, size = 2) + 
  guides(shape = FALSE) + 
  geom_line(aes(year, temp_ma, color = NULL), size = 1) + 
  theme(strip.background = element_blank()) + 
  theme(panel.border = element_rect(color = "black")) + 
  annotate("text", 2014, 1.6, label = "A")

plot_resids_min2

library(gridBase)
library(grid)

# png("figs/plot_resids_spectra_min_special.png", height = 7, width = 3.5, units = "in", res = 300)
# par(mfrow = c(2,1))

png("figs/plot_resids_spectra_min_special.png", height = 3.5, width = 7, units = "in", res = 300)
par(mfrow = c(1,2), oma = c(0,0,0,0), mar = c(3,2,3,2) + 0.1)

plot.new()              ## suggested by @Josh
vps <- baseViewports()
vps
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(0.5,0.5,2,1)) ## create new vp with margins, you play with this values 
print(plot_resids_min2, vp = vp1)

with(df_per, plot(freq, spec, type = "l", col = "black", 
                  las = 1, lwd = 0, xaxt = "n", yaxt = "n", 
                  ylab = " ", xlab = " "))
grid(lty = 1, col = "gray90")
abline(v = 1/yrs.period, col = "gray90")
axis(side = 1, mgp = c(3, 0.5, 0), cex.axis = .9)
axis(side = 2, mgp = c(3, 0.5, 0), las = 1, cex.axis = 0.9)
axis(side = 3, at = 1/yrs.period, labels = yrs.labels, mgp = c(3, 0.5, 0), cex.axis = 0.9)
text(0.5 * 0.95, 4 * 0.95, "B")

mtext(side = 1, line = 1.5, "Frequency (1/years)")
mtext(side = 2, line = 1.5, "Spectrum")
mtext(side = 3, line = 1.5, "Period (years)")

abline(v = 1/18.6, col = "darkgray", lwd = 4)

with(df_per, lines(freq, spec, lwd = 2, pch = 16))
with(df_per, points(freq, spec, lwd = 1, pch = 16))

dev.off()
