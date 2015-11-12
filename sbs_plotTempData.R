#################################################
# Author: Robin Elahi
# Date: 151102

# Temporal shifts in gastropod size-frequency distributions are
# consistent with climate warming

# Plotting summarized temperature data
#################################################

# Set working directory to main project folder
setwd("~/github/sbs_analysis")
getwd()

# Load packages
library(ggplot2)
theme_set(theme_classic(base_size = 8))
library(plyr)
library(dplyr)

source("./R/multiplotF.R")

###########################
# Load daily iButton data from output folder
dat <- read.csv("./output/temp_daily_iButton.csv", header = TRUE)
head(dat)
dat$day <- as.Date(dat$day)

# Mean temperatures
ggplot(dat, aes(day, mean_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Max temperatures
ggplot(dat, aes(day, max_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Min temperatures
ggplot(dat, aes(day, min_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

###########################
# Now plot summarized data for each logger position 
# (summarizes across two iButtons per position)
# Except for: WaraD_mid2; WaraD_hi; WaraD_low
# (these only had one logger per position)

dat <- read.csv("./output/temp_daily_position.csv", header = TRUE)
head(dat)
dat$day <- as.Date(dat$day)

# Mean temperatures
ggplot(dat, aes(day, mean_tempC, color = position)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Max temperatures
ggplot(dat, aes(day, max_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Min temperatures
ggplot(dat, aes(day, min_tempC, color = iButtonID)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ position) + theme(legend.position = "none")

# Mean temperatures
names(dat)
ggplot(dat, aes(day, mean_tempC, color = position)) + 
  geom_line(size = 1, alpha = 0.8) + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ code) + theme(legend.position = "none")

###########################
# Now plot summarized data for each species

dat <- read.csv("./output/temp_daily_species.csv", header = TRUE)
head(dat)
dat$day <- as.Date(dat$day)

# Mean temperatures
ggplot(dat, aes(day, mean_tempC)) + 
  geom_line() + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ code) + theme(legend.position = "none")

# Max temperatures
ggplot(dat, aes(day, max_tempC)) + 
  geom_line() + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ code) + theme(legend.position = "none")

# Min temperatures
ggplot(dat, aes(day, min_tempC)) + 
  geom_line() + 
  ylab("Temperature (C)") + xlab("Date") + 
  facet_wrap(~ code) + theme(legend.position = "none")

# Relevel the species codes
dat$code <- factor(dat$code, levels = rev(c('LIKE', 'LODI', 'CHFU')), 
                   ordered = TRUE)

# Figure out how many daily records per species, and mean tidal height
dat %>% group_by(code) %>% summarise(totalN = n(), 
                                     meanHT = mean(tidalHT, na.rm = TRUE), 
                                     sdHT = sd(tidalHT, na.rm = TRUE))

### Violin plot
ggplot(dat, aes(code, mean_tempC)) + 
  geom_violin(fill = "gray") + 
  geom_boxplot(width = 0.3, notch = TRUE, color = "black") + 
  ylab("Temperature (C)") + xlab("Species") + 
  theme(legend.position = "none") + 
  scale_x_discrete("", labels = c("CHFU" = "Chlorostoma funebralis\nn=344", 
                                  "LODI" = "Lottia digitalis\nn=258", 
                                  "LIKE" = "Littorina keenae\nn=344")) +
  coord_flip()
ggsave("./figs/temp_violin_spp.pdf", width = 3.5, height = 3.5)


### 3 panel plot
# Relevel the species codes
dat$code <- factor(dat$code, levels = c('LIKE', 'LODI', 'CHFU'), 
                   ordered = TRUE)
ULClabel <- theme(plot.title = element_text(hjust = -0.05, vjust = 1, 
                                            size = rel(1.2)))
p1 <- ggplot(dat, aes(code, mean_tempC)) + 
  geom_violin(fill = "gray") + 
  geom_boxplot(width = 0.3, notch = TRUE, color = "black") + 
  ylab(expression(paste("Daily mean temperature (", degree, "C)"))) + 
  xlab("Species") + 
  theme(legend.position = "none") + 
  scale_x_discrete("", 
        labels = c("CHFU" = expression(italic(Chlorostoma)), 
                   "LODI" = expression(italic(Lottia)), 
                   "LIKE" = expression(italic(Littorina)))) + 
  ULClabel + labs(title = "A")

p2 <- ggplot(dat, aes(code, max_tempC)) + 
  geom_violin(fill = "gray") + 
  geom_boxplot(width = 0.3, notch = TRUE, color = "black") + 
  ylab(expression(paste("Daily maximum temperature (", degree, "C)"))) + 
  xlab("Species") + 
  theme(legend.position = "none") + 
  scale_x_discrete("", 
                   labels = c("CHFU" = expression(italic(Chlorostoma)), 
                              "LODI" = expression(italic(Lottia)), 
                              "LIKE" = expression(italic(Littorina)))) + 
  ULClabel + labs(title = "B")

p3 <- ggplot(dat, aes(code, min_tempC)) + 
  geom_violin(fill = "gray") + 
  geom_boxplot(width = 0.3, notch = TRUE, color = "black") + 
  ylab(expression(paste("Daily minimum temperature (", degree, "C)"))) + 
  xlab("Species") + 
  theme(legend.position = "none") + 
  scale_x_discrete("", 
                   labels = c("CHFU" = expression(italic(Chlorostoma)), 
                              "LODI" = expression(italic(Lottia)), 
                              "LIKE" = expression(italic(Littorina)))) + 
  ULClabel + labs(title = "C")

# save as pdf
pdf("./figs/temp_spp_3panel.pdf", width = 3.5, height = 3.5)
multiplot(p1, p2, p3, cols = 1)
dev.off()


###########################
# Plot Littorina data separately for each tidal height
dat <- read.csv("./output/temp_daily_species.csv", header = TRUE)
str(dat$code)
lit <- dat %>% filter(code == "LIKE") %>% droplevels()
levels(lit$nest1)

names(lit)
unique(lit$nest2)

ggplot(lit, aes(nest2, mean_tempC)) + 
  geom_violin(fill = "gray") + 
  geom_boxplot(width = 0.3, notch = TRUE, color = "black") + 
  ylab(expression(paste("Daily mean temperature (", degree, "C)"))) + 
  facet_wrap(~ nest1, nrow = 4)

names(lit)
ggplot(lit, aes(mean_tempC, fill = nest1)) + 
  geom_density(alpha = 0.5) + 
  xlab(expression(paste("Daily mean temperature (", degree, "C)"))) + 
  facet_wrap(~ nest2, nrow = 4)

ggplot(lit, aes(max_tempC, fill = nest1)) + 
  geom_density(alpha = 0.5) + 
  xlab(expression(paste("Daily mean temperature (", degree, "C)"))) + 
  facet_wrap(~ nest2, nrow = 4)

ggplot(lit, aes(min_tempC, fill = nest1)) + 
  geom_density(alpha = 0.5) + 
  xlab(expression(paste("Daily mean temperature (", degree, "C)"))) + 
  facet_wrap(~ nest2, nrow = 4)


###
# Ignore crack/face
ggplot(lit, aes(mean_tempC, fill = nest1)) + 
  geom_density(alpha = 0.5) + 
  xlab(expression(paste("Daily mean temperature (", degree, "C)")))

ggplot(lit, aes(max_tempC, fill = nest1)) + 
  geom_density(alpha = 0.5) + 
  xlab(expression(paste("Daily max temperature (", degree, "C)")))

ggplot(lit, aes(min_tempC, fill = nest1)) + 
  geom_density(alpha = 0.5) + 
  xlab(expression(paste("Daily min temperature (", degree, "C)")))

###
# Facets as zone
# rename and relevel the zones
# what is the average tidal height?
lit %>% group_by(nest1) %>% summarise(totalN = n(), 
                                      meanHT = mean(tidalHT, na.rm = TRUE), 
                                      sdHT = sd(tidalHT, na.rm = TRUE))
# create new factor column
factorList <- unique(lit$nest1)
factorList
factorList2 <- rev(c("7.2m", "6.0m", "3.9m", "2.8m"))
factorList2
lit$zone <- mapvalues(lit$nest1, from = factorList, to = factorList2)
levels(lit$zone)

lit$zone <- factor(lit$zone, levels = c('7.2m', '6.0m', '3.9m', '2.8m'), 
                    ordered = TRUE)
lit$nest1 <- factor(lit$nest1, levels = c('D', 'C', 'B', 'A'), 
                   ordered = TRUE)

### 3 panel plot
ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 1, 
                                            size = rel(1.2)))

p1 <- ggplot(lit, aes(mean_tempC, fill = nest2)) + 
  geom_density(alpha = 0.5) + 
  xlab(expression(paste("Daily mean temperature (", degree, "C)"))) +
  ylab("Probability density") + 
  scale_fill_manual(breaks = c("Crack", "Face"), 
                    values = c("blue", "red")) + 
  theme(legend.position = "none") +
  facet_wrap(~ zone, nrow = 4) + 
  ULClabel + labs(title = "A") + 
  scale_x_continuous(limits = c(10, 45))
  
p1


p2 <- ggplot(lit, aes(max_tempC, fill = nest2)) + 
  geom_density(alpha = 0.5) + 
  xlab(expression(paste("Daily maximum temperature (", degree, "C)"))) +
  ylab("Probability density") + 
  scale_fill_manual(breaks = c("Crack", "Face"), 
                    values = c("blue", "red")) + 
  theme(legend.position = "none") +
  facet_wrap(~ zone, nrow = 4) + 
  ULClabel + labs(title = "B") + 
  scale_x_continuous(limits = c(10, 45))
p2

p3 <- ggplot(lit, aes(min_tempC, fill = nest2)) + 
  geom_density(alpha = 0.5) + 
  xlab(expression(paste("Daily minimum temperature (", degree, "C)"))) +
  ylab("Probability density") + 
  scale_fill_manual(breaks = c("Crack", "Face"), 
                    values = c("blue", "red")) + 
  theme(legend.position = "none") +
  facet_wrap(~ zone, nrow = 4) + 
  ULClabel + labs(title = "C") + 
  scale_x_continuous(limits = c(10, 45))
p3

# save as pdf
pdf("./figs/childs_temp.pdf", width = 7, height = 7)
multiplot(p1, p2, p3, cols = 3)
dev.off()


