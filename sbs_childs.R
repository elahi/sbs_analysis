#################################################
# Author: Robin Elahi
# Date: 150717

# Analysis of historical and modern gastropod body sizes
# Hopkins Marine Station

# Littorina keenae (Childs 1947)
#################################################


library(ggplot2)
theme_set(theme_classic(base_size = 8))
library(dplyr)
library(lme4)
library(lmerTest)

rm(list=ls(all=TRUE)) 

# plotting functions
source("./R/multiplotF.R")
source("./R/summarizeData_150204.R")

# load prepped data
dat <- read.csv("./output/sbsMaster.csv", na.strings = c("NA"))

# change ft to meters
range(dat$tideHTm, na.rm = TRUE)
dat$tideHTm <- dat$tideHTm/3.28084

chil <- droplevels(filter(dat, sp == "LIKE"))
summary(chil)
unique(chil$nest2)
############################################
# get density stats
dens <- read.csv("./data/density_master.csv", na.strings = "NA")
dens
dens_stats <- summarySE(data = dens, measurevar = "dens_m2", 
                        groupvars = c("nest1", "era"))
dens_stats

summarySE(data = dens, measurevar = "dens_m2", groupvars = "era")

############################################
# create new factor with tidal heights
factorList <- unique(chil$nest1)
factorList
unique(chil$tideHTm)

factorList2 <- c("2.7m", "3.8m", "5.9m", "7.3m")
factorList2
tideHT <- plyr::mapvalues(chil$nest1, from = factorList, to = factorList2)

# reorder the levels
tideHT2 <- factor(tideHT, levels = c("7.3m", "5.9m", "3.8m", "2.7m"))
chil$tideHT <- tideHT2

############################################
# subset the data for separate panels
summary(chil)

past_A <- chil %>% filter(era == "past" & nest1 == "zoneA")
past_B <- chil %>% filter(era == "past" & nest1 == "zoneB")
past_C <- chil %>% filter(era == "past" & nest1 == "zoneC")
past_D <- chil %>% filter(era == "past" & nest1 == "zoneD")

present_A <- chil %>% filter(era == "present" & nest1 == "zoneA")
present_B <- chil %>% filter(era == "present" & nest1 == "zoneB")
present_C <- chil %>% filter(era == "present" & nest1 == "zoneC")
present_D <- chil %>% filter(era == "present" & nest1 == "zoneD")

############################################
### quick plots
# histogram
ggplot(chil,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  facet_grid(tideHT ~ era, scales = "free_y") + 
  xlab("Size (mm)") + ylab("Frequency (%)") 

# density
ggplot(chil,  aes(x = size1mm)) +
  geom_density(aes(color = era)) +
  facet_wrap(~ tideHT, scales = "fixed", nrow = 4) + 
  xlab("Size (mm)") + ylab("Probability density") +
  theme(legend.justification = c(1, 1), legend.position = c(0.97, 0.23),
        legend.text = element_text(size = 6), legend.title = element_text(size = 6)) +
  ggtitle("Littorina keenae")

ggsave("./figs/sbs_fig2.pdf", width = 3.5, height = 7)

############################################

ULClabel <- theme(plot.title = element_text(hjust = -0.08, vjust = 1, 
                                            size = rel(1)))

no_legend <- theme(legend.position = "none")
dens_stats

plot_pastA <- ggplot(past_A,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.33)) + 
  labs(title = "A") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "2.7m\n1947\nn = 196\nNo. per m2 = 84.4", 
           x = 18, y = 0.16, size = 2.2)

plot_pastB <- ggplot(past_B,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.33)) + 
  labs(title = "B") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "3.8m\n1947\nn = 239\nNo. per m2 = 102.9", 
           x = 18, y = 0.16, size = 2.2)

plot_pastC <- ggplot(past_C,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.33)) + 
  labs(title = "C") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "5.9m\n1947\nn = 215\nNo. per m2 = 92.6", 
           x = 18, y = 0.16, size = 2.2)

plot_pastD <- ggplot(past_D,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.33)) + 
  labs(title = "D") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "7.3m\n1947\nn = 32\nNo. per m2 = 13.8", 
           x = 18, y = 0.16, size = 2.2)

# present
plot_presentA <- ggplot(present_A,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.33)) + 
  labs(title = "A") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "2.7m\n1947\nn = 375\nNo. per m2 = 20 +- 22.1", 
           x = 18, y = 0.16, size = 2.2)

plot_presentB <- ggplot(present_B,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.33)) + 
  labs(title = "A") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "3.8m\n1947\nn = 114\nNo. per m2 = 20 +- 21.2", 
           x = 18, y = 0.16, size = 2.2)

plot_presentC <- ggplot(present_C,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.33)) + 
  labs(title = "A") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "5.9m\n1947\nn = 194\nNo. per m2 = 54 +- 75.8", 
           x = 18, y = 0.16, size = 2.2)

plot_presentD <- ggplot(present_D,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.33)) + 
  labs(title = "A") + ULClabel + xlab("Size (mm)") + ylab("Frequency (%)") +
  annotate("text", label = "7.3m\n1947\nn = 50\nNo. per m2 = 21.5", 
           x = 18, y = 0.16, size = 2.2)

multiplot(plot_pastD, plot_pastC, plot_pastB, plot_pastA, 
          plot_presentD, plot_presentC, plot_presentB, plot_presentA, 
          cols = 2)


###############################
# save as 7 x 3.5 pdf
pdf("./figs/sbs_fig2.pdf", 7, 7)
multiplot(plot_pastD, plot_pastC, plot_pastB, plot_pastA, 
          plot_presentD, plot_presentC, plot_presentB, plot_presentA, 
          cols = 2)
dev.off()	







############################################

# create random effect
names(chil)
head(chil)
randF <- function(x) return(as.factor(paste(x$era, x$nest1, x$nest2, sep="_")))

chil$rand1 <- randF(chil)
unique(chil$rand1)

### quick model
mod1 <- lmer(size1mm ~ era*nest1 + (era|rand1), 
             data = chil)
;mod1
anova(mod1)

mod1 <- lm(size1mm ~ era*nest1, 
             data = chil)
anova(mod1)



