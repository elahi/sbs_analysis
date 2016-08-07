#################################################
# Author: Robin Elahi
# Date: 151106

# HMS SST data
# From HMS Marine Life Observatory
#################################################

library(ggplot2)
theme_set(theme_classic(base_size = 8))
library(dplyr)
library(tidyr)
library(nlme)

# Set working directory to main project folder
setwd("~/github/sbs_analysis")

# Get uncorrected temperature data from MLO 
# hms_raw <- read.table("http://mlo.stanford.edu/HMS-SST.txt", 
#                       header = TRUE, na.strings = "NaN")
hms_raw <- read.table("./data/HMS-SST.txt", 
                       skip = 0, header = TRUE, na.strings = "NaN", 
                      stringsAsFactors = FALSE)

# Get corrected temperature data (up to 2004)
hms_corr <- read.table("./data/HMStemp.corrected.txt", 
                      skip = 13, header = TRUE, na.strings = "NaN")
head(hms_raw)
head(hms_corr)

str(hms_raw)
hms_raw$dateR <- as.Date(hms_raw$DATE, "%m/%d/%Y")

hms_corr$dateR <- as.Date(with(hms_corr, ISOdate(year, month, day)))
str(hms_corr)

dat1 <- hms_raw %>% select(dateR, SST) 
dat2 <- hms_corr %>% select(dateR, new) %>% rename(SST = new)

# Get uncorrected data for 2005 on
str(dat1$dateR)
dat3 <- dat1 %>% filter(dateR > "2004-12-31")
head(dat3)

# now combine corrected and uncorrected data
master <- rbind(dat2, dat3)
head(master)

# get month and year to calculate averages
master$month <- strftime(master$dateR, "%m")
master$year <- strftime(master$dateR, "%Y")
master$mo_yr <- with(master, paste(month, year, sep = "_"))
master$SST <- as.numeric(master$SST)
str(master)

# this value is suspicious
min(master$SST, na.rm = TRUE)
master %>% filter(SST < 8)
master2 <- master %>% filter(SST < 7.3)
master2

# Quick plot
head(master)
ggplot(data = master, aes(dateR, SST)) + 
  geom_line()

# Quick plot (1944 - 2014)
master %>% filter(year > 1943 & year < 2015) %>%
  ggplot(aes(dateR, SST)) + 
  geom_point() + geom_line() + 
  geom_smooth(method = "lm") + 
  ggtitle("Annual Sea Surface Temperature\n1944 - 2014") + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.position = "none")
  
ggsave("./figs/hms_sst_daily.pdf", width = 3.5, height = 3.5)

###########################
#Make monthly summaries 

hms_monthly <- master %>%
  group_by(mo_yr, month, year) %>%
  summarise(mean = mean(SST, na.rm=T),
            maximum = max(SST, na.rm=T),
            minimum = min(SST, na.rm=T)) %>% 
  ungroup()

str(hms_monthly)
hms_monthly$dateR <- with(hms_monthly, 
                          as.Date(paste(year, month, 15, sep = "-")))

write.csv(hms_monthly, "./output/hms_temp_monthly.csv")

subDat <- hms_monthly %>% filter(year > 1943 & year < 2015)

# analyze monthly records
mod1 <- gls(mean ~ dateR, 
            data = hms_monthly, 
            correlation = corAR1())
summary(mod1)

mod2 <- gls(maximum ~ dateR, 
            data = hms_monthly, 
            correlation = corAR1())
summary(mod2)

mod3 <- gls(minimum ~ dateR, 
            data = hms_monthly, 
            correlation = corAR1())
summary(mod3)

# convert wide to long
head(hms_monthly)
hms_long <- gather(hms_monthly, key = type, value = tempC,
                   mean:minimum)
head(hms_long)
unique(hms_long$type)

subDat <- hms_long %>% filter(year > 1943 & year < 2015)

# Quick plots
ggplot(data = subDat, aes(dateR, tempC, color = type)) + 
  geom_line() + facet_wrap(~ type, nrow = 1) + 
  theme(legend.position = "none") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Monthly Sea Surface Temperature\n1944 - 2014") + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.title = element_blank())

ggsave("./figs/hms_sst_monthly.pdf", width = 7, height = 3.5)


###########################
#Make yearly summaries
hms_yearly <- master %>% 
  group_by(year) %>%
  summarise(mean = mean(SST, na.rm=T),
            maximum = max(SST, na.rm=T),
            minimum = min(SST, na.rm=T)) %>% 
  ungroup()

head(hms_yearly)

write.csv(hms_yearly, "./output/hms_temp_yearly.csv")

# analyze yearly records
mod1 <- gls(mean ~ as.numeric(year), 
           data = hms_yearly, 
           correlation = corAR1())
summary(mod1)

mod2 <- gls(maximum ~ as.numeric(year), 
            data = hms_yearly, 
            correlation = corAR1())
summary(mod2)

mod3 <- gls(minimum ~ as.numeric(year), 
            data = hms_yearly, 
            correlation = corAR1())
summary(mod3)


# convert wide to long
head(hms_yearly)
yearly_long <- gather(hms_yearly, key = type, value = tempC,
                   mean:minimum)
head(yearly_long)
yearly_long$year <- as.numeric(yearly_long$year)


# Quick plots (1944 - 2014)
subDat <- yearly_long %>% filter(year > 1943 & year < 2015)

ggplot(data = subDat, 
       aes(year, tempC, color = type, shape = type)) + 
  geom_point(size = 1.5) + geom_line() + 
  geom_smooth(data = subset(subDat, 
                            type == "mean" | type == "maximum"), 
              aes(year, tempC), 
              method = "lm", color = 'black') + 
  ggtitle("Annual Sea Surface Temperature\n1944 - 2014") + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.title = element_blank())

  
  #theme(legend.position = "none")
#   annotate("text", x = 1950, y = 19, 
#            label = "Maximum", color = "green")

ggsave("./figs/hms_sst_yearly.pdf", width = 3.5, height = 3.5)


