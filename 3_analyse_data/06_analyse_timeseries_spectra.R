################################################################################
##' @title Analyze spectra of temperature time series in context of 18y lunar cycles
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-03-10
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

source("06_analyse_timeseries_resids.R")

library(astsa)

##### SPECTRAL ANALYSIS - PRACTICE #####

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
yrs.period <- rev(c(2, 4, 8, 16, 32, 64))
yrs.labels <- rev(c("2", "4", "8", "16", "32", "64"))
yrs.freqs <- 1/yrs.period * 1/12  #Convert annual period to annual freq, and then to monthly freq
df_per$period <- 1/df_per$freq

ggplot(df_per) + 
  geom_line(aes(x = freq, y = spec))

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec))

ggplot(df_per) + 
  geom_line(aes(x = period, y = spec)) + 
  scale_x_log10("Period (years)", breaks = yrs.period, labels = yrs.labels) 

##### RUN SPECTRA FOR ALL TEMPERATURE METRICS #####

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
