################################################################################
##' @title Process tidal data for Monterey using rtide
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-11-30
##' 
##' @log 
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(rtide)
library(scales) # for the date_format function

dat <- tide_height('Monterey Harbor',from = as.Date('1930-01-01'), 
                  to = as.Date('2020-12-31'), minutes = 60, tz ='PST8PDT')
dat <- dat %>% select(-Station)

save(dat, file = "output/monterey_tides_1930_2020.RData")

##' Subsetted to cover decade prior to sampling
##' LIKE: 1938-1947
##' LODI: 1941-1950
##' CHFU: 1954-1963

# dat1 <- tide_height('Monterey Harbor',from = as.Date('1938-01-01'), 
#                    to = as.Date('1950-12-31'), minutes = 15, tz ='PST8PDT')
# 
# dat2 <- tide_height('Monterey Harbor',from = as.Date('1954-01-01'), 
#                     to = as.Date('1963-12-31'), minutes = 15, tz ='PST8PDT')
# 
# dat3 <- tide_height('Monterey Harbor',from = as.Date('2005-01-01'), 
#                     to = as.Date('2015-12-31'), minutes = 15, tz ='PST8PDT')

dat1 <- tide_height('Monterey Harbor',from = as.Date('2015-01-01'), 
                   to = as.Date('2015-12-31'), minutes = 10, tz ='PST8PDT')
dat2 <- tide_height('Monterey Harbor',from = as.Date('1947-01-01'), 
                    to = as.Date('1947-12-31'), minutes = 10, tz ='PST8PDT')
dat3 <- tide_height('Monterey Harbor',from = as.Date('1950-01-01'), 
                    to = as.Date('1950-12-31'), minutes = 10, tz ='PST8PDT')
dat4 <- tide_height('Monterey Harbor',from = as.Date('1963-01-01'), 
                    to = as.Date('1963-12-31'), minutes = 10, tz ='PST8PDT')

dat_fine <- rbind(dat1, dat2, dat3, dat4)
head(dat_fine)
tail(dat_fine)

save(dat_fine, file = "output/monterey_tides_fine_scale.RData")

