################################################################################
##' @title Testing metafor, metagear package in R
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-05-23
##' 
##' @log Add a log here
################################################################################

##### LOAD PACKAGES #####

library(readr)
library(metafor)

# http://lajeunesse.myweb.usf.edu/metagear/metagear_basic_vignette.html

# first load Bioconductor resources needed to install the EBImage package 
# and accept/download all of its dependencies
source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")
# Now can load
library(metagear)

##### INITIAL WOS SCREENING #####

# searched for:
# ((gastropod* OR snail) AND (Body AND size AND change) AND (historical OR resurv* OR resampl* OR "time series" OR "time-series" OR "temporal change" OR decad* OR multiyear OR "multi-year" OR "multi-decade" OR revisit*)) 

# 26 results

# Add all to marked list

# Load sample search results
datDel <- read.delim("sbs_meta/lit_search_results/savedrecs_delim.txt", 
                  header = TRUE)

datIsi <- read.delim("sbs_meta/lit_search_results/savedrecs_isi.txt", 
                  header = TRUE)



dat <- read.table("sbs_meta/lit_search_results/recs_160607.txt")
head(dat)





##### METAGEAR WORKFLOW #####
data(example_references_metagear)
names(example_references_metagear)





##### JOACHIM'S EXAMPLES #####

dat <- get(data("dat.curtis1998"))
head(dat, 5)

unique(dat$method)

?escalc

dat <- escalc(m1i=m1i, sd1i=sd1i, n1i=n1i, m2i=m2i, sd2i=sd2i, n2i=n2i, measure="ROM", data=dat, append=T)

head(dat)

res1 <- rma(yi, vi, method = "DL", data = dat)
funnel(res1)
forest(res1)

res2 <- rma(yi, vi, data = dat, mods = ~ as.factor(method))
res3 <- rma(yi, vi, data = dat, mods = ~ as.factor(method) - 1)

##### TESTOSTERONE #####

testosterone <- data.frame(r = c(0.02,0.24,0.26,0.06) , 
                           n = c(1678,57,94,113))
ttone <- escalc(ri=r, ni=n, data=testosterone, measure="ZCOR", append=T)
head(ttone)

res_ttone <- rma(yi, vi, data = ttone, method = "REML”)
