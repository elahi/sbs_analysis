#First download and decompress the HadiSST netcdf file found here
#http://www.metoffice.gov.uk/hadobs/hadisst/data/download.html

#The procedure here to crack the file mostly follows this post 
#http://stackoverflow.com/questions/24115110/importing-sea-surface-temperature-text-files-in-ascii-format-into-r
#It seems to work ok for a few locations that I know the patterns for, but it gets stuck for your station in Monterrey

dat <- read.csv("sbs_meta/hadisst/sbs_hadisst_gps.csv")



load_hadsst <- function(file = "../bigFiles/HadISST_sst.nc") {
  b <- raster::brick(file)
  raster::NAvalue(b) <- -1000
  return(b)
}

sst2 <- load_hadsst()
head(sst2)

#inputfile. change the location here
inputfile <- '../bigFiles/HadISST_sst.nc'
  
library(raster)
library(xts)
library(caTools)    

#Getting a look
capture <- raster(inputfile) #Reading only the first layer
image(capture)
rm(capture)
#It all makes sense, there are no funky rotated axis or anything like that

#Open the file:
sst <- brick(inputfile)
sst
names(sst)
Date <- substr(names(sst),2,11) 
Date
Date <- gsub('\\.', '\\-', Date)
Date <- as.Date(Date)

#Extract the time serie for a specific point (lon, lat)
#Longitude must be within -180,180 and latitude within -90, 90 

## Elahi2015 - Monterey
lon <- -122.00001
lat <- 36.63

## Roy2003



## Fisher2009

tserie <- as.vector(extract(sst, cbind(lon, lat)))
tserie <- xts(tserie, order.by=Date)
head(tserie)

#Plotting the timeseries
par(mar = rep(2, 4))
plot(tserie, t='n', main='HadISST')
lines(tserie, col='grey')
lines(xts(runmean(tserie, 12), order.by=Date), col='red', lwd=2)

#Saving the timeseries in working folder
filename <- paste(, lat, lon, ".csv", sep = "_")

write.csv(as.data.frame(tserie), filename)

