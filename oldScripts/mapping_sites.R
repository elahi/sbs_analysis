################################################################################
##' @title Mapping samples and temperature loggers
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-09
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

library(ggmap)
library(leaflet)

# load size data
source("03_identify_size_cutoff.R")
head(dat4)


##### GET LOGGER LOCATIONS #####
# Merge the spreadsheetIDs to bagNames 

# Load file that links bagNames to iButtonIDs
loggers <- read.table("./data/iButton_retrieval.txt", header = TRUE)
unique(loggers$iButtonID)

# Load file that links bagNames to the details of the logger positions
positions <- read.csv("./data/iButton_deployment.csv", header = TRUE)

head(loggers)
head(positions)

envDat <- inner_join(loggers, positions, by = "bagName") %>% 
  arrange(code, nest1, tidalHT)
head(envDat)

##### GET LOCATIONS OF SIZE SAMPLES #####

sizeLL <- dat4 %>% filter(era == "present") %>%
  select(species, site, nest1, nest2, sampleUnit, 
         sampleArea, tideHTm, lat, long) %>% distinct() %>%
  rename(lon = long) %>% 
  arrange(species, site, tideHTm)

llSummary <- sizeLL %>% group_by(species, site) %>%
  summarise(meanLat = mean(lat, na.rm = TRUE), 
            meanLon = mean(lon, na.rm = TRUE)) 
llSummary

##### LEAFLET MAPS #####

m <- leaflet() %>% setView(lng = -121.905, lat = 36.6, zoom = 10)

m %>% addTiles() %>%
  addCircles(data = sizeLL, lng = ~lon, lat = ~lat,
             popup = ~as.character(sampleUnit), 
             radius = 1) %>%
  addCircles(data = envDat, lng = ~lon, lat = ~lat,
             popup = ~as.character(position), 
             radius = 1, color = 'red') 

##### GGMAP - HMS OVERVIEW #####
hms <- get_map(location = c(lon = -121.90440, lat = 36.6213),
                color = "color", source = "google",
                maptype = "satellite", zoom = 18)

hmsMap <- ggmap(hms, extent = "device",
                ylab = "Latitude", xlab = "Longitude")

hmsMap + 
  geom_point(aes(lon, lat), 
             data = envDat, color = "black", size = 1, alpha = 0.5) + 
  geom_point(aes(lon, lat), 
             data = envDat, color = "yellow", size = 0.5, alpha = 0.5) + 
  geom_point(aes(lon, lat, color = species, 
                 shape = species), data = sizeLL, alpha = 0.5, 
             inherit.aes = FALSE) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") 
  
ggsave("figs/map_hms.png", height = 7, width = 7)

##### GGMAP 2 - WARA B #####
hmsB <- get_map(location = c(lon = -121.9052, lat = 36.62183),
                color = "color", source = "google",
                maptype = "satellite", zoom = 20)

hmsMapB <- ggmap(hmsB, extent = "device",
                ylab = "Latitude", xlab = "Longitude")

hmsMapB + 
  geom_point(aes(lon, lat), 
             data = envDat, color = "black", size = 2, alpha = 0.5) + 
  geom_point(aes(lon, lat), 
             data = envDat, color = "yellow", size = 1, alpha = 0.5) + 
  geom_point(aes(lon, lat, color = species, 
                 shape = species), 
             data = sizeLL, 
             alpha = 0.5, inherit.aes = FALSE, size = 3) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") 
# ggsave("figs/map_waraB.png", height = 7, width = 7)


## need to focus on waraB
unique(sizeLL$site)
unique(sizeLL$nest1)
unique(sizeLL$sampleUnit)

waraB <- sizeLL %>% filter(site == "Wara.B") %>%
  group_by(nest1) %>% distinct() %>% ungroup()
range(waraB$tideHTm)
envWara <- envDat %>% filter(tidalHT < 1.9)
  
hmsMapB + 
  geom_point(aes(lon, lat), 
             data = envWara, color = "black", size = 2, alpha = 0.75) + 
  geom_point(aes(lon, lat), 
             data = envWara, color = "yellow", size = 1, alpha = 0.75) + 
  geom_point(aes(lon, lat, color = nest1), 
             data = waraB, 
             alpha = 1, inherit.aes = FALSE, size = 3) + 
  geom_text(aes(lon, lat, label = nest1), data = waraB, color = "white", 
            size = 3, nudge_x = -0.00004) + 
  geom_text(aes(lon, lat, label = position), data = envWara, color = "yellow", 
            size = 3, nudge_x = 0.00004, nudge_y = -0.00001) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") 

ggsave("figs/map_waraB.png", height = 7, width = 7)


##### GGMAP 3 - WARA D #####
hmsD <- get_map(location = c(lon = -121.9037, lat = 36.62147),
                color = "color", source = "google",
                maptype = "satellite", zoom = 20)

hmsMapD <- ggmap(hmsD, extent = "device",
                 ylab = "Latitude", xlab = "Longitude")

hmsMapD + 
  geom_point(aes(lon, lat), 
             data = envDat, color = "black", size = 2, alpha = 0.5) + 
  geom_point(aes(lon, lat), 
             data = envDat, color = "yellow", size = 1, alpha = 0.5) + 
  geom_point(aes(lon, lat, color = species, 
                 shape = species), 
             data = sizeLL, 
             alpha = 0.5, inherit.aes = FALSE, size = 3) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") 
# ggsave("figs/map_waraB.png", height = 7, width = 7)


## need to focus on waraD
waraD <- sizeLL %>% filter(site == "Wara.D") %>%
  group_by(nest1) %>% distinct() %>% ungroup()
range(waraD$tideHTm)

envWara <- envDat %>% filter(tidalHT < 1.9)

hmsMapD + 
  geom_point(aes(lon, lat), 
             data = envWara, color = "black", size = 2, alpha = 0.75) + 
  geom_point(aes(lon, lat), 
             data = envWara, color = "yellow", size = 1, alpha = 0.75) + 
  geom_point(aes(lon, lat, color = nest1), 
             data = waraD, 
             alpha = 1, inherit.aes = FALSE, size = 3) + 
  geom_text(aes(lon, lat, label = nest1), data = waraD, color = "white", 
            size = 3, nudge_x = -0.00004) + 
  geom_text(aes(lon, lat, label = position), data = envWara, color = "yellow", 
            size = 3, nudge_x = 0.00004, nudge_y = -0.00001) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") 

ggsave("figs/map_waraD.png", height = 7, width = 7)


##### MATCHING LOGGER POSITION TO SAMPLEUNITS #####

head(envDat)
write.csv(envDat, "output/loggerLL.csv") # EDIT THIS FILE

head(sizeLL)
write.csv(sizeLL, "output/sizeLL.csv") # EDIT THIS FILE
sampleUnit_vector <- sort(unique(sizeLL$sampleUnit))
