##### BRIEF MAPPING INTERLUDE #####

datLL <- dat %>% filter(era == "present") %>%
  select(sp, site, lat2, long2) %>% distinct()

llSummary <- datLL %>% group_by(sp, site) %>%
  summarise(meanLat = mean(lat2, na.rm = TRUE), 
            meanLong = mean(long2, na.rm = TRUE)) 
llSummary

library(ggmap)

hms1 <- get_map(location = c(lon = -121.9045, lat = 36.6218),
                color = "color", source = "google",
                maptype = "satellite", zoom = 18)

hmsMap <- ggmap(hms1, extent = "device",
                ylab = "Latitude", xlab = "Longitude")

hmsMap + 
  geom_point(aes(meanLong, meanLat, color = sp, 
                 shape = sp), data = llSummary)

hmsMap + 
  geom_point(aes(long2, lat2, color = sp, 
                 shape = sp), data = datLL)

llSummary %>% ungroup() %>%
  summarise(grandLat = mean(meanLat), 
            grandLong = mean(meanLong))