#Let's create a map of where we're looking at. 

#Libraries we'll need
library(raster)
library(rgdal)
library(ggmap)
library(ggplot2)
library(ggrepel)
library(readxl)
library(readr)

#Grabbing unique site data from the current IsoArch Database
maphuman <- subset(ane, select = c(siteName, lat, lon, country))
mapanimal <- subset(aneanimal, select = c(siteName, lat, lon, country))
mapplant <- subset(aneplant, select = c(siteName, lat, lon, country))

map1 <- merge(maphuman, mapanimal, all = T)
map2 <- merge(map1, mapplant, all = T)

map_unique <- subset(map2, lon > 10) #why are there Belgian sites?
map_unique$siteName[grepl('Ephesus', map_unique$siteName)] <- 'Ephesus'
map_unique$siteName[grepl("Ya'amoun", map_unique$siteName)] <- "Ya'amun"
map_unique$siteName[grepl('Kellis', map_unique$siteName)] <- 'Kellis'
map_unique <- distinct(map_unique, siteName, .keep_all = T)

#We have the sites that are going to be integrated soon
upcomingIsoarch <- read_csv("upcomingIsoarch.csv")
upcomingIsoarch$siteName[grepl('Ephesus', upcomingIsoarch$siteName)] <- 'Ephesus'
upcomingIsoarch$siteName[grepl("Ya'amoun", upcomingIsoarch$siteName)] <- "Ya'amun"
upcomingIsoarch$siteName[grepl('Kellis', upcomingIsoarch$siteName)] <- 'Kellis'
upcomingIsoarch <- distinct(upcomingIsoarch, siteName, .keep_all = T)


#maps
ANEMap <- ggmap(get_stamenmap(bbox = c(left = 25, bottom = 20, right = 45, top = 40),
                                  zoom = 7, maptype = c("watercolor"), color = c('bw'), force = T))
#let's get the Arabian Peninsula in there
ANEMap2 <- ggmap(get_stamenmap(bbox = c(left = 25, bottom = 11, right = 61, top = 40),
                              zoom = 7, maptype = c("watercolor"), color = c('bw'), force = T))

ANEMap +
  geom_point(data = map_unique, aes(x = lon, y = lat), size = 2) + 
 # geom_text_repel(data = map_unique, aes(x = lon, y = lat, label = siteName), max.overlaps = 30) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )

ANEMap2 + 
  geom_point(data = upcomingIsoarch, aes(x = lon, y = lat), size = 2) + 
  # geom_text_repel(data = map_unique, aes(x = lon, y = lat, label = siteName), max.overlaps = 30) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )
