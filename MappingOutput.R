#Let's create a map of where we're looking at. 

#Libraries we'll need
library(raster)
library(rgdal)
library(ggmap)
library(ggplot2)
library(ggrepel)
library(readxl)
library(readr)

# These dataframes are present in the main R notebook, but they're repeated here so this 
# code can stand alone. 
human <- read_delim("isoarch_2022-08-09_16_48_25.csv", 
                    ";", trim_ws = TRUE)

animal <- read_delim("isoarch_2022-08-09_13_35_51.csv", 
                     ";", trim_ws = TRUE)

plant <- read_delim("isoarch_2022-08-09_13_10_53.csv",
                    ";", trim_ws = TRUE)

ane <- rbind(human, animal, plant) %>% 
  subset( country == "Egypt"|country =="Israel"|country =="Palestine"|country =="Jordan"|country =="Lebanon"|country =="Turkey"|country =="Syria")

ane$humanAgeEstimAvg = ifelse(is.na(ane$humanAgeEstimMax),
                              ane$humanAgeEstimMin, 
                              (ane$humanAgeEstimMin+ane$humanAgeEstimMax)/2)


#Grabbing unique site data from the current IsoArch Database
map <- subset(ane, select = c(siteName, lat, lon, country))

map_unique <- subset(map, lon > 10) #why are there Belgian sites?
map_unique$siteName[grepl('Ephesus', map_unique$siteName)] <- 'Ephesus'
map_unique$siteName[grepl("Ya'amoun", map_unique$siteName)] <- "Ya'amun"
map_unique$siteName[grepl('Kellis', map_unique$siteName)] <- 'Kellis'
map_unique <- distinct(map_unique, siteName, .keep_all = T) %>% 
  na.omit()

#We have the sites that are going to be integrated soon
upcomingIsoarch <- read_csv("upcomingIsoarch.csv")
upcomingIsoarch$siteName[grepl('Ephesus', upcomingIsoarch$siteName)] <- 'Ephesus'
upcomingIsoarch$siteName[grepl("Ya'amoun", upcomingIsoarch$siteName)] <- "Ya'amun"
upcomingIsoarch$siteName[grepl('Kellis', upcomingIsoarch$siteName)] <- 'Kellis'
upcomingIsoarch <- distinct(upcomingIsoarch, siteName, .keep_all = T)


#maps
ANEMap <- ggmap(get_stamenmap(bbox = c(left = 25, bottom = 20, right = 45, top = 40),
                                  zoom = 7, maptype = c("watercolor"), 
                              #color = c('bw'),
                              force = T))
#let's get the Arabian Peninsula in there
ANEMap2 <- ggmap(get_stamenmap(bbox = c(left = 25, bottom = 11, right = 61, top = 40),
                              zoom = 7, maptype = c("watercolor"), 
                              #color = c('bw'),
                              force = T))

ANEMap +
  geom_point(data = map_unique, aes(x = lon, y = lat), size = 2) + 
 # geom_text_repel(data = map_unique, aes(x = lon, y = lat, label = siteName), max.overlaps = 30) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  ) + 
  theme_void()
ggsave("figures/siteMap.tiff", dpi = 300)

ANEMap2 + 
  geom_point(data = upcomingIsoarch, aes(x = lon, y = lat), size = 2) + 
  # geom_text_repel(data = map_unique, aes(x = lon, y = lat, label = siteName), max.overlaps = 30) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )
