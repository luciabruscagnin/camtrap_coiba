#master map for all coiba stuff
library(rethinking)
library(mapview)
library(rgdal)
library(sf)

cam_ids <- read.csv(file="map/coiba_camtrap_ids_gps.csv") #generated with this file https://github.com/bjbarrett/camtrap_coiba/blob/main/coiba_camtrap_gps_visualization.R
all_cams <- st_as_sf(cam_ids , coords = c("longitude", "latitude"), crs = 4326) #do it again if rading csv
mapview(all_cams)

filelist <- list.files(path="~/Dropbox/camtrap_coiba/map/gpx/bjb_raw_garmin_folders" , full.names=TRUE)
filelist 

#load stream gps points
jic_waterfall_stream <- readOGR(dsn =filelist[3] , layer="track_points")
mapview(jic_waterfall_stream)
jic_waterfall_stream <- jic_waterfall_stream[3:6,]
mapview(jic_waterfall_stream)
jic_waterfall_stream$name <- "Jic TS Catarata Stream"

jic_watersource_stream <- readOGR(dsn =filelist[4] , layer="track_points")
mapview(jic_watersource_stream)
jic_watersource_stream$name <- "Jic TS Watersource Stream"

jic_alicia_stream <- readOGR(dsn =filelist[5] , layer="track_points")
jic_alicia_stream <- jic_alicia_stream[3:108,]
mapview(jic_alicia_stream)
jic_alicia_stream$name <- "Jic TS Alicia Stream"

jic_posa_stream <- readOGR(dsn =filelist[6] , layer="track_points")
str(jic_posa_stream)
jic_posa_stream <- jic_posa_stream[4:30,]
jic_posa_stream$name <- "Jic TS Posa Stream"

mapview(jic_posa_stream) + mapview(jic_waterfall_stream) + mapview(jic_alicia_stream) + mapview(jic_watersource_stream) + mapview(all_cams , col.regions="red" , pch=1)

#lets add in tool sites


#mccir <- readOGR(dsn ="~/Dropbox/Capuchin Monkeys, Coiba National Park_July2018/GPS Points/MCCinreachJan2020.GPX" , layer="waypoints")##inreaches need a space on xml code in first line removed to code

