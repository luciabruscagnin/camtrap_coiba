#master map for all coiba stuff
library(rethinking)
library(mapview)
library(rgdal)
library(sf)

cam_ids <- read.csv(file="map/coiba_camtrap_ids_gps.csv") #generated with this script file https://github.com/bjbarrett/camtrap_coiba/blob/main/coiba_camtrap_gps_visualization.R
all_cams <- st_as_sf(cam_ids , coords = c("longitude", "latitude"), crs = 4326) #do it again if rading csv
all_cams_map <- mapview(all_cams , col.regions="gold" , cex=2 )

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

all_streams_map <- mapview(jic_posa_stream) + mapview(jic_waterfall_stream) + mapview(jic_alicia_stream) + mapview(jic_watersource_stream) + mapview(all_cams , col.regions="red" , pch=1)

#lets add in tool sites

tools_w201707201901 <- readOGR(dsn = "map/gpx/cleaned/Tool Use Sites July 2017 Jan 2018.GPX", layer="waypoints")
tools_w201803 <- readOGR(dsn = "map/gpx/cleaned/Tool Use Sites Mar 2018.GPX", layer="waypoints")
tools_w201807 <- readOGR(dsn = "map/gpx/cleaned/Tool Use Site July 2018.GPX", layer="waypoints")
tools_w201901 <- readOGR(dsn = "map/gpx/cleaned/Tool Use Sites Jan 2019.GPX", layer="waypoints")
tools_w201903 <- readOGR(dsn = "map/gpx/cleaned/Tool Sites Mar 2019.GPX", layer="waypoints")

all_tools_map <- mapview(tools_w201707201901 , col.region="red" , cex=2) + mapview(tools_w201803 , col.region="orange" , cex=2) + mapview(tools_w201807 , col.region="yellow" , cex=2) + mapview(tools_w201901 , col.region="green" , cex=2) + mapview(tools_w201903 , col.region="blue" , cex=2)

all_tools_map + all_streams_map + all_cams_map


### add in almendras
#note, i did not get to finish punta ursudal
almendras <- readOGR(dsn = "map/gpx/cleaned/T_catappa_points.GPX", layer="waypoints")
#mccir <- readOGR(dsn ="~/Dropbox/Capuchin Monkeys, Coiba National Park_July2018/GPS Points/MCCinreachJan2020.GPX" , layer="waypoints")##inreaches need a space on xml code in first line removed to code
most_almendras_map <- mapview(almendras , col.regions="green" , alpha.regions = 0.2 , cex=1.5)
all_tools_map + all_streams_map + all_cams_map + most_almendras_map #map of all trees, tools, camerasm etc


#####
pois <- readOGR(dsn = "map/gpx/cleaned/Coiba POI.GPX", layer="waypoints")
pois2 <- readOGR(dsn = "map/gpx/cleaned/Points of Interest.GPX", layer="waypoints")
mapview(pois)
mapview(pois2)
pois$name
pois2$name
mccir <- readOGR(dsn ="map/gpx/gps_dumps/MCCinreachJan2020.GPX" , layer="waypoints")##inreaches need a space on xml code in first line removed to code
mapview(mccir)
mccir$name
almendras$name
