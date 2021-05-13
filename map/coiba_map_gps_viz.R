#master map for all coiba stuff
library(rethinking)
library(mapview)
library(rgdal)
library(sf)

##### load camera traps
cam_ids <- read.csv(file="map/coiba_camtrap_ids_gps.csv") #generated with this script file https://github.com/bjbarrett/camtrap_coiba/blob/main/coiba_camtrap_gps_visualization.R
all_cams <- st_as_sf(cam_ids , coords = c("longitude", "latitude"), crs = 4326) #do it again if rading csv
all_cams_map <- mapview(all_cams , col.regions="gold"  )

filelist <- list.files(path="~/Dropbox/camtrap_coiba/map/gpx/bjb_raw_garmin_folders" , full.names=TRUE)
filelist 
bjb_cm_se_sw_thru_20170324  <- readOGR(dsn =filelist[7] , layer="tracks")
mapview(bjb_cm_se_sw_thru_20170324)

for(i in 1:length(filelist)){
  x  <- readOGR(dsn =filelist[i] , layer="tracks")
  print(mapview(x))
}
filelist

i <- 18
mapview(readOGR(dsn =filelist[i] , layer="tracks")) + mapview(readOGR(dsn =filelist[i] , layer="track_points"))
mapview(readOGR(dsn =filelist[i] , layer="waypoints" ) , col.regions="red")



##### load stream gps points
#####here is how we turn streams to lines and trim
filelist
#quebrada catarata
s1 <- readOGR(dsn =filelist[3] , layer="track_points")
mapview(s1)
s2 <- s2[3:6,]
coord<-as.data.frame(coordinates(s2))
queb_cat <- Line(as.data.frame(coordinates(s2)))
queb_cat <-Lines(list(queb_cat),ID="queb_catarata")

###discovery/watersource
s1 <- readOGR(dsn =filelist[4] , layer="track_points")
mapview(s1)
coord<-as.data.frame(coordinates(s1))
queb_watersource <- Line(as.data.frame(coordinates(s1)))
queb_watersource <-Lines(list(queb_watersource),ID="queb_watersource")

###bbc/alicia
s1 <- readOGR(dsn =filelist[5] , layer="track_points")
mapview(s1)
s2 <- s1[4:108,]
coord<-as.data.frame(coordinates(s2))
queb_alicia <- Line(as.data.frame(coordinates(s2)))
queb_alicia <-Lines(list(queb_alicia),ID="queb_alicia")

###posa
s1 <- readOGR(dsn =filelist[6] , layer="track_points")
mapview(s1[4:30,])
s2 <- s1[4:30,]
coord<-as.data.frame(coordinates(s2))
queb_posa <- Line(as.data.frame(coordinates(s2)))
queb_posa <-Lines(list(queb_posa),ID="queb_posa")

streams <- SpatialLines(list(queb_cat,queb_watersource,queb_alicia,queb_posa) , proj4string = s2@proj4string )
mapview(streams , color="blue" , lw=3)

all_streams_map <- mapview(streams, color="blue" , lw=3)

#####lets add in tool sites
tools_w201707201901 <- readOGR(dsn = "map/gpx/cleaned/Tool Use Sites July 2017 Jan 2018.GPX", layer="waypoints")
tools_w201803 <- readOGR(dsn = "map/gpx/cleaned/Tool Use Sites Mar 2018.GPX", layer="waypoints")
tools_w201807 <- readOGR(dsn = "map/gpx/cleaned/Tool Use Site July 2018.GPX", layer="waypoints")
tools_w201901 <- readOGR(dsn = "map/gpx/cleaned/Tool Use Sites Jan 2019.GPX", layer="waypoints")
tools_w201903 <- readOGR(dsn = "map/gpx/cleaned/Tool Sites Mar 2019.GPX", layer="waypoints")

all_tools_map <- mapview(tools_w201707201901 , col.region="red" , cex=2) + mapview(tools_w201803 , col.region="orange" , cex=2) + mapview(tools_w201807 , col.region="yellow" , cex=2) + mapview(tools_w201901 , col.region="violet" , cex=2) + mapview(tools_w201903 , col.region="blue" , cex=2)

all_tools_map + all_streams_map + all_cams_map


### add in almendras
#note, i did not get to finish punta ursula with camera trapsl
almendras <- readOGR(dsn = "map/gpx/cleaned/T_catappa_points.GPX", layer="waypoints")
almendras$name
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
mccir_wp <- readOGR(dsn ="map/gpx/gps_dumps/MCCinreachJan2020.GPX" , layer="waypoints")##inreaches need a space on xml code in first line removed to code
mccir_tr <- readOGR(dsn ="map/gpx/gps_dumps/MCCinreachJan2020.GPX" , layer="tracks")##inreaches need a space on xml code in first line removed to code

mapview(mccir_wp) + mapview(mccir_tr)
mccir$name
almendras$name
#useful resource https://cmerow.github.io/RDataScience/04_Spatial.html
