#### Grid for field trip 02.07.2021 ####
## Grid in Jicaron tool use site, 20-25 cameras
# ~ 250 m spacing

library(mapview)
library(rgdal)
library(sf)
library(raster)
library(maptools)

# first run coiba_map_gps_viz.R to create all the layers for the maps, should make this so it is standalone at some point
#source("map/coiba_map_gps_viz.R")

#maybe use gridshapes
mapview(tools_w201803)
grid_shapes <- rbind(tools_w201707201901,tools_w201803,tools_w201807) # define grid area using a subset of tools w/ same number columns
str(grid_shapes)
e <- as(raster::extent(min(grid_shapes@coords[,1]), max(grid_shapes@coords[,1]), min(grid_shapes@coords[,2]), max(grid_shapes@coords[,2])), "SpatialPolygons")
proj4string(e) <- crs(grid_shapes)
e3 <- spTransform(e, CRS("+init=EPSG:32616"))
ebuf <- buffer(e3, width = 500) #add 500 m buffer
e2 <- st_as_sf(e)
e2b <- st_as_sf(ebuf)
grid_1000m <- st_make_grid(e2b, square = T, cellsize = c(1000, 1000) ) %>% 
  st_sf() #100m grid
grid_500m <- st_make_grid(e2b, square = T, cellsize = c(500, 500) ) %>% 
  st_sf() #100m grid
grid_250m <- st_make_grid(e2b, square = T, cellsize = c(250, 250)) %>% # the grid, covering bounding box
  st_sf() # not really required, but makes the grid nicer to work with later
grid_100m <- st_make_grid(e2b, square = T, cellsize = c(100, 100)) %>% # the grid, covering bounding box
  st_sf() # not really required, but makes the grid nicer to work with later

all_tools_map + all_streams_map +  most_almendras_map + all_cams_map + all_tracks + mapview(grid_250m, col.regions = "white")

grid <- SpatialGrid(GridTopology(c(0, 0), c(1, 1), c(5, 4)))

# get rotated grid parallel to coast
grid_100m2 <- st_make_grid(e2b, square = T, cellsize = c(100, 100) ) 

rotang = 50
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
grd_rot <- (grid_100m2 - st_centroid(st_union(grid_100m2))) * rot(rotang * pi / 180) +
  st_centroid(st_union(grid_100m2)) 

grid_100m_t <- st_as_sf(grd_rot)
st_crs(grid_100m_t) <- "+init=EPSG:32616"
mapview(grid_100m_t) + all_cams_map

# get coordinates of center coordinates of each cell
# https://gis.stackexchange.com/questions/203760/calculate-centre-point-of-spatialgrid-object
#above uses sp, so we can convert an sf to an sp object
grid_cams <- sf:::as_Spatial(grid_100m_t) #convert to 
grid_ctr <- sp:::coordinates(grid_cams) #this contains points, need to convert format and save as gpx
plot(grid_cams, axes=TRUE)
points(grid_ctr, col="red", pch=16 , cex=0.3) #converst to spartial points dataframe

# plot over topo map
grid_centers <- as.data.frame(grid_ctr)
grid_centers$name <- paste("TU", 1:nrow(grid_centers), sep = "_") # label per waypoint, needs to be called "name" to show up in GPX file
grid_centers_map <- st_as_sf(grid_centers, coords = c("V1", "V2"), crs = "+init=EPSG:32616")

mapview(grid_centers_map) + all_streams_map
mapview(grid_centers_map , cex=1.5 , label=TRUE , col.regions ="black") + all_streams_map + all_cams_map + mapview(grid_cams , alpha.regions=0.01)
mapview(grid_centers_map , cex=1.5 , label=TRUE , col.regions ="black") + mapview(grid_cams , alpha.regions=0.01) + mapview(all_tracks) + all_streams_map

# check if transformation went okay
plot(grid_cams)
plot(grid_centers_map, add = TRUE)
head(grid_centers_map)

## convert points and save as gpx
grid_centers_map2 <- SpatialPointsDataFrame(coords=grid_centers[,c(1,2)],data=grid_centers,proj4string =CRS("+init=EPSG:32616")) 
# need to transform to longitude/latitude 
grid_centers_map3 <- spTransform(grid_centers_map2, CRS("+init=EPSG:4326"))
crs(grid_centers_map3)

## subset to points of interest
TU_gridcenters <- grid_centers_map3[c(135, 137, 139, 141, 150:158,166:174, 181:190, 197:206, 213:222),]

# attempt to add static labels (doesn't allow for layering of mapview objects)
library(leafem)
mapview(TU_gridcenters, cex=1.5 , label=TRUE , col.regions ="black") %>%
  addStaticLabels(label = TU_gridcenters$name,
                  noHide = TRUE,
                  direction = 'top',
                  textOnly = TRUE,
                  textsize = "10px")

mapview(TU_gridcenters, cex=1.5 , label=TRUE , col.regions ="black") + all_streams_map + mapview(grid_cams, alpha.regions=0.01)

# to write GPX file, only re-run if something changed
# writeOGR(TU_gridcenters, dsn = "grid_sampling/JicaronTU_gridpoints.GPX", dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
# afterwards upload gpx tracks to Garmin basecamp to put on Garmin

### non tool using group
# center on CEBUS-09-02 camera?
NTU <- subset(all_cams, all_cams$camera_id == "SURVEY-CEBUS-09-02")
NTU_map <- mapview(NTU , col.regions="gold"  )
NTU_map

# move polygon with buffer to non tool use site (did this manually)
e_shift <- maptools::elide(ebuf, shift = c(3300,-1600))
proj4string(e_shift) <- crs(ebuf)

mapview(e_shift) + all_cams_map
e_shift2 <- st_as_sf(e_shift)

# get rotated grid parallel to coast
grid_100m2_shift <- st_make_grid(e_shift2, square = T, cellsize = c(100, 100) ) 

rotang2 = 30
grd_rot2 <- (grid_100m2_shift - st_centroid(st_union(grid_100m2_shift))) * rot(rotang2 * pi / 180) +
  st_centroid(st_union(grid_100m2_shift)) 

grid_100m_ts <- st_as_sf(grd_rot2)
st_crs(grid_100m_ts) <- "+init=EPSG:32616"
mapview(grid_100m_ts) + NTU_map

# get coordinates of center coordinates of each cell
# https://gis.stackexchange.com/questions/203760/calculate-centre-point-of-spatialgrid-object
#above uses sp, so we can convert an sf to an sp object
grid_cams2 <- sf:::as_Spatial(grid_100m_ts) #convert to 
grid_ctr2 <- sp:::coordinates(grid_cams2) #this contains points, need to convert format and save as gpx
plot(grid_cams2, axes=TRUE)
points(grid_ctr2, col="red", pch=16 , cex=0.3) #converst to spartial points dataframe

# plot over topo map
grid_centers2 <- as.data.frame(grid_ctr2)
grid_centers2$name <- paste("NTU", 1:nrow(grid_centers2), sep = "_") # label per waypoint, needs to be called "name" to show up in GPX file
grid_centers2_map <- st_as_sf(grid_centers2, coords = c("V1", "V2"), crs = "+init=EPSG:32616")

mapview(grid_centers2_map) + all_streams_map
mapview(grid_centers2_map , cex=1.5 , label=TRUE , col.regions ="black") + all_streams_map + all_cams_map + mapview(grid_cams2 , alpha.regions=0.01)
mapview(grid_centers2_map , cex=1.5 , label=TRUE , col.regions ="black") + NTU_map + mapview(grid_cams2 , alpha.regions=0.01) + mapview(all_tracks) + all_streams_map

## convert points and save as gpx
grid_centers2_map2 <- SpatialPointsDataFrame(coords=grid_centers2[,c(1,2)],data=grid_centers2,proj4string =CRS("+init=EPSG:32616")) 
# need to transform to longitude/latitude 
grid_centers2_map3 <- spTransform(grid_centers2_map2, CRS("+init=EPSG:4326"))
crs(grid_centers2_map3)

## subset to points of interest
NTU_gridcenters <- grid_centers2_map3[c(35:38, 51:56, 67:72, 83:88, 99:104, 115:120, 131:136, 148:153, 164:169),]

# attempt to add static labels (doesn't allow for layering of mapview objects)
library(leafem)
mapview(TU_gridcenters, cex=1.5 , label=TRUE , col.regions ="black") %>%
  addStaticLabels(label = TU_gridcenters$name,
                  noHide = TRUE,
                  direction = 'top',
                  textOnly = TRUE,
                  textsize = "10px")

mapview(NTU_gridcenters, cex=1.5 , label=TRUE , col.regions ="black") + mapview(grid_cams2, alpha.regions=0.01) + NTU_map

# to write GPX file, only re-run if something changed

# writeOGR(NTU_gridcenters, dsn = "grid_sampling/JicaronNTU_gridpoints.GPX", dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)

# afterwards upload gpx tracks to Garmin basecamp to put on Garmin

# write points of interest and camera locations to GPX
str(pois_4_map_and_gps)

writeOGR(pois_4_map_and_gps, dsn = "grid_sampling/pointsofinterest.GPX", dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)

str(all_cams2)
all_cams2 <- as_Spatial(all_cams)
writeOGR(all_cams2, dsn = "grid_sampling/cam_locations.GPX", dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
