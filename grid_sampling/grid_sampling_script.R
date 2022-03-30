#### Grid for field trip 02.07.2021 ####
## Grid in Jicaron tool use site, 20-25 cameras
# ~ 250 m spacing

library(mapview)
library(rgdal)
library(sf)
library(raster)

# first run coiba_map_gps_viz.R to create all the layers for the maps, should make this so it is standalone at some point
#source("map/coiba_map_gps_viz.R")

#maybe use gridshapes
mapview(tools_w201803)
grid_shapes <- rbind(tools_w201707201901,tools_w201803,tools_w201807) # define grid area using a subset of tools w/ same number columns
str(grid_shapes)
e <- as(raster::extent(min(grid_shapes@coords[,1]), max(grid_shapes@coords[,1]), min(grid_shapes@coords[,2]), max(grid_shapes@coords[,2])), "SpatialPolygons")
proj4string(e) <- crs(grid_shapes)
e3 <- spTransform(e, CRS("+init=EPSG:32616"))
ebuf <- buffer(e3, width = 4000) #add 500 m buffer
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

# get coordinates of center coordinates of each cell
# https://gis.stackexchange.com/questions/203760/calculate-centre-point-of-spatialgrid-object
#above uses sp, so we can convert an sf to an sp object
grid_cams <- sf:::as_Spatial(grid_100m) #convert to 
grid_ctr <- sp:::coordinates(grid_cams) #this contains points, need to convert format and save as gpx
plot(grid_cams, axes=TRUE)
points(grid_ctr, col="red", pch=16 , cex=0.3) #converst to spartial points dataframe
mapview(grid_250m) + all_tools_map

# plot over topo map
grid_centers <- as.data.frame(grid_ctr)
grid_centers$name <- paste("ZG", 1:nrow(grid_centers), sep = "_") # label per waypoint, needs to be called "name" to show up in GPX file
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

# to write GPX file, only re-run if something changed
# writeOGR(grid_centers_map3, dsn = "grid_sampling/JicaronTU_gridpoints.GPX", dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)

# afterwards upload gpx tracks to Garmin basecamp to put on Garmin

# write points of interest and camera locations to GPX
str(pois_4_map_and_gps)

writeOGR(pois_4_map_and_gps, dsn = "grid_sampling/pointsofinterest.GPX", dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)

str(all_cams2)
all_cams2 <- as_Spatial(all_cams)
writeOGR(all_cams2, dsn = "grid_sampling/cam_locations.GPX", dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
