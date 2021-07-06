#### Grid for field trip 02.07.2021 ####
## Grid in Jicaron tool use site, 20-25 cameras
# ~ 250 m spacing

# first run coiba_map_gps_viz.R to create all the layers for the maps
library(mapview)
library(rgdal)
library(sf)
library(raster)



#maybe use gridshapes
mapview(tools_w201803)
grid_shapes <- rbind(tools_w201707201901,tools_w201803,tools_w201807) # define grid area using a subset of tools w/ same number columns
str(grid_shapes)
e <- as(raster::extent(min(grid_shapes@coords[,1]), max(grid_shapes@coords[,1]), min(grid_shapes@coords[,2]), max(grid_shapes@coords[,2])), "SpatialPolygons")
proj4string(e) <- crs(grid_shapes)
e3 <- spTransform(e, CRS("+init=EPSG:32616"))
ebuf <- buffer(e3, width = 1500) #add 500 m buffer
e2 <- st_as_sf(e)
e2b <- st_as_sf(ebuf)
grid_1000m <- st_make_grid(e2b, square = T, cellsize = c(1000, 1000) ) %>% 
  st_sf() #100m grid
grid_500m <- st_make_grid(e2b, square = T, cellsize = c(500, 500) ) %>% 
  st_sf() #100m grid
grid_250m <- st_make_grid(e2b, square = T, cellsize = c(250, 250)) %>% # the grid, covering bounding box
  st_sf() # not really required, but makes the grid nicer to work with later

all_tools_map + all_streams_map +  most_almendras_map + all_cams_map + all_tracks + mapview(grid_250m, col.regions = "white")

grid <- SpatialGrid(GridTopology(c(0, 0), c(1, 1), c(5, 4)))

# get coordinates of center coordinates of each cell
# https://gis.stackexchange.com/questions/203760/calculate-centre-point-of-spatialgrid-object
#above uses sp, so we can convert an sf to an sp object
grid_cams <- sf:::as_Spatial(grid_250m) #convert to 
grid_ctr <- sp:::coordinates(grid_cams) #this contains points, need to convert format and save as gpx
plot(grid_cams, axes=TRUE)
points(grid_ctr, col="red", pch=16 , cex=0.3) #converst to spartial points dataframe
mapview(grid) + all_tools_map
