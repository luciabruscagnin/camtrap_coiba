## Calculating distance to coast 

# Supplemental Code for “Tool Use and Tidal Cycles: Exploitation of Coastal 
# Habitat by Island-living White-faced Capuchin Monkeys"

# Zoë Goldsborough, Margaret Crofoot, Shauhin Alavi, Sylvia Garza, 
# Evelyn Del Rosario, Kate Tiedeman, Claudio Monteza & Brendan Barrett
# 2023

# Convert tiff of coastal vegetation boundary to shapefile
# Tiff created by use of Google Earth Engine (see script)
library(raster)
library(terra)

r <- rast("tide_analysis/jicaron-ndvi-clip3.tif")

r2 <- r
r2[r2>0.6] <- 1
plot(r2)

r3 <- as.polygons(r2, dissolve = T, na.rm=T)
plot(r3)

r4 <-fillHoles(r3, inverse=FALSE)
writeVector(r4,"tide_analysis/jicaron-ndvi-shoreline-noholes.shp" )

plot(r4)

### code starting from shapefile
# load in csv with GPS locations of all cameras
## NOTE: GPS locations of camera traps have not been shared for conservation purposes

#tidalcams <- read.csv("tide_analysis/tidal_cams_gps.csv", header = TRUE)

coordinates(tidalcams) <- ~longitude + latitude
plot(tidalcams)

# set the crs to latlong
proj4string(tidalcams) <- CRS("+proj=longlat +datum=WGS84") #first way

# sp transform to get to units with meters
# project to meters specific to where we are
tidalcampts <- spTransform(tidalcams, CRS("+init=EPSG:32617"))

# load in spatial polygon
r4 <- shapefile("tide_analysis/jicaron-ndvi-shoreline-noholes.shp")
crs(r4)
r5 <- spTransform(r4, CRS("+init=EPSG:32617"))

crs(tidalcampts)
crs(r4)

library(rgeos)

d <- gDistance(tidalcampts, as(r5, "SpatialLines"), byid = TRUE) # dist to line
tidalcams2$distcoast <- as.vector(d)

# write file with distance in meters per camera, to include in dataset
#write.csv(tidalcams2, "tide_analysis/tidalcams2.csv")
