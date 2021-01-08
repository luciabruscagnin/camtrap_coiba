library(mapview)
library(sf) 
library(rgdal)
library(lubridate)
library(janitor)
library(stringr)

#setwd("/Users/BJB/Dropbox/Coiba Tool Images/GPX/")

cammar2017 <- readOGR(dsn = "~/Dropbox/Capuchin Monkeys, Coiba National Park_July2018/GPS Points/Camera Deployments Mar 2017.GPX", layer="waypoints")
camjul2017 <- readOGR(dsn = "~/Dropbox/Capuchin Monkeys, Coiba National Park_July2018/GPS Points/Camera Deployments July 2017.GPX", layer="waypoints")
camjan2018 <- readOGR(dsn = "~/Dropbox/Capuchin Monkeys, Coiba National Park_July2018/GPS Points/Camera Deployments Jan 2018.GPX", layer="waypoints")

mccir <- readOGR(dsn ="~/Dropbox/Capuchin Monkeys, Coiba National Park_July2018/GPS Points/MCCinreachJan2020.GPX" , layer="waypoints")##inreaches need a space on xml code in first line removed to code
m <- mccir[1:756,]
m2019 <- m[year(m$time)>2018,]
jan2019 <- m[year(m$time)==2019 & month(m$time)==1,]
mar2019 <- m[year(m$time)==2019 & month(m$time)==3,]
aug2019 <- m[year(m$time)==2019 & month(m$time)==8,]
dec2019 <- m[year(m$time)==2019 & month(m$time)==12,]
mar2018 <- m[year(m$time)==2018 & month(m$time)==3,]


month(m2019$time)
str(m2019) 
m2019$time
mapview(m2019[19,])
sort(m$name)
mapview(cammar2017 , col.region="red") + mapview(camjul2017 , col.region="orange") + mapview(camjan2018 , col.region="green") + mapview(m2019 , col.region="blue")
mapview(m2019 , col.region="blue")
#######interactive maps#######

##edit mar 2017 deployment
cammar2017$old_name <- cammar2017$name
cammar2017$name <- as.character(cammar2017$name)
cammar2017$name[1] <-  "SURVEY-CEBUS-01-01-R1"
cammar2017$name[2] <-  "SURVEY-CEBUS-02-01-R1"
cammar2017$name[3]  <- "SURVEY-CEBUS-02-02-R1"
cammar2017$name[4] <- "SURVEY-CEBUS-03-01-R1"
cammar2017$name[5] <- "SURVEY-CEBUS-03-02-R1"
cammar2017$name[6]  <- "SURVEY-CEBUS-04-01-R1"
cammar2017$name[7] <- "SURVEY-CEBUS-05-01-R1"
cammar2017$name[8] <- "SURVEY-CEBUS-06-01-R1"
cammar2017$name[9] <- "CEBUS-01-R1"
cammar2017$name[10] <- "CEBUS-02-R1"
cammar2017$name[11] <- "SURVEY-CEBUS-08-01-R1"
cammar2017$name[12] <- "SURVEY-CEBUS-09-01-R1"
cammar2017$name[13] <- "SURVEY-CEBUS-09-02-R1"
cammar2017$name[14] <- "SURVEY-CEBUS-10-01-R1"
cammar2017$name[15] <- "SURVEY-CEBUS-11-01-R1"
cammar2017$name[16] <- "SURVEY-CEBUS-12-01-R1"

mapview(cammar2017 , zcol='name')

##edit july 2017 deployment
camjul2017$old_name <- camjul2017$name
camjul2017$name <- as.character(camjul2017$name)
mapview(camjul2017 , zcol='name')
camjul2017$name
camjul2017$name[1] <- "SURVEY-CEBUS-03-03-R2"
camjul2017$name[2] <- "SURVEY-CEBUS-06-01-R2"
camjul2017$name[3] <- "CEBUS-01-R2"
camjul2017$name[4] <- "CEBUS-02-R2"
camjul2017$name[5] <- "SURVEY-CEBUS-13-01-R2"
camjul2017$name[6] <- "SURVEY-CEBUS-14-01-R2"
camjul2017$name[7] <- "SURVEY-CEBUS-15-01-R2"
camjul2017$name[8] <- "SURVEY-CEBUS-16-01-R2"
camjul2017$name[9] <- "CEBUS-08-R2"
camjul2017$name[10] <- "CEBUS-09-R2"
camjul2017$name[11] <- "SURVEY-CEBUS-18-01-R2"
camjul2017$name[12] <-  "SURVEY-CEBUS-19-01-R2"
camjul2017$name[13] <- "SURVEY-CEBUS-20-01-R2"
camjul2017$name[14] <- "SURVEY-CEBUS-21-01-R2"
camjul2017$name[15] <- "SURVEY-CEBUS-06-02-R2"


##ADD MISSING CAMERAS

camjul2017 <- rbind(camjul2017,cammar2017[16,])
camjul2017$name[16] <-"SURVEY-CEBUS-12-01-R2"

####add below still
#CEBUS-05-R2
#SURVEY-CEBUS-16-02-R2






str(camjul2017)
mapview(camjul2017 , zcol='name')



#########jan 2018
camjan2018$old_name <- camjan2018$name
camjan2018$name <- as.character(camjan2018$name)
mapview(camjan2018 , zcol='name')
camjan2018$name
camjan2018$name[1] <- "SURVEY-CEBUS-03-04-R3"
camjan2018$name[3] <- "CEBUS-01-R3"
camjan2018$name[4] <- "CEBUS-02-R3"
camjan2018$name[5] ####whats up with this
camjan2018$name[6] <- "SURVEY-CEBUS-15-02-R3"
camjan2018$name[7] <- "SURVEY-CEBUS-15-03-R3"
camjan2018$name[8] <- "CEBUS-05-R3"
camjan2018$name[9] <- "CEBUS-08-R3"
camjan2018$name[10] <- "SURVEY-CEBUS-22-01-R3"
camjan2018$name[11] <- "SURVEY-CEBUS-23-01-R3"

camjan2018 <- camjan2018[-c(2,5,12),]

camjan2018  <- rbind(camjan2018,camjan2018[2,])
camjan2018$name
camjan2018$name[10] <- "SURVEY-CEBUS-07-03-R3"

camjan2018  <- rbind(camjan2018,cammar2017[1,])

camjan2018$name[11] <- "SURVEY-CEBUS-01-02-R4"

mapview(camjan2018 , zcol='name')

#need to add 
# SURVEY-CEBUS-17-03-R3
camjan2018$name
str(camjan2018)
camjan2018$name[1]
camjan2018@coords[1,]
camjan2018$name[2]
camjan2018@coords[2,]
camjan2018$name[3]
camjan2018@coords[3,]
camjan2018$name[4]
camjan2018@coords[4,]
camjan2018$name[5]
camjan2018@coords[5,]
camjan2018$name[6]
camjan2018@coords[6,]
camjan2018$name[7]
camjan2018@coords[7,]
camjan2018$name[8]
camjan2018@coords[8,]
camjan2018$name[9]
camjan2018@coords[9,]
camjan2018$name[10]
camjan2018@coords[10,]
camjan2018$name[11]
camjan2018@coords[11,]
camjul2017$name[10]
camjul2017@coords[10,]

camjan2018$name[8]
camjan2018@coords[8,]

cbind(cammar2017$name,cammar2017@coords)

######jan 2019
mapview(jan2019)
jan2019$name
mapview(jan2019[17:24,])
jan2019[17:24,]$name
jan2019[17:24,]$time
jan2019[17:24,]$

######mar 2019
str(mar2019)

#######load table of camera trap invensiry, lets add GPS points.
imginv <- read.csv("image_inventory_coiba.csv")
imginv <- clean_names(imginv)
str(imginv)
sort(unique(imginv$final_name))
imginv$deployment <- str_sub(imginv$final_name, start= -2)
str_sub(imginv$final_name, end= -4)
imginv$camera_id <- str_sub(imginv$final_name, end= -4)

#append ids
cam_ids <- as.data.frame(unique(imginv$camera_id)) #make a list of camera IDs
names(cam_ids)[1] <- "camera_id"
cam_ids$latitude <- cam_ids$longitude <-  NA
# cam_ids$longitude[cam_ids$camera_id=="CEBUS-01"] <- cammar2017@coords[cammar2017$name=="CEBUS-01-R1"][1]
# cam_ids$latitide[cam_ids$camera_id=="CEBUS-01"] <- cammar2017@coords[cammar2017$name=="CEBUS-01-R1"][2]
"SURVEY-CEBUS-01-01-R1"
#using gps points from mar 2017
cammar2017$name

cam_ids[cam_ids[1]=="SURVEY-CEBUS-01-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-01-01-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-02-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-02-01-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-02-02",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-02-02-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-03-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-03-01-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-03-02",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-03-02-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-04-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-04-01-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-05-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-05-01-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-06-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-06-01-R1"]
cam_ids[cam_ids[1]=="CEBUS-01",2:3] <- cammar2017@coords[cammar2017$name=="CEBUS-01-R1"]
cam_ids[cam_ids[1]=="CEBUS-02",2:3] <- cammar2017@coords[cammar2017$name=="CEBUS-02-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-08-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-08-01-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-09-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-09-01-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-09-02",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-09-02-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-10-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-10-01-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-11-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-11-01-R1"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-12-01",2:3] <- cammar2017@coords[cammar2017$name=="SURVEY-CEBUS-12-01-R1"]
cam_ids
##july 2017
camjul2017$name
cam_ids[cam_ids[1]=="SURVEY-CEBUS-03-03",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-03-03-R2"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-06-02",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-06-02-R2"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-13-01",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-13-01-R2"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-14-01",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-14-01-R2"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-15-01",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-15-01-R2"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-16-01",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-16-01-R2"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-16-02",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-16-01-R2"] #not a mistake, not precise
cam_ids[cam_ids[1]=="SURVEY-CEBUS-18-01",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-18-01-R2"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-19-01",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-19-01-R2"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-20-01",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-20-01-R2"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-21-01",2:3] <- camjul2017@coords[camjul2017$name=="SURVEY-CEBUS-21-01-R2"]
cam_ids[cam_ids[1]=="CEBUS-08",2:3] <- camjul2017@coords[camjul2017$name=="CEBUS-08-R2"]
cam_ids[cam_ids[1]=="CEBUS-09",2:3] <- camjul2017@coords[camjul2017$name=="CEBUS-09-R2"]
###16-02 is the later good one
###jan2018
camjan2018$name
cam_ids[cam_ids[1]=="CEBUS-05",2:3] <- camjan2018@coords[camjan2018$name=="CEBUS-05-R3"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-03-04",2:3] <- camjan2018@coords[camjan2018$name=="SURVEY-CEBUS-03-04-R3"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-15-02",2:3] <- camjan2018@coords[camjan2018$name=="SURVEY-CEBUS-15-02-R3"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-15-03",2:3] <- camjan2018@coords[camjan2018$name=="SURVEY-CEBUS-15-03-R3"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-22-01",2:3] <- camjan2018@coords[camjan2018$name=="SURVEY-CEBUS-22-01-R3"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-23-01",2:3] <- camjan2018@coords[camjan2018$name=="SURVEY-CEBUS-23-01-R3"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-07-03",2:3] <- camjan2018@coords[camjan2018$name=="SURVEY-CEBUS-07-03-R3"]
cam_ids[cam_ids[1]=="SURVEY-CEBUS-01-02",2:3] <- camjan2018@coords[camjan2018$name=="SURVEY-CEBUS-01-02-R4"]
cam_ids[cam_ids[1]=="CEBUS-03",2:3] <- cammar2017@coords[cammar2017$name=="CEBUS-02-R1"]#cam on log anvil

cam_ids
###jan2019
jan2019$name
cam_ids[cam_ids[1]=="JIC-STREAM-DISC-T-1",2:3] <- jan2019@coords[jan2019$name=="Stream. T. 1"]#crab stream exists
cam_ids[cam_ids[1]=="JIC-STREAM-DISC-T-2",2:3] <- jan2019@coords[jan2019$name=="Stream. T. 2"]#crab stream hypothetical
cam_ids[cam_ids[1]=="JIC-STREAM-DISC-T-3",2:3] <- jan2019@coords[jan2019$name=="Stream. T. 3"]#crab stream may not exist
cam_ids[cam_ids[1]=="JIC-STREAM-CAMP-NO-T-01",2:3] <- jan2019@coords[jan2019$name=="Stream. no. T. 1"]#crab stream may not exist
cam_ids[cam_ids[1]=="JIC-STREAM-CAMP-NO-T-02",2:3] <- jan2019@coords[jan2019$name=="Stream. no. T. 2"]#crab stream may not exist
 mapview(m)
 cam_ids[cam_ids[1]=="JIC-STREAM-CAMP-NO-T-02",2:3] <- jan2019@coords[jan2019$name=="Stream. no. T. 2"]#crab stream may not exist

 cam_ids[cam_ids[1]=="ESC-01",2:3] <- aug2019@coords[aug2019$name=="RioEsc. 1"]
 cam_ids[cam_ids[1]=="ESC-02",2:3] <- aug2019@coords[aug2019$name=="RioEsc. 2"]
 cam_ids[cam_ids[1]=="ESC-03",2:3] <- aug2019@coords[aug2019$name=="RioEsc. 3"]
 cam_ids[cam_ids[1]=="SURVEY-RIO-ESC-00",2:3] <- mar2019@coords[mar2019$name=="Stone. 1"]
 
 
 mapview(mar2019)
 mapview(aug2019)
 aug2019$name
###need moar done
cam_ids[cam_ids[1]=="CEBUS-17-03",2:3] <- cammar2017@coords[cammar2017$name=="CEBUS-02-R1"]#bbc stream entrance, 
cam_ids[cam_ids[1]=="CEBUS-15-04",2:3]  





cammar2017$name
mapview(cam_ids , xcol="longitude" , ycol="latitude")
mapview(cam_ids[1:16,] , xcol=cam_ids$longitude[1:16] , ycol=cam_ids$latitude[1:16] ,  )
