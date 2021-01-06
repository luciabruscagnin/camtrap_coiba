library(camtrapR)

#########copy to external hard drive first, doing it by hand but could also do it via script which is better

#create folders on new hard drive "Coiba Image Data" for raw data
#b/c of subfolders we will drag and drop images, should be solvable for future however
#########################################R8#############################################
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/raw_data", "R8") #first is path, second is station
station_names_new <-c(
  "CEBUS-01-R8",
  "CEBUS-02-R8",
  "CEBUS-03-R8",
  "CEBUS-04-R8",
  "CEBUS-05-R8",
  "CEBUS-06-R8",
  "CEBUS-07-R8",
  "CEBUS-08-R8",
  "CEBUS-09-R8",
  "CEBUS-10-R8",
  "ESC-01-R8",
  "ESC-02-R8",
  "ESC-03-R8"
)

StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)

wd_images_raw <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R8") #location of images  
#will need to do time shifts on cebus 03
# destination for renamed images to be copied to
wd_images_raw_renamed <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R8") #location of renamed images   
#table of new names

renaming.table <- imageRename(inDir               = wd_images_raw, #source path
                              outDir              = wd_images_raw_renamed, #sink path
                              hasCameraFolders    = FALSE, #if old files have camera subfolder
                              copyImages          = TRUE, #if copying will actually happen, set to false to simulate renaminf
                              keepCameraSubfolders = FALSE,
                              writecsv = TRUE
)

#########################################R7#############################################
# Mar 2019 to Jul 2019; i feel like SD cards are missing
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/raw_data", "R7") #first is path, second is station
station_names_new <-c(
  "CEBUS-01-R7",
  "CEBUS-06-R7",
  "CEBUS-07-R7",
  "CEBUS-09-R7",
  "CEBUS-10-R7",
  "SURVEY-RIO-ESC-01-R7"
)

StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)

wd_images_raw <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R7") #location of images  
# destination for renamed images to be copied to
wd_images_raw_renamed <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R7") #location of renamed images   
#table of new names

renaming.table <- imageRename(inDir               = wd_images_raw, #source path
                              outDir              = wd_images_raw_renamed, #sink path
                              hasCameraFolders    = FALSE, #if old files have camera subfolder
                              copyImages          = TRUE, #if copying will actually happen, set to false to simulate renaminf
                              keepCameraSubfolders = FALSE,
                              writecsv = TRUE
)
#########################################R6#############################################
###deployment R6 Jan 2019 to Mar 2019
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/raw_data", "R6") #first is path, second is station
station_names_new <-c("CEBUS-01-R6",
                      "CEBUS-02-R6",
                      "CEBUS-03-R6",
                      "CEBUS-04-R6",
                      "CEBUS-05-R6",
                      "CEBUS-06-R6",
                      "CEBUS-07-R6",
                      "CEBUS-08-R6",
                      "CEBUS-09-R6")
# to create station directories in  wd_createStationDir on hard disk
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)
##apparently there are more jicron camera traps, lets create dirs for them
station_names_new_2 <-c(
  "JIC-STREAM-CAMP-NO-T-01",
  "JIC-STREAM-CAMP-NO-T-02",
  "JIC-STREAM-CAMP-NO-T-03",
  "JIC-STREAM-DISC-T-1",
  "JIC-STREAM-DISC-T-2",
  "JIC-STREAM-DISC-T-3")

wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/raw_data", "R6") #first is path, second is station
StationFolderCreate2 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new_2, 
                                              createinDir = TRUE)

#pick up here with renaming of images for R6
wd_images_raw <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R6") #location of images  
# destination for renamed images to be copied to
wd_images_raw_renamed <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R6") #location of renamed images   
#table of new names

renaming.table <- imageRename(inDir               = wd_images_raw, #source path
                                  outDir              = wd_images_raw_renamed, #sink path
                                  hasCameraFolders    = FALSE, #if old files have camera subfolder
                                  copyImages          = TRUE, #if copying will actually happen, set to false to simulate renaminf
                                  keepCameraSubfolders = FALSE,
                                  writecsv = TRUE
)

#########################################R5#############################################
###deployment R5 July 2018 to Dec 2018
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/raw_data", "R5") #first is path, second is station
station_names_new <-c("CEBUS-02-R5",
                      "SURVEY-CEBUS-16-02-R5",
                      "SURVEY-CEBUS-05-02-R5",
                      "SURVEY-CEBUS-15-04-R5",
                      "SURVEY-CEBUS-24-01-R5",
                      "CEBUS-09-R5",
                      "CEBUS-05-R5",
                      "SURVEY-CEBUS-17-03-R5",
                      "CEBUS-06-R5",
                      "CEBUS-03-R5",
                      "CEBUS-08-R5",
                      "CEBUS-01-R5")



# to create station directories in  wd_createStationDir on hard disk
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)
#create home for renamed files
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data", "R5") #first is path, second is station directory to create
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)

wd_images_raw <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R5") #location of images  
# destination for renamed images to be copied to
wd_images_raw_renamed <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R5") #location of renamed images   
#table of new names
renaming.table.csv <- imageRename(inDir               = wd_images_raw, #source path
                               outDir              = wd_images_raw_renamed, #sink path
                               hasCameraFolders    = FALSE, #if old files have camera subfolder
                               copyImages          = FALSE, #if copying will actually happen, set to false to simulate renaminf
                               keepCameraSubfolders = FALSE,
                               writecsv = TRUE
)

write.csv(renaming.table.csv, "R5renamingtablejpgs.csv")

renaming.table2 <- imageRename(inDir               = wd_images_raw, #source path
                               outDir              = wd_images_raw_renamed, #sink path
                               hasCameraFolders    = FALSE, #if old files have camera subfolder
                               copyImages          = TRUE, #if copying will actually happen, set to false to simulate renaminf
                               keepCameraSubfolders = FALSE
)

#####################################################R4#################################################

###deployment R4 March 2018 to July 2018
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/raw_data", "R4") #first is path, second is station directory to create
station_names_new <- c("SURVEY-CEBUS-01-02-R4" , "CEBUS-01-R4" , "CEBUS-02-R4" , "CEBUS-03-R4" , "SURVEY-CEBUS-15-04-R4" ,
                       "CEBUS-05-R4" , "CEBUS-06-R4" , "CEBUS-08-R4" , "CEBUS-09-R4" , "SURVEY-CEBUS-17-03-R4" , "SURVEY-CEBUS-24-01-R4")
# to create station directories in  wd_createStationDir on hard disk
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)
###create home for renamed files
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data", "R4") #first is path, second is station directory to create
station_names_new <- c("SURVEY-CEBUS-01-02-R4" , "CEBUS-01-R4" , "CEBUS-02-R4" , "CEBUS-03-R4" , "SURVEY-CEBUS-15-04-R4" ,
                       "CEBUS-05-R4" , "CEBUS-06-R4" , "CEBUS-08-R4" , "CEBUS-09-R4" , "SURVEY-CEBUS-17-03-R4" , "SURVEY-CEBUS-24-01-R4")
# to create station directories in  wd_createStationDir on hard disk
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)

##rename

wd_images_raw <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R4") #location of images  
# destination for renamed images to be copied to
wd_images_raw_renamed <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R4") #location of renamed images       

#table of new names
renaming.table2 <- imageRename(inDir               = wd_images_raw, #source path
                               outDir              = wd_images_raw_renamed, #sink path
                               hasCameraFolders    = FALSE, #if old files have camera subfolder
                               copyImages          = FALSE, #if copying will actually happen, set to false to simulate renaminf
                               keepCameraSubfolders = FALSE
)




#####################################################R3#################################################

###deployment R3 March 2018 to July 2018
#raw files
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/raw_data", "R3") #first is path, second is station directory to create
station_names_new <- c(
   "SURVEY-CEBUS-03-04-R3" , 
   "CEBUS-01-R3" , 
   "CEBUS-02-R3" , 
   "SURVEY-CEBUS-07-03-R3" , 
   "SURVEY-CEBUS-15-02-R3",
   "SURVEY-CEBUS-15-03-R3", 
   "CEBUS-05-R3" , 
   "CEBUS-08-R3" , 
   "CEBUS-09-R3", 
   "SURVEY-CEBUS-22-01-R3" , 
   "SURVEY-CEBUS-23-01-R3")

length(station_names_new)

# to create station directories in  wd_createStationDir on hard disk
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)
###create home for renamed files
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data", "R3") #first is path, second is station directory to create

StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)


wd_images_raw <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R3") #location of images  
# destination for renamed images to be copied to
wd_images_raw_renamed <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R3") #location of renamed images       

#table of new names
renaming.table2 <- imageRename(inDir               = wd_images_raw, #source path
                               outDir              = wd_images_raw_renamed, #sink path
                               hasCameraFolders    = FALSE, #if old files have camera subfolder
                               copyImages          = TRUE, #if copying will actually happen, set to false to simulate renaminf
                               keepCameraSubfolders = FALSE
)



####################################################R2#################################################
###deployment R2 July 2017 to Dec 2017
#raw files
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/raw_data", "R2") #first is path, second is station directory to create
station_names_new <- c(
   "SURVEY-CEBUS-03-03-R2",
   "SURVEY-CEBUS-06-01-R2",
   "SURVEY-CEBUS-06-02-R2",
   "CEBUS-01-R2",
   "CEBUS-02-R2",
   "SURVEY-CEBUS-12-01-R2",
   "SURVEY-CEBUS-13-01-R2",
   "SURVEY-CEBUS-14-01-R2",
   "SURVEY-CEBUS-15-01-R2",
   "SURVEY-CEBUS-16-01-R2",
   "CEBUS-05-R2",
   "SURVEY-CEBUS-16-02-R2",
   "CEBUS-08-R2",
   "CEBUS-09-R2",
   "SURVEY-CEBUS-18-01-R2",
   "SURVEY-CEBUS-19-01-R2",
   "SURVEY-CEBUS-20-01-R2",
   "SURVEY-CEBUS-21-01-R2")

length(station_names_new)

# to create station directories in  wd_createStationDir on hard disk
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)
###create home for renamed files
wd_createStationDir <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data", "R2") #first is path, second is station directory to create

StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = station_names_new, 
                                              createinDir = TRUE)


wd_images_raw <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R2") #location of images  
# destination for renamed images to be copied to
wd_images_raw_renamed <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R2") #location of renamed images       

#table of new names
renaming.tableR2 <- imageRename(inDir               = wd_images_raw, #source path
                               outDir              = wd_images_raw_renamed, #sink path
                               hasCameraFolders    = FALSE, #if old files have camera subfolder
                               copyImages          = TRUE, #if copying will actually happen, set to false to simulate renaminf
                               keepCameraSubfolders = FALSE
)


####################################################R1#################################################
###deployment R2 March 2017 to July 2017
#raw files
wd_createStationDir_R1 <- file.path("/Volumes/Coiba\ Image\ Data/raw_data", "R1") #first is path, second is station directory to create
station_names_new_R1 <- c(
  "SURVEY-CEBUS-01-01-R1",
  "SURVEY-CEBUS-02-01-R1" ,
  "SURVEY-CEBUS-02-02-R1" ,
  "SURVEY-CEBUS-03-01-R1" ,
  "SURVEY-CEBUS-03-02-R1" ,
  "SURVEY-CEBUS-04-01-R1" ,
  "SURVEY-CEBUS-05-01-R1" ,
  "SURVEY-CEBUS-06-01-R1" ,
  "CEBUS-01-R1" ,
  "CEBUS-02-R1" ,
  "SURVEY-CEBUS-08-01-R1" ,
  "SURVEY-CEBUS-09-01-R1" ,
  "SURVEY-CEBUS-09-02-R1" ,
  "SURVEY-CEBUS-10-01-R1" ,
  "SURVEY-CEBUS-11-01-R1" ,
"SURVEY-CEBUS-12-01-R1" 
)

length(station_names_new_R1)

# to create station directories in  wd_createStationDir on hard disk
StationFolderCreate_R1 <- createStationFolders (inDir       = wd_createStationDir_R1,
                                              stations    = station_names_new_R1, 
                                              createinDir = TRUE)
###create home for renamed files
wd_createStationDir_R1 <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data", "R1") #first is path, second is station directory to create

StationFolderCreate_R1 <- createStationFolders (inDir       = wd_createStationDir_R1,
                                              stations    = station_names_new_R1, 
                                              createinDir = TRUE)


wd_images_raw_R1 <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R1") #location of images  
# destination for renamed images to be copied to
wd_images_renamed_R1 <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R1") #location of renamed images       

#table of new names
renaming.table_R1 <- imageRename(inDir               = wd_images_raw_R1, #source path
                                outDir              = wd_images_renamed_R1, #sink path
                                hasCameraFolders    = FALSE, #if old files have camera subfolder
                                copyImages          = TRUE, #if copying will actually happen, set to false to simulate renaminf
                                keepCameraSubfolders = FALSE
)

########lets read file names in image folders
#Cebus-01-R5 or what it was originally
stuff <- list.files(path = "/Volumes/LaCie/Camera Traps July 2017/Cebus-06-01-BBC")
stuff[700] #this is 700, JUST AS NOSTRADMUS PREDICTED
stuff <- list.files(path = "/Volumes/LaCie/Camera Traps July 2017/Cebus-07-1B BBC/100RECNX")
stuff[length(stuff)]
length(stuff)
stuff <- list.files(path = "/Volumes/LaCie/Camera Traps July 2017/Cebus-07-1B BBC/101RECNX")
stuff[length(stuff)]
length(stuff)
stuff <- list.files(path = "/Volumes/LaCie/Camera Traps July 2017/Cebus-07-1B BBC/102RECNX")
stuff[length(stuff)]
length(stuff)
###########BELOW IS CODE SANDBOX###########

##########video tomfoolery
wd_video_raw <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R4/") #location of videos  and jpgs

exifTagNames(wd_video_raw) #gives first folder
exifTagNames(wd_video_raw ,  whichSubDir="CEBUS-03-R4" , fileName="RCNX0006_101RECNX.MP4") #gives first folder
exifTagNames(wd_video_raw ,  whichSubDir="CEBUS-03-R4" , fileName="RCNX0001_100RECNX.JPG") #gives first folder
exifTagNames(wd_video_raw ,  whichSubDir="CEBUS-03-R4" ) #gives first folder

wd_video_raw2 <- file.path("/Volumes/Coiba\ Image\ Data/raw_data/R4/CEBUS-03-R4") #location of videos  and jpgs
exifTagNames(wd_video_raw2 , fileName="RCNX0001_100RECNX.JPG") #gives meta data of image
exifTagNames(wd_video_raw2 , fileName="RCNX0006_101RECNX.MP4") #gives meta data of video

##rename

# destination for renamed images to be copied to
wd_video_raw_renamed2 <- file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R4/CEBUS-03-R4") #location of renamed images       

#table of new names
renaming.table2 <- imageRename(inDir               = wd_video_raw2, #source path
                               outDir              = wd_video_raw_renamed2, #sink path
                               hasCameraFolders    = FALSE, #if old files have camera subfolder
                               copyImages          = FALSE, #if copying will actually happen, set to false to simulate renaminf
                               keepCameraSubfolders = FALSE
)

renaming.table2


#############recordtable
wd_images_ID_species <-  file.path("/Volumes/Coiba\ Image\ Data/renamed_data/R5")

rec_table1 <- recordTable(inDir                 = wd_images_ID_species,
                            IDfrom                 = "directory",
                            minDeltaTime           = 30,
                            deltaTimeComparedTo    = "lastRecord",
                            writecsv               = FALSE,
                            timeZone               =
                            additionalMetadataTags = c("EXIF:Model", "EXIF:Make")
  )