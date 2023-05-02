library(camtrapR)

### Set working directory to external harddrive that contains the raw files
## all jpgs have to be in one folder, no subfolders (so manually copy the files from the 100RECNX folders into the main folder)
setwd("D:/")
# setwd("C:/Users/Zoe Goldsborough/Documents/RawData_PhD")

# need directory structure DeploymentNumber/Locations. E.g. R10 folder containing CEBUS-02,CEBUS-04 etc
# IMPORTANT: Make sure all the folders are already fully labeled with their final name, so CEBUS-02-R10. 

# say which directory is the 'station' directory 
wd_createStationDir <- file.path("raw/", "R12")
# define where the raw data is
wd_images_raw<- file.path("raw/R12")
# define where the renamed data has to go
wd_images_raw_renamed <- file.path("renamed/R12")

# rename the entire folder of R11
# this will rename all the JPGs, not the MP4s!!
renaming.table <- imageRename(inDir               = wd_images_raw, #source path
                              outDir              = wd_images_raw_renamed, #sink path
                              hasCameraFolders    = FALSE, #if old files have camera subfolder, need to be set to false
                              copyImages          = TRUE, #if copying will actually happen, set to false to simulate renaminf
                              keepCameraSubfolders = FALSE,
                              writecsv = FALSE)

# after this then run the exiftool shellscript also in this folder manually per video location to rename the MP4s
