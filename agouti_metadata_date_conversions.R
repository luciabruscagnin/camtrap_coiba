library(lubridate)


###lets write a fcn for repeats
agouti_rename <- function(x){
  x$CreateDate <- gsub("_", " ", x$CreateDate, fixed=TRUE) #replace _ with space
  x$create_date_tz_panama <- as.POSIXct(x$CreateDate , format = "%d/%m/%Y %H:%M:%S", tz = "America/Panama") #convert to panama time
  x$create_date_tz_monterrey_mx <- with_tz(x$create_date_tz_panama, "America/Monterrey") 
  x$create_date_tz_monterrey_mx_agouti <- format(as_datetime(x$create_date_tz_monterrey_mx), "%d/%m/%Y %H:%M:%S" , tz = "America/Monterrey") #monterrey, mx time for sylvia to agouti
  x$create_date_tz_berlin <- with_tz(x$create_date_tz_panama, "Europe/Berlin") 
  x$create_date_tz_berlin_agouti <- format(as_datetime(x$create_date_tz_berlin), "%d/%m/%Y %H:%M:%S" , tz = "Europe/Berlin") # konstanz time to agouti
  return(x)
}

###########R1
d <- read.csv("exiftool_metadata/Cebus-01-R1_exif_metadata_clean_mp4.csv") #load in metadata CSV after 
#Survey Cebus-02-01
d <- read.csv("exiftool_metadata/Survey-Cebus-02-01-R1_exif_metadata_clean_mp4.csv") 
d2 <- agouti_rename(d)
#Survey Cebus-03-01
d <- read.csv("exiftool_metadata/Survey-Cebus-03-01-R1_exif_metadata_clean_mp4.csv") 
d2 <- agouti_rename(d) 
write.csv(d2 , "agoutidates_Survey-Cebus-03-01-R1.csv")
#Survey Cebus-06-01
d <- read.csv("exiftool_metadata/Survey-Cebus-06-01-R1_exif_metadata_clean_mp4.csv") 
d2 <- agouti_rename(d) 


###########R2

##below needs uploaddownload r mac

d <- read.csv("exiftool_metadata/Cebus-08-R2_exif_metadata_clean_mp4.csv") 
d2 <- agouti_rename(d)

d <- read.csv("exiftool_metadata/Cebus-08-R4_exif_metadata_clean_mp4.csv") 
d2 <- agouti_rename(d)
