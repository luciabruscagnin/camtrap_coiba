library(lubridate)

###########R1
####this is first vdeo manually before fuunction
d <- read.csv("exiftool_metadata/Cebus-01-R1_exif_metadata_clean_mp4.csv") #load in metadata CSV after exiftooled
str(d)

#panama is UTC -5 . agouti like having Europe time which is +1. this is stupid.
d$CreateDate <- gsub("_", " ", d$CreateDate, fixed=TRUE) #replace _ with space
OlsonNames() #show timezenoe names
#d$create_date_tz <- as_datetime(d$CreateDate)
date(d$CreateDate)
d$create_date_tz_panama <- as.POSIXct(d$CreateDate , format = "%d/%m/%Y %H:%M:%S", tz = "America/Panama") #convert to panama time
d$create_date_tz_berlin <- with_tz(d$create_date_tz_panama, "Europe/Berlin") #agouti reads local time
d$create_date_tz_berlin_agouti <- format(as_datetime(d$create_date_tz_berlin), "%d/%m/%Y %H:%M:%S" , tz = "Europe/Berlin") #this is what agouti desires
d2 <- d[]


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
####below is to read a batch of csvs
##do.call read a batch of csvs // #list
# 
# readFunP <- function(pattern1,pathf){
#   lfiles <- list.files(path=pathf,
#                        pattern1, full.names = T, recursive = T) #pattern/glob2rx(paste(p1,p2,,sep="*")) 
#   x <- lapply(lfiles, read.csv)
#   return(x)
#   #pattern1=.csv 
#   #recursive is for sub folders
#   #lappaly fcn on returned list
# }

