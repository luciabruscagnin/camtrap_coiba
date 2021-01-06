library(lubridate)
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
