## Exiftool metadata cleaning: Temperature and Sequence Length
# First Brendan ran exiftool on his Mac to extract ambient temperature (only on JPGs) and mediaduration (sequence length of videos)
# need to get to a format where every MP4 also has a temperature (namely the one of the JPGs right before it)
# the media duration is format ss.ms for times under 30 seconds and h:min:ss for above 30 seconds, have to fix. 

library(tidyverse)
library(stringr)

# navigate to folder where all exiftool output CSVs are stored
setwd("exiftool_metadata/temperature_seqlength_exif/")

# make a list of the csv per deployment
files <- list.files(pattern="*.csv")
# read all the csvs into one big dataframe, rbind
exifgross <- do.call(rbind, lapply(files, function(x) read.csv(x, header = TRUE, stringsAsFactors = FALSE)))

### Add temperature to videos
exifgross$TemperatureVideo <- lag(exifgross$AmbientTemperature) # this works assuming the row order is correct. I think you could also arrange by filename to be extra safe?

# extract video dataset
exifvideos <- exifgross[(exifgross$video <- str_detect(exifgross$FileName, "MP4")), ]

## Fix video duration problem
# have two options 
# 1. string starts with 1-9, it's a ss.ms set-up. Can be identified by starting with not a 0, or by ending on s
# 2. string is over 30 seconds and starts with 0, it's a 00:00:00 setup, need to read that as a date-time variable and convert to seconds?

# first replace the 's' with a blank space
# assuming no videos are over 60 seconds (currently not the case), can simply only keep the last 2 characters of the 00:00: ones
# detect strings with : in them, and then for these only keep the last two characters
# then round the millisecond ones to a whole number
exifvideos <- exifvideos %>% 
  mutate(MediaDuration = str_replace(MediaDuration, "s", ""),
        sequencelength = ifelse((str_detect(MediaDuration, ":")),  
                                             (substr(MediaDuration, nchar(MediaDuration)-1, nchar(MediaDuration))), 
                                             MediaDuration),
         seq_lengthv = as.integer(round(as.numeric(sequencelength))),
         temperature = str_replace(TemperatureVideo, "C", ""),
         FileName = str_replace(FileName, "MP4", "mp4")
)

# clean up dataframe to what we want to match
exifvideosclean <- exifvideos[,c("FileName", "seq_lengthv", "temperature")]

exifstills <- exifgross[str_detect(exifgross$FileName, "JPG"),]
exifstills$temperature <- str_replace(exifstills$AmbientTemperature, "C", "")
exifstillsclean <- exifstills[,c("FileName", "temperature")]