# CLEANING AGOUTI OUTPUT
require(dplyr)
require(lubridate)
require(stringr)

# open Agouti output file (observations)
agoutigross <- read.csv("agouti_output/coiba-national-park-tool-use-20210518123935/observations.csv", header = TRUE)

# deployment keys
depl_keys <- read.csv("agouti_output/coiba-national-park-tool-use-20210518123935/deployments.csv", header = TRUE)

# filter out test ones/not relevant ones (so create variable to filter test ones)
depl_keys$flag <- ifelse(grepl("Test", depl_keys$tags) | depl_keys$tags == "", 1, 0)

# add deployment length in hours, this is an exposure
depl_keys$dep_start <- str_replace(depl_keys$start, "T", " ")
depl_keys$dep_end<- str_replace(depl_keys$end, "T", " ")
depl_keys$dep_start <-  as.POSIXct(depl_keys$dep_start, tz = "America/Panama", format = "%Y-%m-%d %H:%M:%S")
depl_keys$dep_end <-  as.POSIXct(depl_keys$dep_end, tz = "America/Panama", format = "%Y-%m-%d %H:%M:%S")
depl_keys$dep_length_hours <-as.numeric(difftime(depl_keys$dep_end,depl_keys$dep_start,units="hours"))

# match deployment IDS in keys to observations
agoutigross <- left_join(agoutigross, depl_keys, "deployment_id")

# filter out deployments we're not using
agoutigross <- agoutigross[agoutigross$flag == 0,]

# create column "capuchin present 1/0" and "capuchin count"
agoutigross$capuchin <- ifelse(agoutigross$scientific_name == "Cebus imitator", 1, 0)

agoutigross_cap <- agoutigross[agoutigross$capuchin == 1, ]
cap_numbers <- agoutigross_cap %>% 
  count(sequence_id)
  
agoutigross <- left_join(agoutigross, cap_numbers, "sequence_id")
# replace NAs with 0 for the capuchin count
agoutigross$n[is.na(agoutigross$n)] <- 0

# drop duplicated sequences to get one row per sequence
agoutisequence <- agoutigross[!duplicated(agoutigross$sequence_id),]
length(unique(agoutisequence$sequence_id)) 

# turn sequence start time into POSIXct format same as tide stuff
# replace T with a space
agoutisequence$time <- str_replace(agoutisequence$timestamp, "T", " ")
agoutisequence$time <- as.POSIXct(agoutisequence$time, tz = "America/Panama", format = "%Y-%m-%d %H:%M:%S")

# identify wrong timestamps
# multimedia excel with correct timestamps
multimedia <- read.csv("agouti_output/coiba-national-park-tool-use-20210518123935/multimedia.csv", header = TRUE)

# first those that were just entry mistakes (e.g. 1970)
# create dataset with these sequence ids
wrongseq <- agoutisequence$sequence_id[agoutisequence$timestamp < 2017-01-01 | agoutisequence$timestamp > Sys.Date()]

# extract rows of multimedia excel with the same sequence ids
multimedia2 <- subset(multimedia, multimedia$sequence_id %in% wrongseq)

# use stringr to get the correct timestamp
# overwrite those in the old one?
multimedia2$date <- sapply(str_split(multimedia2$file_name, "__"), '[', 2) 
multimedia2$time <- sapply(str_split(multimedia2$file_name, "__"), '[', 3) 
multimedia2$time <- substr(multimedia2$time, 1, nchar(multimedia2$time)-4)
multimedia2$timestamp_correct <- as.POSIXct(paste(multimedia2$date, multimedia2$time), tz = "America/Panama", format = "%Y-%m-%d %H-%M-%S")

#make new time column
agoutisequence$time[which(agoutisequence$sequence_id %in% multimedia2$sequence_id)] <- multimedia2$timestamp_correct
which(agoutisequence$sequence_id %in% multimedia2$sequence_id)
agoutisequence[which(agoutisequence$sequence_id %in% multimedia2$sequence_id),]
for (i in 1:nrow(multimedia2)) {
  if (agoutisequence$sequence_id == multimedia2$sequence_id[i]) {
    agoutisequence$time <- multimedia2$timestamp_correct
  }
}
# then look for one's that are around midnight (00:00:00) to see if there's a mistake there
str(agoutisequence$time)
# need to fix deployment start variable too (at 1970 now) and date end.... 
agoutisequence$timeonly <- format(agoutisequence$time, format = "%H:%M:%S")
wrongseq2 <- agoutisequence$sequence_id[agoutisequence$timeonly == "00:00:00"]
multimedia3 <- subset(multimedia, multimedia$sequence_id %in% wrongseq2)


agoutisequence[,agoutisequence$timeonly == 00:00:00]
agoutisequence
## TIDAL


# for each sequence get time to nearest low tide (need to match day and get low tide times then)
agoutisequence$tidedif <- NA
which(is.na(agoutisequence$time))

for (i in 1:nrow(agoutisequence)) {
  agoutisequence$tidedif[i] <- min(abs(difftime(agoutisequence$time[i], TidesLow$TIDE_TIME, units = "hours")))
}

# temporarily exclude the wrong dates 
agoutisequence2 <- subset(agoutisequence, agoutisequence$tidedif < 12)
hist(agoutisequence2$tidedif)  
plot(agoutisequence2$tidedif, agoutisequence2$count)

#checking distributions of hours
h.lub <- hour(agoutisequence2$time)
hist(h.lub)

# for each camera trap add get distance from coast
str(agoutisequence2)



### exploring

# loop over camera id make density plot for each camera (potentially only with capuchin present)
