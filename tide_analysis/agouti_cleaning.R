# CLEANING AGOUTI OUTPUT
require(dplyr)
require(lubridate)
require(stringr)
require(ggplot2)
require(DescTools)

# check if working directory is set okay
setwd("~/Git/camtrap_coiba")

#### GENERAL CLEANING ####
# open Agouti output file (observations) that you have downloaded from the agouti website. Use most recent version
agoutigross <- read.csv("agouti_output/coiba-national-park-tool-use-20211013101430/observations.csv", header = TRUE)

# open the associated deployment keys (also downloaded from agouti.eu)
depl_keys <- read.csv("agouti_output/coiba-national-park-tool-use-20211013101430/deployments.csv", header = TRUE)

# filter out test deployments/not relevant ones (so create variable to filter test ones)
## THIS WILL NEED TO BE MORE FINETUNED LATER. THERE ARE SOME TRIAL/WRONG DATA ON THERE THAT MAY NOT BE CAPTURED NOW. 
depl_keys$flag <- ifelse(grepl("Test", depl_keys$tags) | depl_keys$tags == "", 1, 0 )

# match deployment IDS in keys to observations
agoutigross <- left_join(agoutigross, depl_keys, "deployment_id")

# filter out deployments we're not using
agoutigross <- agoutigross[agoutigross$flag == 0,]

# create column per sequence if a capuchin was present 1/0 and if capuchins, how many in the sequence (a count)
agoutigross$capuchin <- ifelse(agoutigross$scientific_name == "Cebus imitator", 1, 0)

agoutigross_cap <- agoutigross[agoutigross$capuchin == 1, ]
cap_numbers <- agoutigross_cap %>% 
  count(sequence_id)
  
agoutigross <- left_join(agoutigross, cap_numbers, "sequence_id")
# replace NAs with 0 for the capuchin count
agoutigross$n[is.na(agoutigross$n)] <- 0

# turn sequence start time into POSIXct format
# replace T with a space
agoutigross$time <- str_replace(agoutigross$timestamp, "T", " ")
agoutigross$time <- as.POSIXct(agoutigross$time, tz = "America/Panama", format = "%Y-%m-%d %H:%M:%S")

# identify and correct wrong timestamps
# open the multimedia csv containing the correct timestamps (also from agouti)
multimedia <- read.csv("agouti_output/coiba-national-park-tool-use-20211013101430/multimedia.csv", header = TRUE)

# have both timestamps we entered incorrectly (e.g. 1970) and those that shifted 5 hours by accident
multimedia$time <- str_replace(multimedia$timestamp, "T", " ")
multimedia$time <- as.POSIXct(multimedia$time, tz = "America/Panama", format = "%Y-%m-%d %H:%M:%S")

# obtain correct timestamp from file name
multimedia$time_file <- paste(sapply(str_split(multimedia$file_name, "__"), "[", 2), sapply(str_split(multimedia$file_name, "__"), "[", 3), " ")
# mp4 and jpg strings are extracted differently
multimedia$time_file[which(str_detect(multimedia$time_file, "mp4"))] <- substr(multimedia$time_file[which(str_detect(multimedia$time_file, "mp4"))], 1,
                                                                               nchar(multimedia$time_file[which(str_detect(multimedia$time_file, "mp4"))])-6)

multimedia$time_file <- sapply(str_split(multimedia$time_file, "\\("), '[', 1)
multimedia$time_file <- as.POSIXct(multimedia$time_file, tz = "America/Panama", format = "%Y-%m-%d %H-%M-%S")

# now flag those where the timestamp does not match the file time
multimedia$timeflag <- ifelse(multimedia$time == multimedia$time_file, 0, 1)
ftable(multimedia$deployment_id, multimedia$timeflag)

# the file time is always correct, so can just use that one for the one's that are flagged
# covers both the entry mistakes and the ones that drifted 5 hours
# create timestamp_correct 
multimedia$timestamp_correct <- multimedia$timestamp
multimedia$timestamp_correct[which(multimedia$timeflag == 1)] <- as.character(multimedia$time_file[which(multimedia$timeflag == 1)])
multimedia$timestamp_correct <- multimedia$timestamp_correct %>%
  as.character(.) %>%
  str_replace(., "T", " ") %>%
  as.POSIXct(., tz = "America/Panama", format = "%Y-%m-%d %H:%M:%S")

# create sequence_start, sequence_end and duration column (so this is per sequence)
startseq <- aggregate(x = list(seq_start = multimedia$timestamp_correct), by = list(sequence_id = multimedia$sequence_id), FUN = min)
endseq <- aggregate(x = list(seq_end = multimedia$timestamp_correct), by = list(sequence_id = multimedia$sequence_id), FUN = max)

multimedia <- left_join(multimedia, startseq, "sequence_id")
multimedia <- left_join(multimedia, endseq, "sequence_id")
multimedia$seq_lengthstill <-as.numeric(difftime(multimedia$seq_end,multimedia$seq_start,units="secs"))

#### Add temperature and video sequence length
# first run script "exiftempseq_cleaning.R" in the exiftool_metadata folder
## clean filename in multimedia agouti (take off the upload time, which is always 15 characters)
multimedia$FileName <- substring(multimedia$file_name, 16)

multimedia <- left_join(multimedia, exifstillsclean, "FileName")
multimedia <- left_join(multimedia, exifvideosclean, "FileName")

multimedia$temperature <- ifelse(is.na(multimedia$temperature.x), multimedia$temperature.y, multimedia$temperature.x)
multimedia$seq_length <- ifelse((multimedia$file_mediatype == "video/mp4"), multimedia$seq_lengthv, multimedia$seq_lengthstill) 

# left join does not work if the y dataframe (so multimedia2) has duplicated values in the column you're matching by (so sequence_id). Need to drop duplicates before the left_join
multimedia2 <- multimedia[,c("sequence_id", "seq_start", "seq_end", "seq_length", "temperature")]
multimedia2 <- multimedia2[!duplicated(multimedia2$sequence_id),]
agoutigross <- left_join(agoutigross, multimedia2, "sequence_id")

# add correct deployment start and end time (can still double check, now took the minimum and maximum sequence time within deployment id)
startdep <- aggregate(x = list(dep_start = agoutigross$seq_start), by = list(deployment_id = agoutigross$deployment_id), FUN = min)
enddep <- aggregate(x = list(dep_end = agoutigross$seq_start), by = list(deployment_id = agoutigross$deployment_id), FUN = max)

agoutigross <- left_join(agoutigross, startdep, "deployment_id")
agoutigross <- left_join(agoutigross, enddep, "deployment_id")
# add deployment length in hours, this is an exposure
agoutigross$dep_length_hours <-as.numeric(difftime(agoutigross$dep_end,agoutigross$dep_start,units="hours"))

#### ADDING EXTRA VARIABLES E.G. TIME OF YEAR/TOOL USE INFO ####
# make month column
agoutigross$month <- month(agoutigross$seq_start)

# make wet season may-nov and dry season dec-april
agoutigross$season <- ifelse(agoutigross$month == 12 | agoutigross$month == 1 | agoutigross$month == 2 | agoutigross$month == 3 | 
                         agoutigross$month == 4, "Dry", "Wet") 

# pull island location and tool use/non tool use from coiba_camtrap_ids_gps.csv
deployment_info <- read.csv("coiba_camtrap_ids_gps.csv")
deployment_info$location_name <- deployment_info$camera_id

# drop columns we don't want to attach
deployment_info2 <- deployment_info[, !names(deployment_info) %in% c("camera_id", "number", "longitude", "latitude")]

agoutigross <- left_join(agoutigross, deployment_info2, "location_name")

# whether tool-using occurred in the sequence or not
agoutigross$tooluse <- str_detect(agoutigross$behaviour, "TAF") # now just takes all types of tool use, also unknown
agoutigross_tools <- agoutigross[agoutigross$tooluse == TRUE,]
tooluse_count <- agoutigross_tools %>% 
  count(sequence_id)
colnames(tooluse_count)[2] <- "n_tooluse"

agoutigross <- left_join(agoutigross, tooluse_count, "sequence_id")
# replace NAs with 0 for the tool use
agoutigross$n_tooluse[is.na(agoutigross$n_tooluse)] <- 0

# can still clean up by removing unnecessary columns
# keep checking if these are the right ones to remove
agouticlean <- agoutigross[, !names(agoutigross) %in% c("timestamp","multimedia_id", "start", "end", "camera_id", "camera_model", "bait_use", "feature_type",
                                             "comments.y", "time")]

## FORMAT: ONE ROW PER SEQUENCE
# drop duplicated sequences, necessary form for tidal analyses and data exploration
# lose individual info etc so to analyze like that need to use agoutigross
agoutisequence <- agouticlean[!duplicated(agouticlean$sequence_id),]
agoutisequence$tooluse <- agoutisequence$n_tooluse > 0
length(unique(agoutisequence$sequence_id)) 

### EXPLORING DATA ####
# below is just me attempting many things

### NOTES
# could make on sequence level variable that says how many males, how many females, how many adults, how many juveniles, how many unclassified (?)
# can look at individuals that are IDed where they show up the most 


#### ACTIVITY PER HOUR (many histograms)

hist(hour(agoutisequence$seq_start[agoutisequence$capuchin == 1]), xlab = "Time of Day", ylab = "Number of Sequences", main = "Capuchin Detections Across All Locations")
hist(hour(agoutisequence$seq_start[agoutisequence$scientific_name == "Homo sapiens"]), xlab = "Time of Day", ylab = "Number of Sequences", main = "Human Detections")
ftable(agoutisequence$location_name[agoutisequence$scientific_name == "Homo sapiens"])
## Per location

# loop over camera ID, density plot for each camera when capuchins are present
# use location name (where the camera is, can have separate deployments on that location)
# number of sequences per location name
aggregate(agoutisequence$sequence_id, by = list(location_name = agoutisequence$location_name), FUN = length)

# filter out when capuchins weren't present
onlycap <- subset(agoutisequence, agoutisequence$capuchin == 1)
onlycap$hour <- hour(onlycap$seq_start)

locations <- unique(onlycap$location_name)
table(agoutisequence$location_name, agoutisequence$capuchin ) # nr of sequences per location with and without capuchins

# to get pdf of output
# pdf("tide_analysis/camera_traps_density.pdf", width = 9, height = 11)
# par(mfrow=c(4,3)) #sets number of rows and columns per page, could also change margins
# par(cex = 0.5)

for (l in 1:length(locations)) {
  hist(onlycap$hour[onlycap$location_name == locations[l]], main = locations[l], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), xlab = "Time of Day", ylab = "Nr of sequences with capuchins")
}

# dev.off()

# add dry vs wet season comparison
season <- c("Dry", "Wet")

# pdf("tide_analysis/camera_traps_density_season.pdf", width = 9, height = 11)
# par(mfrow=c(4,2)) #sets number of rows and columns per page, could also change margins
# par(cex = 0.5)

# sort order you want (by island, tool use non tool use, location). Can plot over that

for (l in 1:length(locations)) {
  for (s in 1:length(season)) {
  hist(onlycap$hour[onlycap$location_name == locations[l] & onlycap$season == season[s]], main = paste(locations[l], season[s]), breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), xlab = "Time of Day", ylab = "Nr of sequences with capuchins")
  }
} 

# dev.off()

table(onlycap$location_name, onlycap$season) # see how many observations of capuchins per season
# also still need to look into how many deployment days per season 

## Mean number of capuchins per sequence

# to get PDF, the par doesn't work for ggplot
# can't really find how to do this in base r with barplot() 

# pdf("tide_analysis/camera_traps_counts.pdf", width = 9, height = 11)
# par(mfrow=c(4,3)) #sets number of rows and columns per page, could also change margins
# par(cex = 0.5)

for (l in 1:length(locations)) {
  dora_l <- subset(onlycap, onlycap$location_name == locations[l])
  print(ggplot(dora_l) + geom_bar(aes(x = hour, y = n), stat = "summary", fun = "mean") + xlab("Time of Day") + ylab("Average Number of Capuchins per Sequence") + xlim(0, 24) + labs(title = locations[l]))
}

# dev.off()

### Histogram time

# colors for two histograms in one
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

## NEED TO MAKE TOOL_SITE COLUMN. sO WHICH PACES ARE TOOL SITES. 

### Tool users vs non tool users
histtool <- hist(onlycap$hour[onlycap$tool_site == 1], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histnotool <- hist(onlycap$hour[onlycap$tool_site == 0], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histnotool, col = c2, freq = FALSE, main = "Tool users (blue) vs non-tool users (red)", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins")
plot(histtool, col = c1, freq = FALSE, add = TRUE)

### considering presence of tool use
ftable(onlycap$tooluse)
# mostly see tool-using of one individual in a sequence, not of more
ftable(onlycap$n_tooluse)

# more individuals, more tool using?
plot(onlycap$n_tooluse, onlycap$n)

# how many locations in the tool using range show tool use and how much (of the sequences with capuchins in them)?
ftable(onlycap$location_name[onlycap$tool_site == 1], onlycap$tooluse[onlycap$tool_site == 1])
ftable(onlycap$tool_site)

ftable(onlycap$island)

### Wet vs Dry
# occurrence of capuchins in general
histwet <- hist(onlycap$hour[onlycap$season == "Wet"], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histdry <- hist(onlycap$hour[onlycap$season == "Dry"], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histwet, col = c1, freq = FALSE, main = "Wet (blue) vs dry (red) season", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins")
plot(histdry, col = c2, freq = FALSE, add = TRUE)

# occurrence of tool use
histwett <- hist(onlycap$hour[onlycap$season == "Wet" & onlycap$tooluse == TRUE], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histdryt <- hist(onlycap$hour[onlycap$season == "Dry" & onlycap$tooluse == TRUE], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histwett, col = c1, freq = FALSE, main = "Wet (blue) vs dry (red) season", xlab = "Time of Day", ylab = "Proportion of sequences with tool use")
plot(histdryt, col = c2, freq = FALSE, add = TRUE)

# compare tool users within each season
# wet
histwettool <- hist(onlycap$hour[onlycap$season == "Wet" & onlycap$tool_site == TRUE], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histwetnotool <- hist(onlycap$hour[onlycap$season == "Wet" & onlycap$tool_site == FALSE], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histwetnotool, col = c2, freq = FALSE, main = "Tool using (blue) vs non-tool using (red) capuchins: Wet Season", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins")
plot(histwettool, col = c1, freq = FALSE, add = TRUE)

# dry
histdrytool <- hist(onlycap$hour[onlycap$season == "Dry" & onlycap$tool_site == TRUE], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24),  freq = FALSE)
histdrynotool <- hist(onlycap$hour[onlycap$season == "Dry" & onlycap$tool_site == FALSE], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histdrynotool, col = c2, freq = FALSE, main = "Tool using (blue) vs non-tool using (red) capuchins: Dry Season", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins")
plot(histdrytool, col = c1, freq = FALSE, add = TRUE)

# is there more tool use in the dry or wet season?
table(onlycap$tooluse, onlycap$season)

#### Late night parties
# can select all sequences with time > sunset (or 8 pm or something) and then try to extract the behaviors? Will have to come from the agouticlean dataset, not agoutisequences
agouticlean$hour <- hour(agouticlean$seq_start)

latenight <- subset(agouticlean, agouticlean$hour > 20 & agouticlean$capuchin == 1)
# drop down to sequences level
latenightsequence <- latenight[!duplicated(latenight$sequence_id),]

ftable(latenightsequence$hour)
# are they using tools?
ftable(latenightsequence$tooluse)
# is this the tool using pop or the non toolusers
ftable(latenightsequence$tool_site)
# which sites?
ftable(latenightsequence$location_name)

# how many capuchins are spotted?
ftable(latenightsequence$n)

sum(str_detect(latenight$behaviour, "Infant"))

## TIDAL

# for each sequence get time to nearest low tide (need to match day and get low tide times then)
# two options, either get absolute difference (so always positive)
# or before/after difference
# Absolute
agoutisequence$tidedifabs <- NA
which(is.na(agoutisequence$time))

for (i in 1:nrow(agoutisequence)) {
  agoutisequence$tidedifabs[i] <- min(abs(difftime(agoutisequence$seq_start[i], TidesLow$TIDE_TIME, units = "hours")))
}

hist(agoutisequence$tidedifabs)  
plot(agoutisequence$tidedifabs, agoutisequence$count)

# Both positive and negative

agoutisequence$tidedif <- NA

for (i in 1:nrow(agoutisequence)) {
  agoutisequence$tidedif[i] <- Closest((as.vector(difftime(agoutisequence$seq_start[i], TidesLow$TIDE_TIME,   units = "hours"))), 0)
}

# get an error but it does seem to work... 
hist(agoutisequence$tidedif)
plot(agoutisequence$tidedif, agoutisequence$count)

# only sequences with capuchins
hist(agoutisequence$tidedif[agoutisequence$capuchin == 1])

# per location 
# pdf("tide_analysis/tidediff_capuchin.pdf", width = 9, height = 11)
# par(mfrow=c(4,3)) #sets number of rows and columns per page, could also change margins
# par(cex = 0.5)

for (l in 1:length(locations)) {
hist(agoutisequence$tidedif[agoutisequence$capuchin == 1 & agoutisequence$location_name == locations[l]], xlab = "Hours from Low Tide", ylab = "Number of Sequences with Capuchins", main = locations[l])
}

# dev.off()

# looking at tool using specifically
hist(agoutisequence$tidedif[agoutisequence$capuchin == 1 & agoutisequence$tooluse == TRUE])

# below doesn't work because some locations don't have tool use and it tries to do those. FIX THIS. 
locations_toolsites <- deployment_info$location_name[deployment_info$tool_site == 1]

for (l in 1:length(locations_toolsites)) {
  hist(agoutisequence$tidedif[agoutisequence$capuchin == 1 && agoutisequence$location_name == locations_toolsites[l]], xlab = "Hours from Low Tide", ylab = "Number of Sequences with Capuchins", main = locations[l])
}

# for each camera trap add get distance from coast
