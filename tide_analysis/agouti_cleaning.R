# CLEANING AGOUTI OUTPUT
require(dplyr)
require(lubridate)
require(stringr)
require(ggplot2)

# open Agouti output file (observations)
agoutigross <- read.csv("agouti_output/coiba-national-park-tool-use-20210518123935/observations.csv", header = TRUE)

 # deployment keys
depl_keys <- read.csv("agouti_output/coiba-national-park-tool-use-20210518123935/deployments.csv", header = TRUE)

# filter out test ones/not relevant ones (so create variable to filter test ones)
depl_keys$flag <- ifelse(grepl("Test", depl_keys$tags) | depl_keys$tags == "", 1, 0)

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

# turn sequence start time into POSIXct format same as tide stuff
# replace T with a space
agoutigross$time <- str_replace(agoutigross$timestamp, "T", " ")
agoutigross$time <- as.POSIXct(agoutigross$time, tz = "America/Panama", format = "%Y-%m-%d %H:%M:%S")

# identify wrong timestamps
# multimedia excel with correct timestamps
multimedia <- read.csv("agouti_output/coiba-national-park-tool-use-20210518123935/multimedia.csv", header = TRUE)

# first those that were just entry mistakes (e.g. 1970)
# create dataset with these sequence ids
wrongseq <- agoutigross$sequence_id[agoutigross$timestamp < 2017-01-01 | agoutigross$timestamp > Sys.Date()]

# extract rows of multimedia excel with the same sequence ids
multimedia2 <- subset(multimedia, multimedia$sequence_id %in% wrongseq)

# use stringr to get the correct timestamp
# overwrite those in the old one?
multimedia2$date <- sapply(str_split(multimedia2$file_name, "__"), '[', 2) 
multimedia2$time <- sapply(str_split(multimedia2$file_name, "__"), '[', 3) 
multimedia2$time <- substr(multimedia2$time, 1, nchar(multimedia2$time)-4)
multimedia2$timestamp_correct <- as.POSIXct(paste(multimedia2$date, multimedia2$time), tz = "America/Panama", format = "%Y-%m-%d %H-%M-%S")
multimedia2 <- multimedia2[,c("sequence_id", "timestamp_correct")]

#make new time column
agoutigross <- left_join(agoutigross, multimedia2, "sequence_id")

agoutigross <- agoutigross %>%
  mutate(timestamp_correct = coalesce(timestamp_correct, time))

# add correct deployment start and end time (can still double check, now took the minimum and maximum sequence time within deployment id)
startdep <- aggregate(x = list(dep_start = agoutigross$timestamp_correct), by = list(deployment_id = agoutigross$deployment_id), FUN = min)
enddep <- aggregate(x = list(dep_end = agoutigross$timestamp_correct), by = list(deployment_id = agoutigross$deployment_id), FUN = max)

agoutigross <- left_join(agoutigross, startdep, "deployment_id")
agoutigross <- left_join(agoutigross, enddep, "deployment_id")
# add deployment length in hours, this is an exposure
agoutigross$dep_length_hours <-as.numeric(difftime(agoutigross$dep_end,agoutigross$dep_start,units="hours"))

#### TIME OF YEAR/WET OR DRY SEASON ####
# make month column
agoutigross$month <- month(agoutigross$timestamp_correct)

# make wet season may-nov and dry deason dec-april
agoutigross$season <- ifelse(agoutigross$month == 12 | agoutigross$month == 1 | agoutigross$month == 2 | agoutigross$month == 3 | 
                         agoutigross$month == 4, "Dry", "Wet") 

# pull island location and tool use/non tool use from coiba_camtrap_ids_gps.csv
deployment_info <- read.csv("coiba_camtrap_ids_gps.csv")
deployment_info$location_name <- deployment_info$camera_id

# drop columns we don't want to attach
deployment_info2 <- deployment_info[, !names(deployment_info) %in% c("camera_id", "number", "longitude", "latitude")]
str(deployment_info2)

agoutigross <- left_join(agoutigross, deployment_info2, "location_name")

# can still clean up by removing unnecessary columns, e.g.
# keep checking if these are the right ones to remove
agouticlean <- agoutigross[, !names(agoutigross) %in% c("timestamp","multimedia_id", "start", "end", "camera_id", "camera_model", "bait_use", "feature_type",
                                             "comments.y", "time")]

## FORMAT: ONE ROW PER SEQUENCE
# drop duplicated sequences, necessary form for tidal analyses and data exploration
# lose individual info etc so to analyze like that need to use agoutigross
agoutisequence <- agouticlean[!duplicated(agouticlean$sequence_id),]
length(unique(agoutisequence$sequence_id)) 

### EXPLORING DATA ####

h.lub <- hour(agoutisequence$time[agoutisequence$capuchin == 1])
hist(h.lub)
head(agoutisequence)

# loop over camera id make density plot for each camera when capuchins are present
# use location name (where the camera is, can have separate deployments on that location)
# number of sequences per location name
length(unique(agoutisequence$sequence_id))

aggregate(agoutisequence$sequence_id, by = list(location_name = agoutisequence$location_name), FUN = length)

# filter out when capuchins weren't present
onlycap <- subset(agoutisequence, agoutisequence$capuchin == 1)
onlycap$hour <- hour(onlycap$timestamp_correct)

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

#pdf("tide_analysis/camera_traps_density_season.pdf", width = 9, height = 11)
#par(mfrow=c(4,2)) #sets number of rows and columns per page, could also change margins
#par(cex = 0.5)

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

# looking at tool users vs non tool users
hist(onlycap$hour[onlycap$tool_site == 1], main = "tool users", breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), xlab = "Time of Day", ylab = "Nr of sequences with capuchins")
hist(onlycap$hour[onlycap$tool_site == 0], main = "non-tool users", breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), xlab = "Time of Day", ylab = "Nr of sequences with capuchins")


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
