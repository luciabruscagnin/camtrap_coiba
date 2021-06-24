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

# whether tool-using occurred in the sequence or not
agoutigross$tooluse <- str_detect(agoutigross$behaviour, "TAF")
agoutigross_tools <- agoutigross[agoutigross$tooluse == TRUE,]
tooluse_count <- agoutigross_tools %>% 
  count(sequence_id)
colnames(tooluse_count)[2] <- "n_tooluse"

agoutigross <- left_join(agoutigross, tooluse_count, "sequence_id")
# replace NAs with 0 for the capuchin count
agoutigross$n_tooluse[is.na(agoutigross$n_tooluse)] <- 0

# can still clean up by removing unnecessary columns, e.g.
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

# colors for two histograms in one
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

# looking at tool users vs non tool users
# sequences with capuchins present 
histtool <- hist(onlycap$hour[onlycap$tool_site == 1], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histnotool <- hist(onlycap$hour[onlycap$tool_site == 0], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histtool, col = c1, freq = FALSE, main = "Tool users (blue) vs non-tool users (red)", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins")
plot(histnotool, col = c2, freq = FALSE, add = TRUE)

# wet vs dry season
histwet <- hist(onlycap$hour[onlycap$season == "Wet"], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histdry <- hist(onlycap$hour[onlycap$season == "Dry"], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histdry, col = c2, freq = FALSE, main = "Wet (blue) vs dry (red) season", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins")
plot(histwet, col = c1, freq = FALSE, add = TRUE)

histwet <- hist(onlycap$hour[onlycap$season == "Wet" & onlycap$tooluse == TRUE], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histdry <- hist(onlycap$hour[onlycap$season == "Dry" & onlycap$tooluse == TRUE], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

# is there more tool use in the dry or wet season?
table(onlycap$tooluse, onlycap$season)


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

## when do capuchins use tools (what time of day)
hist(onlycap$hour[onlycap$tooluse == TRUE], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), xlab = "Time of Day", ylab = "Nr of sequences with tool using capuchins")
## compared to all occurrences of tool users
hist(onlycap$hour[onlycap$tool_site == 1], main = "Tool users", breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), xlab = "Time of Day", ylab = "Nr of sequences with capuchins")
## occurrences of tool using group, not tool using
hist(onlycap$hour[onlycap$tool_site == 1 & onlycap$tooluse == FALSE], main = "Tool users, no tool use", breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), xlab = "Time of Day", ylab = "Nr of sequences with capuchins not using tools")

# plot together tool use and non tool use behaviors of tool using groups


#### Late night parties
# can select all sequences with time > sunset (or 8 pm or something) and then try to extract the behaviors? Will have to come from the agouticlean dataset, not agoutisequences
agouticlean$hour <- hour(agouticlean$timestamp_correct)

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


