# CLEANING AGOUTI OUTPUT
require(dplyr)
require(lubridate)

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

# drop duplicated sequences to get one row per sequence
agoutisequence <- agoutigross[!duplicated(agoutigross$sequence_id),]
length(unique(agoutisequence$sequence_id)) 

# turn sequence start time into POSIXct format same as tide stuff
# replace T with a space
agoutisequence$time <- str_replace(agoutisequence$timestamp, "T", " ")
agoutisequence$time <- as.POSIXct(agoutisequence$time, tz = "America/Panama", format = "%Y-%m-%d %H:%M:%S")

agoutisequence$tidedif <- NA
which(is.na(agoutisequence$time))

# for each sequence get time to nearest low tide (need to match day and get low tide times then)

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
