## Investigating group cohesion in grid data of fixed anvil tool-using vs non-tool-using groups 
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

# start with agouticlean which is cleaned, not yet aggregated to sequence level or excluded incomplete deployments
# start with the agoutisequence_c dataframe that's cleaned and aggregated to the sequence level 
# alternatively have agoutiselect2 data that is on level of day-hour with zero's added in

## Packages required
library(stringr)
library(ggplot2)
library(mgcv)
library(gratia)
library(reshape2)
library(asnipe)
library(igraph)
library(activity)

## Notes for analyses:

# Need to find some way to incorporate detection distance in the models, see how it varies between the cameras. 
# --> have this information on Kobo, can add it to the csv with camera information? --> this is incomplete
# will need to find out how to calculate detection distance from camera images

##### DIAGNOSTICS ####

## Subset to only grid cameras
# observation level
gridclean <- agouticlean[which(str_detect(agouticlean$locationName, "TU") == TRUE),]
ftable(gridclean$locationName)
# sequence level
gridsequence <- agoutisequence_c[which(str_detect(agoutisequence_c$locationName, "TU") == TRUE),]
ftable(gridsequence$locationName)

## exclude grid cameras that are blank
# NTU-151 and TU-168 have only blanks, TU-152 was pointed at the ground and therefore had mostly blanks from the start. For now exclude these three
gridclean_c <- gridclean[! gridclean$locationName %in% c("NTU-151", "TU-168", "TU-152"),]
gridclean_c$gridtype <- ifelse(str_detect(gridclean_c$locationName, "NTU") == TRUE, "NTU", "TU")

gridsequence_c <-  gridsequence[! gridsequence$locationName %in% c("NTU-151", "TU-168", "TU-152"),]
gridsequence_c$gridtype <- ifelse(str_detect(gridsequence_c$locationName, "NTU") == TRUE, "NTU", "TU")

## Did we have at least one capuchin detection at all remaining cameras?
ftable(gridsequence_c[which(gridsequence_c$capuchin == 1),]$locationName)

## any weird time issues?
hist(gridsequence_c[which(gridsequence_c$capuchin == 1),]$hour)
# all looks fine

###### Exposure, how many trapping nights TU vs NTU ####
## How many camera trapping days
griddays <- gridsequence_c
griddays$dayloc <- paste(griddays$locationfactor, griddays$seqday, sep = " ")
griddays2 <- griddays[!duplicated(griddays$dayloc),]

# make overview of deployments we have and their start and end days
gridlocations_t <- data.frame(uniqueloctag = unique(gridsequence_c$uniqueloctag)) 
gridlocations_t <- left_join(gridlocations_t, gridsequence_c[,c("uniqueloctag", "dep_start", "dep_end", "locationfactor", "gridtype")], by = "uniqueloctag")
gridlocations_t <- gridlocations_t[!duplicated(gridlocations_t$uniqueloctag),]
# take time off and keep just date variable
gridlocations_t$dep_startday <- as.Date(gridlocations_t$dep_start, tz = "America/Panama", "%Y-%m-%d")
gridlocations_t$dep_endday <- as.Date(gridlocations_t$dep_end, tz = "America/Panama", "%Y-%m-%d")
# calculate days in each deployment (round up)
gridlocations_t$dep_days <- ceiling(difftime(gridlocations_t$dep_end, gridlocations_t$dep_start, units = c("days")))
# number of rows in the griddays2 dataframe (so how many days we have)
for (i in 1:nrow(gridlocations_t)) {
  gridlocations_t$nrow[i] <- nrow(griddays2[griddays2$uniqueloctag == gridlocations_t$uniqueloctag[i],])
}

gridlocations_t2 <- aggregate(gridlocations_t$dep_days, list(locationfactor  = gridlocations_t$locationfactor, gridtype = gridlocations_t$gridtype), FUN = sum)

sum(gridlocations_t$dep_days[gridlocations_t$gridtype == "NTU"])
sum(gridlocations_t$dep_days[gridlocations_t$gridtype == "TU"])
## so comparable number of trapping days, but more in NTU than TU grid

# How many locations
ftable(gridlocations_t2$gridtype) # one more location in NTU grid
# Average number of trapping days per location
summary(as.numeric(gridlocations_t2$x[gridlocations_t2$gridtype == "NTU"]))
summary(as.numeric(gridlocations_t2$x[gridlocations_t2$gridtype == "TU"]))

# slightly longer deployments in NTU grid than TU grid, but no dramatic differences

###### Detection of other animals vs capuchin detections ####
head(gridsequence_c)

# can do per camera, nr of capuchin detections, nr of other animal detections
# then calculate ratio and have overall ratio NTU-TU?

# first try just visually

# capuchin is easy, that is the variable "capuchin"  1 or 0
# make other animal column
nocaps <- gridsequence_c[gridsequence_c$capuchin == 0,]
# options are
# pick/up deployment sequence
# humans (homo sapiens) 
# unknown
# basically if scientific name is not homo sapiens or blank then yes
gridspecies <- subset(gridsequence_c, gridsequence_c$scientificName != " Homo sapiens" & gridsequence_c$scientificName != "")
gridspecies$species <- ifelse(gridspecies$scientificName == "Cebus imitator", "Capuchin", 
                              ifelse(gridspecies$scientificName == "Dasyprocta coibae", "Agouti",
                                     ifelse(gridspecies$scientificName == "Rattus rattus", "Rat",
                                            ifelse(gridspecies$scientificName == "Alouatta palliata", "Howler",
                                                   ifelse(gridspecies$scientificName %in% c("Leptotila battyi", "Leptotila cassini"), "Pigeon",
                                                          ifelse(gridspecies$scientificName %in% c("Iguana iguana", "Holcosus quadrilineatus", "Clelia clelia"), "Reptile", "Other bird"))))))
ggplot(data = gridspecies, aes(x = species, group = gridtype, fill = gridtype)) + geom_histogram(stat = "count", position = "dodge") +  
  stat_count(binwidth = 1, geom = "text", aes(label = after_stat(count), group = gridtype), position = position_dodge(width = 1)) + theme_bw()

# so seems like at NTU grid there were many (many!) more agouti sightings, more pigeons, more rats. In general more animals that were not capuchins. 
# this is not yet offset to the number of observation days (which is slightly higher in NTU)
# so this is literally just the number of sequences with 1 or more animals present of each species (type)

# information overload, per camera
ggplot(data = gridspecies, aes(x = species, group = gridtype, fill = gridtype)) + geom_histogram(stat = "count", position = "dodge") +  
  stat_count(binwidth = 1, geom = "text", aes(label = after_stat(count), group = gridtype), position = position_dodge(width = 1)) + theme_bw() + facet_wrap(~locationName)

###### Detection distance ####

# load in gridadditionalinfo csv file
# see if average detection distance is similar between TU and NTU grid


###### Are we capturing one NTU group? ####
# max group size seen
NTUgridseq <- gridsequence_c[gridsequence_c$gridtype == "NTU",]
NTUgridseq[which(NTUgridseq$n == max(NTUgridseq$n)),]
# in max group size, see 4 adult females, 5 adult males, 5 juveniles (of which one infant), 2 subadult males

# look at supposed group composition (max number of adult males and adult females seen in one sequence and how many we have IDed)
max(NTUgridseq$nAF) # max of 4 adult females (have identified 5)
max(NTUgridseq$nAM) # max of 5 adult males (have identified 5, potentially 6)
max(NTUgridseq$nJU) # max of 6 juveniles
max(NTUgridseq$nSM) # max of 2 subadult males ( have identified 2, maybe 3)

# co-occurrence of identifiable individuals (SNA network)
# look at who occurs together in the same sequence. get nodes and see who has not been seen with anyone. 
# step 1: working with gridclean dataframe, need to turn the IDstrings into the ID key codes we use for clarity.
# need to get the file that links strings back to names. Asked Agouti for it
# until then just work with the ID strings as they are

# step 2: per sequence, get some kind of dyadic information of who was seen with whom
# make dataframe with individual variation
gridagesex <- gridclean_c[,c("individualID","lifeStage", "sex", "gridtype")]
gridagesex <- gridagesex[! gridagesex$individualID == "" & ! duplicated(gridagesex$individualID),]
NTUgridagesex <- gridagesex[gridagesex$gridtype == "NTU",]
# for now just add real names in manually, later use key file
NTUgridagesex <- NTUgridagesex[order(NTUgridagesex$individualID),]
NTUgridagesex$ID <- c("QUA", "XAV", "LEX", "PIP", "JUN", "CON", "DRO", "FRA", "ELA", "HAN", "OCT", "MIR", "HEL", "BLO", "KAI")

# I think data format needs to be sequenceID/individualID
# go to only NTU grid data and only sequence ID and individual ID (when individual ID was known)
NTUassoc <- gridclean_c[gridclean_c$gridtype == "NTU" & ! gridclean_c$individualID == "", c("sequenceID", "individualID")]
NTUassoc <- left_join(NTUassoc, NTUgridagesex[,c("individualID", "ID")])
NTUassoc <- NTUassoc[,c("sequenceID", "ID")]

# then go from long to wide?
NTUassoc_w <- dcast(NTUassoc, sequenceID ~ ID)
NTUassoc_w2 <- NTUassoc_w
NTUassoc_w2[is.na(NTUassoc_w2) == FALSE] <- 1
NTUassoc_w2$sequenceID <- NTUassoc_w$sequenceID
NTUassoc_w2[is.na(NTUassoc_w2)] <- 0
NTUassoc_w2[,2:16] <- as.numeric(unlist(NTUassoc_w2[,2:16]))
rownames(NTUassoc_w2) <- NTUassoc_w2$sequenceID
NTUassoc_w2 <- NTUassoc_w2[,-1]
## now we have a dataframe with all associations (whenever individuals were seen together in the same sequence) in GBI (group by individual) format
# use this with asnipe package to get a network 
adj.m <- get_network(NTUassoc_w2, association_index = "SRI")
assoc.g <- graph_from_adjacency_matrix(adj.m, "undirected", weighted = T)
plot(assoc.g, edge.width = E(assoc.g)$weight*100)

# largely appears to be one group, but I will still do a double-check of the IDs and try to identify more individuals in the big group sightings

# filter down to only capuchin detections
gridseq_oc <- gridsequence_c[gridsequence_c$capuchin == 1,]

###### Trap shyness ####
# inspired by McCarthy et al 2018
# can look at 1: if party size increases over time/if individuals are more likely to be spotted over time (only first is feasible with our data I think)
# 2: are you more likely to spot capuchins at a camera trap the longer it has been out? So this is then a model where per day you have 1 capuchin 0 no capuchin and time of days since cam deployment




##### DAILY ACTIVITY PATTERN #####
# first just visually, what time of day do we see activity of capuchins? 
# colors for two histograms in one
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

### Tool users vs non tool users
histTU <- hist(gridseq_oc$hour[gridseq_oc$gridtype == "TU"], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histNTU <- hist(gridseq_oc$hour[gridseq_oc$gridtype == "NTU"], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histNTU, col = c2, freq = FALSE, main = "Tool users (blue) vs non-tool users (red)", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins", ylim = c(0, 0.12))
plot(histTU, col = c1, freq = FALSE, add = TRUE)

# in general TU group appears to be active more later in the afternoon
# all early morning and late evening activity is from the TU group
# look at this closer
nightowls <- gridseq_oc[gridseq_oc$hour < 5 | gridseq_oc$hour > 19,]
# all of these cameras seem to have the correct time set. So this means we truly have a capuchin detection at midnight and one at 4 AM!

## still make proper daily activity model of this, look at Lester's paper for example code etc
# following this  vignette https://bookdown.org/c_w_beirne/wildCo-Data-Analysis/activity.html

## I think the right data format is what agouti gives you not aggregated to a sequence
# so each sequence gets repeated for each individual sighting
# use data with only capuchins in it
head(gridseq_oc)
# relevant columns are the gridtype (TU or NTU)
# seq_start time, which is when the observation occurred

# have various ways of representing time
# the best and most relevant to me, seems to be solar time, which uses coordinates of observations to determine sunrise and sunset
# then the activity is classified as being during the day or during the night
gridseq_oc$timestamp <- ymd_hms(gridseq_oc$seq_start, tz = "America/Panama")

tmp <- solartime(gridseq_oc$timestamp,
                 gridseq_oc$latitude,
                 gridseq_oc$longitude,
                 tz = -5,
                 format = "%Y-%m-%d %H:%M:%S")

gridseq_oc$solar <- tmp$solar
gridseq_oc$clock <- tmp$clock

plot(gridseq_oc$solar, gridseq_oc$clock)

# compare TU to NTU
# TU
act_m1 <- fitact(gridseq_oc$solar[gridseq_oc$gridtype == "TU"], sample = "model", reps = 100) # need to use 1000 reps
#saveRDS(act_m1, "gridanalyses/RDS/act_m1.RDS")
plot(act_m1)
act_m1@act[1] * 24
# this means they spend 0.38 * 24 = 9 hours per day active

# NTU
act_m2 <- fitact(gridseq_oc$solar[gridseq_oc$gridtype == "NTU"], sample = "model", reps = 100) # need to use at least 1000 reps
#saveRDS(act_m2, "gridanalyses/RDS/act_m2.RDS")
plot(act_m2)
act_m2@act[1] * 24
# this means they spend 0.38 * 24 = 10 hours per day active


# plot both together on same axis
plot(act_m1, yunit="density", data="none", las=1, lwd=2,
     tline=list(lwd=2), # Thick line 
     cline=list(lty=0)) # Supress confidence intervals

plot(act_m2, yunit="density", data="none", add=TRUE, 
     tline=list(col="red", lwd=2),
     cline=list(lty=0))

legend("topright", c("TU", "NTU"), col=1:2, lty=1, lwd=2)

# overlap between the two
compareCkern(act_m1, act_m2, reps = 100)
# 0.897366727 lot of overlap

# just out of curiosity, if we do the same on all the Jicaron TU data (not the grid)
jtonly <- agouticlean[agouticlean$island == "Jicaron" & agouticlean$tool_site == 1 & str_detect(agouticlean$locationName, "TU") == FALSE & agouticlean$capuchin == 1,]
jtonly$timestamp <- ymd_hms(jtonly$seq_start, tz = "America/Panama")

tmp2 <- solartime(jtonly$timestamp,
                 jtonly$latitude,
                 jtonly$longitude,
                 tz = -5,
                 format = "%Y-%m-%d %H:%M:%S")

jtonly$solar <- tmp2$solar
jtonly$clock <- tmp2$clock

# TU based on all data
# Note: this is biased because this contains both individuals passing through and using tools, esp the latter really retriggers the camera
# I was just curious, cant use it in this form. 
act_m3 <- fitact(jtonly$solar, sample = "model", reps = 100) # need to use 1000 reps
#saveRDS(act_m3, "gridanalyses/RDS/act_m3.RDS")
plot(act_m3)
act_m3@act[1] * 24

# plot TU grid and non-grid together on same axis
plot(act_m3, yunit="density", data="none", las=1, lwd=2,
     tline=list(lwd=2), # Thick line 
     cline=list(lty=0)) # Supress confidence intervals

plot(act_m1, yunit="density", data="none", add=TRUE, 
     tline=list(col="red", lwd=2),
     cline=list(lty=0))

legend("topleft", c("Non grid", "Grid"), col=1:2, lty=1, lwd=2)

### STILL DO:
# make activity for anvil cameras vs random vs streambed vs grid. 
# plot them all together! 
# that will be interesting. 



##### GROUP SIZE ####

###### 1: Does the number of capuchins per sequence differ between TU and NTU grid? ####
# account for camera and detection distance (if we have it)
mean(gridseq_oc$n[gridseq_oc$gridtype == "NTU"])
mean(gridseq_oc$n[gridseq_oc$gridtype == "TU"])
## need to model this obvs, some kind of poisson?

## GAM I want to do below that looks at time of day/number of capuchins also does comparison of mean group size between TU and NTU! so maybe can be one model?

###### 2. Does the number of capuchins per sequence fluctuate depending on the hour of day, and does this relationship differ between TU and NTU ####
# use a gam
## hour of day is not cyclic spline, as we have no observations at midnight and early in morning (explained in bottom of the heap youtube)
gridseq_oc$gridtype <- as.factor(gridseq_oc$gridtype)
grid_gam1 <- gam(n ~ s(hour, by = gridtype) + gridtype, data = gridseq_oc, method = "REML", family = poisson())

summary(grid_gam1)
draw(grid_gam1)
# This simplest model seems to suggest for NTU there is no real nonlinear relationship (slight trend).
# if anything, is lower party size in morning and then constant throughout day until evening
# for TU, see higher party size in mornings and evenings, lower throughout the day

gam.check(grid_gam1)
# seems to still be a lot of unexplained variation we are not capturing
# probably we'd need to deal with the heavy 1 inflation to understand this model well
# it's now all being flattened by the many 1s sprinkled everywhere
# maybe we should include the 0s too? 

# including locationfactor as a random effect
grid_gam2 <- gam(n ~ s(hour, by = gridtype) + gridtype +  s(locationfactor, bs = "re"), data = gridseq_oc, method = "REML", family = poisson())

summary(grid_gam2)
draw(grid_gam2)
gam.check(grid_gam2)

## Need to think further on how to model this. Is poisson appropriate without 0s? should be 1-inflated. Once I'm satisfied with it could take it to brms

##### GROUP COMPOSITION ####


##### CO-OCCURRENCES ####

