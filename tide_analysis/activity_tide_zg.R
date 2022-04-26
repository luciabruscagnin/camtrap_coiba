## Tool use & tidal cycles + Daily Activity analysis
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

require(tidyr)
require(brms)
require(mgcv)
require(ggplot2)
require(gratia)
require(fitdistrplus)
require(itsadug)

# start with the agoutisequence dataframe that's cleaned and aggregated to the sequence level
##### ACTIVITY DURING DAY #####
# start using the cleaned agoutisequence
str(agoutisequence)

# need to filter out deployments that are not fully coded
# have a lot of unclassified sequences in --> these seem to largely be the 00:00:00 automated timecapture moments, that weren't coded
# so have two variables for this, whether a sequence is a timelapse sequence (1 yes, 0 no) and whether it is uncoded (1 yes, 0 no)
# want to exclude deployments that have uncoded sequences
# can either very strictly subset on only 100% coded deployments or less strictly on all that have less than 5 uncoded sequences or something
cd <- as.data.frame(ftable(agoutisequence$uniqueloctag, agoutisequence$uncoded))
codeddeployments_total <- as.character(cd$Var1[cd$Var2 == 1 & cd$Freq < 5])

# subset only fully coded deployments 
agoutisequence_c <- agoutisequence[(agoutisequence$uniqueloctag %in% codeddeployments_total),]
agoutisequence_c$hour <- hour(agoutisequence_c$seq_start)
agoutisequence_c$toolusers <- as.factor(agoutisequence_c$tool_site)
agoutisequence_c$locationfactor <- as.factor(agoutisequence_c$locationName)

unique(agoutisequence_c$locationfactor)

### select #####

# DON'T RUN THE FOLLOWING LINE IF YOU ADDED IN THE HOURS THAT CAMERAS WERENT TRIGGERED
agoutiselect2 <- agoutisequence_c

# create flag for deployment pick up and setup days
# most of the pickup/setup days were at the start/end of the deployment. But sometimes people came to mess with cameras in middle of deployment. Should exclude those too
picksetupdays <- unique(agoutiselect2$seqday[which(agoutiselect2$seqday == date(agoutiselect2$dep_start) | agoutiselect2$seqday == date(agoutiselect2$dep_end) | agoutiselect2$cameraSetup ==  "True")] )
agoutiselect2$picksetup <- ifelse(agoutiselect2$seqday %in% picksetupdays , 1, 0)
agoutiselect2$dataorigin <- "agoutidata"
agoutiselect2 <- agoutiselect2[c("deploymentID", "sequenceID", "scientificName", "locationName", "longitude", "latitude", "cameraSetup", "seq_start", 
                                                               "seq_end", "seq_length", "temperature", "dep_start", "dep_end", "dep_length_hours", "month", 
                                                               "season", "island", "tool_anvil", "tool_site", "streambed", "capuchin", "n", "tooluse", "tidedifabs", 
                                                               "tidedif", "tidedif2", "uniqueloctag", "seqday", "hour", "toolusers",  "picksetup", "dataorigin")] # add "noanimal if you added 0's)

#### Adding Claudio's non tool use data for tidal questions ######
cldata <- read.csv("tide_analysis/coiba-bioblitz_observations_2018-06-03_06-58-00..csv")

# exclude CBA-0 deployment in 2011
cldata <- cldata[!cldata$Array == "CT-CBA-0", ]

length(unique(cldata$Camera_Deployment_ID)) # 43 cameras
str(cldata)
# time stamp in same format as agoutiselect2 and rename variables
cldata$deploymentID <- cldata$Camera_Deployment_ID
cldata$sequenceID <- cldata$Image_Sequence_ID
cldata$scientificName <- cldata$Species_Name_Latin
cldata$locationName <- cldata$Camera_Site
cldata$longitude <- cldata$Camera_Site_Longitude
cldata$latitude <- cldata$Camera_Site_Latitude
cldata$cameraSetup <- ifelse(cldata$Species_Name_Common == "Setup_Pickup", "True", "False")
cldata$seq_start <- as.POSIXct(cldata$Image_Sequence_DateTime, tz = "America/Panama", format = "%Y-%m-%d %H:%M")
cldata$seq_end <- NA
cldata$seq_length <- NA 
# had this in the other dataset Claudio gave me, but not here, and also not full duration (do we need it?)
cldata$temperature <- NA
cldata$dep_start <- as.POSIXct(cldata$Camera_Deployment_Start, tz = "America/Panama", format = "%Y-%m-%d %H:%M")
cldata$dep_end <- as.POSIXct(cldata$Camera_Deployment_End, tz = "America/Panama", format = "%Y-%m-%d %H:%M")
cldata$dep_length_hours <- as.numeric(difftime(cldata$dep_end,cldata$dep_start,units="hours"))
cldata$month <- cldata$Image_Sequence_Month
cldata$season <- ifelse(cldata$month == 12 | cldata$month == 1 | cldata$month == 2 | cldata$month == 3 | 
                          cldata$month == 4, "Dry", "Wet") 
cldata$island <- ifelse(str_detect(cldata$Array, "JIC"), "Jicaron", "Coiba")
cldata$tool_anvil <- 0 # check this
cldata$tool_site <- 0 # check this
cldata$streambed <- 0  # check this
cldata$capuchin <- ifelse(cldata$scientificName == "Cebus capucinus imitator", 1, 0)
cldata$n <- ifelse(cldata$capuchin == 1, cldata$Count, 0)
cldata$tooluse <- FALSE
cldata$tidedifabs <- NA
for (i in 1:nrow(cldata)) {
  cldata$tidedifabs[i] <- min(abs(difftime(cldata$seq_start[i], TidesLow$TIDE_TIME, units = "hours")))
}
cldata$tidedif <- NA
for (i in 1:nrow(cldata)) {
  cldata$tidedif[i] <- Closest((as.vector(difftime(cldata$seq_start[i], TidesLow$TIDE_TIME,   units = "hours"))), 0)
}
cldata$tidedif2 <- NA
for (i in 1:nrow(cldata)) {
  cldata$tidedif2[i] <- Closest((as.vector(difftime(cldata$seq_start[i], TidesHigh$TIDE_TIME,   units = "hours"))), 0)
}
cldata$uniqueloctag <- cldata$locationName
cldata$seqday <- as.Date(format(cldata$seq_start, "%Y-%m-%d"))
cldata$hour <- hour(cldata$seq_start)
cldata$toolusers <- 0 # check this
cldata$picksetup <- ifelse(cldata$seqday == date(cldata$dep_start) | cldata$seqday == date(cldata$dep_end), 1, 0)
cldata$dataorigin <- "cldata"

cldata2 <- cldata[which(colnames(cldata) %in% colnames(agoutiselect2))]

## should now be in right order and same columns as agoutiselect2
# then add rows cldata2 to agoutiselect 2 in big new dataframe (agoutiselect_t) for tidal analyses
## exclude pickup set up days (so only keep picksetup == 0) or agoutisequence$cameraSetup == "FALSE" 
agoutiselect_t <- rbind(agoutiselect2, cldata2)
# exclude days on which cameras were deployed or picked up (to take away that bias)
agoutiselect_t <- agoutiselect_t[(agoutiselect_t$picksetup == 0),]
### exclude only NAs rows
agoutiselect_t <- agoutiselect_t[rowSums(is.na(agoutiselect_t)) != ncol(agoutiselect_t),]
agoutiselect_t$locationfactor <- as.factor(agoutiselect_t$locationName)

### CHECK: I think we should only work on sequences with capuchins, as we don't have all 'real 0' triggers (but only triggers of other animals that are not capuchins)
# so work on assumption: if capuchin is present, when are more present?
# if we want to work on assumption of when are capuchins present we will have to add in the 0 days (which we could do, but see below for code for that)
# so for hourlevel question this means adding a 0 for each day-hour combination within each deployment length
onlycap_t <- agoutiselect_t[agoutiselect_t$capuchin == 1,]

##### TIDAL #####
# Just like above, might need to add in the not observed zero days. Maybe this is better to do for all deployments in the agouti cleaning script?? think about where this makes sense
# this is harder here cause it's not on the day level but on the sequence level and per hour to low tide
# so can't as easily add in the True 0's or differentiate them from the false ones. 
# is it necessary??? Think about this.

## GAMs
## outcome variable:
# number of capuchins per sequence (to be consistent with Claudio's data)
# currently don't include sequence length as offset, if we would, need to get it for Claudio's data
# can do only Jicaron, or both Jicaron and Coiba

## predictors:
# - time until next low tide (tidedif). Either as absolute difference (tidedifabs) or not absolute (tidedif). Also have tidedif2 which is time to closest high tide
# - tool users 1/0, tool using group on Jicaron/Coiba vs not tool using groups
# - location factor, factor for each location

## make only Jicaron dataset initially
onlycap_tj <- onlycap_t[onlycap_t$island == "Jicaron" & onlycap_t$locationfactor != "CEBUS-03",]

## distance to coast (preliminary based on google maps)
dist2coast <- read.csv("tide_analysis/Dist2coast.csv", header = TRUE)

onlycap_tj <- left_join(onlycap_tj, dist2coast, by = "locationfactor")
onlycap_tj$distcat <- as.factor(onlycap_tj$distcat)
onlycap_tj$distcat2 <- factor(ifelse(onlycap_tj$distcat == "100-150" | onlycap_tj$distcat == "150-200", "100-200", 
                              ifelse(onlycap_tj$distcat == "250-300" | onlycap_tj$distcat == "300-350", "200-400", 
                                     ifelse(onlycap_tj$distcat == "<50" | onlycap_tj$distcat == "50-100", "<100", ">400"))), 
                              levels = c("<100", "100-200", "200-400", ">400"))

onlycap_tj$gmaps <- as.numeric(onlycap_tj$gmaps)
onlycap_tj$locationfactor <- as.factor(onlycap_tj$locationfactor)

## Model 1: split by tool use vs non tool users
# no abs
tm1 <- gam(n ~ s(tidedif, bs = "cc", by = toolusers, k = 10) + toolusers, family = poisson, data = onlycap_tj, method = "REML",  knots = list(tidedif = c(-6,6)))
summary(tm1) 
# visualize
plot(tm1, all.terms = TRUE, pages = 1)
draw(tm1)
# check assumptions
gam.check(tm1) 

# abs
tm1abs <- gam(n ~ s(tidedifabs, by = toolusers) + toolusers, family = poisson, data = onlycap_tj, method = "REML" )
summary(tm1abs) 
# visualize
plot(tm1abs, all.terms = TRUE, pages = 1)
draw(tm1abs)
# check assumptions
gam.check(tm1abs) 

## Model 2: add location of camera as random effect
# no abs n of capuchins
tm2 <- gam(n ~ s(tidedif, bs = "cc", by = toolusers, k = 10) + toolusers + 
               s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML", knots = list(tidedif =c(-6,6)) )
summary(tm2) 
# visualize
plot(tm2, all.terms = TRUE, pages = 1)
draw(tm2)
# check assumptions
gam.check(tm2) 

# abs
tm2abs <- gam(n ~ s(tidedifabs, by = toolusers) + toolusers + s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML" )
summary(tm2abs) 
# visualize
plot(tm2abs, all.terms = TRUE, pages = 1)
draw(tm2abs)
# check assumptions
gam.check(tm2abs) 

## Model 3: check what happens if we use time to high tide instead of low
# so 0 is high tide
# no abs
tm3 <- gam(n ~ s(tidedif2, bs = "cc", by = toolusers) + toolusers + s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML" )
summary(tm3) 
# visualize
plot(tm3, all.terms = TRUE, pages = 1)
draw(tm3)
# check assumptions
gam.check(tm3) 

## distance to coast preliminary
# non absolute tide dif
# distance in meters (guess from google maps)
# te(x, z) includes both the smooth main effects of x and z, plus their smooth interaction. 
# ti() is just the pure smooth interaction of x and z.
# te() is like x*Z or x + z + x:z if you want to write it all out
# ti() is like X:Z only

tm4 <- gam(n ~ ti(tidedif, gmaps, bs = c("cc", "tp"), by = toolusers, k = 10) + toolusers +
             s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML", knots = list(tidedif =c(-6,6)) )
summary(tm4) 
draw(tm4)
plot(tm4)
gam.check(tm4)

# rotate with theta
# non tool users
vis.gam(tm4, view = c("tidedif", "gmaps"), color = "heat", theta = -30, cond = list(toolusers=0))
# tool users
vis.gam(tm4, view = c("tidedif", "gmaps"), color = "heat", theta = -30, cond = list(toolusers=1))

# partial effects
par(mfrow=c(1,2))
# Note: specify zlim when comparing two plots
pvisgam(tm4, view=c("tidedif", "gmaps"), select=1, 
        main='Group=Non-toolusers', labcex=.8,
        zlim=c(-1,1), print.summary=FALSE)
pvisgam(tm4, view=c("tidedif", "gmaps"), select=2, 
        main='Group=Toolusers', labcex=.8,
        zlim=c(-1,1), print.summary=FALSE)
dev.off()

# distance categorical
tm4b <- gam(n ~ s(tidedif, bs = "cc", by = toolusers, k = 10) + toolusers + s(tidedif, bs = "cc", by = distcat2) +
              s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML", knots = list(tidedif =c(-6,6)) )
summary(tm4b)
draw(tm4b)
gam.check(tm4b)

# non absolute tide dif
# distance in meters (guess from google maps)
tm5 <- gam(n ~ ti(tidedifabs, gmaps, bs = c("cc", "tp"), by = toolusers, k = 10) + toolusers +
             s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML", knots = list(tidedif =c(-6,6)) )
summary(tm5) 
draw(tm5)
plot(tm5)
gam.check(tm5)

# distance categorical
tm5b <- gam(n ~ s(tidedifabs, bs = "cc", by = toolusers, k = 10) + toolusers + s(tidedifabs, bs = "cc", by = distcat2) +
              s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML", knots = list(tidedif =c(-6,6)) )
summary(tm5b)
draw(tm5b)

# by location
tm7 <- gam(n ~ s(tidedif, bs = "cc", by = toolusers, k = 10) + toolusers + s(tidedif, bs = "cc", by = locationfactor) +
              locationfactor, family = poisson, data = onlycap_tj, method = "REML", knots = list(tidedif =c(-6,6)) )
summary(tm7)
plot(tm7)

## BRMS
# number of capuchins by tidedif (absolute) and split by toolusers, with locationfactor as random effect
# abs
tbm1a <- brm(n ~ s(tidedifabs, by = toolusers, k = 10) + toolusers + s(locationfactor, bs = "re"), family = poisson,
            data = onlycap_tj, chain = 2, core = 2, iter = 4000, control = list(adapt_delta = 0.99), 
            backend = "cmdstanr")

#saveRDS(tbm1a, "tide_analysis/ModelRDS/tbm1a.rds")
# tbm1a <- readRDS("tide_analysis/ModelRDS/tbm1a.rds")

summary(tbm1a)
plot(tbm1a)

plot(conditional_effects(tbm1a))
plot(conditional_smooths(tbm1a))
pp_check(tbm1a) 
plot(tbm1a)
pairs(tbm1a)

# plot with actual data
# number of capuchins per sequence tool use vs not tool use
ncap <- plot(conditional_effects(tbm1a), plot = FALSE)[[1]]
ncap + labs(y = "Average number of capuchins per sequence", x = "Tool Using Group Yes(1)/No(0)") + theme_bw()

#tidedifabs by tool users vs non tool users
abstu <- plot(conditional_effects(tbm1a), plot = FALSE)[[3]]
abstu + labs(y = "Average number of capuchins per sequence", x = "Hours to nearest low tide (absolute)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj, aes(y = n, x = tidedifabs, group = toolusers, color = toolusers), bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE)

# number of capuchins per locations
# give colors to tool users vs non tool users locations
ncaploc <- plot(conditional_effects(tbm1a), plot = FALSE)[[4]]
ncaploc + labs(y = "Average number of capuchins per sequence", x = "Camera locations", color = "Tool users") + theme_bw() +
  stat_summary(data = onlycap_tj, aes(y = n, x = locationfactor, color = toolusers), geom = "point", fun = mean, inherit.aes = FALSE)

### non absolute
# number of capuchins by tidedif (not absolute) and split by toolusers, with locationfactor as 
tbm1 <- brm(n ~ s(tidedif, bs = "cc", by = toolusers, k = 8) + toolusers + s(locationfactor, bs = "re"), family = poisson,
            data = onlycap_tj, knots = list(tidedif = c(-6,6)), chain = 2, core = 2, iter = 4000, 
            control = list(adapt_delta = 0.99, max_treedepth = 12))

# saveRDS(tbm1, "tide_analysis/ModelRDS/tbm1.rds")
# tbm1 <- readRDS("tide_analysis/ModelRDS/tbm1.rds")

summary(tbm1)
plot(tbm1)

plot(conditional_effects(tbm1))
plot(conditional_smooths(tbm1))
pp_check(tbm1) 
plot(tbm1)
pairs(tbm1)

#tidedif by tool users vs non tool users
nonabstu <- plot(conditional_effects(tbm1), plot = FALSE)[[3]]
nonabstu + labs(y = "Average number of capuchins per sequence", x = "Hours to nearest low tide (absolute)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj, aes(y = n, x = tidedif, group = toolusers, color = toolusers), bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE)


# NEXT STEP: incorporate distance to coast for each camera location! 
# IMPORTANT: also include sequence length (but how for Claudio's data?)

#### ACTIVITY TOOL USERS VS NON TOOL USERS ####
# decide if only Jicaron or both islands
# preliminary, better for this would be grid data if we get it

## distribution of data (with 0s in)
ftable(agoutiselect_t$n)
testdist1 <- fitdist(agoutiselect_t$n, "pois")
plot(testdist1)

testdist2 <- fitdist(agoutiselect_t$n, "gamma", "mme")
plot(testdist2)

hist(agoutiselect_t$n)

## zero inflated poisson makes sense
# or potentially gamma

## distribution of data (only capuchin sequences)
testdist1.2 <- fitdist(onlycap_t$n, "pois")
plot(testdist1.2)

testdist2.2 <- fitdist(onlycap_t$n, "gamma", "mme")
plot(testdist2.2)

ftable(onlycap_t$n)
## poisson makes sense or gamma makes sense

## just plots of number of capuchins per hour by tool use vs non tool use

# colors for two histograms in one
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

### Tool users vs non tool users
histtool <- hist(onlycap_t$hour[onlycap_t$tool_site == 1 & onlycap_t$capuchin == 1], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histnotool <- hist(onlycap_t$hour[onlycap_t$tool_site == 0 & onlycap_t$capuchin == 1], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histnotool, col = c2, freq = FALSE, main = "Tool users (blue) vs non-tool users (red)", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins", ylim = c(0, 0.3))
plot(histtool, col = c1, freq = FALSE, add = TRUE)

## GAMs
## outcome variable:
# number of capuchins per sequence (to be consistent with Claudio's data)
# currently don't include sequence length as offset, if we would, need to get it for Claudio's data
# can do only Jicaron, or both Jicaron and Coiba

## hour of day is not cyclic spline, as we have no observations at midnight and early in morning (explained in bottom of the heap youtube)

## Model 1: number of capuchins in sequences with capuchins depending on hour of day, by tool use/vs non tool users
am1 <- gam(n ~ s(hour, by = toolusers, k = 15) + toolusers,
              family = poisson(), data = onlycap_t, method = "REML")
summary(am1) 
# visualize
plot(am1, all.terms = TRUE, pages = 1)
draw(am1)
# check assumptions
gam.check(am1) # k too low? need to find k that works

# with factor smooth instead of by smooth
am1.2 <- gam(n ~ s(hour, toolusers, bs = "fs"), family = poisson(), data = onlycap_t, method = "REML")
summary(am1.2)
plot(am1.2, all.terms = TRUE)
draw(am1.2)

## Model 2: including location as random effect
am2 <- gam(n ~ s(hour, by = toolusers, k = 12) +  toolusers + s(locationfactor, bs = "re"), family = poisson(), data = onlycap_t, method = "REML")
summary(am2)

draw(am2)
plot(am2, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am2) 

# with factor smooth instead of by smooth
# CHECK THIS MODEL 
am2.2 <- gam(n ~ s(hour, toolusers, bs = "fs") + s(locationfactor, bs = "re"), family = poisson(), data = onlycap_t, method = "REML")
summary(am2.2)
plot(am2.2, all.terms = TRUE, pages = 1)
draw(am2.2)


## old plotting method
locationcol <- c("#5081db",
                 "#a0bf52",
                 "#746dd8",
                 "#c09f43",
                 "#583687",
                 "#4cc186",
                 "#41aaeb",
                 "#639a49",
                 "#d177ca",
                 "#33d4d1",
                 "#932c6c",
                 "#6789cf",
                 "#d6587d",
                 "#c47236",
                 "#ba4c46")

# change this plot, try to figure it out. Only plot tool user points in one graph and non tool users in other. 
# FIGURE OUT SCALES?
par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
plot(am3_zp, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(am3_zp)[1], ylim = c(0, log(max(agoutiselect2$n))))
points(agoutiselect2$hour, log(agoutiselect2$n), col = locationcol[(as.numeric(agoutiselect2$locationfactor) + 1)], pch = agoutiselect2$tool_site + 1)
legend("topright", inset=c(-0.35,0), legend = levels(agoutiselect2$locationfactor), pch = 19, col = locationcol, cex = 0.9 )
legend("bottomright", inset=c(-0.25,0), legend = c("Non-tool-users", "Tool-users"), pch = c(1,2),cex = 0.9 )
dev.off()


## save dataframes we need for analysis as rds
#### NOTES #######
## still use this?
## CHECK IF THIS WORKS AND IF IT'S NECESSARY
# maybe instead calculate number of capuchins per minute/hour whatever (per sequence using sequencelength offset)
# and then make average of that per day-hour 
# still figure this out, need to do some math (to also incorporate all the minutes/seconds there were no capuchins? unclear)
# or add all sequence lengths and all capuchins per hour and then divide capuchins by total seq length? Think about independence etc and what this means

## create variable for nr of capuchins per hour
agoutiselect2$n_hour <- (agoutiselect2$n/agoutiselect2$seq_length) * 3600
agoutiselect2$dayhour <- paste(agoutiselect2$seqday, agoutiselect2$hour, sep = " ")
agoutidayhour <- aggregate(agoutiselect2$n, by = list(dayhour = agoutiselect2$dayhour, uniqueloctag = agoutiselect2$uniqueloctag), FUN = mean)
aggregate(agoutiselect2$seq_length, by = list(dayhour = agoutiselect2$dayhour, uniqueloctag = agoutiselect2$uniqueloctag), FUN = sum)



## if we want to generate 0 days (not sure if necessary for tidal analysis, not if we work on number of capuchins)
#  ## SKIP THIS? need to add in the hours that the camera wasn't triggered as 0s  #####
# first make overview of deployments we have in this dataframe and their start and end days
# NOTE: CAN LIKELY DO THIS EASIER BY GENERATING THIS ENTIRE THING ONCE AND THEN FILTERING? 
locations2 <- data.frame(uniqueloctag = unique(agoutisequence_c$uniqueloctag)) 
locations2 <- left_join(locations2, agoutisequence_c[,c("uniqueloctag", "dep_start", "dep_end")], by = "uniqueloctag")
locations2 <- locations2[!duplicated(locations2$uniqueloctag),]
# take time off and keep just date variable
locations2$dep_startday <- as.Date(format(locations2$dep_start, "%Y-%m-%d"))
locations2$dep_endday <- as.Date(format(locations2$dep_end, "%Y-%m-%d"))

###  generate all the days that should be present within each deployment
# first create dataframe for first one
depldays3 <<- data.frame(uniqueloctag = locations2$uniqueloctag[1], seqday = seq(locations2$dep_startday[1], locations2$dep_endday[1], by = "days"))
# now iterate over all the other ones and append those to the dataframe
for (i in 2:nrow(locations2)) {
  depldays4 = data.frame(uniqueloctag = locations2$uniqueloctag[i], seqday = seq(locations2$dep_startday[i], locations2$dep_endday[i], by = "days"))
  depldays3 <<- rbind(depldays3, depldays4)
} 

# need to expand this dataframe to have an hour a day for each deployment.
depldayhour <- data.frame(uniqueloctag = rep(depldays3$uniqueloctag, 24), seqday = rep(depldays3$seqday, 24))
depldayhour <- depldayhour[order(depldayhour$uniqueloctag),]
depldayhour$hour <- rep(1:24, (nrow(depldayhour)/24))

agoutiselect2 <- left_join(depldayhour[depldayhour$uniqueloctag %in% agoutisequence_c$uniqueloctag,], agoutisequence_c, by = c("uniqueloctag", "seqday", "hour"))
agoutiselect2$noanimal <- ifelse(is.na(agoutiselect2$sequenceID), 1, 0)

## fill in NAs like above, using a metadata file
metadata2 <- agoutisequence_c[!duplicated(agoutisequence_c$uniqueloctag), c("uniqueloctag", "deploymentID", "locationName", "tags", "dep_start", "dep_end", "dep_length_hours",
                                                                            "island", "tool_anvil", "tool_site")]

for (i in 1:nrow(metadata2)) {
  agoutiselect2$locationName[agoutiselect2$uniqueloctag == metadata2$uniqueloctag[i]] <- metadata2$locationName[metadata2$uniqueloctag == metadata2$uniqueloctag[i]]
  agoutiselect2$tags[agoutiselect2$uniqueloctag == metadata2$uniqueloctag[i]] <- metadata2$tags[metadata2$uniqueloctag == metadata2$uniqueloctag[i]]
  agoutiselect2$dep_start[agoutiselect2$uniqueloctag == metadata2$uniqueloctag[i]] <- metadata2$dep_start[metadata2$uniqueloctag == metadata2$uniqueloctag[i]] 
  agoutiselect2$dep_end[agoutiselect2$uniqueloctag == metadata2$uniqueloctag[i]] <- metadata2$dep_end[metadata2$uniqueloctag == metadata2$uniqueloctag[i]] 
  agoutiselect2$dep_length_hours[agoutiselect2$uniqueloctag == metadata2$uniqueloctag[i]] <- metadata2$dep_length_hours[metadata2$uniqueloctag == metadata2$uniqueloctag[i]] 
  agoutiselect2$island[agoutiselect2$uniqueloctag == metadata2$uniqueloctag[i]] <- metadata2$island[metadata2$uniqueloctag == metadata2$uniqueloctag[i]] 
  agoutiselect2$tool_anvil[agoutiselect2$uniqueloctag == metadata2$uniqueloctag[i]] <- metadata2$tool_anvil[metadata2$uniqueloctag == metadata2$uniqueloctag[i]]
  agoutiselect2$deploymentID[agoutiselect2$uniqueloctag == metadata2$uniqueloctag[i]] <- metadata2$deploymentID[metadata2$uniqueloctag == metadata2$uniqueloctag[i]]
  agoutiselect2$tool_site[agoutiselect2$uniqueloctag == metadata2$uniqueloctag[i]] <- metadata2$tool_site[metadata2$uniqueloctag == metadata2$uniqueloctag[i]]
  
} 

agoutiselect2$n[agoutiselect2$noanimal == 1] <- 0
agoutiselect2$capuchin[agoutiselect2$noanimal == 1] <- 0
# not sure if we should change seq_length to 0
agoutiselect2$seq_length[agoutiselect2$noanimal == 1] <- 0

