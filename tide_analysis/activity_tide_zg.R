## Tool use & tidal cycles + Daily Activity analysis
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

## Packages required
require(tidyr)
require(brms)
require(mgcv)
require(ggplot2)
require(gratia)
require(fitdistrplus)
require(itsadug)
require(mgcViz)
require(rgl)
require(ggthemes)
require(viridis)
require(truncdist)
require(tidybayes)
library(akima)
library(reshape2)
library(matrixStats)

### Prepare dataframe tidal analyses ####

# start with the agoutisequence dataframe that's cleaned and aggregated to the sequence level
# and subsetted to only coded deployments
agoutisequence_c$hour <- hour(agoutisequence_c$seq_start)
agoutisequence_c$toolusers <- factor(agoutisequence_c$tool_site, levels = c(0,1), labels = c("Non-tool-users", "Tool-users"))
agoutisequence_c$locationfactor <- as.factor(agoutisequence_c$locationName)

unique(agoutisequence_c$locationfactor)

# Could add in the hours that the cameras werent triggered but were deployed (see bottom of script)
# If this is done, then don't run following line
agoutiselect2 <- agoutisequence_c

# create flag for deployment pick up and setup days
# most of the pickup/setup days were at the start/end of the deployment. But sometimes people came to mess with cameras in middle of deployment. Should exclude those too
picksetupdays <- unique(agoutiselect2$seqday[which(agoutiselect2$seqday == date(agoutiselect2$dep_start) | agoutiselect2$seqday == date(agoutiselect2$dep_end) | agoutiselect2$cameraSetup ==  "True")] )
agoutiselect2$picksetup <- ifelse(agoutiselect2$seqday %in% picksetupdays , 1, 0)

agoutiselect2$dataorigin <- "agoutidata" # to differentiate from other dataset
agoutiselect2 <- agoutiselect2[c("deploymentID", "sequenceID", "scientificName", "locationName", "longitude", "latitude", "cameraSetup", "seq_start", 
                                                               "seq_end", "seq_length", "temperature", "dep_start", "dep_end", "dep_length_hours", "month", 
                                                               "season", "island", "tool_anvil", "tool_site", "streambed", "capuchin", "n", "tooluse", "tidedifabs", 
                                                               "tidedif", "tidedif2", "uniqueloctag", "seqday", "hour", "toolusers",  "picksetup", "dataorigin")] # add "noanimal if you added 0's)

#### Adding Claudio's non tool use data ######
cldata <- read.csv("tide_analysis/coiba-bioblitz_observations_2018-06-03_06-58-00..csv")
cl_seqlength <- read.csv("tide_analysis/coiba-bioblitz_sequence_duration_2022-05-24_17-47-14.csv")

cldata <- left_join(cldata, cl_seqlength, by = c("Image_Sequence_ID" = "IMAGE_SEQUENCE_ID") )

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
cldata$seq_start <- as.POSIXct(cldata$SEQUENCE_START, tz = "America/Panama", format = "%Y-%m-%d %H:%M")
cldata$seq_end <- as.POSIXct(cldata$SEQUENCE_END, tz = "America/Panama", format = "%Y-%m-%d %H:%M")
cldata$seq_length <- cldata$SEQUENCE_DURATION
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
cldata$toolusers <- factor(cldata$tool_site, levels = c(0,1), labels = c("Non-tool-users", "Tool-users")) 
cldata$picksetup <- ifelse(cldata$seqday == date(cldata$dep_start) | cldata$seqday == date(cldata$dep_end), 1, 0)
cldata$dataorigin <- "cldata"

cldata2 <- cldata[which(colnames(cldata) %in% colnames(agoutiselect2))]

## should now be in right order and same columns as agoutiselect2
# add rows cldata2 to agoutiselect 2 in big new dataframe (agoutiselect_t) for tidal analyses
agoutiselect_t <- rbind(agoutiselect2, cldata2)
# exclude days on which cameras were deployed or picked up (to take away that bias)
agoutiselect_t <- agoutiselect_t[(agoutiselect_t$picksetup == 0),]
# exclude only NAs rows
agoutiselect_t <- agoutiselect_t[rowSums(is.na(agoutiselect_t)) != ncol(agoutiselect_t),]
agoutiselect_t$locationfactor <- as.factor(agoutiselect_t$locationName)

### CHECK: I think we should only work on sequences with capuchins, as we don't have all 'real 0' triggers (but only triggers of other animals that are not capuchins)
# so work on assumption: if capuchin is present, when are more present?
# if we want to work on assumption of when are capuchins present we will have to add in the 0 days (which we could do, but see below for code for that)
# so for hourlevel question this means adding a 0 for each day-hour combination within each deployment length
onlycap_t <- agoutiselect_t[agoutiselect_t$capuchin == 1,]

## make only Jicaron dataset initially
onlycap_tj <- onlycap_t[onlycap_t$island == "Jicaron" & onlycap_t$locationfactor != "CEBUS-03",]

## distance to coast (preliminary based on google maps)
dist2coast <- read.csv("tide_analysis/tidalcams2.csv", header = TRUE)

onlycap_tj <- left_join(onlycap_tj, dist2coast, by = "locationfactor")
onlycap_tj$locationfactor <- as.factor(onlycap_tj$locationfactor)
# remove only NA rows
onlycap_tj <- onlycap_tj[which(is.na(onlycap_tj$deploymentID) == FALSE),]
# new predictor, whether it is the Wet (May-November) or the Dry season (December-April)
onlycap_tj$seasonF <- as.factor(onlycap_tj$season)

## sequence length
p1 <- hist(onlycap_tj$seq_length[which(onlycap_tj$dataorigin == "cldata")])
p2 <- hist(onlycap_tj$seq_length[which(onlycap_tj$dataorigin == "agoutidata" & onlycap_tj$toolusers == "Non-tool-users")])
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,6000), ylim = c(0,1000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,6000), ylim = c(0,1000), add=T)

#zoomed in
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,1500), ylim = c(0,1000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,1500), ylim = c(0,1000), add=T)

p1 <- hist(onlycap_tj$n[which(onlycap_tj$dataorigin == "cldata")])
p2 <- hist(onlycap_tj$n[which(onlycap_tj$dataorigin == "agoutidata" & onlycap_tj$toolusers == "Non-tool-users")])
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10), ylim = c(0,800))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), ylim = c(0,800), add=T)

# red is agoutidata, blue is claudio data 

## very uneven sampling between tool users and non tool users and very different range
hist(onlycap_tj$distcoast[onlycap_tj$toolusers == "Non-tool-users"])
hist(onlycap_tj$distcoast[onlycap_tj$toolusers == "Tool-users"])
summary(onlycap_tj$distcoast[onlycap_tj$toolusers == "Tool-users"])
summary(onlycap_tj$distcoast[onlycap_tj$toolusers == "Non-tool-users"])

## some resources on interactions in GAMs
# see https://stats.stackexchange.com/questions/472434/gam-2d-factor-smooth-with-uneven-sampling-in-xz-space-across-factor-levels-r
# https://stats.stackexchange.com/questions/432227/smooth-bivariate-interaction-decomposition-in-gam-models
# https://stats.stackexchange.com/questions/45446/intuition-behind-tensor-product-interactions-in-gams-mgcv-package-in-r
# https://stackoverflow.com/questions/68659805/mgcv-gam-more-than-one-variable-in-by-argument-smooth-varying-by-more-than-1
# te(x, z) includes both the smooth main effects of x and z, plus their smooth interaction. 
# ti() is just the pure smooth interaction of x and z.
# te() is like x*Z or x + z + x:z if you want to write it all out
# ti() is like X:Z only

testdist1.1 <- fitdist(onlycap_tj$n, "pois")
plot(testdist1.1)

testdist1.2 <- fitdist(onlycap_tj$n, "gamma", "mme")
plot(testdist1.2)

testdist1.3 <- fitdist(onlycap_tj$n, "exp", "mme")
plot(testdist1.3)

testdist1.4 <- fitdist(onlycap_tj$n, "weibull", "mle")
plot(testdist1.4)

# Gamma is better than poisson. Or exponential
# zero-truncated poisson?

## could consider using sequence length as another metric of capuchin presence, but relationship might not be straightforward/linear
## investigate relationship number of capuchins and sequence length
onlycap_tj$tooluseF <- as.factor(onlycap_tj$tooluse)
# compare tool use sequences and non tool use sequences
seqmodel <- gam(seq_length ~ s(n, by = tooluseF) + tooluseF + s(locationfactor, bs = "re"),  family = poisson, data = onlycap_tj[onlycap_tj$dataorigin == "agoutidata",], method = "REML" )
summary(seqmodel)
draw(seqmodel)

# compare tool site to non tool site cameras
seqmodel2 <- gam(seq_length ~ s(n, by = toolusers) + toolusers + s(locationfactor, bs = "re"),  family = poisson, data = onlycap_tj[onlycap_tj$dataorigin == "agoutidata",], method = "REML" )
summary(seqmodel2)
draw(seqmodel2)

hist(onlycap_tj$seq_length, breaks = 100)
hist(onlycap_tj$n[onlycap_tj$toolusers == "Non-tool-users"], breaks = 100)
hist(onlycap_tj$n[onlycap_tj$toolusers == "Tool-users"], breaks = 100)

# so might not be converging line of evidence/useful metric of capuchin activity, because it differs so much between tool use and non tool use sequences

#### TIDAL GAMS ####
## outcome variable:
# number of capuchins per sequence
# only Jicaron

## predictors:
# - time until next low tide (tidedif). Either as absolute difference (tidedifabs) or not absolute (tidedif). Also have tidedif2 which is time to closest high tide
# - tool users 1/0, tool using group on Jicaron/Coiba vs not tool using groups
# - location factor, factor for each location

#### MGCV #### 

## Model 1: Effect of time to nearest low tide on number of capuchins, split for tool using and non tool using groups and with locationfactor as random effect
###
# non-absolute time to low tide (-6 hours to +6 hours, where 0 is low tide)
tm1 <- gam(n ~ s(tidedif, bs = "cc", by = toolusers, k = 10) + toolusers + 
               s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML", knots = list(tidedif =c(-6,6)) )
summary(tm1) 
# visualize
plot(tm1, all.terms = TRUE, pages = 1)
draw(tm1)
# check assumptions
gam.check(tm1) 

# absolute time to low tide (between 0 and 6 where 0 is low tide)
tm1abs <- gam(n ~ s(tidedifabs, by = toolusers) + toolusers + s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML" )
summary(tm1abs) 
# visualize
plot(tm1abs, all.terms = TRUE, pages = 1)
draw(tm1abs)
# check assumptions
gam.check(tm1abs) 

## Check: use time to high tide instead of low
# so 0 is high tide
tm1high <- gam(n ~ s(tidedif2, bs = "cc", by = toolusers) + toolusers + s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML" )
summary(tm1high) 
# visualize
plot(tm1high, all.terms = TRUE, pages = 1)
draw(tm1high)
# check assumptions
gam.check(tm1high) 

## Model 2: Including distance to coast in model 1
###
# distance in meters for each camera (see dist2coast.R script for how it was calculated)

# non-absolute tide difference
tm2 <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
             te(tidedif, distcoast, bs = c("cc", "tp"), by = toolusers, k = c(10,6), m = 1) + toolusers +
             s(locationfactor, bs = "re"), family = Gamma(link = "log"), data = onlycap_tj, method = "REML",
           select = TRUE, knots = list(tidedif =c(-6,6)) )

summary(tm2) 
# visualize
plot(tm2, pages = 1)
draw(tm2)
gam.check(tm2) # gam check seems ok
concurvity(tm2)

# look at the tidedif, distcoast interaction for tool users in more detail
# too far is to say how much the smooth should extrapolate beyond the data
vis.gam(tm2, view = c("tidedif", "distcoast"), plot.type = "contour", too.far = 0.05, cond = list(toolusers = "Tool-users"))
vis.gam(tm2, view = c("tidedif", "distcoast"), plot.type = "perspective", too.far = 0.05, cond = list(toolusers = "Tool-users"))
# have the way too big y axis when using vis.gam (due to scale difference), switch to mgcviz

b <- getViz(tm2)
# with points on
plot(sm(b, 3), ylim = c(0,65)) +  l_fitRaster() + l_fitContour() + l_points()
# with rug (and alpha to show where most data is)
plot(sm(b,3), ylim = c(0,65)) + l_fitRaster() + l_fitContour() + l_rug(alpha =0.05)

# interactive 3d plot
plotRGL(sm(b, 3), ylim = c(0,65), residuals = TRUE)

# absolute tide difference
tm2abs <- gam(n ~ te(tidedifabs, distcoast, bs = c("tp", "tp"), k = c(10, 6)) +
             te(tidedifabs, distcoast, bs = c("tp", "tp"), by = toolusers, k = c(10,6), m = 1) + toolusers +
             s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML",
           select = TRUE)
summary(tm2abs) 
plot(tm2abs, pages = 1)
draw(tm2abs)
gam.check(tm2abs) # gam check seems ok
concurvity(tm2abs)

b2 <- getViz(tm2abs)
# with points on
plot(sm(b2, 3), ylim = c(0,65)) + l_fitRaster() + l_fitContour() + l_points()
# with rug
plot(sm(b2,3), ylim = c(0,65)) + l_fitRaster() + l_fitContour() + l_rug(alpha = 0.05)

# interactive 3d plot
plotRGL(sm(b2, 3), ylim = c(0,65), residuals = TRUE)

### Model 3: Including seasonality
###
# Two approaches, can either look at a split model (one for tool users, one for non tool users) or a combined one with a 4-way interaction

## Option A: Split models for tool use and non tool use
# tool using group
tm3 <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
            te(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
            s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], method = "REML",
          select = TRUE, knots = list(tidedif =c(-6,6)) )
# saveRDS(tm3, "tide_analysis/ModelRDS/tm3.rds")
# tm3 <- readRDS("tide_analysis/ModelRDS/tm3.rds")

summary(tm3)
plot(tm3, pages = 1)
draw(tm3)
gam.check(tm3) 
concurvity(tm3)

b3 <- getViz(tm3)
plot(sm(b3, 2)) + l_fitRaster() + l_fitContour() + l_rug(alpha=0.05)
plot(sm(b3, 3)) + l_fitRaster() + l_fitContour() + l_rug(alpha=0.05)
# interactive 3d plot
plotRGL(sm(b3, 3), ylim = c(0,65), residuals = TRUE)

## time of day instead of tide difference
tm3_h <- gam(n ~ te(hour, distcoast, bs = c("tp", "tp"), k = c(10, 6)) +
               te(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
               s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], method = "REML",
             select = TRUE)
# saveRDS(tm3_h, "tide_analysis/ModelRDS/tm3_h.rds")
#tm3_h <- readRDS("tide_analysis/ModelRDS/tm3_h.rds")

summary(tm3_h)
plot(tm3_h, pages = 1)
draw(tm3_h)
gam.check(tm3_h) 
concurvity(tm3_h)

# looking at non tool users
tm3b <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
            te(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
            s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], method = "REML",
          select = TRUE, knots = list(tidedif =c(-6,6)) )
# saveRDS(tm3b, "tide_analysis/ModelRDS/tm3b.rds")
# tm3b <- readRDS("tide_analysis/ModelRDS/tm3b.rds")

summary(tm3b)
plot(tm3b, pages = 1)
draw(tm3b)
gam.check(tm3b) 
concurvity(tm3b)

## time of day instead of tide difference
tm3b_h <- gam(n ~ te(hour, distcoast, bs = c("tp", "tp"), k = c(10, 6)) +
               te(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
               s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], method = "REML",
             select = TRUE)
# saveRDS(tm3b_h, "tide_analysis/ModelRDS/tm3b_h.rds")
#tm3b_h <- readRDS("tide_analysis/ModelRDS/tm3b_h.rds")

summary(tm3b_h)
plot(tm3b_h, pages = 1)
draw(tm3b_h)
gam.check(tm3b_h) 
concurvity(tm3b_h)



## Option B: Together in one model with a 4-way interaction
tm4 <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
             te(tidedif, distcoast, bs = c("cc", "tp"), by = interaction(seasonF, toolusers), k = c(10,6), m = 1) + toolusers + seasonF + 
             s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML",
           select = TRUE, knots = list(tidedif =c(-6,6)) )
# saveRDS(tm4, "tide_analysis/ModelRDS/tm4.rds")
# tm4 <- readRDS("tide_analysis/ModelRDS/tm4.rds")

summary(tm4)
plot(tm4, pages = 1)
draw(tm4)
gam.check(tm4) # gam check seems ok
concurvity(tm4)

vis.gam(tm4, view = c("tidedif", "distcoast"), plot.type = "contour", too.far = 0.05, cond = list(seasonF = "Wet"))

b4 <- getViz(tm4)
# with points on
plot(sm(b4,5), ylim = c(0,65)) + l_fitRaster() + l_fitContour() + l_rug(alpha = 0.05) 
plot(sm(b4,4), ylim = c(0,65)) + l_fitRaster() + l_fitContour() + l_rug(alpha = 0.05) 
plot(sm(b4,2)) + l_fitRaster() + l_fitContour() + l_rug(alpha = 0.05)

# interactive 3d plot
plotRGL(sm(b4, 3), ylim = c(0,65), residuals = TRUE)

## need to figure out which k makes sense for tensor product
onlycap_tj$toolusers2 <- relevel(onlycap_tj$toolusers, ref = "Tool-users")

tm4b <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10,8)) +
            te(tidedif, distcoast, bs = c("cc", "tp"), by = interaction(seasonF, toolusers), k = c(5,4), m = 1) + toolusers + seasonF + 
            s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML",
          select = TRUE, knots = list(tidedif =c(-6,6)) )

summary(tm4b)
plot(tm4b, pages = 1)
draw(tm4b)
gam.check(tm4b) # gam check seems ok

###
## Model 5: Comparing hot and cold low tides to look at temperature 
###

str(onlycap_tj)

str(TidesLow)
TidesLow$Temp <- ifelse(hour(TidesLow$TIDE_TIME) < 12 | hour(TidesLow$TIDE_TIME) > 17 , "Cold", "Hot")

onlycap_tj$tidecold <- NA
for (i in 1:nrow(onlycap_tj)) {
  onlycap_tj$tidecold[i] <- Closest((as.vector(difftime(onlycap_tj$seq_start[i], TidesLow$TIDE_TIME[which(TidesLow$Temp == "Cold")],   units = "hours"))), 0)
}

onlycap_tj$tidetemp <- ifelse(onlycap_tj$tidedif == onlycap_tj$tidecold, "Cold", "Hot")
onlycap_tj$tidetemp <- as.factor(onlycap_tj$tidetemp)

## Split models for tool use and non tool use
# tool using group
tm5 <- gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
            te(tidedif, distcoast, bs = c("cc", "tp"), by = interaction(seasonF, tidetemp), k = c(10,6), m = 1) + seasonF + tidetemp +
            s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], method = "REML",
          select = TRUE, knots = list(tidedif =c(-6,6)) )
#saveRDS(tm5, "tide_analysis/ModelRDS/tm5.rds")
#tm5 <- readRDS("tide_analysis/ModelRDS/tm5.rds")

summary(tm5)
plot(tm5, pages = 1)
draw(tm5)
gam.check(tm5) 
concurvity(tm5)

# looking at non tool users
tm5b <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
             te(tidedif, distcoast, bs = c("cc", "tp"), by = interaction(seasonF, tidetemp), k = c(10,6), m = 1) + seasonF + tidetemp +
             s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], method = "REML",
           select = TRUE, knots = list(tidedif =c(-6,6)) )
# saveRDS(tm5b, "tide_analysis/ModelRDS/tm5b.rds")
# tm5b <- readRDS("tide_analysis/ModelRDS/tm5b.rds")

summary(tm5b)
plot(tm5b, pages = 1)
draw(tm5b)
gam.check(tm5b) 
concurvity(tm5b)

#### BRMS ####

# priors for tidal models
tidal_prior <- c(prior(normal(0, 2), class = Intercept),
               prior(normal(0,2), class = b),
               prior(normal(0,2), class = sds))

# prior simulation
tbm1_prior <- brm(n | trunc(lb=1) ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
              t2(tidedif, distcoast, bs = c("cc", "tp"), by = toolusers, k = c(10, 6), m = 1) + toolusers +
              s(locationfactor, bs = "re"), family = poisson(),  knots = list(tidedif =c(-6,6)),  data = onlycap_tj, chain = 2, core = 2, iter = 2000,
              prior = tidal_prior, sample_prior = "only", backend = "cmdstanr")

summary(tbm1_prior)
prior_summary(tbm1_prior)
plot(tbm1_prior)

##### MODEL 1: TU AND NTU TOGETHER, NO SEASON ######
## Number of capuchins by tidedif (not absolute) and split by toolusers, with locationfactor as random effect and distance to coast
####
tbm1 <- brm(n  ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
              t2(tidedif, distcoast, bs = c("cc", "tp"), by = toolusers, k = c(10, 6), m = 1) + toolusers +
              s(locationfactor, bs = "re"), family = poisson(),  knots = list(tidedif =c(-6,6)),  data = onlycap_tj, chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
            control = list(adapt_delta = 0.99, max_treedepth = 12), backend = "cmdstanr", prior = tidal_prior)

#tbm1 <- add_criterion(tbm1, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99, max_treedepth = 12), backend = "cmdstanr", ndraws = 2000) 
#saveRDS(tbm1, "tide_analysis/ModelRDS/tbm1_pois.rds")
# tbm1 <- readRDS("tide_analysis/ModelRDS/tbm1_pois.rds")

mcmc_plot(tbm1,type = "trace")
mcmc_plot(tbm1, type = "hist") #show histograms of the posterior distributions
mcmc_plot(tbm1) #plot posterior intervals
mcmc_plot(tbm1, type = "acf_bar")

summary(tbm1)
plot(tbm1)

# ce_tbm1 <- readRDS("tide_analysis/ModelRDS/ce_tbm1_prior.rds") # prior for truncated one, poisson for non truncated one
plot(conditional_effects(tbm1))

# cs_tbm1 <- readRDS("tide_analysis/ModelRDS/cs_tbm1_prior.rds")
plot(conditional_smooths(tbm1))

plot(conditional_effects(tbm1, effects = "tidedif:toolusers", spaghetti = TRUE, ndraws = 200))

## Checks
pp_check(tbm1, ndraw = 100) 
pairs(tbm1)
loo(tbm1)
loo_R2(tbm1)
bayes_R2(tbm1)
mcmc_plot(tbm1, variable = c("b_Intercept", "b_toolusersToolMusers", "bs_t2tidedifdistcoast_1", "sds_slocationfactor_1"), type = "areas")

## Visualizing
# Option 1: Using conditional_smooths and effects
distcoastplot <- plot(conditional_smooths(tbm1, rug = TRUE), plot = FALSE)[[2]]
# distcoastplot <- readRDS("tide_analysis/ModelRDS/distcoastplot_brms_prior.rds") # zero-truncated one

distcoastplot + ggplot2::ylim(0, 50) + theme_bw() + theme(panel.grid = element_blank()) +  labs(x = "Hours to nearest low tide (0)", y = "Distance to coast (m)", title = "Tool users") +
  guides(color=guide_legend(title="Change in number of capuchins per sequence"))
# if we want to fill it etc we need to predict or get fit out of brms object and make it ourselves with geom_contour

distcoastplot$data$toolusers <- relevel(distcoastplot$data$toolusers, ref = "Tool-users")

# png("tide_analysis/ModelRDS/tuvsntu_50.png", width = 12, height = 6, units = 'in', res = 300)
ggplot(distcoastplot$data, aes(x = tidedif, y = distcoast, z = estimate__)) + geom_contour_filled() + ylim(0,50) + scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme_bw() + theme(panel.grid = element_blank()) + 
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change in number of capuchins per sequence") +
  geom_rug(data = onlycap_tj, aes(x = tidedif, y = distcoast), inherit.aes = FALSE)  +
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16), axis.text = element_text(size = 14)) +
  facet_wrap(~toolusers)
#dev.off()

#tidedif by tool users vs non tool users
# distances below 50 meters
coastdist <- unique(onlycap_tj$distcoast[which(onlycap_tj$distcoast < 50)])

tidalcapstu <- plot(conditional_effects(tbm1), plot = FALSE)[[5]]
#png("tide_analysis/ModelRDS/tidedifdistcoast.png", width = 10, height = 6, units = 'in', res = 300)
tidalcapstu_pois + labs(y = "Number of capuchins", x = "Hours until and after nearest low tide (=0)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj, aes(y = n, x = tidedif, group = toolusers, color = toolusers), bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =20), axis.text = element_text(size = 14))
# dev.off()

# Option 2: Compute posterior predictions with posterior_epred and plot contourplot from that

## Contourplot like below for seasons
predict_tbm1_p <- posterior_smooths(tbm1, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=toolusers,k=c(10,6),m=1)')

# mean of each column is what I'm looking for
tbm1$data$fit_tooltide <- as.numeric(colMedians(predict_tbm1_p))

d1_tu <- with(tbm1$data[tbm1$data$toolusers == "Tool-users",], interp(x = tidedif, y = distcoast, z = fit_tooltide, duplicate = "mean"))
d1_ntu <-  with(tbm1$data[tbm1$data$toolusers == "Non-tool-users",], interp(x = tidedif, y = distcoast, z = fit_tooltide, duplicate = "mean"))

d2_tu <- melt(d1_tu$z, na.rm = TRUE)
names(d2_tu) <- c("x", "y", "fit")
d2_tu$tidedif <- d1_tu$x[d2_tu$x]
d2_tu$distcoast <- d1_tu$y[d2_tu$y]

d2_ntu <- melt(d1_ntu$z, na.rm = TRUE)
names(d2_ntu) <- c("x", "y", "fit")
d2_ntu$tidedif <- d1_ntu$x[d2_ntu$x]
d2_ntu$distcoast <- d1_ntu$y[d2_ntu$y]

d2_tu$toolusers <- "Tool-users"
d2_ntu$toolusers <- "Non-tool-users"

d2_t <- rbind(d2_tu, d2_ntu)
d2_t$toolusers <- as.factor(d2_t$toolusers)
d2_t$toolusers <- relevel(d2_t$toolusers, ref = "Tool-users")

# png("tide_analysis/ModelRDS/tuvsntu_pred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot_pred.png", width = 12, height = 6))
ggplot(data = d2_t, aes(x = tidedif, y = distcoast, z = fit)) +
  geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj, aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16), axis.text = element_text(size = 14)) +
  facet_wrap(~toolusers, scales = "free")
# dev.off()

## Option 2A: predicting fictional cameras distances to coast
# is not taking locationfactor random effect into account
newdata_tbm1 <- expand_grid(tidedif = -6:6,
                            toolusers = c("Tool-users", "Non-tool-users"),
                            distcoast = c(1,20, 40))

# use posterior smooth to get estimated responses 
predict_tbm1 <- posterior_smooths(tbm1, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=toolusers,k=c(10,6),m=1)', newdata_tbm1)

# mean of each row is what I'm looking for
newdata_tbm1$fit_tooltide <- as.numeric(colMeans(predict_tbm1))
newdata_tbm1$min <- colQuantiles(predict_tbm1, prob = c(0.05))
newdata_tbm1$max <- colQuantiles(predict_tbm1, prob = c(0.95))
newdata_tbm1$distcoastF <- as.factor(newdata_tbm1$distcoast)

ggplot(newdata_tbm1) + geom_line(aes(x = tidedif, y = fit_tooltide, group = distcoastF, color = distcoastF)) +
  geom_ribbon(aes(x = tidedif, y = fit_tooltide, group = distcoastF, fill = distcoastF, ymin=min, ymax = max), alpha = 0.2) + 
  labs(y = "Change in capuchin activity", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() + facet_wrap(~toolusers)

## Option 2B: predicting on existing data
newdata_tbm1_tu <- expand_grid(tidedif = -6:6,
                               toolusers = c("Tool-users"),
                               locationfactor = unique(onlycap_tj$locationfactor[which(onlycap_tj$toolusers == "Tool-users")]))

# for now remove cameras that are not in the tbm1 when I ran it
newdata_tbm1_tu2 <- newdata_tbm1_tu[(newdata_tbm1_tu$locationfactor %in% tbm1$data$locationfactor == TRUE),]

newdata_tbm1_tu2$distcoast <- NA
for (i in 1:nrow(newdata_tbm1_tu2)) {
    newdata_tbm1_tu2$distcoast[i] <- dist2coast$distcoast[which(newdata_tbm1_tu2$locationfactor[i] == dist2coast$locationfactor)]
} 

newdata_tbm1_nt <- expand_grid(tidedif = -6:6,
                               toolusers = c("Non-tool-users"),
                               locationfactor = unique(onlycap_tj$locationfactor[which(onlycap_tj$toolusers == "Non-tool-users")]))

# for now remove cameras that are not in the tbm1 when I ran it
newdata_tbm1_nt2 <- newdata_tbm1_nt[(newdata_tbm1_nt$locationfactor %in% tbm1$data$locationfactor == TRUE),]

newdata_tbm1_nt2$distcoast <- NA
for (i in 1:nrow(newdata_tbm1_nt2)) {
  newdata_tbm1_nt2$distcoast[i] <- dist2coast$distcoast[which(newdata_tbm1_nt2$locationfactor[i] == dist2coast$locationfactor)]
} 

newdata_tbm1_f <- rbind(newdata_tbm1_nt2, newdata_tbm1_tu2)
newdata_tbm1_f <- droplevels.data.frame(newdata_tbm1_f)

# use posterior smooth to get estimated respones 
predict_tbm1_f <- posterior_smooths(tbm1, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=toolusers,k=c(10,6),m=1)', newdata_tbm1_f)

# mean of each row is what I'm looking for
newdata_tbm1_f$fit_tooltide <- as.numeric(colMeans(predict_tbm1_f))
newdata_tbm1_f$min <- colQuantiles(predict_tbm1_f, prob = c(0.025))
newdata_tbm1_f$max <- colQuantiles(predict_tbm1_f, prob = c(0.975))

# with different lines for different distances to coast
ggplot(newdata_tbm1_f) + geom_line(aes(x = tidedif, y = fit_tooltide, group = distcoast, color = distcoast)) +
  labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() + facet_wrap(~toolusers)

newdata_tbm1_f$distance <- ifelse(newdata_tbm1_f$distcoast < 10, "<10", ifelse(newdata_tbm1_f$distcoast > 10 & newdata_tbm1_f$distcoast < 30, "10-30", "30-50"))
# for distcoast below 50 and with uncertainty
newdata_tbm1_f$toolusers <- as.factor(newdata_tbm1_f$toolusers)
newdata_tbm1_f$toolusers <- relevel(newdata_tbm1_f$toolusers, ref = "Tool-users")

ggplot(newdata_tbm1_f[newdata_tbm1_f$distcoast < 50,]) +   geom_ribbon(aes(x = tidedif, y = fit_tooltide, group = distcoast, fill = distance, ymin=min, ymax = max), alpha = 0.05) + 
  geom_line(aes(x = tidedif, y = fit_tooltide, group = distcoast, color = distance)) +
  labs(y = "Change in capuchin activity", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() + facet_wrap(~toolusers)

##### MODEL 2 AND 2A: ADDING SEASON, SPLIT BY TU/NTU #####
####
# tool users
tbm2 <- brm(n ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
            t2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
            s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], 
          knots = list(tidedif =c(-6,6)), chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
          control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2 <- add_criterion(tbm2, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
#saveRDS(tbm2, "tide_analysis/ModelRDS/tbm2_poisson.rds")
#tbm2 <- readRDS("tide_analysis/ModelRDS/tbm2_poisson.rds")

mcmc_plot(tbm2,type = "trace")
mcmc_plot(tbm2, type = "hist") #show histograms of the posterior distributions
mcmc_plot(tbm2) #plot posterior intervals
mcmc_plot(tbm2, type = "acf_bar")

summary(tbm2)
plot(tbm2)

# ce_tbm2 <- readRDS("tide_analysis/ModelRDS/ce_tbm2_prior.rds")
plot(conditional_effects(tbm2))

# cs_tbm2 <- readRDS("tide_analysis/ModelRDS/cs_tbm2_prior.rds")
plot(conditional_smooths(tbm2))

plot(conditional_smooths(tbm2))

## Checks
pp_check(tbm2, ndraw = 100) 
mcmc_plot(tbm2, variable = c("b_Intercept", "b_seasonFWet", "bs_t2tidedifdistcoast_1", "sds_slocationfactor_1"), type = "areas")
loo(tbm2)
loo_R2(tbm2)
bayes_R2(tbm2)

## Visualizing
## Option 1: Conditional_smooths and effects

toolusersplot <- plot(conditional_smooths(tbm2, rug = TRUE), plot = FALSE)[[2]]
# toolusersplot <- readRDS("tide_analysis/ModelRDS/toolusersplot_brms.rds")

# png("tide_analysis/ModelRDS/toolusersplot.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot.png", width = 12, height = 6))
ggplot(toolusersplot$data, aes(x = tidedif, y = distcoast, z = estimate__)) + geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + facet_wrap(~seasonF) +
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =20))
# dev.off()

toolusersseason <- plot(conditional_effects(tbm2), plot = FALSE)[[5]]
toolusersseason + labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(y = n, x = tidedif, group = seasonF, color = seasonF), bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE)

## Option 2: Predicting data
predict_tbm2 <- posterior_smooths(tbm2, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2$data$fit_seasontide <- as.numeric(colMedians(predict_tbm2))

d1_wet <- with(tbm2$data[tbm2$data$seasonF == "Wet",], interp(x = tidedif, y = distcoast, z = fit_seasontide, duplicate = "mean"))
d1_dry <-  with(tbm2$data[tbm2$data$seasonF == "Dry",], interp(x = tidedif, y = distcoast, z = fit_seasontide, duplicate = "mean"))

d2_wet <- melt(d1_wet$z, na.rm = TRUE)
names(d2_wet) <- c("x", "y", "fit")
d2_wet$tidedif <- d1_wet$x[d2_wet$x]
d2_wet$distcoast <- d1_wet$y[d2_wet$y]

d2_dry <- melt(d1_dry$z, na.rm = TRUE)
names(d2_dry) <- c("x", "y", "fit")
d2_dry$tidedif <- d1_dry$x[d2_dry$x]
d2_dry$distcoast <- d1_dry$y[d2_dry$y]

d2_dry$seasonF <- "Dry"
d2_wet$seasonF <- "Wet"

d2 <- rbind(d2_dry, d2_wet)
d2$seasonF <- as.factor(d2$seasonF)

# png("tide_analysis/ModelRDS/toolusersplot_pred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot_pred.png", width = 12, height = 6))
ggplot(data = d2, aes(x = tidedif, y = distcoast, z = fit)) +
  geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16), axis.text = element_text(size=14)) +
  facet_wrap(~seasonF)
# dev.off()

## plot of number of capuchins
## number of capuchins per sequence wet vs dry season
ncap <- plot(conditional_effects(tbm2), plot = FALSE)[[1]]
ncap + labs(y = "Average number of capuchins per sequence", x = "Season") + theme_bw() 

### non tool users
tbm2a <- brm(n  ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
               t2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
               s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], 
             knots = list(tidedif =c(-6,6)), chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
             control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2a <- add_criterion(tbm2a, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
# check add criterion, gives error longer object length is not a multiple of shorter object length
# saveRDS(tbm2a, "tide_analysis/ModelRDS/tbm2a_poisson.rds")
# tbm2a <- readRDS("tide_analysis/ModelRDS/tbm2a_poisson.rds")

mcmc_plot(tbm2a,type = "trace")
mcmc_plot(tbm2a, type = "hist") #show histograms of the posterior distributions
mcmc_plot(tbm2a) #plot posterior intervals
mcmc_plot(tbm2a, type = "acf_bar")

summary(tbm2a)
plot(conditional_smooths(tbm2a))
plot(conditional_effects(tbm2a))

nontoolusersplot <- plot(conditional_smooths(tbm2a, rug = TRUE), plot = FALSE)[[2]]

# png("tide_analysis/ModelRDS/nontoolusersplot.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/nontoolusersplot.png", width = 12, height = 6))
ggplot(nontoolusersplot$data, aes(x = tidedif, y = distcoast, z = exp(estimate__))) + geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + facet_wrap(~seasonF) +
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =20))
# dev.off()

ntoolusersseason <- plot(conditional_effects(tbm2a), plot = FALSE)[[5]]
ntoolusersseason + labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(y = n, x = tidedif, group = seasonF, color = seasonF), bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE)

predict_tbm2a <- posterior_smooths(tbm2a, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2a$data$fit_seasontide <- as.numeric(colMedians(predict_tbm2a))

d1a_wet <- with(tbm2a$data[tbm2a$data$seasonF == "Wet",], interp(x = tidedif, y = distcoast, z = fit_seasontide, duplicate = "mean"))
d1a_dry <-  with(tbm2a$data[tbm2a$data$seasonF == "Dry",], interp(x = tidedif, y = distcoast, z = fit_seasontide, duplicate = "mean"))

d2a_wet <- melt(d1a_wet$z, na.rm = TRUE)
names(d2a_wet) <- c("x", "y", "fit")
d2a_wet$tidedif <- d1a_wet$x[d2a_wet$x]
d2a_wet$distcoast <- d1a_wet$y[d2a_wet$y]

d2a_dry <- melt(d1a_dry$z, na.rm = TRUE)
names(d2a_dry) <- c("x", "y", "fit")
d2a_dry$tidedif <- d1a_dry$x[d2a_dry$x]
d2a_dry$distcoast <- d1a_dry$y[d2a_dry$y]

d2a_dry$seasonF <- "Dry"
d2a_wet$seasonF <- "Wet"

d2a <- rbind(d2a_dry, d2a_wet)
d2a$seasonF <- as.factor(d2a$seasonF)

# png("tide_analysis/ModelRDS/nontoolusersplot_pred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/nontoolusersplot_pred.png", width = 12, height = 6))
ggplot(data = d2a, aes(x = tidedif, y = distcoast, z = fit)) +
  geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +
  facet_wrap(~seasonF)
#dev.off()

###### TIME OF DAY ######
# tool users
tbm2_h <- brm(n ~ t2(hour, distcoast, bs = c("tp", "tp"), k = c(10, 6), full = TRUE) +
              t2(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
              s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], 
            chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
            control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2_h <- add_criterion(tbm2_h, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
#saveRDS(tbm2_h, "tide_analysis/ModelRDS/tbm2_h.rds")
# tbm2_h <- readRDS("tide_analysis/ModelRDS/tbm2_h.rds")

mcmc_plot(tbm2_h,type = "trace")
mcmc_plot(tbm2_h, type = "hist") #show histograms of the posterior distributions
mcmc_plot(tbm2_h) #plot posterior intervals
mcmc_plot(tbm2_h, type = "acf_bar")

summary(tbm2_h)
plot(conditional_effects(tbm2_h))
plot(conditional_smooths(tbm2_h))

## plot from predicting
predict_tbm2_h <- posterior_smooths(tbm2_h, smooth = 't2(hour,distcoast,bs=c("tp","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2_h$data$fit_seasonhour <- as.numeric(colMedians(predict_tbm2_h))

d1h_wet <- with(tbm2_h$data[tbm2_h$data$seasonF == "Wet",], interp(x = hour, y = distcoast, z = fit_seasonhour, duplicate = "mean"))
d1h_dry <-  with(tbm2_h$data[tbm2_h$data$seasonF == "Dry",], interp(x = hour, y = distcoast, z = fit_seasonhour, duplicate = "mean"))

d2h_wet <- melt(d1h_wet$z, na.rm = TRUE)
names(d2h_wet) <- c("x", "y", "fit")
d2h_wet$hour <- d1h_wet$x[d2h_wet$x]
d2h_wet$distcoast <- d1h_wet$y[d2h_wet$y]

d2h_dry <- melt(d1h_dry$z, na.rm = TRUE)
names(d2h_dry) <- c("x", "y", "fit")
d2h_dry$hour <- d1h_dry$x[d2h_dry$x]
d2h_dry$distcoast <- d1h_dry$y[d2h_dry$y]

d2h_dry$seasonF <- "Dry"
d2h_wet$seasonF <- "Wet"

d2h <- rbind(d2h_dry, d2h_wet)
d2h$seasonF <- as.factor(d2h$seasonF)

# png("tide_analysis/ModelRDS/toolusersplot_pred_hour.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot_pred_hour.png", width = 12, height = 6))
ggplot(data = d2h, aes(x = hour, y = distcoast, z = fit)) +
  geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hour of the day", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = hour, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +
  facet_wrap(~seasonF)
#dev.off()

# non tool users
tbm2a_h <- brm(n ~ t2(hour, distcoast, bs = c("tp", "tp"), k = c(10, 6), full = TRUE) +
                t2(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
                s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], 
              chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
              control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2a_h <- add_criterion(tbm2a_h, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
# saveRDS(tbm2a_h, "tide_analysis/ModelRDS/tbm2a_h.rds")
# tbm2a_h <- readRDS("tide_analysis/ModelRDS/tbm2a_h.rds")

mcmc_plot(tbm2a_h,type = "trace")
mcmc_plot(tbm2a_h, type = "hist") #show histograms of the posterior distributions
mcmc_plot(tbm2a_h) #plot posterior intervals
mcmc_plot(tbm2a_h, type = "acf_bar")

summary(tbm2a_h)
plot(conditional_effects(tbm2a_h))
plot(conditional_smooths(tbm2a_h))

nontoolusersplot_hour <- plot(conditional_smooths(tbm2a_h, rug = TRUE), plot = FALSE)[[2]]
#saveRDS(nontoolusersplot_hour, "tide_analysis/ModelRDS/nontoolusersplot_hour_brms.rds")
# nontoolusersplot <- readRDS("tide_analysis/ModelRDS/nontoolusersplot_brms.rds")

ggplot(nontoolusersplot_hour$data, aes(x = hour, y = distcoast, z = estimate__)) + geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hour of the day", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(x = hour, y = distcoast), alpha = 0.05, inherit.aes = FALSE) +  facet_wrap(~seasonF) +
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =20))

########### REDUCED SAMPLE ###########
## only agoutidata

#### Model 1: Reduced sample TU vs NTU
tbm1_r <- brm(n  ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
                t2(tidedif, distcoast, bs = c("cc", "tp"), by = toolusers, k = c(10, 6), m = 1) + toolusers +
                s(locationfactor, bs = "re"), family = poisson(),  knots = list(tidedif =c(-6,6)),  data = onlycap_tj[onlycap_tj$dataorigin == "agoutidata",], chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
              control = list(adapt_delta = 0.99, max_treedepth = 12), backend = "cmdstanr", prior = tidal_prior)

#tbm1_r <- add_criterion(tbm1_r, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99, max_treedepth = 12), backend = "cmdstanr", ndraws = 2000) 
#saveRDS(tbm1_r, "tide_analysis/ModelRDS/tbm1_r.rds")
# tbm1_r <- readRDS("tide_analysis/ModelRDS/tbm1_r.rds")

mcmc_plot(tbm1_r,type = "trace")
mcmc_plot(tbm1_r, type = "hist") #show histograms of the posterior distributions
mcmc_plot(tbm1_r) #plot posterior intervals
mcmc_plot(tbm1_r, type = "acf_bar")

summary(tbm1_r)
plot(tbm1_r)

# ce_tbm1_r <- readRDS("tide_analysis/ModelRDS/ce_tbm1_r.rds") # prior for truncated one, poisson for non truncated one
plot(ce_tbm1_r)

# cs_tbm1_r <- readRDS("tide_analysis/ModelRDS/cs_tbm1_r.rds")
plot(cs_tbm1_r)

plot(conditional_effects(tbm1_r, effects = "tidedif:toolusers", spaghetti = TRUE, ndraws = 200))

## Checks
pp_check(tbm1_r, ndraw = 100) 
pairs(tbm1_r)
loo(tbm1_r)
loo_R2(tbm1_r)
bayes_R2(tbm1_r)
mcmc_plot(tbm1_r, variable = c("b_Intercept", "b_toolusersToolMusers", "bs_t2tidedifdistcoast_1", "sds_slocationfactor_1"), type = "areas")

## Visualizing
## Contourplot like below for seasons
predict_tbm1_rp <- posterior_smooths(tbm1_r, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=toolusers,k=c(10,6),m=1)')

# mean of each column is what I'm looking for
tbm1_r$data$fit_tooltide <- as.numeric(colMedians(predict_tbm1_rp))

d1_rtu <- with(tbm1_r$data[tbm1_r$data$toolusers == "Tool-users",], interp(x = tidedif, y = distcoast, z = fit_tooltide, duplicate = "mean"))
d1_rntu <-  with(tbm1_r$data[tbm1_r$data$toolusers == "Non-tool-users",], interp(x = tidedif, y = distcoast, z = fit_tooltide, duplicate = "mean"))

d2_rtu <- melt(d1_rtu$z, na.rm = TRUE)
names(d2_rtu) <- c("x", "y", "fit")
d2_rtu$tidedif <- d1_rtu$x[d2_rtu$x]
d2_rtu$distcoast <- d1_rtu$y[d2_rtu$y]

d2_rntu <- melt(d1_rntu$z, na.rm = TRUE)
names(d2_rntu) <- c("x", "y", "fit")
d2_rntu$tidedif <- d1_rntu$x[d2_rntu$x]
d2_rntu$distcoast <- d1_rntu$y[d2_rntu$y]

d2_rtu$toolusers <- "Tool-users"
d2_rntu$toolusers <- "Non-tool-users"

d2_rt <- rbind(d2_rtu, d2_rntu)
d2_rt$toolusers <- as.factor(d2_rt$toolusers)
d2_rt$toolusers <- relevel(d2_rt$toolusers, ref = "Tool-users")

# png("tide_analysis/ModelRDS/tuvsntu_pred_r.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot_pred.png", width = 12, height = 6))
ggplot(data = d2_rt, aes(x = tidedif, y = distcoast, z = fit)) +
  geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$dataorigin == "agoutidata",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16), axis.text = element_text(size = 14)) +
  facet_wrap(~toolusers, scales = "free")
# dev.off()



### Model 2: Reduced sample NTU season
tbm2a_r <- brm(n  ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
                 t2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
                 s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users" & onlycap_tj$dataorigin == "agoutidata",], 
               knots = list(tidedif =c(-6,6)), chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
               control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2a_r <- add_criterion(tbm2a_r, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
# saveRDS(tbm2a_r, "tide_analysis/ModelRDS/tbm2a_r.rds")
# tbm2a_r <- readRDS("tide_analysis/ModelRDS/tbm2a_r.rds")

mcmc_plot(tbm2a_r,type = "trace")
mcmc_plot(tbm2a_r, type = "hist") #show histograms of the posterior distributions
mcmc_plot(tbm2a_r) #plot posterior intervals
mcmc_plot(tbm2a_r, type = "acf_bar")

summary(tbm2a_r)
plot(conditional_smooths(tbm2a_r))
plot(conditional_effects(tbm2a_r))
loo(tbm2a_r)
loo_R2(tbm2a_r)
bayes_R2(tbm2a_r)

predict_tbm2a_r <- posterior_smooths(tbm2a_r, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2a_r$data$fit_seasontide <- as.numeric(colMedians(predict_tbm2a_r))

d1a_rwet <- with(tbm2a_r$data[tbm2a_r$data$seasonF == "Wet",], interp(x = tidedif, y = distcoast, z = fit_seasontide, duplicate = "mean"))
d1a_rdry <-  with(tbm2a_r$data[tbm2a_r$data$seasonF == "Dry",], interp(x = tidedif, y = distcoast, z = fit_seasontide, duplicate = "mean"))

d2a_rwet <- melt(d1a_rwet$z, na.rm = TRUE)
names(d2a_rwet) <- c("x", "y", "fit")
d2a_rwet$tidedif <- d1a_rwet$x[d2a_rwet$x]
d2a_rwet$distcoast <- d1a_rwet$y[d2a_rwet$y]

d2a_rdry <- melt(d1a_rdry$z, na.rm = TRUE)
names(d2a_rdry) <- c("x", "y", "fit")
d2a_rdry$tidedif <- d1a_rdry$x[d2a_rdry$x]
d2a_rdry$distcoast <- d1a_rdry$y[d2a_rdry$y]

d2a_rdry$seasonF <- "Dry"
d2a_rwet$seasonF <- "Wet"

d2a_r <- rbind(d2a_rdry, d2a_rwet)
d2a_r$seasonF <- as.factor(d2a_r$seasonF)

# png("tide_analysis/ModelRDS/nontoolusersplot_r.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/nontoolusersplot_r.png", width = 12, height = 6))
ggplot(data = d2a_r, aes(x = tidedif, y = distcoast, z = fit)) +
  geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users" & onlycap_tj$dataorigin == "agoutidata",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +
  facet_wrap(~seasonF, scales = "free")
#dev.off()


### Model 3: Reducated sample NTU hour of day
tbm2a_rh <- brm(n ~ t2(hour, distcoast, bs = c("tp", "tp"), k = c(10, 6), full = TRUE) +
                  t2(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
                  s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users" & onlycap_tj$dataorigin == "agoutidata",], 
                chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
                control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2a_rh <- add_criterion(tbm2a_rh, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
# saveRDS(tbm2a_rh, "tide_analysis/ModelRDS/tbm2a_rh.rds")
#tbm2a_rh <- readRDS("tide_analysis/ModelRDS/tbm2a_rh.rds")

summary(tbm2a_rh)
loo(tbm2a_rh)
plot(conditional_effects(tbm2a_rh))
plot(conditional_smooths(tbm2a_rh))

## plot from predicting
predict_tbm2a_rh <- posterior_smooths(tbm2a_rh, smooth = 't2(hour,distcoast,bs=c("tp","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2a_rh$data$fit_seasonhour <- as.numeric(colMedians(predict_tbm2a_rh))

d1har_wet <- with(tbm2a_rh$data[tbm2a_rh$data$seasonF == "Wet",], interp(x = hour, y = distcoast, z = fit_seasonhour, duplicate = "mean"))
d1har_dry <-  with(tbm2a_rh$data[tbm2a_rh$data$seasonF == "Dry",], interp(x = hour, y = distcoast, z = fit_seasonhour, duplicate = "mean"))

d2har_wet <- melt(d1har_wet$z, na.rm = TRUE)
names(d2har_wet) <- c("x", "y", "fit")
d2har_wet$hour <- d1har_wet$x[d2har_wet$x]
d2har_wet$distcoast <- d1har_wet$y[d2har_wet$y]

d2har_dry <- melt(d1har_dry$z, na.rm = TRUE)
names(d2har_dry) <- c("x", "y", "fit")
d2har_dry$hour <- d1har_dry$x[d2har_dry$x]
d2har_dry$distcoast <- d1har_dry$y[d2har_dry$y]

d2har_dry$seasonF <- "Dry"
d2har_wet$seasonF <- "Wet"

d2har <- rbind(d2har_dry, d2har_wet)
d2har$seasonF <- as.factor(d2har$seasonF)

# png("tide_analysis/ModelRDS/nontoolusersplot_pred_hourred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/nontoolusersplot_pred_hourred.png", width = 12, height = 6))
ggplot(data = d2har, aes(x = hour, y = distcoast, z = fit)) +
  geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hour of the day", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users" & onlycap_tj$dataorigin == "agoutidata",], aes(x = hour, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +
  facet_wrap(~seasonF, scales = "free")
# dev.off()

########### DERIVATIVES OF GAMS #########

## run function below (now copied from Shauhin's script on 23.06.2022)
deriv_plot <- function (model, dimensions = 1, by = FALSE, term, main, eps, response = NULL, spaghetti=FALSE, rug = TRUE, confidence=95,output){
  require(dplyr)
  require(ggplot2)
  require(plotly)
  require(brms)
  
  ###model must be a brms model object
  ###dimensions should be the number of variables in your spline
  ###term is a character string of the smooth term, same syntax as used in the model
  ###main is a character string (or vector of characters equal to dimensions) of the predictor variable, must not be wrapped in a smooth function
  ###eps is the amount to offset the original data (or a vector of offsets equal to dimensions), to be differenced from original to calculate slope
  ###response is an optional character string indicating the response variable to use, only relevant in the multivariate case
  ###confidence is the confidence level used to calculate the posterior intervals
  ###The desired name of the resulting ggplot object 
  Response=response
  if(is.null(Response)){
    Response=model$formula$resp
  }
  
  if(length(names(model$data))>6){
    model$data=model$data[,c(1:6)]
  }
  
  upper=(50+(confidence/2))/100
  lower=(50-(confidence/2))/100
  
  newdat=model$data
  newdat_b=model$data
  newdat_c=model$data
  newdat_d=model$data
  
  ##for 2D smooth finite difference aprox something like this
  ##fxy(x,y)~(f(x+eps_h,y+eps_k)-f(x+eps_h,y-eps_k)-f(x-eps_h,y+eps_k)+f(x-eps_h,y-eps_k))/(4*eps_h*eps*k)
  if(dimensions > 1) {
    if(length(eps)==1){
      eps[2]=eps[1]
    }
    for(i in 1:dimensions) {
      newdat[,which(names(newdat)==main[i])]=newdat[,which(names(newdat)==main[i])]+eps[i] # h + K
      newdat_b[,which(names(newdat_b)==main[i])]=newdat_b[,which(names(newdat_b)==main[i])]-eps[i] # -h - K
      
    }
    
    
    #h - k
    newdat_c[,which(names(newdat_c)==main[1])]=newdat_c[,which(names(newdat_c)==main[1])]+eps[1] 
    newdat_c[,which(names(newdat_c)==main[2])]=newdat_c[,which(names(newdat_c)==main[2])]-eps[2] 
    
    #-h + k
    newdat_d[,which(names(newdat_d)==main[1])]=newdat_d[,which(names(newdat_d)==main[1])]-eps[1]  
    newdat_d[,which(names(newdat_d)==main[2])]=newdat_d[,which(names(newdat_d)==main[2])]+eps[2] 
    
    
  } else{
    newdat[,which(names(newdat)==main)]=newdat[,which(names(newdat)==main)]+eps
  } 
  
  if(dimensions > 1){
    #dir=posterior_smooths(model, smooth = term, resp=response)
    dir2=posterior_smooths(model, smooth = term, resp=response, newdata = newdat)
    dir2_b=posterior_smooths(model, smooth = term, resp=response, newdata = newdat_b)
    dir2_c=posterior_smooths(model, smooth = term, resp=response, newdata = newdat_c)
    dir2_d=posterior_smooths(model, smooth = term, resp=response, newdata = newdat_d)
    
    dir_model=(dir2-dir2_c-dir2_d+dir2_b)/(4*prod(eps))
    
    mean_der <- apply(dir_model,MARGIN = 2,FUN = mean)
    lower_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = lower)
    upper_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = upper)
    
    
    der_data = cbind(mean_der, lower_der, upper_der)
    
    for(i in 1:length(main)) {
      der_data = cbind(der_data, model$data[,which(names(model$data)==main[i])])
    }
    der_data <- as.data.frame(der_data)
    
    colnames(der_data)=c("mean","lower","upper", main[1:length(main)])
    
    if(is.null(by)==TRUE) {
      interpdat <- with(der_data, akima::interp(x = der_data[,4], y = der_data[,5], z = mean, duplicate = "mean"))
      interpdat2 <- reshape2::melt(interpdat$z, na.rm = TRUE)
      names(interpdat2) <- c("x", "y", "dir")
      interpdat2$main1 <- interpdat$x[interpdat2$x]
      interpdat2$main2 <- interpdat$y[interpdat2$y]
      interpdat_low <- with(der_data, akima::interp(x = der_data[,4], y = der_data[,5], z = lower, duplicate = "mean"))
      interpdat2_low <- reshape2::melt(interpdat_low$z, na.rm = TRUE)
      names(interpdat2_low) <- c("x", "y", "dir")
      interpdat2_low$main1 <- interpdat_low$x[interpdat2_low$x]
      interpdat2_low$main2 <- interpdat_low$y[interpdat2_low$y]
      interpdat_high <- with(der_data, akima::interp(x = der_data[,4], y = der_data[,5], z = upper, duplicate = "mean"))
      interpdat2_high <- reshape2::melt(interpdat_high$z, na.rm = TRUE)
      names(interpdat2_high) <- c("x", "y", "dir")
      interpdat2_high$main1 <- interpdat_high$x[interpdat2_high$x]
      interpdat2_high$main2 <- interpdat_high$y[interpdat2_high$y]
      interpdat2$upper=interpdat2_high$dir
      interpdat2$lower=interpdat2_low$dir
      interpdat2$threshold=0
    } else {
      # add by column to der_data
      # for now only set up for by variable with TWO LEVELS and in quite explicit/roundabout way
      der_data = cbind(der_data, model$data[,which(names(model$data)==by)])
      colnames(der_data)=c("mean","lower","upper", main[1:length(main)], by)
      
      # factor level 1
      der_data_by1 <- der_data[which(der_data[,6] == levels(der_data[,6])[1]),]
      interpdat_a <- with(der_data_by1, akima::interp(x = der_data_by1[,4], y = der_data_by1[,5], z = mean, duplicate = "mean"))
      interpdat_a2 <- reshape2::melt(interpdat_a$z, na.rm = TRUE)
      names(interpdat_a2) <- c("x", "y", "dir")
      interpdat_a2$main1 <- interpdat_a$x[interpdat_a2$x]
      interpdat_a2$main2 <- interpdat_a$y[interpdat_a2$y]
      interpdat_a_low <- with(der_data_by1, akima::interp(x = der_data_by1[,4], y = der_data_by1[,5], z = lower, duplicate = "mean"))
      interpdat_a2_low <- reshape2::melt(interpdat_a_low$z, na.rm = TRUE)
      names(interpdat_a2_low) <- c("x", "y", "dir")
      interpdat_a2_low$main1 <- interpdat_a_low$x[interpdat_a2_low$x]
      interpdat_a2_low$main2 <- interpdat_a_low$y[interpdat_a2_low$y]
      interpdat_a_high <- with(der_data_by1, akima::interp(x = der_data_by1[,4], y = der_data_by1[,5], z = upper, duplicate = "mean"))
      interpdat_a2_high <- reshape2::melt(interpdat_a_high$z, na.rm = TRUE)
      names(interpdat_a2_high) <- c("x", "y", "dir")
      interpdat_a2_high$main1 <- interpdat_a_high$x[interpdat_a2_high$x]
      interpdat_a2_high$main2 <- interpdat_a_high$y[interpdat_a2_high$y]
      interpdat_a2$upper=interpdat_a2_high$dir
      interpdat_a2$lower=interpdat_a2_low$dir
      interpdat_a2$threshold=0
      assign(paste(output, "1", sep = "_"),interpdat_a2, envir = parent.frame())
      
      
      # factor level 2
      der_data_by2 <- der_data[which(der_data[,6] == levels(der_data[,6])[2]),]
      interpdat_b <- with(der_data_by2, akima::interp(x = der_data_by2[,4], y = der_data_by2[,5], z = mean, duplicate = "mean"))
      interpdat_b2 <- reshape2::melt(interpdat_b$z, na.rm = TRUE)
      names(interpdat_b2) <- c("x", "y", "dir")
      interpdat_b2$main1 <- interpdat_b$x[interpdat_b2$x]
      interpdat_b2$main2 <- interpdat_b$y[interpdat_b2$y]
      interpdat_b_low <- with(der_data_by2, akima::interp(x = der_data_by2[,4], y = der_data_by2[,5], z = lower, duplicate = "mean"))
      interpdat_b2_low <- reshape2::melt(interpdat_b_low$z, na.rm = TRUE)
      names(interpdat_b2_low) <- c("x", "y", "dir")
      interpdat_b2_low$main1 <- interpdat_b_low$x[interpdat_b2_low$x]
      interpdat_b2_low$main2 <- interpdat_b_low$y[interpdat_b2_low$y]
      interpdat_b_high <- with(der_data_by2, akima::interp(x = der_data_by2[,4], y = der_data_by2[,5], z = upper, duplicate = "mean"))
      interpdat_b2_high <- reshape2::melt(interpdat_b_high$z, na.rm = TRUE)
      names(interpdat_b2_high) <- c("x", "y", "dir")
      interpdat_b2_high$main1 <- interpdat_b_high$x[interpdat_b2_high$x]
      interpdat_b2_high$main2 <- interpdat_b_high$y[interpdat_b2_high$y]
      interpdat_b2$upper=interpdat_b2_high$dir
      interpdat_b2$lower=interpdat_b2_low$dir
      interpdat_b2$threshold=0
      assign(paste(output, "2", sep = "_"),interpdat_b2, envir = parent.frame())
      
    }
    
    if(is.null(by)==TRUE){
      axx <- list(
        title = names(model$data)[3]
      )
      
      axy <- list(
        title = names(model$data)[4]
      )
      
      
      p <- plot_ly(interpdat2, x=~main1, y=~main2, 
                   z=~dir, intensity = ~dir,type="mesh3d") %>% 
        add_mesh(x=~main1, y=~main2, 
                 z=~upper, intensity = ~upper, opacity=0.30) %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~lower, intensity = ~lower, opacity=0.30)  %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~threshold, intensity = ~threshold, colorscale='Hot' )
      p=p%>% hide_colorbar()
      p <- p %>% layout(title = "Derivative",
                        scene = list(xaxis=axx, yaxis=axy,
                                     aspectmode='cube'))
      assign(output,p, envir = parent.frame())
      return(p)
    } else{ 
      axx <- list(
        title = names(model$data)[3]
      )
      
      axy <- list(
        title = names(model$data)[4]
      )
      
      p1 <- plot_ly(interpdat_a2, x=~main1, y=~main2, 
                    z=~dir, intensity = ~dir, scene= 'scene1', type="mesh3d") %>% 
        add_mesh(x=~main1, y=~main2, 
                 z=~upper, intensity = ~upper, opacity=0.30) %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~lower, intensity = ~lower, opacity=0.30)  %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~threshold, intensity = ~threshold, colorscale='Hot' )
      p1=p1%>% hide_colorbar()
      p1 <- p1 %>% layout(annotations = list(x = 0.2 , y = 0.95, text = paste(by, levels(der_data[,6])[1], sep = ": "),
                                             showarrow = F, xref='paper', yref='paper', font = list(size = 15)), showlegend = FALSE) 
      
      p2 <- plot_ly(interpdat_b2, x=~main1, y=~main2, 
                    z=~dir, intensity = ~dir, scene= 'scene2', type="mesh3d") %>% 
        add_mesh(x=~main1, y=~main2, 
                 z=~upper, intensity = ~upper, opacity=0.30) %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~lower, intensity = ~lower, opacity=0.30)  %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~threshold, intensity = ~threshold, colorscale='Hot' )
      p2=p2%>% hide_colorbar()
      p2 <- p2 %>% layout(annotations = list(x = 0.2 , y = 0.95, text = paste(by, levels(der_data[,6])[2], sep = ": "),
                                             showarrow = F, xref='paper', yref='paper', font = list(size = 15)), showlegend = FALSE) 
      
      pp <- subplot(p1, p2)
      pp <- pp %>% layout(title = paste("Derivative at confidence", confidence, sep = " "),
                          scene = list(xaxis=axx, yaxis=axy,
                                       aspectmode='cube'),
                          scene2 = list(xaxis=axx, yaxis=axy,
                                        aspectmode='cube'))
      assign(output,pp, envir = parent.frame())
      return(pp) 
    }
    
    
    
  } else{
    newdat=model$data
    newdat[,which(names(newdat)==main)]=newdat[,which(names(newdat)==main)]+eps
    dir=posterior_smooths(model, smooth = term, resp=response)
    dir2=posterior_smooths(model, smooth = term, resp=response, newdata = newdat)
    
    dir_model=(dir2-dir)/eps
    
    mean_der <- apply(dir_model,MARGIN = 2,FUN = mean)
    lower_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = lower)
    upper_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = upper)
    
    der_data=data.frame(mean_der) %>%
      cbind(lower_der) %>%
      cbind(upper_der) %>%
      cbind(model$data[,which(names(model$data)==main)])
    colnames(der_data)=c("mean","lower","upper","main")
    
    
    der_data$Significance=NA
    der_data$Significance[which(sign(der_data$lower)<0&sign(der_data$upper)<0)]="Significant"
    der_data$Significance[which(sign(der_data$lower)>0&sign(der_data$upper)>0)]="Significant"
    der_data$Significance[which(sign(der_data$lower)!=sign(der_data$upper))]="Not Significant"
    #sigranges=tapply(der_data$main,as.factor(der_data$Significance),range)
    
    der_data$Significance=NA
    der_data$Significance[which(sign(der_data$lower)<0&sign(der_data$upper)<0)]=-1
    der_data$Significance[which(sign(der_data$lower)>0&sign(der_data$upper)>0)]=1
    der_data$Significance[which(sign(der_data$lower)!=sign(der_data$upper))]=0
    #der_data=der_data[with(der_data, order(der_data[,4], der_data[,5])),]
    der_data$siglab <- with(rle(der_data$Significance), rep(cumsum(lengths >= 1),lengths))
    
    
    if(length(which(der_data$Significance!=0))==0){
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug,errorbar_args = list(alpha=0.1),plot=FALSE)
      if(is.null(response)){
        index=which(names(model_plot)==paste(main,sep=""))
      }else{
        index=which(names(model_plot)==paste(response,".",response,"_",main,sep=""))
      }    
      model_est <- as.data.frame(model_plot[[index]][[1]])
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug,errorbar_args = list(alpha=0.1),plot=FALSE)[[index]]
      
      index2=which(names(model_est)==main)
      colnames(model_est)[index2]="Main"
      
      model_plot2=model_plot+
        geom_line(data=model_est,aes(Main,estimate__,color=I("black")),size=1)+
        ylab(Response)+xlab(main)+
        theme_classic()+ guides(color="none")
      assign(output,model_plot2, envir = parent.frame())
      return(model_plot2)
      
    } else{
      der_data_SIG=der_data[which(der_data$Significance!=0),]
      
      sigranges=tapply(der_data_SIG$main,as.factor(der_data_SIG$siglab),range, na.rm=T)
      
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug,errorbar_args = list(alpha=0.1),plot=FALSE)
      if(is.null(response)){
        index=which(names(model_plot)==paste(main,sep=""))
      }else{
        index=which(names(model_plot)==paste(response,".",response,"_",main,sep=""))
      }    
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug, errorbar_args = list(alpha=0.1),plot=FALSE)[[index]]
      
      model_est <- as.data.frame(model_plot[[1]])
      model_est$Sig=NA
      model_est$Sig2=NA
      model_est$Sig2[which(model_est$Sig==0)]=.8
      model_est$Sig2[which(model_est$Sig==1)]=1.5
      index2=which(names(model_est)==main)
      colnames(model_est)[index2]="Main"
      
      
      for(i in 1:nrow(model_est)){
        for(j in 1:length(sigranges)){
          if(model_est$Main[i]>=sigranges[[j]][1] & model_est$Main[i]<sigranges[[j]][2]){
            model_est$Sig[i]=1
          }
        }
        
        
      }
      model_est$Sig[-which(model_est$Sig==1)]=0
      if(length(which(model_est$Sig==1))==0){
        model_est$Sig=0
      }
      model_plot2=model_plot+ 
        geom_line(data=model_est,aes(Main,estimate__,color=(Sig)),size=1)+
        scale_color_gradient2(low="black", mid="black",high="cyan" )+
        ylab(Response)+xlab(main)+
        theme_classic()+ guides(color="none")
      
      assign(output,model_plot2, envir = parent.frame())
      output2=gsub("_plot", "", output)
      output2=paste("VOI",output2,sep="_")
      if(length(which(model_est$Sig==1))>0){
        VOIdat=model_est[which(model_est$Sig==1),]
        assign(output2,VOIdat, envir = parent.frame())
      }
    }
    return(model_plot2)
    
  }
  
}

## follow up function to isolate ranges that are significant

##### Time to low tide

### tool users vs non tool users
## 50 confidence
deriv_plot(tbm1, dimensions = 2, by = c("toolusers"), term = 't2(tidedif, distcoast, bs = c("cc", "tp"), by = toolusers, k = c(10, 6), m = 1)', main = c("tidedif", "distcoast"),
           eps = 0.01, confidence = 50, output = "derivplot_tbm1_50")

## no regions that are reliably on one side of 0, already at such low confidence, so no need to proceed 
# means that the effect is not really supported

### tool users: dry vs wet season
## 50 confidence
deriv_plot(tbm2, dimensions = 2, by = c("seasonF"), term = 't2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("tidedif", "distcoast"), eps = 0.01, confidence = 50, output = "derivplot_tbm2season_50")
# save the data frames as rds so I don't have to keep running this
#saveRDS(derivplot_tbm2season_50_1, file = "tide_analysis/ModelRDS/derivplot_tbm2season_50_1.rds")
#saveRDS(derivplot_tbm2season_50_2, file = "tide_analysis/ModelRDS/derivplot_tbm2season_50_2.rds")
derivplot_tbm2season_50_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_50_1.rds")
derivplot_tbm2season_50_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_50_2.rds")

## 70 confidence
deriv_plot(tbm2, dimensions = 2, by = c("seasonF"), term = 't2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("tidedif", "distcoast"), eps = 0.01, confidence = 70, output = "derivplot_tbm2season_70")
# save the data frames as rds so I don't have to keep running this
#saveRDS(derivplot_tbm2season_70_1, file = "tide_analysis/ModelRDS/derivplot_tbm2season_70_1.rds")
#saveRDS(derivplot_tbm2season_70_2, file = "tide_analysis/ModelRDS/derivplot_tbm2season_70_2.rds")
derivplot_tbm2season_70_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_70_1.rds")
derivplot_tbm2season_70_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_70_2.rds")

## 90 confidence
deriv_plot(tbm2, dimensions = 2, by = c("seasonF"), term = 't2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("tidedif", "distcoast"), eps = 0.01, confidence = 90, output = "derivplot_tbm2season_90")

#saveRDS(derivplot_tbm2season_90_1, file = "tide_analysis/ModelRDS/derivplot_tbm2season_90_1.rds")
#saveRDS(derivplot_tbm2season_90_2, file = "tide_analysis/ModelRDS/derivplot_tbm2season_90_2.rds")
derivplot_tbm2season_90_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_90_1.rds")
derivplot_tbm2season_90_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_90_2.rds")

### non tool users: dry vs wet season
## 50 confidence
deriv_plot(tbm2a, dimensions = 2, by = c("seasonF"), term = 't2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("tidedif", "distcoast"), eps = 0.01, confidence = 50, output = "derivplot_tbm2aseason_50")

### no regions that are reliably on one side of 0, already at such low confidence, so no need to proceed
# means that the effect is not really supported

##### Hour of the day

### tool users: dry vs wet season
# 50 confidence
deriv_plot(tbm2_h, dimensions = 2, by = c("seasonF"), term = 't2(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour", "distcoast"), eps = 0.01, confidence = 50, output = "derivplot_tbm2hseason_50")
#saveRDS(derivplot_tbm2hseason_50_1, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_50_1.rds")
#saveRDS(derivplot_tbm2hseason_50_2, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_50_2.rds")
derivplot_tbm2hseason_50_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_50_1.rds")
derivplot_tbm2hseason_50_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_50_2.rds")

# 70 confidence
deriv_plot(tbm2_h, dimensions = 2, by = c("seasonF"), term = 't2(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour", "distcoast"), eps = 0.01, confidence = 70, output = "derivplot_tbm2hseason_70")
#saveRDS(derivplot_tbm2hseason_70_1, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_70_1.rds")
#saveRDS(derivplot_tbm2hseason_70_2, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_70_2.rds")
derivplot_tbm2hseason_70_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_70_1.rds")
derivplot_tbm2hseason_70_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_70_2.rds")


# 90 confidence
deriv_plot(tbm2_h, dimensions = 2, by = c("seasonF"), term = 't2(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour", "distcoast"), eps = 0.01, confidence = 90, output = "derivplot_tbm2hseason_90")
#saveRDS(derivplot_tbm2hseason_90_1, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_90_1.rds")
#saveRDS(derivplot_tbm2hseason_90_2, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_90_2.rds")
derivplot_tbm2hseason_90_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_90_1.rds")
derivplot_tbm2hseason_90_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_90_2.rds")


### non tool users: dry vs wet season
# 50 confidence
deriv_plot(tbm2a_h, dimensions = 2, by = c("seasonF"), term = 't2(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour", "distcoast"), eps = 0.01, confidence = 50, output = "derivplot_tbm2ahseason_50")

# 70 confidence
deriv_plot(tbm2a_h, dimensions = 2, by = c("seasonF"), term = 't2(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour", "distcoast"), eps = 0.01, confidence = 70, output = "derivplot_tbm2ahseason_70")

#### REDUCED SAMPLE
## Tbm1_reduced
deriv_plot(tbm1_r, dimensions = 2, by = c("toolusers"), term = 't2(tidedif, distcoast, bs = c("cc", "tp"), by = toolusers, k = c(10, 6), m = 1)', main = c("tidedif", "distcoast"),
           eps = 0.01, confidence = 50, output = "derivplot_tbm1_r_50")
#saveRDS(derivplot_tbm1_r_50_1, file = "tide_analysis/ModelRDS/derivplot_tbm1_r_50_1.rds")
#saveRDS(derivplot_tbm1_r_50_2, file = "tide_analysis/ModelRDS/derivplot_tbm1_r_50_2.rds")
derivplot_tbm1_r_50_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm1_r_50_1.rds")
derivplot_tbm1_r_50_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm1_r_50_2.rds")


# Tbm2a _reduced
deriv_plot(tbm2a_r, dimensions = 2, by = c("seasonF"), term = 't2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("tidedif", "distcoast"), eps = 0.01, confidence = 50, output = "derivplot_tbm2arseason_50")
#saveRDS(derivplot_tbm2arseason_50_1, file = "tide_analysis/ModelRDS/derivplot_tbm2a_r_50_1.rds")
#saveRDS(derivplot_tbm2arseason_50_2, file = "tide_analysis/ModelRDS/derivplot_tbm2a_r_50_2.rds")
derivplot_tbm2arseason_50_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2a_r_50_1.rds")
derivplot_tbm2arseason_50_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2a_r_50_2.rds")

# Tbm2a hour of day_reduced
deriv_plot(tbm2a_rh, dimensions = 2, by = c("seasonF"), term = 't2(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour", "distcoast"), eps = 0.01, confidence = 50, output = "derivplot_tbm2arhseason_50")
#saveRDS(derivplot_tbm2arhseason_50_1, file = "tide_analysis/ModelRDS/derivplot_tbm2a_rh_50_1.rds")
#saveRDS(derivplot_tbm2arhseason_50_2, file = "tide_analysis/ModelRDS/derivplot_tbm2a_rh_50_2.rds")
derivplot_tbm2arhseason_50_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2a_rh_50_1.rds")
derivplot_tbm2arhseason_50_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2a_rh_50_2.rds")


#### Creating overlay 2D contour plot #####
deriv_ranges <- function(der_data_50_1, der_data_50_2, der_data_70_1, der_data_70_2, der_data_90_1, der_data_90_2, factorlevels, modelname, seventy = TRUE, ninety = TRUE){
  # supply all derivative dataframes
  # levels of factor
  # name of model
  
  der_data_50_1$Significance <- ifelse(sign(der_data_50_1$lower) <0 & sign(der_data_50_1$upper)<0 | sign(der_data_50_1$lower) >0 & sign(der_data_50_1$upper)>0, 1, 0)
  der_data_50_2$Significance <- ifelse(sign(der_data_50_2$lower) <0 & sign(der_data_50_2$upper)<0 | sign(der_data_50_2$lower) >0 & sign(der_data_50_2$upper)>0, 1, 0)
  der_data_50_1$factor <- factorlevels[1]
  der_data_50_2$factor <- factorlevels[2]
  der_data_50_1$confidence <- 50
  der_data_50_2$confidence <- 50 
  
  if(seventy == TRUE){
    der_data_70_1$Significance <- ifelse(sign(der_data_70_1$lower) <0 & sign(der_data_70_1$upper)<0 | sign(der_data_70_1$lower) >0 & sign(der_data_70_1$upper)>0, 1, 0)
    der_data_70_2$Significance <- ifelse(sign(der_data_70_2$lower) <0 & sign(der_data_70_2$upper)<0 | sign(der_data_70_2$lower) >0 & sign(der_data_70_2$upper)>0, 1, 0)
    der_data_70_1$factor <- factorlevels[1]
    der_data_70_2$factor <- factorlevels[2]
    der_data_70_1$confidence <- 70
    der_data_70_2$confidence <- 70 
    }
  if(ninety==TRUE){
    der_data_90_1$Significance <- ifelse(sign(der_data_90_1$lower) <0 & sign(der_data_90_1$upper)<0 | sign(der_data_90_1$lower) >0 & sign(der_data_90_1$upper)>0, 1, 0)
    der_data_90_2$Significance <- ifelse(sign(der_data_90_2$lower) <0 & sign(der_data_90_2$upper)<0 | sign(der_data_90_2$lower) >0 & sign(der_data_90_2$upper)>0, 1, 0)
    der_data_90_1$factor <- factorlevels[1]
    der_data_90_2$factor <- factorlevels[2]
    der_data_90_1$confidence <- 90
    der_data_90_2$confidence <- 90
  }
  
  if(seventy==FALSE & ninety == FALSE){
    der_data <- rbind(der_data_50_1, der_data_50_2)
  }
  
  if(seventy == TRUE & ninety == FALSE){
    der_data <- rbind(der_data_50_1, der_data_50_2, der_data_70_1, der_data_70_2)
  }
  
  if(seventy == TRUE & ninety == TRUE){
    der_data <- rbind(der_data_50_1, der_data_50_2, der_data_70_1, der_data_70_2, der_data_90_1, der_data_90_2)
  }
  
assign(paste(modelname, "overlay", sep = "_"), der_data, envir = parent.frame())
  
}

### color scheme for confidence
confcol <- c("black","#3FC9BD", "#ff148d")


### TOOL USERS MODEL: TBM2
deriv_ranges(derivplot_tbm2season_50_1, derivplot_tbm2season_50_2, derivplot_tbm2season_70_1, derivplot_tbm2season_70_2, derivplot_tbm2season_90_1, derivplot_tbm2season_90_2, 
             factorlevels = c("Dry", "Wet"), modelname <- "tbm2", seventy = TRUE, ninety = TRUE)

#### need to figure out how to get them on the same scale. 
# facet_wrap works for the geom_contour but doesnt seem to work for the geom_tile
# if I use something like  shapefile instead of geom_tile this should be more doable
tbm2_dry <- ggplot() +
  geom_contour_filled(data = d2[d2$seasonF == "Dry",], aes(x = tidedif, y = distcoast, z = fit)) + scale_fill_viridis(option = "inferno", discrete = TRUE) + 
  geom_tile(data = tbm2_overlay[tbm2_overlay$factor == "Dry" & tbm2_overlay$Significance == 1,], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0) +
  theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change nr of capuchins", title = "Dry Season", color = "Confidence") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users" & onlycap_tj$seasonF == "Dry",], aes(x = tidedif, y = distcoast),alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
       legend.title = element_text(size =12), axis.text = element_text(size=12)) + scale_color_manual(values = confcol)

#png("tide_analysis/ModelRDS/tooluserplot_dry.png", width = 8, height = 6, units = 'in', res = 300)
tbm2_dry
#dev.off()

tbm2_wet <- ggplot() +
  geom_contour_filled(data = d2[d2$seasonF == "Wet",], aes(x = tidedif, y = distcoast, z = fit)) + scale_fill_viridis(option = "inferno", discrete = TRUE) + 
  geom_tile(data = tbm2_overlay[tbm2_overlay$factor == "Wet" & tbm2_overlay$Significance == 1,], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0) +
  theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change nr of capuchins", title = "Wet Season", color = "Confidence") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users" & onlycap_tj$seasonF == "Wet",], aes(x = tidedif, y = distcoast),alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12)) + scale_color_manual(values = confcol)

#png("tide_analysis/ModelRDS/tooluserplot_wet.png", width = 8, height = 6, units = 'in', res = 300)
tbm2_wet
#dev.off()

## once I have common legend can then arrange them using grid arrange
# so option 1: get facet wrap to work with geom_tile
# option 2: make contourplot have common legend in separate plots and use grid arrange
# NEED TO PICK BETTER CONTRASTING COLORS! 

require(gridExtra) 
grid.arrange(tbm2_dry, tbm2_wet, nrow = 2)

## look at minimum convex polygon instead of tiles to show it nicer.
ggplot(data = tbm2_overlay[tbm2_overlay$Significance == 1 & tbm2_overlay$confidence == 50 & tbm2_overlay$seasonF == "Dry",], aes(x = main1, y = main2)) + geom_tile(alpha = 0, color = "black") + theme_bw() + scale_colour_discrete(na.translate = F) + theme(panel.grid = element_blank()) 

#### GENERAL MODEL REDUCED: TBM1_R
deriv_ranges(derivplot_tbm1_r_50_1, derivplot_tbm1_r_50_2, factorlevels = c("Non-tool-users", "Tool-users"), modelname = "tbm1_r", seventy = FALSE, ninety = FALSE)

tbm1_r_tu <- ggplot() +
  geom_contour_filled(data = d2_rt[d2_rt$toolusers == "Tool-users",], aes(x = tidedif, y = distcoast, z = fit)) + scale_fill_viridis(option = "inferno", discrete = TRUE) + 
  geom_tile(data = tbm1_r_overlay[tbm1_r_overlay$factor == "Tool-users" & tbm1_r_overlay$Significance == 1,], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0) +
  theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change nr of capuchins", title = "Tool-users", color = "Confidence") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = tidedif, y = distcoast),alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12)) + scale_color_manual(values = confcol)

#png("tide_analysis/ModelRDS/tbm1_r_tooluserplot.png", width = 8, height = 6, units = 'in', res = 300)
tbm1_r_tu
#dev.off()

tbm1_r_ntu <- ggplot() +
  geom_contour_filled(data = d2_rt[d2_rt$toolusers == "Non-tool-users",], aes(x = tidedif, y = distcoast, z = fit)) + scale_fill_viridis(option = "inferno", discrete = TRUE) + 
  geom_tile(data = tbm1_r_overlay[tbm1_r_overlay$factor == "Non-tool-users" & tbm1_r_overlay$Significance == 1,], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0) +
  theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change nr of capuchins", title = "Non-tool-users", color = "Confidence") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users" & onlycap_tj$dataorigin == "agoutidata",], aes(x = tidedif, y = distcoast),alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12)) + scale_color_manual(values = confcol)

#png("tide_analysis/ModelRDS/tbm1_r_ntooluserplot.png", width = 8, height = 6, units = 'in', res = 300)
tbm1_r_ntu
# dev.off()

#### NON TOOL USERS REDUCED TBM2AR
deriv_ranges(derivplot_tbm2arseason_50_1, derivplot_tbm2arseason_50_2, factorlevels = c("Dry", "Wet"), modelname = "tbm2a_r", seventy = FALSE, ninety = FALSE)

tbm2ar_dry <- ggplot() +
  geom_contour_filled(data = d2a_r[d2a_r$seasonF == "Dry",], aes(x = tidedif, y = distcoast, z = fit)) + scale_fill_viridis(option = "inferno", discrete = TRUE) + 
  geom_tile(data = tbm2a_r_overlay[tbm2a_r_overlay$factor == "Dry" & tbm2a_r_overlay$Significance == 1,], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0) +
  theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change nr of capuchins", title = "Dry Season", color = "Confidence") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users" & onlycap_tj$seasonF == "Dry" & onlycap_tj$dataorigin == "agoutidata",], aes(x = tidedif, y = distcoast),alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12)) + scale_color_manual(values = confcol)

#png("tide_analysis/ModelRDS/tbm2a_r_dryplot.png", width = 8, height = 6, units = 'in', res = 300)
tbm2ar_dry
#dev.off()

tbm2ar_wet <- ggplot() +
  geom_contour_filled(data = d2a_r[d2a_r$seasonF == "Wet",], aes(x = tidedif, y = distcoast, z = fit)) + scale_fill_viridis(option = "inferno", discrete = TRUE) + 
  geom_tile(data = tbm2a_r_overlay[tbm2a_r_overlay$factor == "Wet" & tbm2a_r_overlay$Significance == 1,], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0) +
  theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change nr of capuchins", title = "Wet Season", color = "Confidence") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users" & onlycap_tj$seasonF == "Wet" & onlycap_tj$dataorigin == "agoutidata",], aes(x = tidedif, y = distcoast),alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12)) + scale_color_manual(values = confcol)


#png("tide_analysis/ModelRDS/tbm2a_r_wetplot.png", width = 8, height = 6, units = 'in', res = 300)
tbm2ar_wet
#dev.off()

### TOOL USERS MODELHOUR OF DAY: TBM2_h
deriv_ranges(derivplot_tbm2hseason_50_1, derivplot_tbm2hseason_50_2, derivplot_tbm2hseason_70_1, derivplot_tbm2hseason_70_2, derivplot_tbm2hseason_90_1, derivplot_tbm2hseason_90_2, 
             factorlevels = c("Dry", "Wet"), modelname <- "tbm2_h", seventy = TRUE, ninety = TRUE)

#### need to figure out how to get them on the same scale. 
# facet_wrap works for the geom_contour but doesnt seem to work for the geom_tile
# if I use something like  shapefile instead of geom_tile this should be more doable
tbm2_h_dry <- ggplot() +
  geom_contour_filled(data = d2h[d2h$seasonF == "Dry",], aes(x = hour, y = distcoast, z = fit)) + scale_fill_viridis(option = "inferno", discrete = TRUE) + 
  geom_tile(data = tbm2_h_overlay[tbm2_h_overlay$factor == "Dry" & tbm2_h_overlay$Significance == 1,], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0) +
  theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hour of day", y = "Distance to coast (m)", fill = "Change nr of capuchins", title = "Dry Season", color = "Confidence") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users" & onlycap_tj$seasonF == "Dry",], aes(x = hour, y = distcoast),alpha = 1, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12)) + scale_color_manual(values = confcol)

#png("tide_analysis/ModelRDS/tooluserploth_dry.png", width = 8, height = 6, units = 'in', res = 300)
tbm2_h_dry
#dev.off()

tbm2_h_wet <- ggplot() +
  geom_contour_filled(data = d2h[d2h$seasonF == "Wet",], aes(x = hour, y = distcoast, z = fit)) + scale_fill_viridis(option = "inferno", discrete = TRUE) + 
  geom_tile(data = tbm2_h_overlay[tbm2_h_overlay$factor == "Wet" & tbm2_h_overlay$Significance == 1,], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0) +
  theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hour of day", y = "Distance to coast (m)", fill = "Change nr of capuchins", title = "Wet Season", color = "Confidence") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users" & onlycap_tj$seasonF == "Wet",], aes(x = hour, y = distcoast),alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12)) + scale_color_manual(values = confcol)

#png("tide_analysis/ModelRDS/tooluserploth_wet.png", width = 8, height = 6, units = 'in', res = 300)
tbm2_h_wet
#dev.off()

## fixed having the tiles facet_wrapped! Need to create them separately for each season
tbm2_h_overlay$seasonF <- factor(tbm2_h_overlay$factor, levels = c("Dry", "Wet"))
tbm2_hplot <- ggplot() +
  geom_contour_filled(data = d2h, aes(x = hour, y = distcoast, z = fit)) + scale_fill_viridis(option = "inferno", discrete = TRUE) + 
  geom_tile(data = tbm2_h_overlay[tbm2_h_overlay$Significance == 1 & tbm2_h_overlay$seasonF == "Dry",], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0, size = 0.6) +
  geom_tile(data = tbm2_h_overlay[tbm2_h_overlay$Significance == 1 & tbm2_h_overlay$seasonF == "Wet",], aes(x = main1, y = main2, color = as.factor(confidence)), alpha = 0, size = 0.6) +
  theme_bw() + theme(panel.grid = element_blank(), aspect.ratio =  1)  +
  labs(x = "Hour of day", y = "Distance to coast (m)", fill = "Change nr of capuchins", color = "Confidence") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = hour, y = distcoast),alpha = 1, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12)) + scale_color_manual(values = confcol) + facet_wrap(~seasonF)

png("tide_analysis/ModelRDS/tooluserploth.png", width = 12, height = 10, units = 'in', res = 300)
tbm2_hplot
dev.off()

head(tbm2_h_overlay)
head(d2h)
tbm2_hmerge <- left_join(d2h, tbm2_h_overlay, by = c("hour" = "main1", "distcoast" = "main2"))
head(tbm2_hmerge)

### try to get grayscale heatmap of whole contour plot and then on it in color parts that are significant (e.g. at 70 percent confidence)
# have problems that they can't have several scale_fill in one ggplot. But seems like there are ways around this so google it. 
ggplot() +
  geom_contour_filled(data = tbm2_hmerge, aes(x = hour, y = distcoast, z = fit)) + scale_fill_grey() +
  geom_contour_filled(data = tbm2_hmerge[tbm2_hmerge$confidence == 70 & tbm2_hmerge$Significance == 1,], aes(x = hour, y = distcoast, z = fit)) +
                      scale_fill_viridis(option = "inferno", discrete = TRUE) +facet_wrap(~seasonF.x)


ggplot() +
  geom_contour(data = tbm2_hmerge, aes(x = hour, y = distcoast, z = fit)) + scale_colour_grey() +
  geom_contour_filled(data = tbm2_hmerge[tbm2_hmerge$confidence == 70 & tbm2_hmerge$Significance == 1,], aes(x = hour, y = distcoast, z = fit)) +
  scale_fill_viridis(option = "inferno", discrete = TRUE) +facet_wrap(~seasonF.x)

## make heatmap grayscale or have alpha vary depending on significance


#### Descriptives for presentation and paper ####
# How many camera trapping days
# need to add in the zero's

tidaldays <- onlycap_tj
tidaldays$dayloc <- paste(tidaldays$locationfactor, tidaldays$seqday, sep = " ")
tidaldays2 <- tidaldays[!duplicated(tidaldays$dayloc),]

# make overview of deployments we have and their start and end days
locations_t <- data.frame(uniqueloctag = unique(onlycap_tj$uniqueloctag)) 
locations_t <- left_join(locations_t, onlycap_tj[,c("uniqueloctag", "dep_start", "dep_end", "locationfactor", "toolusers")], by = "uniqueloctag")
locations_t <- locations_t[!duplicated(locations_t$uniqueloctag),]
# take time off and keep just date variable
locations_t$dep_startday <- as.Date(format(locations_t$dep_start, "%Y-%m-%d"))
locations_t$dep_endday <- as.Date(format(locations_t$dep_end, "%Y-%m-%d"))
# calculate days in each deployment (round up)
locations_t$dep_days <- ceiling(difftime(locations_t$dep_end, locations_t$dep_start, units = c("days")))
# number of rows in the tidaldays2 dataframe (so how many days we have)
for (i in 1:nrow(locations_t)) {
  locations_t$nrow[i] <- nrow(tidaldays2[tidaldays2$uniqueloctag == locations_t$uniqueloctag[i],])
}

locations_t2 <- aggregate(locations_t$dep_days, list(locationfactor  = locations_t$locationfactor, toolusers = locations_t$toolusers), FUN = sum)

sum(locations_t$dep_days[locations_t$toolusers == "Tool-users"])
sum(locations_t2$x)

# How many locations
nrow(locations_t2)
ftable(locations_t2$toolusers)
# Average number of trapping days per location
summary(as.numeric(locations_t2$x))
# average number of trapping days per deployment
summary(as.numeric(locations_t$dep_days))
# number of deployments per location
max(as.matrix(ftable(locations_t$locationfactor)))
mean(as.matrix(ftable(locations_t$locationfactor)))

## number of capuchins per sequence
# did we see capuchins on all cameras
agoutiselect_tj <- agoutiselect_t[agoutiselect_t$island == "Jicaron" & agoutiselect_t$locationfactor != "CEBUS-03",]
table(agoutiselect_tj$capuchin, agoutiselect_tj$uniqueloctag)
mean(onlycap_tj$n[which(onlycap_tj$toolusers == "Tool-users")])
max(onlycap_tj$n)

## reduced sample
tidaldays_r <- onlycap_tj[onlycap_tj$dataorigin == "agoutidata",]
tidaldays_r$dayloc <-  paste(tidaldays_r$locationfactor, tidaldays_r$seqday, sep = " ")
tidaldays2_r <- tidaldays_r[!duplicated(tidaldays_r$dayloc),]

# make overview of deployments we have and their start and end days
locations_tr <- data.frame(uniqueloctag = unique(onlycap_tj$uniqueloctag[which(onlycap_tj$dataorigin == "agoutidata")])) 
locations_tr <- left_join(locations_tr, onlycap_tj[,c("uniqueloctag", "dep_start", "dep_end", "locationfactor", "toolusers")], by = "uniqueloctag")
locations_tr <- locations_tr[!duplicated(locations_tr$uniqueloctag),]
# take time off and keep just date variable
locations_tr$dep_startday <- as.Date(format(locations_tr$dep_start, "%Y-%m-%d"))
locations_tr$dep_endday <- as.Date(format(locations_tr$dep_end, "%Y-%m-%d"))
# calculate days in each deployment (round up)
locations_tr$dep_days <- ceiling(difftime(locations_tr$dep_end, locations_tr$dep_start, units = c("days")))
# number of rows in the tidaldays2 dataframe (so how many days we have)
for (i in 1:nrow(locations_tr)) {
  locations_tr$nrow[i] <- nrow(tidaldays2_r[tidaldays2_r$uniqueloctag == locations_tr$uniqueloctag[i],])
}

locations_tr2 <- aggregate(locations_tr$dep_days, list(locationfactor  = locations_tr$locationfactor, toolusers = locations_tr$toolusers), FUN = sum)

sum(locations_tr$dep_days[locations_tr$toolusers == "Non-tool-users"])
sum(locations_tr2$x)

# How many locations
nrow(locations_tr2)
ftable(locations_tr2$toolusers)
# Average number of trapping days per location
summary(as.numeric(locations_tr2$x))
# average number of trapping days per deployment
summary(as.numeric(locations_tr$dep_days))
# number of deployments per location
max(as.matrix(ftable(locations_tr$locationfactor)))
mean(as.matrix(ftable(locations_tr$locationfactor)))




########## TEMPERATURE ########
# First need to identify cameras that are definitely wrong
# per deployment, plot temperature per hour of the day with color of the camera location
plot(onlycap_tj$seq_start, onlycap_tj$temperature)

ggplot(data = onlycap_tj[str_detect(onlycap_tj$uniqueloctag, "R6") == TRUE,], aes(x = seq_start, y = temperature, group = locationfactor, color = locationfactor)) +
  geom_point()
# in R1: Survey CEBUS-01-01 pops out with very high values (~45 degrees)
# in R2: Everything seems alright and pretty consistent
# in R3: Also seems alright and consistent
# in R4: CEBUS-09 goes up to 50 degrees a few times, also SURVEY-CEBUS-15-04 goes up a few times
# in R5: CEBUS-01 seems broken (goes up to 50 degrees). CEBUS-04 has spike in 50's once.
# in R6: CEBUS-04 goes above 40 a few times, but not very far

# variation in cameras within one day
ggplot(data = onlycap_tj[onlycap_tj$seqday == "2018-02-18",], aes(x = hour, y = temperature, group = locationfactor, color = locationfactor)) +
  geom_point()

# does distance to coast matter a lot?
# consider temperature  vs distance to coast (see if it changes within hour)
ggplot(data = onlycap_tj[onlycap_tj$seqday == "2018-02-18",], aes(x = hour, y = temperature, color = distcoast)) +
  geom_point()
ggplot(data = onlycap_tj[str_detect(onlycap_tj$uniqueloctag, "R1") == TRUE,], aes(x = hour, y = temperature, color = distcoast)) +
  geom_point()
# doesn't seem to matter that much

# difference jicaron tool use and non tool use
ggplot(data = onlycap_tj[str_detect(onlycap_tj$uniqueloctag, "R6") == TRUE,], aes(x = hour, y = temperature, color = toolusers)) +
  geom_point()

## Filter out wrong ones, then get to average temperature per seqday per hour (for sequences that are present)
# R1
onlycap_tj[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-01-01-R1", c("seqday","hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-01-01-R1",], aes(x = hour, y = temperature)) +  geom_point()
# is likely due to camera being in the direct sun, is at hotter part of the day
# but 2017-06-01  at  7 being 37 degrees is likely wrong 

# R4
onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-09-R4",c("hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-09-R4",], aes(x = hour, y = temperature)) +  geom_point()
onlycap_tj[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4",c("seqday", "hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4",], aes(x = hour, y = temperature)) +  geom_point()
# seems to be that at 2018-07-05 at 8 AM the estimates are all very high (although at 7 they are around 26 degrees, now suddenly around 30-40). Take average of 7 AM?

# R5
onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-01-R5", c("seqday", "hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-01-R5",], aes(x = hour, y = temperature)) +  geom_point()
# seems that on 2018-09-07 have many of the over 40 values, also in the 50s. Fix by setting over 40's to NA?

onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-04-R5", c("seqday", "hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-04-R5",], aes(x = hour, y = temperature)) +  geom_point()

# R6
onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-04-R6", c("seqday", "hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-04-R6",], aes(x = hour, y = temperature)) +  geom_point()

### Filter out obviously wrong ones

# create new temperature variable, set wrong cameras to NA and wrong temperatures (>40) to NA
onlycap_tj$temp <- ifelse(onlycap_tj$temperature > 40, NA, onlycap_tj$temperature)
onlycap_tj$temp[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4" & onlycap_tj$seqday == "2018-07-05" & hour == 8,]
# correct the time at 2018-07-05 in SURVEY-CEBUS-15-04-R4
onlycap_tj$temp[which(onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4" & onlycap_tj$seqday == "2018-07-05" & onlycap_tj$hour == 8)] <-
  median(onlycap_tj$temp[which(onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4" & onlycap_tj$seqday == "2018-07-05" & onlycap_tj$hour == 7)])

### OPTION 1: Run in this form (or impute data). Keep variation
tidetemp_m1 <- gam(n ~ te(tidedif, distcoast, bs = c("tp", "tp"), k = c(10, 6)) +
                     te(tidedif, distcoast, temp, bs = c("tp", "tp"), k = c(10,6), m = 1) +
                     s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], method = "REML",
                   select = TRUE)

summary(tidetemp_m1)
draw(tidetemp_m1)

### OPTION 2: Take average temperature of all cameras active at that date and hour (lose variation)


# aggregate by seqday, hour, temperature (mean)


# replace NA's with temp at same hour on previous day

# how to deal with Claudio's data? 

# add temp2 to dataframe by leftjoining on seqday + hour









# standardize within cameras vs get average per region (e.g. tool users) or whole island even?





####
#### ACTIVITY TOOL USERS VS NON TOOL USERS ####
####
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

########################################
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

