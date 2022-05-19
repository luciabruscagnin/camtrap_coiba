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
cldata$toolusers <- factor(cldata$tool_site, levels = c(0,1), labels = c("Non-tool-users", "Tool-users")) 
cldata$picksetup <- ifelse(cldata$seqday == date(cldata$dep_start) | cldata$seqday == date(cldata$dep_end), 1, 0)
cldata$dataorigin <- "cldata"

# add duration (from Claudio's other dataset)
cldata3 <- read.csv("tide_analysis/S1-Data-CapuchinFiveSites.csv") # date is in days since january 1st 1900
# strip dataframe to only data we need (frames with capuchins)
cldata3 <- cldata3[which(cldata3$Species_Name_Latin == "Cebus capucinus imitator") & str_detect(cldata3$Camera_Site, "Coiba") == TRUE, c("Image_Sequence_ID", "Duration")]
cldata3$Image_Sequence_ID <- as.integer(cldata3$Image_Sequence_ID)

cldata <- left_join(cldata, cldata3, by = "Image_Sequence_ID")
cldata[which(is.na(cldata$Duration) == TRUE & cldata$capuchin == 1),]
# have two NA's now 
# need to ask Claudio what this is and see if I can find the duration for these 
cldata$seq_length <- round(cldata$Duration * 60,1) # I think it's in minutes so I'm making it in seconds like our data

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
tidal_prior <- c(prior(normal(1, 2), class = Intercept, lb = 1),
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

## Model 1: number of capuchins by tidedif (not absolute) and split by toolusers, with locationfactor as random effect and distance to coast
####
tbm1 <- brm(n | trunc(lb=1) ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
              t2(tidedif, distcoast, bs = c("cc", "tp"), by = toolusers, k = c(10, 6), m = 1) + toolusers +
              s(locationfactor, bs = "re"), family = poisson(),  knots = list(tidedif =c(-6,6)),  data = onlycap_tj, chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
            control = list(adapt_delta = 0.99, max_treedepth = 12), backend = "cmdstanr", prior = tidal_prior)

#tbm1 <- add_criterion(tbm1, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99, max_treedepth = 12), backend = "cmdstanr", ndraws = 2000) 
# saveRDS(tbm1, "tide_analysis/ModelRDS/tbm1_ztpois.rds")
# tbm1 <- readRDS("tide_analysis/ModelRDS/tbm1_ztpois.rds")
# then can do
loo(tbm1)
loo_R2(tbm1)
bayes_R2(tbm1)
# loo r squared etc 
prior_summary(tbm1)
summary(tbm1)
plot(tbm1)

ce_tbm1 <- conditional_effects(tbm1)
# saveRDS(ce_tbm1, "tide_analysis/ModelRDS/ce_tbm1.rds")
# ce_tbm1 <- readRDS("tide_analysis/ModelRDS/ce_tbm1.rds")
plot(ce_tbm1)

cs_tbm1 <- conditional_smooths(tbm1)
# saveRDS(cs_tbm1, "tide_analysis/ModelRDS/cs_tbm1.rds")
# cs_tbm1 <- readRDS("tide_analysis/ModelRDS/cs_tbm1.rds")
plot(cs_tbm1)

plot(conditional_effects(tbm1, effects = "tidedif:toolusers", spaghetti = TRUE, ndraws = 200))

plot(marginal_smooths(tbm1))
pp_check(tbm1, ndraw = 100) 
plot(tbm1)
pairs(tbm1)

mcmc_plot(tbm1, variable = c("b_Intercept", "b_toolusersToolMusers", "bs_t2tidedifdistcoast_1", "sds_slocationfactor_1"), type = "areas")

distcoastplot <- plot(conditional_smooths(tbm1, rug = TRUE, int_conditions = list(toolusers = "Tool-users")), plot = FALSE)[[2]]
# saveRDS(distcoastplot, "tide_analysis/ModelRDS/distcoastplot_brms.rds")
# distcoastplot <- readRDS("tide_analysis/ModelRDS/distcoastplot_brms.rds")

distcoastplot + ggplot2::ylim(0, 65) + theme_bw() + theme(panel.grid = element_blank()) +  labs(x = "Hours to nearest low tide (0)", y = "Distance to coast (m)", title = "Tool users") +
  guides(color=guide_legend(title="Change in number of capuchins per sequence"))
# if we want to fill it etc we need to predict or get fit out of brms object and make it ourselves with geom_contour
ggplot(distcoastplot$data, aes(x = tidedif, y = distcoast, z = estimate__)) + geom_contour_filled() + ylim(0,65) + scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme_bw() + theme(panel.grid = element_blank()) + 
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", title = "Tool users", fill = "Change in number of capuchins per sequence") +
  geom_rug(data = onlycap_tj, aes(x = tidedif, y = distcoast), inherit.aes = FALSE) 

#tidedif by tool users vs non tool users
tidalcapstu <- plot(conditional_effects(tbm1), plot = FALSE)[[5]]
tidalcapstu + labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj, aes(y = n, x = tidedif, group = toolusers, color = toolusers), bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE)



# or can compute posterior predictions with posterior_epred
# and plot contourplot from that

## predicting fictional cameras distances to coast
newdata_tbm1 <- expand_grid(tidedif = -6:6,
                            toolusers = c("Tool-users", "Non-tool-users"),
                            distcoast = c(1,20, 40))

# use posterior smooth to get estimated respones 
predict_tbm1 <- posterior_smooths(tbm1, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=toolusers,k=c(10,6),m=1)', newdata_tbm1)

# mean of each row is what I'm looking for
newdata_tbm1$fit_tooltide <- as.numeric(colMeans(predict_tbm1))
newdata_tbm1$min <- colQuantiles(predict_tbm1, prob = c(0.05))
newdata_tbm1$max <- colQuantiles(predict_tbm1, prob = c(0.95))
newdata_tbm1$distcoastF <- as.factor(newdata_tbm1$distcoast)

ggplot(newdata_tbm1) + geom_line(aes(x = tidedif, y = fit_tooltide, group = distcoastF, color = distcoastF)) +
  geom_ribbon(aes(x = tidedif, y = fit_tooltide, group = distcoastF, fill = distcoastF, ymin=min, ymax = max), alpha = 0.2) + 
  labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() + facet_wrap(~toolusers)


## predicting on existing data
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

ggplot(newdata_tbm1_f) + geom_smooth(method = "gam", aes(x = tidedif, y = exp(fit_tooltide), group = toolusers, color = toolusers), se = FALSE) +
  labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() 

# with different lines for different distances to coast
ggplot(newdata_tbm1_f) + geom_line(aes(x = tidedif, y = exp(fit_tooltide), group = distcoast, color = distcoast)) +
  labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() + facet_wrap(~toolusers)

## contourplot like below for seasons
predict_tbm1_p <- posterior_smooths(tbm1, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=toolusers,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm1$data$fit_tooltide <- as.numeric(colMeans(predict_tbm1_p))

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

# on real scale
# png("tide_analysis/ModelRDS/tuvsntu_pred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot_pred.png", width = 12, height = 6))
ggplot(data = d2_t, aes(x = tidedif, y = distcoast, z = exp(fit))) +
  geom_contour_filled(bins = 11) + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Number of capuchins") +
  geom_rug(data = onlycap_tj, aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +
  facet_wrap(~toolusers, scales = "free")
# dev.off()



### Model 2: Adding season but split by tool use/non tool use
####
# tool users
tbm2 <- brm(n | trunc(lb=1) ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
            t2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
            s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], 
          knots = list(tidedif =c(-6,6)), chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
          control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2 <- add_criterion(tbm2, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
# saveRDS(tbm2, "tide_analysis/ModelRDS/tbm2_prior.rds")
# tbm2 <- readRDS("tide_analysis/ModelRDS/tbm2_11052022.rds")

loo(tbm2)
loo_R2(tbm2)
bayes_R2(tbm2)

summary(tbm2)
plot(tbm2)

ce_tbm2 <- conditional_effects(tbm2)
# saveRDS(ce_tbm2, "tide_analysis/ModelRDS/ce_tbm2.rds")
# ce_tbm2 <- readRDS("tide_analysis/ModelRDS/ce_tbm2.rds")
plot(ce_tbm2)


cs_tbm2 <- conditional_smooths(tbm2)
# saveRDS(cs_tbm2, "tide_analysis/ModelRDS/cs_tbm2.rds")
# cs_tbm2 <- readRDS("tide_analysis/ModelRDS/cs_tbm2.rds")
plot(cs_tbm2)

plot(conditional_smooths(tbm2))

pp_check(tbm2, ndraw = 100) 
summary(tbm2)
mcmc_plot(tbm2, variable = c("b_Intercept", "b_seasonFWet", "bs_t2tidedifdistcoast_1", "sds_slocationfactor_1"), type = "areas")

toolusersplot <- plot(conditional_smooths(tbm2, rug = TRUE), plot = FALSE)[[2]]
# saveRDS(toolusersplot, "tide_analysis/ModelRDS/toolusersplot_brms.rds")
# toolusersplot <- readRDS("tide_analysis/ModelRDS/toolusersplot_brms.rds")

## contourplots for tool users for wet and dry season
# png("tide_analysis/ModelRDS/toolusersplot.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot.png", width = 12, height = 6))
ggplot(toolusersplot$data, aes(x = tidedif, y = distcoast, z = estimate__)) + geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + facet_wrap(~seasonF) +
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =20))
# dev.off()

toolusersseason <- plot(ce_tbm2, plot = FALSE)[[5]]
toolusersseason + labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(y = n, x = tidedif, group = seasonF, color = seasonF), bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE)

ggplot(toolusersseason$data) + geom_smooth(method = "gam", se = TRUE, formula = y ~ s(x, bs = "cc"), aes(x = tidedif, y = estimate__-1, group = seasonF, color = seasonF)) + labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(y = n, x = tidedif, group = seasonF, color = seasonF), bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE)

ggplot(toolusersseason$data) + geom_line(aes(x = tidedif, y = estimate__-1, group = seasonF, color = seasonF)) +
  geom_ribbon(aes(x = tidedif, y = estimate__-1, group = seasonF, fill = seasonF, ymin=lower__-1, ymax = upper__-1), alpha = 0.2) + labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(y = n, x = tidedif, group = seasonF, color = seasonF), bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE)

predict_tbm2 <- posterior_smooths(tbm2, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2$data$fit_seasontide <- as.numeric(colMeans(predict_tbm2))

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

#saveRDS(d2, "tide_analysis/ModelRDS/d2_contourtoolsonly.RDS")

# on real scale
# png("tide_analysis/ModelRDS/toolusersplot_pred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot_pred.png", width = 12, height = 6))
ggplot(data = d2, aes(x = tidedif, y = distcoast, z = exp(fit))) +
  geom_contour_filled(bins = 10) + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +
  facet_wrap(~seasonF)
 dev.off()

# on log scale
ggplot(data = d2, aes(x = tidedif, y = distcoast, z = fit)) +
  geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change in number of capuchins") +
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +
  facet_wrap(~seasonF)

# non tool users
tbm2a <- brm(n | trunc(lb=1) ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
               t2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
               s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], 
             knots = list(tidedif =c(-6,6)), chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
             control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2a <- add_criterion(tbm2a, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
# check add criterion, gives error longer object length is not a multiple of shorter object length
# saveRDS(tbm2a, "tide_analysis/ModelRDS/tbm2a_prior.rds")
# tbm2a <- readRDS("tide_analysis/ModelRDS/tbm2a_12052022.rds")

summary(tbm2a)
plot(conditional_smooths(tbm2a))
plot(conditional_effects(tbm2a))

nontoolusersplot <- plot(conditional_smooths(tbm2a, rug = TRUE), plot = FALSE)[[2]]

## contourplots for non tool users for wet and dry season
# png("tide_analysis/ModelRDS/nontoolusersplot.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/nontoolusersplot.png", width = 12, height = 6))
ggplot(nontoolusersplot$data, aes(x = tidedif, y = distcoast, z = exp(estimate__))) + geom_contour_filled() + scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + facet_wrap(~seasonF) +
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =20))
# dev.off()

predict_tbm2a <- posterior_smooths(tbm2a, smooth = 't2(tidedif,distcoast,bs=c("cc","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2a$data$fit_seasontide <- as.numeric(colMeans(predict_tbm2a))

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

# on real scale
# png("tide_analysis/ModelRDS/nontoolusersplot_pred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/nontoolusersplot_pred.png", width = 12, height = 6))
ggplot(data = d2a, aes(x = tidedif, y = distcoast, z = exp(fit))) +
  geom_contour_filled(bins = 11) + scale_fill_viridis(option = "inferno", discrete = TRUE) + theme_bw() + theme(panel.grid = element_blank()) +  
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Number of capuchins") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +
  facet_wrap(~seasonF)
# dev.off()





## practicing determining derivatives
# tool users only
test_tm <- brm(n ~ s(tidedif, bs = "cc", k = 10) + s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], 
               knots = list(tidedif =c(-6,6)), backend = "cmdstanr", core = 2, chain = 2, iter = 2000 )

summary(test_tm)
plot(test_tm)
plot(conditional_effects(test_tm))
plot(conditional_smooths(test_tm))

# non tool users only
test_tm2 <- brm(n ~ s(tidedif, bs = "cc", k = 10) + s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], 
               knots = list(tidedif =c(-6,6)), backend = "cmdstanr", core = 2, chain = 2, iter = 2000 )

summary(test_tm2)
plot(test_tm2)
plot(conditional_effects(test_tm2))
plot(conditional_smooths(test_tm2))

## with by-smooth
test_tm3 <- brm(n ~ s(tidedif, bs = "cc", k = 10, by = toolusers) + toolusers + s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, 
                knots = list(tidedif =c(-6,6)), backend = "cmdstanr", core = 2, chain = 2, iter = 2000 )

summary(test_tm3)
plot(conditional_effects(test_tm3))
plot(conditional_smooths(test_tm3))


#tbm2 <- brm(n | trunc(lb=1) ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
#              t2(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
#              s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], 
#            knots = list(tidedif =c(-6,6)), chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
#            control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)


deriv_plot(tbm1, dimensions = 2, term = 't2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE)', main = c("tidedif", "distcoast"),
           eps = 0.01, confidence = 90, "testplotderiv_tbm1")





# run gam first derivative plot function code from Shauhin to get functions
deriv_plot(test_tm, 's(tidedif,bs="cc",k=10)', "tidedif", 0.01, response = NULL, spaghetti=FALSE, rug = TRUE, confidence = 90, "testplot") 
deriv_plot(test_tm2, 's(tidedif,bs="cc",k=10)', "tidedif", 0.01, response = NULL, spaghetti=FALSE, rug = TRUE, confidence = 90, "testplot2") 
  ###model must be a brms model object
  ###term is a character string of the smooth term, same syntax as used in the model
  ###main is a character string of the predictor variable, must not be wrapped in a smooth function
  ###eps is the amount to offset the original data, to be differenced from original to calculate slope
  ###response is an optional character string indicating the response variable to use, only relevant in the multivariate case
  ###confidence is the confidence level used to calculate the posterior intervals
  ###The desired name of the resulting ggplot object 

deriv_plot2(test_tm, 's(tidedif,bs="cc",k=10)', "tidedif",  0.01, response = NULL, confidence = 95, "testplotderiv")
deriv_plot2(test_tm2, 's(tidedif,bs="cc",k=10)', "tidedif",  0.01, response = NULL, confidence = 95, "testplotderiv2")
  ###model must be a brms model object
  ###term is a character string of the smooth term, same syntax as used in the model
  ###main is a character string of the predictor variable, must not be wrapped in a smooth function
  ###eps is the amount to offset the original data, to be differenced from original to calculate slope
  ###response is an optional character string indicating the response variable to use, only relevant in the multivariate case
  ###confidence is the confidence level used to calculate the posterior intervals
  ###The desired name of the resulting ggplot object 

# can't get by interaction to work yet
species_interact_deriv(test_tm3, 's(tidedif, bs = "cc", k = 10, by = toolusers)', 'tidedif', 0.01, confidence = 95, "testplot3")
###model must be a brms model object
###term is a character string of the smooth term, same syntax as used in the model
###main is a character string of the predictor variable, must not be wrapped in a smooth function
###eps is the amount to offset the original data, to be differenced from original to calculate slope
###response is an optional character string indicating the response variable to use, only relevant in the multivariate case
###confidence is the confidence level used to calculate the posterior intervals
###The desired name of the resulting ggplot object 

#### Descriptives for presentation ####
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

