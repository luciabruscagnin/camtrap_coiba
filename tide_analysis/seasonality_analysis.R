## Seasonality of tool use analysis
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

# start with the agoutisequence dataframe that's cleaned and aggregated to the sequence level
# for the seasonality analysis, we consider a subset of the data at repeat locations across seasons. 
# Aggregate to the day level per camera
# potentially for activity patterns on the day can work with same script but on all cameras

## PREP
# create unique variable (like deployment ID) that is location name + tag
agoutisequence$uniqueloctag <- paste(agoutisequence$locationName, agoutisequence$tag, sep = "-")
# make temperature numerical
agoutisequence$temperature <- as.numeric(agoutisequence$temperature)
# add seqday variable (RDate format)
agoutisequence$seqday <- as.Date(format(agoutisequence$seq_start, "%Y-%m-%d"))

## EXPOSURE
# how long the camera was running on a day
## NOTE: in later deployments (R10 on) to conserve batteries we have cameras on a schedule to be off at night, but before run for 24 hours
# extract the day of the sequence and also the day of the deployment start and end
# if day of sequence is in day of deployment start or end, then it's time for that. otherwise it's 24 hours
agoutisequence$dep_startday <- format(agoutisequence$dep_start, "%Y-%m-%d")
agoutisequence$dep_endday <- format(agoutisequence$dep_end, "%Y-%m-%d")
agoutisequence$seq_startday <- format(agoutisequence$seq_start, "%Y-%m-%d")
agoutisequence$dep_starttime <- hour(agoutisequence$dep_start)
agoutisequence$dep_endtime <- hour(agoutisequence$dep_end)

# maybe too coarse, only on level of hours now 
agoutisequence$exposure <- ifelse(agoutisequence$seq_startday == agoutisequence$dep_startday, 
                                  24-agoutisequence$dep_starttime, 
                                  ifelse(agoutisequence$seq_startday == agoutisequence$dep_endday,
                                         24 - agoutisequence$dep_endtime, 24))

##### TOOL USE #####
# filter dataset down to cameras that were deployed in the Jicaron tool using groups range
agoutisequence_jt <- agoutisequence[which(agoutisequence$tool_site == 1 & agoutisequence$island == "Jicaron"),]

# if tool use in sequence, then sequence duration is tool use duration
agoutisequence_jt$tooluseduration <- ifelse(agoutisequence_jt$tooluse == TRUE, agoutisequence_jt$seq_length, 0)
## need to pay attention here, because if seq_length is not available yet (due to exifdata not being pulled) then it will look the same as when there's no tool use. 
# currently everything up to and including R5 has seq_length available

# aggregate per day & deployment-location (uniqueloctag), summing all the tool use durations
# NOTE: would also need to aggregate temperature per day etc if we already jump to this format. Haven't done that yet
agoutiday <- aggregate(agoutisequence_jt$tooluseduration, by = list(seq_startday = agoutisequence_jt$seq_startday, uniqueloctag = agoutisequence_jt$uniqueloctag), FUN = sum)
names(agoutiday)[names(agoutiday) == "x"] <- "toolusedurationday"
agoutiday2 <- left_join(agoutiday, agoutisequence_jt, by = c("seq_startday", "uniqueloctag"))
## need unique identifier for what we want to get down to per row, which is per day per location per deployment
agoutiday2$identifier <- paste(agoutiday2$seq_startday, agoutiday2$uniqueloctag, sep = "-")
agoutiday2 <- agoutiday2[!duplicated(agoutiday2$identifier),]

### DEPLOYMENT DAYS WITHOUT TRIGGERS
## can miss days 1. because camera wasn't deployed or 2. because camera was deployed but not triggered (or sequence was blank and thus not in this output)
## need to add all the days that fall under category 2, as these are also absence of tool use at this location

## check whether we are missing days (if not deployment length in days should match nrow in the day dataframe)
## If we'd aggregate the entire agoutisequence to the day level (e.g. for activity), we should do this for that dataframe

# first make overview of deployments we have and their start and end days
locations <- data.frame(uniqueloctag = unique(agoutisequence_jt$uniqueloctag)) 
locations <- left_join(locations, agoutisequence_jt[,c("uniqueloctag", "dep_start", "dep_end")], by = "uniqueloctag")
locations <- locations[!duplicated(locations$uniqueloctag),]
# take time off and keep just date variable
locations$dep_startday <- as.Date(format(locations$dep_start, "%Y-%m-%d"))
locations$dep_endday <- as.Date(format(locations$dep_end, "%Y-%m-%d"))
# calculate days in each deployment (round up)
locations$dep_days <- ceiling(difftime(locations$dep_end, locations$dep_start, units = c("days")))
# number of rows in the agoutiday2 dataframe (so how many days we have)
for (i in 1:nrow(locations)) {
  locations$nrow[i] <- nrow(agoutiday2[agoutiday2$uniqueloctag == locations$uniqueloctag[i],])
}
# we are indeed missing days, so need to get these in

# generate all the days that should be present within each deployment
# first create dataframe for first one
depldays <<- data.frame(uniqueloctag = locations$uniqueloctag[1], seqday = seq(locations$dep_startday[1], locations$dep_endday[1], by = "days"))
# now iterate over all the other ones and append those to the dataframe
for (i in 2:nrow(locations)) {
  depldays2 = data.frame(uniqueloctag = locations$uniqueloctag[i], seqday = seq(locations$dep_startday[i], locations$dep_endday[i], by = "days"))
  depldays <<- rbind(depldays, depldays2)
} 

# sanity check: checking for deployments how many days are missed
sum((depldays$seqday[depldays$uniqueloctag == locations$uniqueloctag[1]] %in% agoutiday2$seqday[agoutiday2$uniqueloctag == locations$uniqueloctag[1]])==FALSE)

## I would add this only at the stage of the "agoutiselect" dataframe, as you only want to do this for deployments that have fully been coded
# REPRESENTATIVE SAMPLE SELECTION
# exclude CEBUS-03 (as CEBUS-02 and CEBUS-03 are on the same anvil)
# exclude one-off surveys ("SURVEY-CEBUS-07-03-R3", "SURVEY-CEBUS-15-04-R5", # "SURVEY-CEBUS-17-03-R4") 
# SURVEY-CEBUS-24-01 is basically CEBUS-04 anvil (leaving out for now, include with "SURVEY-CEBUS-24-01-R4", "SURVEY-CEBUS-24-01-R5")
# manually identify deployments that have been fully coded
codeddeployments <- c("CEBUS-01-R1", "CEBUS-01-R2", "CEBUS-01-R3", "CEBUS-01-R5", "CEBUS-02-R1", "CEBUS-02-R2", "CEBUS-02-R3", "CEBUS-02-R4", "CEBUS-02-R5",
                      "CEBUS-05-R3", "CEBUS-05-R5", "CEBUS-06-R4", "CEBUS-06-R5", "CEBUS-08-R2", "CEBUS-08-R3", "CEBUS-08-R4", "CEBUS-08-R5", "CEBUS-09-R2", 
                      "CEBUS-09-R3", "CEBUS-09-R4", "CEBUS-09-R5")
agoutiselect <- agoutiday2[agoutiday2$uniqueloctag %in% codeddeployments,]

## for these deployments, add in the days that the camera was running but not triggered (and flag these)
agoutiselect <- left_join(depldays[depldays$uniqueloctag %in% agoutiselect$uniqueloctag,], agoutiselect, by = c("uniqueloctag", "seqday"))
agoutiselect$noanimal <- ifelse(is.na(agoutiselect$sequenceID), 1, 0)

## make sure these rows have all the variables they need for analyses 
# I think easiest way to fill up the NAs is by having a metadata frame to pull info from
metadata <- agoutiselect[!duplicated(agoutiselect$uniqueloctag), c("uniqueloctag", "deploymentID", "locationName", "tags", "dep_start", "dep_end", "dep_length_hours",
                                                                     "island", "tool_anvil", "identifier")]

for (i in 1:nrow(agoutiselect)) {
  if (agoutiselect$noanimal[i] == 1) {
    agoutiselect$locationName[i] <- metadata$locationName[metadata$uniqueloctag == agoutiselect$uniqueloctag[i]]
    agoutiselect$tags[i] <- metadata$tags[metadata$uniqueloctag == agoutiselect$uniqueloctag[i]]
    agoutiselect$dep_start[i] <- metadata$dep_start[metadata$uniqueloctag == agoutiselect$uniqueloctag[i]] 
    agoutiselect$dep_end[i] <- metadata$dep_end[metadata$uniqueloctag == agoutiselect$uniqueloctag[i]] 
    agoutiselect$dep_length_hours[i] <- metadata$dep_length_hours[metadata$uniqueloctag == agoutiselect$uniqueloctag[i]] 
    agoutiselect$island[i] <- metadata$island[metadata$uniqueloctag == agoutiselect$uniqueloctag[i]] 
    agoutiselect$tool_anvil[i] <- metadata$tool_anvil[metadata$uniqueloctag == agoutiselect$uniqueloctag[i]]
    agoutiselect$deploymentID[i] <- metadata$deploymentID[metadata$uniqueloctag == agoutiselect$uniqueloctag[i]]
  }
}

agoutiselect$toolusedurationday <- ifelse(agoutiselect$noanimal == 1, 0, agoutiselect$toolusedurationday)
agoutiselect$toolusedurationday[agoutiselect$noanimal == 1] <- 0
# for now just set exposure on these days to 24, assuming they dont occur on pick up or deployment days
agoutiselect$exposure[agoutiselect$noanimal == 1] <- 24
# add month and season
agoutiselect$month[agoutiselect$noanimal == 1] <- month(agoutiselect$seqday[agoutiselect$noanimal == 1])
agoutiselect$season <- ifelse(agoutiselect$month == 12 | agoutiselect$month == 1 | agoutiselect$month == 2 | agoutiselect$month == 3 | 
                                agoutiselect$month == 4, "Dry", "Wet") 

# add time variable for different years. So is the date as a numerical variable to look for trend or between-year variation
agoutiselect$time <- as.numeric(agoutiselect$seqday)
# make a numerical variable of the day of the year (so from 1-365 which day of the year it is)
agoutiselect$yrday <- yday(agoutiselect$seqday)
# make location a factor
agoutiselect$locationfactor <- as.factor(agoutiselect$locationName)
# add the year
agoutiselect$year <- year(agoutiselect$seqday)

## exclude deployment start and end days as our presence and setting up/picking up cameras may have affected the animals' behavior
agoutiselect <- agoutiselect[agoutiselect$exposure == 24, ]
# drop all the columns we don't need
agoutiselect <- agoutiselect[,c("seqday", "toolusedurationday", "deploymentID", "locationName", "tags", "dep_start", "dep_end", "dep_length_hours", "month", 
                                "season","island", "exposure", "time", "yrday", "year", "locationfactor", "tool_anvil", "uniqueloctag", "noanimal", "identifier")]
agoutiselect <- droplevels.data.frame(agoutiselect)

#### DATA FOR ANALYSES
# Agoutiselect is the data frame we'd use for analyses of seasonality. 
# Each row is one day of observation, which is included as an RDate (seqday) and as a continuous variable in days since 1970 (time) 
# A variable for each month (month) and the day of the year between 1 and 365 (yrday), there is also a variable for dry or wet season (season) and for the year (year)
# Each camera location is represented as a character (location_name) and a factor (locationfactor), and also each island (island) (if we'd want to include Coiba)
# Each deployment has its own tag (tags) and deployment ID (deployment_id). The POSIXct start and end day of each deployment is also included, as well as its length in hours
# the dependent variable of interest is the seconds of tool use per day (toolusedurationday)
# Includes the amount of hours that camera could have been recording this day (exposure), for now is all 24 as we've excluded the pickup and deploy days.

### ASSUMPTIONS
# look at distribution of response variable
require("fitdistrplus")
descdist(agoutiselect$toolusedurationday)
testdist1 <- fitdist(agoutiselect$toolusedurationday, "gamma", method = "mme")
plot(testdist1)

# only values over 0
testdist2 <- fitdist(agoutiselect$toolusedurationday[agoutiselect$toolusedurationday > 0], "gamma", method = "mme")
plot(testdist2)
# so hurdle gamma would be good fit
hist(agoutiselect$toolusedurationday[agoutiselect$toolusedurationday>0])

### SEASONALITY OF TOOL USE
# just plots
plot(toolusedurationday ~ seqday, data = agoutiselect)
plot(toolusedurationday ~ month, data = agoutiselect)

#### GAM ####
library(mgcv)
library(brms)
library(gratia)
library(tidyr)
library(tidymv)
library(ggplot2)
library(dplyr)

##### MGCV ####
# Things that are important to do now:
# - Understand the AR autocorrelation structure and how to deal with that (see the from the bottom of the heap web pages)
# - compare models using AIC (?)
# - what to do with and how to interpret the k-check indicating it wants more k (if this is the case)

# using mgcv package 
# some notes:
# set bs = "cc" for month and day of the year as you need a cyclic cubic spine, since there should be no discontinuity between january and december
# set knots at 0 & 12 or at 0 and 366 as January and December should match at end of January and beginning of December, but can still be different
## must include 'by' factor in model as well as they are centered!
# can use 'select = TRUE' to penalize on the null space (look up what that is)
# Use ziplss, two step zero-inflated poisson. Gives estimate of 0/1 and poisson component. 

#### MODEL 1: Model GI with ziplss family.
# Using day of the year rather than month, no smooth for time or between years difference (as we only have <1.5 years)
# allows for 1/0 and poisson component to be modeled separately
# for now give them the same covariates 
## setting the k of the group level effect way above the k of the global effect can help encode in the model that the location smooths can be quite wiggly but we want a more smooth global estimate
# this seems to work AS LONG AS THERE IS NO MISSING DATA. In the missing data it is going absolutely crazy and estimating very high things. Need to do something about this
# so need to tweak this and figure this out but it seems to be the right direction?? 
# still predicting crazy things in the "false 0"/missing data space. 
m2 <- gam(list(toolusedurationday ~ s(yrday, bs = "cc", k = 10) + s(locationfactor, bs = "re") + s(yrday, by = locationfactor, bs = "cc", k =15), 
               ~ s(yrday, bs = "cc") + s(locationfactor, bs = "re") + s(yrday, by = locationfactor, bs = "cc")),  data = agoutiselect, 
          knots = list(yrday=c(0,365)), family = ziplss(), method = "REML", select= TRUE)

summary(m2)
draw(m2)

gam.check(m2)

# plot the smooth of each camera by predicting data
new_data <- tidyr::expand(agoutiselect, nesting(locationfactor), yrday = unique(yrday))

m2_pred <- bind_cols(new_data,
                     as.data.frame(predict(m2, newdata = new_data, type = "link")))
# V1 is predicted value of response from Poisson part of model on scale of linear predictor (log scale)
# V2 is predicted value of zero-inflation component and is on log-log scale
# need to transform them back and multiply together to get actual predicted values
ilink <- binomial(link = "cloglog")$linkinv # to transform log-log back
m2_pred$fit <- exp(m2_pred$V1)*ilink(m2_pred$V2)

# estimates split per location with each its own scale of axes and overlay of real datapoints
ggplot(m2_pred, aes(x = yrday, y = fit, group = locationfactor, color = locationfactor)) +
  geom_line() +
  geom_point(data = agoutiselect, aes(y = toolusedurationday)) +
  facet_wrap(~ locationfactor, scales = "free")
head(m2_pred)

# check assumptions
# concurvity (correlation between predictors)
concurvity(m2, full = TRUE)
concurvity(m2, full = FALSE)
# autocorrelation
# STILL NEED TO UNDERSTAND THIS
acf(resid(m2), lag.max = 36, main = "ACF") 
pacf(resid(m2), lag.max = 36, main = "pACF")



### Try out with Shauhin, load saved rds!

### Add month as interaction effect
m5.2 <- gam(list(toolusedurationday ~ s(month, bs = "cc", k = 8) + s(yrday, bs = "cc", k = 8) + ti(month, yrday, bs ="cc") + s(locationfactor, bs = "re") +
                   ti(month, yrday, by = locationfactor, bs = c("cc", "cc") ) +  s(yrday, by = locationfactor, bs = "cc", k =8) +  s(month, by = locationfactor, bs = "cc", k = 8),
                 ~ s(month, bs = "cc", k = 8) + s(yrday, bs = "cc", k = 8) + ti(month, yrday, bs = c("cc", "cc")) + s(locationfactor, bs = "re") +
                   ti(month, yrday, by = locationfactor, bs = "cc", k =8) +  s(yrday, by = locationfactor, bs = "cc", k =8) +  s(month, by = locationfactor, bs = "cc", k = 8)),  data = agoutiselect, 
            knots = list(yrday=c(0,365), month = c(0,8)), family = ziplss(), method = "REML", select= TRUE)

# saveRDS(m5.2, file = "m5.2.rds")
# m5.2 <- readRDS("m5.2.rds")

m5.2_pred <- bind_cols(new_data2,
                       as.data.frame(predict(m5.2, newdata = new_data2, type = "link")))
# V1 is predicted value of response from Poisson part of model on scale of linear predictor (log scale)
# V2 is predicted value of zero-inflation component and is on log-log scale
# need to transform them back and multiply together to get actual predicted values
ilink <- binomial(link = "cloglog")$linkinv # to transform log-log back
m5.2_pred$fit <- exp(m5.2_pred$V1)*ilink(m5.2_pred$V2)

ggplot(m5.2_pred, aes(x = month, y = fit, group = locationfactor, color = locationfactor)) +
  geom_line() +
  geom_point(data = agoutiselect, aes(y = toolusedurationday)) +
  facet_wrap(~ locationfactor, scales = "free")

summary(m5.2)
draw(m5.2)

vis.gam(m5.2) # visualize interaction effect
# read up on vis.gam and gratia to visualize interaction effects (also mgcv vis)

##### BRMS ####
# need to go to brms to be able to use the hurdle_gamma family (which we need)

## MODEL 2: Model GI 
# can add spatial location instead s(yrday) it will be t2(yrday, locationspace x, locationspace y, bs = c("cc", "ds", "ds")) + (yrday|locationfactor(nonspatial)). Location in UTM 
# likely still need to deal with autocorrelation
# in bottom of the heap blog say: "the samples needed thinning to deal with some strong autocorrelation in the Markov chains — thin = 10," Do this?
# also needs month?
# priors
bm2 <- brm(toolusedurationday ~ s(yrday, bs = "cc", k = 12) + s(locationfactor, bs = "re") + s(yrday, by = locationfactor, bs = "cc", k = 15), knots = list(yrday = c(0,365)),
           family = hurdle_gamma(), data = agoutiselect, chain = 4, core = 4, iter = 10000, control = list(adapt_delta = 0.99, max_treedepth = 10))
# saveRDS(bm2, file = "bm2_full.rds")
# bm2 <- readRDS("bm2_full.rds")

summary(bm2)
plot(bm2)
plot(conditional_smooths(bm2))
plot(conditional_effects(bm2))

# if you want to zoom in past crazy outliers (which I dont have now)
testplot <- plot(conditional_effects(bm2), plot = FALSE)[[3]]
testplot + coord_cartesian(ylim = c(0,300))

#  check for autocorrelation

# plot
season_bm <- plot(conditional_effects(bm2), plot = FALSE)[[1]]
season_bm + labs(y = "Total tool use duration per day (seconds)", x = "Day of the year") + theme_bw()

location_bm <- plot(conditional_effects(bm2), plot = FALSE)[[2]]
location_bm + labs(y = "Total tool use duration per day (seconds)", x = "Camera location") + theme_bw()

seasonsplit_bm <- plot(conditional_effects(bm2), plot = FALSE)[[3]]
# all in one plot
seasonsplit_bm + labs(y = "Total tool use duration per day(seconds)", x = "Camera location")

# with real points plotted on it and separate plots
seasonsplit_bm + facet_wrap("locationfactor", scales = "free") + 
  geom_point(data = agoutiselect, aes(x = yrday, y = toolusedurationday, color = locationfactor, group = locationfactor), alpha = 0.3, inherit.aes = FALSE) +
  labs(y = "Total tool use duration per day (seconds)", x = "Day of the year")

# set scales to the same
seasonsplit_bm + facet_wrap("locationfactor") + 
  geom_point(data = agoutiselect, aes(x = yrday, y = toolusedurationday, color = locationfactor, group = locationfactor), alpha = 0.3, inherit.aes = FALSE) +
  labs(y = "Total tool use duration per day (seconds)", x = "Day of the year")

# without real points but separate plots
seasonsplit_bm + facet_wrap("locationfactor") +
  labs(y = "Total tool use duration per day (seconds)", x = "Day of the year")



## look into Kat's script and the autocorrelation 

## collection of useful resources so I don't accidentally lose them

## General Intro to GAMs
# https://fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/
# https://bookdown.org/content/4857/
# https://noamross.github.io/gams-in-r-course/chapter3
# https://cran.r-project.org/web/packages/tidymv/vignettes/predict-gam.html

## Specific on seasonal data/time series GAMs
# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
# https://stats.stackexchange.com/questions/244042/trend-in-irregular-time-series-data
# https://fromthebottomoftheheap.net/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/

## Random Effects & GAMs
# https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
# https://fromthebottomoftheheap.net/2017/05/04/compare-mgcv-with-glmmtmb/
# https://www.youtube.com/watch?v=sgw4cu8hrZM
# https://stats.stackexchange.com/questions/403772/different-ways-of-modelling-interactions-between-continuous-and-categorical-pred
# https://stats.stackexchange.com/questions/413559/plotting-summed-effects-of-factor-levels-in-interactions-in-a-gamm-model
# https://stackoverflow.com/questions/42848570/mgcv-plotting-factor-by-smooths
# https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/
# https://www.tjmahr.com/random-effects-penalized-splines-same-thing/

## Missing Data & GAMs
# https://rdrr.io/cran/mgcv/man/missing.data.html
# https://stats.stackexchange.com/questions/12712/how-to-handle-gaps-in-a-time-series-when-doing-gamm

## Zero inflated (ziplss)
# https://gatesdupontvignettes.com/2019/05/29/Auto-Nested-Mod.html
# https://fromthebottomoftheheap.net/2017/05/04/compare-mgcv-with-glmmtmb/
# https://stats.stackexchange.com/questions/560656/residuals-of-gam-models-not-improving-with-poisson-or-ziplss-but-better-with-ne


## BRMS
# https://cran.r-project.org/web/packages/brms/vignettes/brms_distreg.html#zero-inflated-models
# https://www.rensvandeschoot.com/tutorials/brms-started/

## MGCV and Plotting
# https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/family.mgcv.html
# https://mfasiolo.github.io/mgcViz/articles/mgcviz.html#mgcviz-basics
# https://fromthebottomoftheheap.net/2018/10/23/introducing-gratia/
# https://gavinsimpson.github.io/gratia/
# https://cran.r-project.org/web/packages/tidymv/vignettes/plot-smooths.html

## Offsets
# https://stats.stackexchange.com/questions/136037/modelling-count-data-where-offset-variable-is-0-for-some-observations
# https://stackoverflow.com/questions/50491178/using-offset-in-gam-zero-inflated-poisson-zip-model 

## Articles
# https://link.springer.com/content/pdf/10.1198/jabes.2009.08038.pdf
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2010.00017.x
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7593165/
# https://lionstats.wordpress.com/2018/09/03/93/
# https://peerj.com/articles/6876/ 
# https://www.sciencedirect.com/science/article/pii/S0304380002001941?via%3Dihub
# https://www.sciencedirect.com/science/article/pii/S0160412019309341
# https://www.frontiersin.org/articles/10.3389/fevo.2018.00149/full
# https://peerj.com/articles/6876/?td=tw
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8698177/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8648868/#MOESM1

########## TOOL USE ANALYSIS ON SEQUENCE LEVEL ######
# use agoutisequence dataset that only contains tool use sequences at Jicaron tool site
agoutiselect_seq <- subset(agoutisequence_jt, agoutisequence_jt$tooluse == TRUE & agoutisequence_jt$tags != "R6") # for now exclude everything past R5
agoutiselect_seq <- agoutiselect_seq[,c("seqday", "tooluseduration", "deploymentID", "locationName", "tags", "dep_start", "dep_end", "dep_length_hours", "month",
                                "season","island", "exposure", "tool_anvil", "uniqueloctag", "seq_item", "n_tooluse", "n", "seq_start")]
agoutiselect_seq <- droplevels.data.frame(agoutiselect_seq)
unique(agoutiselect_seq$uniqueloctag)

# make locationfactor and yrday variable 
agoutiselect_seq$locationfactor <- as.factor(agoutiselect_seq$locationName)
agoutiselect_seq$yrday <- yday(agoutiselect_seq$seqday)
agoutiselect_seq$hour <- hour(agoutiselect_seq$seq_start)


hist(agoutiselect_seq$hour)
ftable(agoutiselect_seq$seq_item)


#### TOOL USE ITEMS ####
# for looking at tool use item, exclude unknown ones and collapse crabs, insects and snails into invertebrates??
# so then would have almendra, coconut, invertebrate, palm, other. 
ftable(agoutiselect_seq$seq_item, agoutiselect_seq$locationName)


#### TOOL USE AND TIME OF DAY & LOCATIONS #####
plot(agoutiselect_seq$hour, agoutiselect_seq$tooluseduration)


#### HOW MANY INDIVIDUALS USE TOOLS #####
plot(agoutiselect_seq$n_tooluse, agoutiselect_seq$n)
ftable(agoutiselect_seq$hour, agoutiselect_seq$n_tooluse)
# feel like proportion of tool using events iwth more than 1 tool users are rarer in morning/evening. How to model this?




