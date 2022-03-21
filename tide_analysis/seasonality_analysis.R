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
hist(agoutiselect$toolusedurationday)
## a poisson distribution makes most sense, zero-inflated

### TOOL USE OVER TIME
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
# - Understand why the Q-Q plots are not good at all (need different family than zero inflated poisson (gamma???)? Need to transform response variable?). 
#   Would need to go to gamlss or brms to use ZINB, which might also not be right family
# - Understand the AR autocorrelation structure and how to deal with that (see the from the bottom of the heap web pages)
# - Really read and comprehend the Hierarchical GAMs paper and how what I'm doing fits into that
# - How to deal with missing data in a time series and what the model is currently doing
# - compare models using AIC (?)
# - what to do with and how to interpret the k-check indicating it wants more k (if this is the case)
# - can get posterior probabilities like brms with https://www.rdocumentation.org/packages/mgcv/versions/1.8-34/topics/ginla 

# using mgcv package 
# some notes:
# set bs = "cc" for month and day of the year as you need a cyclic cubic spine, since there should be no discontinuity between january and december
# potentially set knots at 0.5 & 12.5 or at 0.5 and 366.5 as January and December should match at end of January and beginning of December, but can still be different
## must include 'by' factor in model as well as they are centered!
# can use 'select = TRUE' to penalize on the null space (look up what that is)

## MODEL 1: Dependent: tool use duration per day, Independent: smooth of month & smooth of time (days since 1970). Over all cameras ####
# based on this https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# allows for comparison of tool use duration between years (time variable) and within years on month level
m1 <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time), family = ziP, data = agoutiselect, method = "REML", knots = list(month = c(0.5,12.5)))
summary(m1) # smooths are significant, explain nearly all of variation in dataset
# visualize
plot(m1, all.terms = TRUE, pages = 1)
# can also plot only one effect, e.g. only the month. adding the intercept value and uncertainty
plot(m1, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m1)[1])

# check assumptions
gam.check(m1) 
# concurvity (correlation between predictors)
concurvity(m1, full = TRUE)
concurvity(m1, full = FALSE)
# autocorrelation
# STILL NEED TO UNDERSTAND THIS
acf(resid(m1), lag.max = 36, main = "ACF") 
pacf(resid(m1), lag.max = 36, main = "pACF")

#### INCLUDING CAMERA LOCATION
### MODEL 2: using day of the year rather than month (more detail) and no smooth for time or between year difference (as we only have 2 years)
# includes locationfactor as both a random intercept and a random slope
# I think this is model I from the peerJ paper by Simpson et al about hierarchical GAMs. This is based on blogpost bottom of the heap about random effects in mgcv with mice data. 
# so different levels of wiggliness for each smooth and no global smooth
m2 <- gam(toolusedurationday ~ s(yrday, bs = "cc", k = 20) + s(locationfactor, bs = "re") + s(locationfactor, yrday, bs = "re"), data = agoutiselect,
          family = ziP, method = "REML", knots = list(yrday = c(0, 365)))
summary(m2)

## Plotting of overall effects
plot(m2, all.terms = TRUE, pages = 1)

# plot the smooth of each camera, two ways
# add transform = exp everywhere to get the data on the real scale (seconds of tool use per day)
# way 1: all in one plot using the tidymv package
plot_smooths(model = m2, transform = exp, series = yrday, comparison = locationfactor) + theme(legend.position = "top")

# way 2: predict from the model for each yrday - locationfactor combination and then use ggplot
new_data <- tidyr::expand(agoutiselect, nesting(locationfactor), yrday = unique(yrday))
m1_pred <- bind_cols(new_data,
                     as.data.frame(predict(m2, newdata = new_data, se.fit = TRUE)))

# same plot as with tidymv package
ggplot(m1_pred, aes(x = yrday, y = exp(fit), group = locationfactor, color = locationfactor)) +
  geom_line() 

# estimates split per location with each its own scale of axes and overlay of real datapoints
ggplot(m1_pred, aes(x = yrday, y = exp(fit), group = locationfactor, color = locationfactor)) +
  geom_line() +
  geom_point(data = agoutiselect, aes(y = toolusedurationday)) +
  facet_wrap(~ locationfactor, scales = "free")

## Model checking
# to check whether model with random intercept and random slope or with random intercept alone is preferred
m2_2 <- gam(toolusedurationday ~ s(yrday, bs = "cc") + s(locationfactor, bs = "re"), data = agoutiselect, family = ziP, method = "REML", knots = list(yrday = c(0,365)))
AIC(m2, m2_2) # model with both random intercept and random slope has preference

gam.check(m2) # still don't understand assumption checking

## MODEL 3: trying to run model GI from hierarchical GAM paper
## setting the k of the group level effect way above the k of the global effect can help encode in the model that the location smooths can be quite wiggly but we want a more smooth global estimate
# this seems to work AS LONG AS THERE IS NO MISSING DATA. In the missing data it is going absolutely crazy and estimating very high things. Need to do something about this
# so need to tweak this and figure this out but it seems to be the right direction?? 

m3 <- gam(toolusedurationday ~ s(yrday, bs = "cc", k = 7) + s(locationfactor, bs = "re") + s(yrday, by = locationfactor, bs = "cc", k = 14), data = agoutiselect, 
          knots = list(yrday=c(0,365)), family = ziP, method = "REML", select = TRUE)
summary(m3)

plot(m3, all.terms = TRUE, pages = 1)

# way 2: predict from the model for each yrday - locationfactor combination and then use ggplot
m3_pred <- bind_cols(new_data,
                     as.data.frame(predict(m3, newdata = new_data, se.fit = TRUE)))

# same plot as with tidymv package
ggplot(m3_pred, aes(x = yrday, y = exp(fit), group = locationfactor, color = locationfactor)) +
  geom_line() 

# estimates split per location with each its own scale of axes and overlay of real datapoints
ggplot(m3_pred, aes(x = yrday, y = exp(fit), group = locationfactor, color = locationfactor)) +
  geom_line() +
  geom_point(data = agoutiselect, aes(y = toolusedurationday)) +
  facet_wrap(~ locationfactor, scales = "free")

plot_smooths(m3, series = yrday, transform = exp)

## Model checking
gam.check(m3)

## need to set k much higher, and also set k of group level effects higher than k of global smooth (to avoid smooth becoming very flat)
m3.2 <- gam(toolusedurationday ~ s(yrday, bs = "cc", k = 12) + s(locationfactor, bs = "re") + s(yrday, by = locationfactor, bs = "cc", k = 15), data = agoutiselect, 
            knots = list(yrday=c(0,365)), family = ziP, method = "REML", select= TRUE)

gam.check(m3.2)
summary(m3.2)
plot(m3.2, all.terms = TRUE, pages = 1)

m3.2_pred <- bind_cols(new_data,
                       as.data.frame(predict(m3.2, newdata = new_data, se.fit = TRUE)))

ggplot(m3.2_pred, aes(x = yrday, y = exp(fit), group = locationfactor, color = locationfactor)) +
  geom_line() +
  geom_point(data = agoutiselect, aes(y = toolusedurationday)) +
  facet_wrap(~ locationfactor, scales = "free")


#### MODEL 4: Model GS from hierarchical GAM paper
## allowing for variation between cameras but only a shared penalty
m4 <- gam(toolusedurationday ~ s(yrday, bs = "cc", k = 35) + s(yrday, locationfactor, bs = "re"), data = agoutiselect, 
            knots = list(yrday=c(0,365)), family = ziP, method = "REML", select= TRUE)

summary(m4)
plot(m4, all.terms = TRUE, pages = 1)

gam.check(m4)


m4_pred <- bind_cols(new_data,
                       as.data.frame(predict(m4, newdata = new_data, se.fit = TRUE)))

ggplot(m4_pred, aes(x = yrday, y = exp(fit), group = locationfactor, color = locationfactor)) +
  geom_line() +
  geom_point(data = agoutiselect, aes(y = toolusedurationday)) +
  facet_wrap(~ locationfactor, scales = "free")

# need very high k to pass gam-check but now seems definitely overfitted.... 
# also only allows variation of intercept, not of shape of curve



### from here on down still needs to be checked










## MODEL 6: expanding model 2 including camera location as factor
# still include yrday on its own without by factor?
# need to include factor as parametric effect. 
m6_zp <- gam(toolusedurationday ~ s(yrday, bs = "cc", by = locationfactor) + locationfactor, data = agoutiselect, family = ziP, method = "REML")
summary(m6_zp)
plot(m6_zp, all.terms = TRUE, trans=exp)
gam.check(m6_zp)

m6_zp$coefficients
# saveRDS(m6_zp, file = "m6_zp.rds")


plot_smooths(model = m6_zp,transform = exp, series = yrday, comparison = locationfactor) + theme(legend.position = "top") 

# for now, drop survey-24 and cebus-06 (miss too much data?)

longdeployments <- c("CEBUS-01-R1", "CEBUS-01-R2", "CEBUS-01-R3", "CEBUS-01-R5", "CEBUS-02-R1", "CEBUS-02-R2", "CEBUS-02-R3", "CEBUS-02-R4", "CEBUS-02-R5",
                      "CEBUS-05-R3", "CEBUS-05-R5",  "CEBUS-08-R2", "CEBUS-08-R3", "CEBUS-08-R4", "CEBUS-08-R5", "CEBUS-09-R2", 
                      "CEBUS-09-R3", "CEBUS-09-R4", "CEBUS-09-R5")
agoutiselect2 <- droplevels.data.frame(agoutiselect[agoutiselect$uniqueloctag %in% longdeployments,])

m6_2 <- gam(toolusedurationday ~ s(yrday, bs = "cc", by = locationfactor) + locationfactor, data = agoutiselect2, family = ziP, method = "REML")
plot_smooths(model = m6_2,transform = exp, series = yrday, comparison = locationfactor) + theme(legend.position = "top") 

# need to write loop to make graph for each location (so select gets higher numbers)
plot(m6_zp, select = 2)
points(agoutiselect$yrday[agoutiselect$locationfactor == "CEBUS-02"], log(agoutiselect$toolusedurationday[agoutiselect$locationfactor == "CEBUS-02"]))

plot(m6_zp, select = 3)
points(agoutiselect$yrday[agoutiselect$locationfactor == "CEBUS-05"], log(agoutiselect$toolusedurationday[agoutiselect$locationfactor == "CEBUS-05"]))
codeddeployments

## still try to plot this the same as m4_zp
require(mgcViz)
b <- getViz(m6_zp)
print(plot(b, allTerms = T), pages = 1)
plot(b, allTerms = TRUE, select = 4) + geom_hline(yintercept = 0)

## MODEL 7: expanding model 2 but factor smooth
# is closer to idea of random effect
# not sure about this as now you can't define yrday as a cyclic cubic 
m7_zp <- gam(toolusedurationday ~ s(yrday, locationfactor, bs = "fs"), data = agoutiselect, family = ziP, method = "REML")
summary(m7_zp)
plot(m7_zp, all.terms = TRUE)


#### BRMS ####
# best models from above but then in brms

## MODEL 1: Zero inflated, no camera location
bm <- brm(toolusedurationday ~ s(yrday, bs = "cc"), family = zero_inflated_poisson(), data = agoutiselect, chain = 4, core = 4, iter = 1000, control = list(adapt_delta = 0.9, max_treedepth = 15))
# saveRDS(bm, file = "bm.rds")
summary(bm)
plot(conditional_smooths(bm))
pp_check(bm) # don't get this
plot(bm)
pairs(bm)

## MODEL 2: camera as grouping factor rather than random effect
bm2 <- brm(toolusedurationday ~ s(yrday, bs = "cc", k = 12) + + s(locationfactor, bs = "re") + s(yrday, by = locationfactor, bs = "cc", k = 15), knots = list(yrday = c(0,365)),
           family = zero_inflated_poisson(), method = "REML", data = agoutiselect, chain = 4, core = 4, iter = 1000, control = list(adapt_delta = 0.90, max_treedepth = 15))

## MODEL 3: Zero inflated, including camera location as random effect 
# need to run for more iterations, bulk and tail ESS both low
bm3 <- brm(toolusedurationday ~ s(yrday, bs = "cc") + (1|locationfactor), family = zero_inflated_poisson(), data = agoutiselect, iter = 1000, chain = 4, core = 4, control = list(adapt_delta = 0.9, max_treedepth = 15))
# saveRDS(bm3, file = "bm3.rds")
# bm3 <- readRDS("bm3.rds")
summary(bm3)
plot(conditional_smooths(bm3))
pp_check(bm3, type = "hist") # don't know what's wrong here
pp_check(bm3, type = "ecdf_overlay")
plot(bm3)

# compare bm3 to m4_zp (the mgcv version of fitting locationfactor as a random effect)
gam.vcomp(m4_zp, rescale = FALSE)

# use marginal smooths to extract the marginal effect of the spline
plot(m4_zp)

# extract group-level estimates
bm3_camest <- as.data.frame(ranef(bm3))
# need to do exp(estimate) to get to the actual rate
bm3_camest$estimate <- exp(bm3_camest$locationfactor.Estimate.Intercept)
bm3_camest$Q2.5 <- exp(bm3_camest$locationfactor.Q2.5.Intercept)
bm3_camest$Q97.5 <- exp(bm3_camest$locationfactor.Q97.5.Intercept)
bm3_camest$est_error <- exp(bm3_camest$locationfactor.Est.Error.Intercept)

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
# 

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


## old code to be chucked (likely)
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


par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
plot(m2, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m4_zp)[1], ylim = c(-3, log(max(agoutiselect$toolusedurationday))))
points(agoutiselect$yrday, log(agoutiselect$toolusedurationday), col = locationcol[(as.numeric(agoutiselect$locationfactor) + 1)])
legend("topright", inset=c(-0.57,0), legend = levels(agoutiselect$locationfactor), pch = 19, col = locationcol, cex = 0.9, xpd = TRUE )
dev.off()

# try to plot that on real scale
# outcome looks like log seconds per day


require(mgcViz)
# https://mfasiolo.github.io/mgcViz/articles/mgcviz.html
b <- getViz(m2)
o <- plot( sm(b, 1) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()

print(plot(b, allTerms = T), pages = 1)
plot(b, allTerms = TRUE, select = 2) + geom_hline(yintercept = 0)