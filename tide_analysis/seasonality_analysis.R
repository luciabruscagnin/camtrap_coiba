## Seasonality of tool use analysis

# first run agouti_cleaning script


### need to add days within the deployment length that were not observed as a 0 tool use day. Should be included in the model. 
## BUT in theory each camera trap is triggered every day to take a timelapse photo
## So including the timelapse sequences should be enough
## find a way to check if these are in? Deployment days should match nrow when we're down to the day level

## can use the package padr to pad the dataframe otherwise if you need to
## but you'd somehow have to do this per deployment, is a bit unclear
# below is some trial and error on this
# brute force method
#agoutisequence$seqday <- as.Date(agoutisequence$seq_startday)
#alldays <- data.frame(seqday = seq(min(as.Date(agoutisequence$dep_startday)), max(as.Date(agoutisequence$dep_endday)), by = "days"))
#new <- left_join(alldays, agoutisequence, by = "seqday")
# now the dataframe contains all the days between the start of the first deployment and the end of the last deployment
# create flag for days outside of the deployments

# use agoutisequence dataframe that's cleaned on the sequence level
## EXPOSURE
# extract the day of the sequence and also the day of the deployment start and end
# if day of sequence is in day of deployment start or end, then it's time for that. otherwise it's 24 hours
# not too fast but works
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


## PREP
# create unique variable (like deployment ID) that is location name + tag
agoutisequence$uniqueloctag <- paste(agoutisequence$location_name, agoutisequence$tag, sep = "-")
# make date an R Date class (which means it counts from jan 1970 so just have a numerical value) can plot with it
# add time variable for different years. So is the date as a numerical variable, divided by 1000 to look for trend or between-year variable
agoutisequence$time <- as.numeric(as.Date(agoutisequence$seq_startday))
# make temperature numerical
agoutisequence$temperature <- as.numeric(agoutisequence$temperature)
# make location a factor
agoutisequence$locationfactor <- as.factor(agoutisequence$location_name)
# make a numerical variable of the day of the year
agoutisequence$yrday <- yday(agoutisequence$seq_startday)
# add seqday variable (RDate format)
agoutisequence$seqday <- as.Date(agoutisequence$seq_startday)

## TOOL USE
# filter dataset down to cameras that were deployed in the Jicaron tool using groups range
agoutisequence_jt <- agoutisequence[(agoutisequence$tool_site == 1 & agoutisequence$island == "Jicaron"),]

# if tool use in sequence, then sequence duration is tool use duration
agoutisequence_jt$tooluseduration <- ifelse(agoutisequence_jt$tooluse == TRUE, agoutisequence_jt$seq_length, 0)
## need to pay attention here, because if seq_length is not available yet (due to exifdata not being pulled) then it will look the same as when there's no tool use. 
# also the timelapse shots at noon and midnight also have sequence length = 0

# aggregate per day & deployment-location (uniqueloctag), summing all the tool use durations
# NOTE: would also need to aggregate temperature per day etc if we already jump to this format. Haven't done that yet
agoutiday <- aggregate(agoutisequence_jt$tooluseduration, by = list(seq_startday = agoutisequence_jt$seq_startday, uniqueloctag = agoutisequence_jt$uniqueloctag), FUN = sum)
names(agoutiday)[names(agoutiday) == "x"] <- "toolusedurationday"
agoutiday2 <- left_join(agoutiday, agoutisequence_jt, by = c("seq_startday", "uniqueloctag"))
## need unique identifier for what we want to get down to per row, which is per day per location per deployment
agoutiday2$identifier <- paste(agoutiday2$seq_startday, agoutiday2$uniqueloctag, sep = "-")
agoutiday2 <- agoutiday2[!duplicated(agoutiday2$identifier),]

# WHEN WE HAVE CODED REPRESENTATIVE SAMPLE, SELECT THAT HERE
agoutiselect <- agoutiday2

## exclude deployment start and end days for now. Discussed with Urs and he thought that if they are only small part of sample makes it easier to exclude them
agoutiselect <- agoutiselect[agoutiselect$exposure == 24, ]
# drop all the columns we don't need
agoutiselect <- agoutiselect[,c("seq_startday", "toolusedurationday", "deployment_id", "location_name", "tags", "dep_start", "dep_end", "dep_length_hours", "month", 
                               "season","island", "exposure", "time", "yrday", "locationfactor", "tool_anvil", "uniqueloctag", "autotimelapse")]
agoutiselect <- droplevels.data.frame(agoutiselect)

sum(agoutiselect$autotimelapse)

### Need to include days on which the cameras were deployed but not triggered, and not include days on which the cameras were not deployed 






## check whether we are missing days (so deployment length in days should match nrow in the day dataframe)
locations <- data.frame(uniqueloctag = unique(agoutisequence$uniqueloctag)) 
locations <- left_join(locations, agoutisequence[,c("uniqueloctag", "dep_start", "dep_end")], by = "uniqueloctag")
locations <- locations[!duplicated(locations$uniqueloctag),]
# take time off
locations$dep_startday <- as.Date(format(locations$dep_start, "%Y-%m-%d"))
locations$dep_endday <- as.Date(format(locations$dep_end, "%Y-%m-%d"))
locations$dep_days <- ceiling(difftime(locations$dep_end, locations$dep_start, units = c("days")))
for (i in 1:nrow(locations)) {
  locations$nrow[i] <- nrow(agoutiday2[agoutiday2$uniqueloctag == locations$uniqueloctag[i],])
}
str(locations)
# we are missing days, so need to get these in

## do this per deployment at a time
# solving it for one deployment
locations$uniqueloctag[1]
# we pick CEBUS-01-R1
# generate all the days that should be present within each deployment
# first create dataframe for first one
depldays <<- data.frame(uniqueloctag = locations$uniqueloctag[1], seqday = seq(locations$dep_startday[1], locations$dep_endday[1], by = "days"))
# now iterate over all the other ones and append those to the dataframe
for (i in 2:nrow(locations)) {
  depldays2 = data.frame(uniqueloctag = locations$uniqueloctag[i], seqday = seq(locations$dep_startday[i], locations$dep_endday[i], by = "days"))
  depldays <<- rbind(depldays, depldays2)
} 

new <- left_join(depldays, agoutiday2, by = c("uniqueloctag", "seqday"))


# 


# make vector including all the days that cameras were deployed
my_vec <- seq(locations$dep_startday[1], locations$dep_endday[1], by = "days")
for (i in 2:nrow(locations)) {
  new_vec <- seq(locations$dep_startday[i], locations$dep_endday[i], by = "days")
  my_vec <- c(my_vec, new_vec)
}
# remove duplicates
my_vec <- my_vec[!duplicated(my_vec)]
# can use this vector to add these days to the agoutisequence dataframe. 

# need to have these days for each location
# so need to add a blank day for each location that didn't have an observation that day
depldays <- data.frame(seqday = sort(rep(my_vec, length(unique(agoutisequence$location_name)))))
depldays$location_name <- rep(unique(agoutisequence$location_name), length(my_vec))


str(agoutiday2)
new <- left_join(depldays, agoutiday2, by = c("seqday", "location_name"))
new$notrigger <- ifelse(new$sequence_id )

## Sanity check
# now have vector with all days including days that we didnt have cameras out
## RIGHT NOW THIS IS ONLY ON ALL SCORED DEPLOYMENTS, SO OBVIOUSLY HAVE GAPS IN THERE
# but just to check whether this likely worked
allpossibledays <- seq(min(locations$dep_startday), max(locations$dep_endday), by = "days")
# identify days on which we didn't have a camera out
lostdays <- allpossibledays[!(allpossibledays %in% my_vec)]
lostdays
?seq
sort(unique(agoutisequence$seq_startday[agoutisequence$uniqueloctag == "CEBUS-01-R1"]))
sort(unique(agouticlean$seq_startday[agouticlean$location_name == "CEBUS-01" & agouticlean$tags == "R1"]))

agouticlean$seq_startday <- format(agouticlean$seq_start, "%Y-%m-%d")
agouticlean$seq_startday[agouticlean$location_name == "CEBUS-01"]
str(agouticlean)
#### DATA FOR ANALYSES
# Agoutiselect is the dataframe we'd use for analyses of seasonality. 
# Each row is one day of observation, which is included as a character (seq_startday) and as a continuous variable in days since 1970 (time) 
# A variable for each month (month) and the day of the year between 1 and 365 (yrday), there is also a variable for dry or wet season (season)
# Each camera location is represented as a character (location_name) and a factor (locationfactor), and also each island (island)
# Each deployment has its own tag (tags) and deployment ID (deployment_id). The POSIXct start and end day of each deployment is also included, as well as its length in hours
# the dependent variable of interest is the seconds of tool use per day (toolusedurationday)
# Includes the amount of hours that camera could have been recording this day (exposure), for now is all 24 as we've excluded the pickup and deploy days.

### ASSUMPTIONS
# look at distribution of response variable
require("fitdistrplus")
descdist(agoutiselect$toolusedurationday)
hist(agoutiselect$toolusedurationday)
## a poisson distribution makes most sense, likely zero-inflated

### TOOL USE OVER TIME
# just plots
plot(toolusedurationday ~ time, data = agoutiselect)
plot(toolusedurationday ~ month, data = agoutiselect)

#### GAM ####
require("mgcv")
require("brms")

### MGCV
# using mgcv package 
# set bs = "cc" for month and day of the year as you need a cyclic cubic spine, since there should be no discontinuity between january and december

## MODEL 1: Dependent: tool use duration per day, Independent: smooth of month & smooth of time (days since 1970). Over all cameras
# based on this https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# allows for comparison of tool use duration between years (time variable) and within years on month level

## POISSON
m1_p <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time), family = poisson, data = agoutiselect, method = "REML")
summary(m1_p) # smooths are significant, explains 31% of deviance
# visualize
plot(m1_p, all.terms = TRUE, pages = 1)
# can also plot only one effect, e.g. only the month. adding the intercept value and uncertainty
plot(m1_p, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m1_p)[1])

# check assumptions
gam.check(m1_p) # both are significant, so k needs to be higher?
# concurvity (correlation between predictors)
concurvity(m1_p, full = TRUE)
concurvity(m1_p, full = FALSE)
# autocorrelation
# STILL NEED TO UNDERSTAND THIS
acf(resid(m1_p), lag.max = 36, main = "ACF") 
pacf(resid(m1_p), lag.max = 36, main = "pACF")

# ZERO INFLATED POISSON
m1_zp <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time), family = ziP, data = agoutiselect, method = "REML")
summary(m1_zp) # smooths are significant, explains 100% of deviance (interesting)
# visualize
plot(m1_zp, all.terms = TRUE, pages = 1)
# can also plot only one effect, e.g. only the month. adding the intercept value and uncertainty
plot(m1_zp, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m1_zp)[1])

# check assumptions
gam.check(m1_zp) 
# concurvity (correlation between predictors)
concurvity(m1_zp, full = TRUE)
concurvity(m1_zp, full = FALSE)
# autocorrelation
# STILL NEED TO UNDERSTAND THIS
acf(resid(m1_zp), lag.max = 36, main = "ACF") 
pacf(resid(m1_zp), lag.max = 36, main = "pACF")

## MODEL 2: Dependent: tool use duration per day. Independent: smooth of day of the year. Over all cameras
# based on Kat's script, only looking at one smooth, simplifies it slightly

# POISSON
m2_p <- gam(toolusedurationday ~ s(yrday, bs = "cc"), family = poisson, data = agoutiselect, method = "REML")
summary(m2_p) # smooth is significant, explains 16% of deviance
# visualize
plot(m2_p, all.terms = TRUE, pages = 1)

# check assumptions
gam.check(m2_p) 
# autocorrelation
# STILL NEED TO UNDERSTAND THIS
acf(resid(m2_p), lag.max = 36, main = "ACF") 
pacf(resid(m2_p), lag.max = 36, main = "pACF")

# ZERO INFLATED POISSON
m2_zp <- gam(toolusedurationday ~ s(yrday, bs = "cc"), family = ziP, data = agoutiselect, method = "REML")
summary(m2_zp) # smooth is significant, explains 100% of deviance (?)
# visualize
plot(m2_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(m2_zp) # both are significant, so k needs to be higher?
# autocorrelation
# STILL NEED TO UNDERSTAND THIS
acf(resid(m2_zp), lag.max = 36, main = "ACF") 
pacf(resid(m2_zp), lag.max = 36, main = "pACF")

#### INCLUDING CAMERA LOCATION
## MODEL 3: expanding model 1 with inclusion of camera location as a random smooth
# POISSON
m3_p <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time) + s(locationfactor, bs = "re"), data = agoutiselect, family = poisson, method = "REML") 
summary(m3_p) # explains 35% of deviance
plot(m3_p, all.terms =  TRUE, pages = 1)
gam.check(m3_p)

# ZERO INFLATED POISSON
m3_zp <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time) + s(locationfactor, bs = "re"), data = agoutiselect, family = ziP, method = "REML") 
summary(m3_zp)
plot(m3_zp, all.terms =  TRUE, pages = 1)
gam.check(m3_zp)

## MODEL 4: expanding model 2 with inclusion of camera location as random smooth
# POISSON
m4_p <- gam(toolusedurationday ~ s(yrday, bs = "cc") + s(locationfactor, bs = "re"), data = agoutiselect, family = poisson, method = "REML")
summary(m4_p) # explains 33% of deviance
plot(m4_p, all.terms = TRUE, pages = 1)
gam.check(m4_p)

# ZERO INFLATED POISSON
m4_zp <- gam(toolusedurationday ~ s(yrday, bs = "cc") + s(locationfactor, bs = "re"), data = agoutiselect, family = ziP, method = "REML")
summary(m4_zp) 
plot(m4_zp, all.terms = TRUE, pages = 1)
gam.check(m4_zp)

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
plot(m4_zp, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m4_zp)[1], ylim = c(-3, log(max(agoutiselect$toolusedurationday))))
points(agoutiselect$yrday, log(agoutiselect$toolusedurationday), col = locationcol[(as.numeric(agoutiselect$locationfactor) + 1)], pch = agoutiselect$tool_anvil + 1)
legend("topright", inset=c(-0.45,0), legend = levels(agoutiselect$locationfactor), pch = 19, col = locationcol, cex = 0.9 )
legend("bottomright", inset=c(-0.21,0), legend = c("No anvil", "Anvil"), pch = c(1,2), cex = 0.9 )
dev.off()

# try to plot that on real scale
# outcome looks like log seconds per day

## MODEL 5: expanding model 1 including camera location as factor
# NOTE: likely need more data here to be able to get these estimates per camera location. Maybe also limit dataset to locations we have enough data for
# POISSON
m5_p <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(month, bs = "cc", by = locationfactor) + s(time), data = agoutiselect, family = poisson, method = "REML")
summary(m5_p)
plot(m5_p, all.terms = TRUE)
gam.check(m5_p)

# ZERO INFLATED POISSON
m5_zp <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(month, bs = "cc", by = locationfactor) + s(time), data = agoutiselect, family = ziP, method = "REML")
summary(m5_zp)
plot(m5_zp, all.terms = TRUE)
gam.check(m5_zp)

## MODEL 6: expanding model 2 including camera location as factor
# THIS ONE SEEMS PROMISING, ASK BRENDAN
# POISSON
m6_p <- gam(toolusedurationday ~ s(yrday, bs = "cc", k = 8) + s(yrday, bs = "cc", by = locationfactor), data = agoutiselect, family = poisson, method = "REML")
summary(m6_p)
plot(m6_p, all.terms = TRUE)
gam.check(m6_p)

# ZERO INFLATED POISSON
m6_zp <- gam(toolusedurationday ~ s(yrday, bs = "cc", k = 8) + s(yrday, bs = "cc", by = locationfactor), data = agoutiselect, family = ziP, method = "REML")
summary(m6_zp)
plot(m6_zp, all.terms = TRUE)
gam.check(m6_zp)

m6_zp$coefficients

plot(m6_zp, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m6_zp)[1])
## still try to plot this the same as m4_zp


## MODEL 7: expanding model 2 but factor smooth
# not sure about this as now you can't define yrday as a cyclic cubic 
m7_zp <- gam(toolusedurationday ~ s(yrday, locationfactor, bs = "fs"), data = agoutiselect, family = ziP, method = "REML")
summary(m7_zp)
plot(m7_zp, all.terms = TRUE)

## BRMS
# best models from above but then in brms

## MODEL 2: Zero inflated, no camera location
bm2 <- brm(toolusedurationday ~ s(yrday, bs = "cc"), family = zero_inflated_poisson(), data = agoutiselect, chain = 4, core = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
saveRDS(bm2, file = "bm2.rds")
summary(bm2)
plot(conditional_smooths(bm2))
pp_check(bm2) # don't get this
plot(bm2)

## MODEL 3: Zero inflated, including camera location as random effect 
# need to run for more iterations, bulk and tail ESS both low
bm3 <- brm(toolusedurationday ~ s(yrday, bs = "cc") + (1|locationfactor), family = zero_inflated_poisson(), data = agoutiselect, chain = 4, core = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
saveRDS(bm3, file = "bm3.rds")
summary(bm3)
plot(conditional_smooths(bm3))
pp_check(bm3)
plot(bm3)

## look into Kat's script and the autocorrelation 

## collection of useful resources so I don't accidentally lose them
# https://fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/
# https://bookdown.org/content/4857/
# https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
# https://noamross.github.io/gams-in-r-course/chapter3
# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# https://fromthebottomoftheheap.net/2017/05/04/compare-mgcv-with-glmmtmb/
# https://www.youtube.com/watch?v=sgw4cu8hrZM
# https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
# https://cran.r-project.org/web/packages/brms/vignettes/brms_distreg.html#zero-inflated-models
# https://stats.stackexchange.com/questions/403772/different-ways-of-modelling-interactions-between-continuous-and-categorical-pred
# https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/family.mgcv.html
