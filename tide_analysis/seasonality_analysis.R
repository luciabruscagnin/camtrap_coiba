## Seasonality of tool use analysis
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies

# start with the agoutisequence dataframe that's cleaned and aggregated to the sequence level
# for the seasonality analysis, only want to look at the Jicaron tool users and aggregate to the day level
# potentially for activity patterns on the day can work with same script but on all cameras

## PREP
# create unique variable (like deployment ID) that is location name + tag
agoutisequence$uniqueloctag <- paste(agoutisequence$locationName, agoutisequence$tag, sep = "-")
# make temperature numerical
agoutisequence$temperature <- as.numeric(agoutisequence$temperature)
# add seqday variable (RDate format)
agoutisequence$seqday <- as.Date(format(agoutisequence$seq_start, "%Y-%m-%d"))

## EXPOSURE
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
agoutisequence_jt <- agoutisequence[(agoutisequence$tool_site == 1 & agoutisequence$island == "Jicaron"),]

# if tool use in sequence, then sequence duration is tool use duration
agoutisequence_jt$tooluseduration <- ifelse(agoutisequence_jt$tooluse == TRUE, agoutisequence_jt$seq_length, 0)
## need to pay attention here, because if seq_length is not available yet (due to exifdata not being pulled) then it will look the same as when there's no tool use. 

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
# WHEN WE HAVE CODED REPRESENTATIVE SAMPLE, SELECT THAT HERE
# for now manually which ones have been fully coded
codeddeployments <- c("CEBUS-01-R1", "CEBUS-01-R2", "CEBUS-01-R3", "CEBUS-02-R1", "CEBUS-02-R4", "CEBUS-02-R5", "CEBUS-08-R2",
                      "CEBUS-08-R3", "CEBUS-09-R2", "CEBUS-09-R3", "CEBUS-09-R4", "CEBUS-09-R5", "CEBUS-05-R5", "SURVEY-CEBUS-07-03-R3", "SURVEY-CEBUS-15-04-R5",
                      "SURVEY-CEBUS-17-03-R4" ,"SURVEY-CEBUS-24-01-R4", "SURVEY-CEBUS-24-01-R5")
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
# NOTE: need to check if they could occur on this day (I don't think so?)
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

## exclude deployment start and end days for now. Discussed with Urs and he thought that if they are only small part of sample makes it easier to exclude them
agoutiselect <- agoutiselect[agoutiselect$exposure == 24, ]
# drop all the columns we don't need
agoutiselect <- agoutiselect[,c("seqday", "toolusedurationday", "deploymentID", "locationName", "tags", "dep_start", "dep_end", "dep_length_hours", "month", 
                                "season","island", "exposure", "time", "yrday", "locationfactor", "tool_anvil", "uniqueloctag", "noanimal", "identifier")]
agoutiselect <- droplevels.data.frame(agoutiselect)

#### DATA FOR ANALYSES
# Agoutiselect is the dataframe we'd use for analyses of seasonality. 
# Each row is one day of observation, which is included as an RDate (seqday) and as a continuous variable in days since 1970 (time) 
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
plot(toolusedurationday ~ seqday, data = agoutiselect)
plot(toolusedurationday ~ month, data = agoutiselect)

#### GAM ####
require("mgcv")
require("brms")

### MGCV
# using mgcv package 
# set bs = "cc" for month and day of the year as you need a cyclic cubic spine, since there should be no discontinuity between january and december

## MODEL 1: Dependent: tool use duration per day, Independent: smooth of month & smooth of time (days since 1970). Over all cameras ####
# based on this https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# allows for comparison of tool use duration between years (time variable) and within years on month level

## POISSON
m1_p <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time), family = poisson, data = agoutiselect, method = "REML")
summary(m1_p) # smooths are significant, explains 21% of deviance
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

## MODEL 2: Dependent: tool use duration per day. Independent: smooth of day of the year. Over all cameras ####
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
gam.check(m2_zp) 
# autocorrelation
# STILL NEED TO UNDERSTAND THIS
acf(resid(m2_zp), lag.max = 36, main = "ACF") 
pacf(resid(m2_zp), lag.max = 36, main = "pACF")

#### INCLUDING CAMERA LOCATION
## MODEL 3: expanding model 1 with inclusion of camera location as a random smooth ####
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
summary(m4_p) # explains 36% of deviance
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
legend("topright", inset=c(-0.50,0), legend = levels(agoutiselect$locationfactor), pch = 19, col = locationcol, cex = 0.9 )
legend("bottomright", inset=c(-0.25,0), legend = c("No anvil", "Anvil"), pch = c(1,2),cex = 0.9 )
dev.off()

# try to plot that on real scale
# outcome looks like log seconds per day

require(mgcViz)
# https://mfasiolo.github.io/mgcViz/articles/mgcviz.html
b <- getViz(m4_zp)
o <- plot( sm(b, 1) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()

print(plot(b, allTerms = T), pages = 1)
plot(b, allTerms = TRUE, select = 2) + geom_hline(yintercept = 0)
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
require(mgcViz)
b <- getViz(m6_zp)
print(plot(b, allTerms = T), pages = 1)
plot(b, allTerms = TRUE, select = 4) + geom_hline(yintercept = 0)

## MODEL 7: expanding model 2 but factor smooth
# not sure about this as now you can't define yrday as a cyclic cubic 
m7_zp <- gam(toolusedurationday ~ s(yrday, locationfactor, bs = "fs"), data = agoutiselect, family = ziP, method = "REML")
summary(m7_zp)
plot(m7_zp, all.terms = TRUE)

## BRMS
# best models from above but then in brms

## MODEL 2: Zero inflated, no camera location
bm2 <- brm(toolusedurationday ~ s(yrday, bs = "cc"), family = zero_inflated_poisson(), data = agoutiselect, chain = 4, core = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
# saveRDS(bm2, file = "bm2.rds")
summary(bm2)
plot(conditional_smooths(bm2))
pp_check(bm2) # don't get this
plot(bm2)
pairs(bm2)

## MODEL 3: Zero inflated, including camera location as random effect 
# need to run for more iterations, bulk and tail ESS both low
bm3 <- brm(toolusedurationday ~ s(yrday, bs = "cc") + (1|locationfactor), family = zero_inflated_poisson(), data = agoutiselect, chain = 4, core = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
# saveRDS(bm3, file = "bm3.rds")
summary(bm3)
plot(conditional_smooths(bm3))
pp_check(bm3, type = "hist") # don't know what's wrong here
pp_check(bm3, type = "ecdf_overlay")
plot(bm3)

# compare bm3 to m4_zp (the mgcv version of fitting locationfactor as a random effect)
gam.vcomp(m4_zp, rescale = FALSE)

# use marginal smooths to extract the marginal effect of the spline
msms <- marginal_smooths(bm3)
plot(msms)
plot(m4_zp)

# extract group-level estimates
bm3_camest <- as.data.frame(ranef(bm3))
# need to do exp(estimate) to get to the actual rate
bm3_camest$estimate <- exp(bm3_camest$locationfactor.Estimate.Intercept)
bm3_camest$Q2.5 <- exp(bm3_camest$locationfactor.Q2.5.Intercept)
bm3_camest$Q97.5 <- exp(bm3_camest$locationfactor.Q97.5.Intercept)
bm3_camest$est_error <- exp(bm3_camest$locationfactor.Est.Error.Intercept)

## MODEL 4: camera as grouping factor rather than random effect
bm4 <- brm(toolusedurationday ~ s(yrday, bs = "cc") + s(yrday, by = locationfactor, bs = "cc"), family = zero_inflated_poisson(), data = agoutiselect, chain = 4, core = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))



??dotplot
dotchart(ranef(bm3))

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


##### ACTIVITY DURING DAY #####
# start using the cleaned agoutisequence
str(agoutisequence)


# need to filter out deployments that are not fully coded
# have a lot of unclassified sequences in --> these seem to largely be the 00:00:00 automated timecapture moments, that weren't coded
# so have two variables for this, whether a sequence is a timelapse sequence (1 yes, 0 no) and whether it is uncoded (1 yes, 0 no)
# want to exclude deployments that have uncoded sequences
# can either very strictly subset on only 100% coded deployments or less strictly on all that have less than 5 uncoded sequences or something
cd <- as.data.frame(ftable(agoutisequence$uniqueloctag, agoutisequence$uncoded))
codeddeployments_total <- as.character(cd$Var1[cd$Freq == 0])

# subset only fully coded deployments
agoutisequence_c <- agoutisequence[agoutisequence$uniqueloctag %in% codeddeployments_total,]
# need to add in the days that camera wasn't triggered (?) Consider if this is necessary and if so do it with the code from above. For now can leave it 

agoutisequence_c$hour <- hour(agoutisequence_c$seq_start)
agoutisequence_c$toolusers <- as.factor(agoutisequence_c$tool_site)
agoutisequence_c$locationfactor <- as.factor(agoutisequence_c$locationName)


## GAMs
# factor by = tool_site 0/1 (tool using group yes or no)
require("mgcv")

require("fitdistrplus")
descdist(agoutisequence_c$n)
hist(agoutisequence_c$n)
## a poisson distribution makes most sense, likely zero-inflated

### TOOL USE OVER TIME

## FOR EVERYTHING ON THE SEQUENCE LEVEL: need to add offset of sequence length!!! 
####

# just plots
plot(n ~ hour, data = agoutisequence_c)

## Model 1: number of capuchins depending on hour of day, everything together
am1_zp <- gam(n ~ s(hour, bs = "cc", k = 15), family = ziP, data = agoutisequence_c, method = "REML")
summary(am1_zp) 
# visualize
plot(am1_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am1_zp) 

## Model 2: number of capuchins depending on hour of day, by tool use/vs non tool users
am2_zp <- gam(n ~ s(hour, bs = "cc", by = toolusers), family = ziP, data = agoutisequence_c, method = "REML")
summary(am2_zp) 
# visualize
plot(am2_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am2_zp) 


## Model 3: including location as random effect
am3_zp <- gam(n ~ s(hour, bs = "cc", by = toolusers) + s(locationfactor, bs = "re"), family = ziP, data = agoutisequence_c, method = "REML" )
summary(am3_zp) 
# visualize
plot(am3_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am3_zp) 


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
plot(am3_zp, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(am3_zp)[1], ylim = c(0, log(max(agoutisequence_c$n))))
points(agoutisequence_c$hour, log(agoutisequence_c$n), col = locationcol[(as.numeric(agoutisequence_c$locationfactor) + 1)], pch = agoutisequence_c$tool_site + 1)
legend("topright", inset=c(-0.35,0), legend = levels(agoutisequence_c$locationfactor), pch = 19, col = locationcol, cex = 0.9 )
legend("bottomright", inset=c(-0.25,0), legend = c("Non-tool-users", "Tool-users"), pch = c(1,2),cex = 0.9 )
dev.off()

## Model 4: add temperature
am4_zp <- gam(n ~ s(hour, bs = "cc", by = toolusers) + s(locationfactor, bs = "re") + s(temperature), family = ziP, data = agoutisequence_c, method = "REML" )
summary(am4_zp) 
# visualize
plot(am4_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am4_zp) 

##### TIDAL #####
## only taking into account sequences with capuchins?
onlycap <- agoutisequence_c[agoutisequence_c$capuchin == 1,]
str(onlycap)

descdist(onlycap$n)
hist(onlycap$n)

as.matrix(ftable(onlycap$locationfactor, onlycap$toolusers))

## Model 1: n of capuchins per tide difference (abs or not)
# abs
tm1_zp <- gam(n ~ s(tidedifabs), family = ziP, data = onlycap, method = "REML" )
summary(tm1_zp) 
# visualize
plot(tm1_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm1_zp) 

# no abs
tm1.2_zp <- gam(n ~ s(tidedif, bs = "cc"), family = ziP, data = onlycap, method = "REML" )
summary(tm1.2_zp) 
# visualize
plot(tm1.2_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm1.2_zp) 

## Model 2: split by tool use vs non tool users
# abs
tm2_zp <- gam(n ~ s(tidedifabs, by = toolusers), family = ziP, data = onlycap, method = "REML" )
summary(tm2_zp) 
# visualize
plot(tm2_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm2_zp) 

# no abs
tm2.2_zp <- gam(n ~ s(tidedif, bs = "cc", by = toolusers), family = ziP, data = onlycap, method = "REML" )
summary(tm2.2_zp) 
# visualize
plot(tm2.2_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm2.2_zp) 

## Model 3: add location of camera as random effect
# abs
tm3_zp <- gam(n ~ s(tidedifabs, by = toolusers) + s(locationfactor, bs = "re"), family = ziP, data = onlycap, method = "REML" )
summary(tm3_zp) 
# visualize
plot(tm3_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm3_zp) 

# no abs
tm3.2_zp <- gam(n ~ s(tidedif, bs = "cc", by = toolusers) + s(locationfactor, bs = "re"), family = ziP, data = onlycap, method = "REML" )
summary(tm3.2_zp) 
# visualize
plot(tm3.2_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm3.2_zp) 

## check what happens if we use time to high tide instead of low
# so 0 is high tide
# no abs
tm3.3_zp <- gam(n ~ s(tidedif2, bs = "cc", by = toolusers) + s(locationfactor, bs = "re"), family = ziP, data = onlycap, method = "REML" )
summary(tm3.3_zp) 
# visualize
plot(tm3.3_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm3.3_zp) 

## Model 4: add location as by factor (and not tool-use/non-tool-use then I guess?)
# abs
tm4_zp <- gam(n ~ s(tidedifabs, by = locationfactor), family = ziP, data = onlycap, method = "REML" )
summary(tm4_zp) 
# visualize
plot(tm4_zp, all.terms = TRUE)
# check assumptions
gam.check(tm4_zp) 

# no abs
tm4.2_zp <- gam(n ~ s(tidedif, bs = "cc", by = locationfactor), family = ziP, data = onlycap, method = "REML" )
summary(tm4.2_zp) 
# visualize
plot(tm4.2_zp, all.terms = TRUE)
# check assumptions
gam.check(tm4.2_zp) 

## BRMS
require(brms)
tbm1 <- brm(n ~ s(tidedif, bs = "cc", by = toolusers) + (1|locationfactor), family = zero_inflated_poisson(), data = onlycap, chain = 4, core = 4, control = list(adapt_delta = 0.99))
# saveRDS(tbm1, file = "tbm1.rds")
summary(tbm1)
plot(conditional_smooths(tbm1))
pp_check(tbm1) # don't get this
plot(tbm1)
pairs(tbm1)

# need to add distance from coast?

