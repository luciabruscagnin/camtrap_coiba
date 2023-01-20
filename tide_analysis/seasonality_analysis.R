## Seasonality of tool use analysis
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

# start with the agoutisequence dataframe that's cleaned and aggregated to the sequence level
# Aggregate to the day level per camera
# for the seasonality analysis, then consider a subset of the data at repeat locations across seasons. 
# potentially for activity patterns on the day can work with same script but on all cameras

# call packages
library(reshape2)
library(mgcv)
library(brms)
library(gratia)
library(tidyr)
library(tidymv)
library(ggplot2)
library(dplyr)
library(lubridate)

##### SEASONALITY (OF TOOL USE) ANALYSIS #####
# filter only fully coded dataset down to cameras that were deployed in the Jicaron tool using groups range
agoutisequence_jt <- agoutisequence_c[which(agoutisequence_c$tool_site == 1 & agoutisequence_c$island == "Jicaron"),]

# if tool use in sequence, then sequence duration is tool use duration
agoutisequence_jt$tooluseduration <- ifelse(agoutisequence_jt$tooluse == TRUE, agoutisequence_jt$seq_length, 0)
## need to pay attention here, because if seq_length is not available yet (due to exifdata not being pulled) then it will look the same as when there's no tool use. 
# currently everything up to and including R6 has seq_length available

#### Going to per-day instead of per-sequence format ####
# aggregate per day & deployment-location (uniqueloctag), summing all the tool use durations
# also summing the number of capuchins foraging on each of the four types, and the total number of capuchins in sequences, to get average proportion per day
# NOTE: would also need to aggregate temperature per day etc if we already jump to this format. Haven't done that yet
agoutiday <- aggregate(list(toolusedurationday = agoutisequence_jt$tooluseduration, nr_almendraday = agoutisequence_jt$nr_almendra, nr_coconutday = agoutisequence_jt$nr_coconut,  
                            nr_fruitday = agoutisequence_jt$nr_fruit, nr_invertebrateday = agoutisequence_jt$nr_invertebrate, nr_otherday = agoutisequence_jt$nr_other, 
                            nr_unknownday = agoutisequence_jt$nr_unknown, nday = agoutisequence_jt$n),
                       by = list(seq_startday = agoutisequence_jt$seq_startday, uniqueloctag = agoutisequence_jt$uniqueloctag), FUN = sum)

agoutiday2 <- left_join(agoutiday, agoutisequence_jt, by = c("seq_startday", "uniqueloctag"))
## need unique identifier for what we want to get down to per row, which is per day per location per deployment
agoutiday2$identifier <- paste(agoutiday2$seq_startday, agoutiday2$uniqueloctag, sep = "-")
agoutiday2 <- agoutiday2[!duplicated(agoutiday2$identifier),]

#### Adding deployment days without triggers ####
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

#### Select a representative sample (day-level dataset) ####

# 1. For seasonality of what they forage on
# 2. For seasonality of tool using
# For both 1 and 2 exclude CEBUS-03 (as CEBUS-02 and CEBUS-03 are on the same anvil)
# For 2. exclude one-off surveys ("SURVEY-CEBUS-07-03-R3", "SURVEY-CEBUS-15-04-R5", # "SURVEY-CEBUS-17-03-R4") 
# so first to make the bigger one (option 1)
agoutiselect_full <- agoutiday2[which(agoutiday2$locationName != "CEBUS-03"),]

## for these deployments, add in the days that the camera was running but not triggered (and flag these)
agoutiselect_full <- left_join(depldays[depldays$uniqueloctag %in% agoutiselect_full$uniqueloctag,], agoutiselect_full, by = c("uniqueloctag", "seqday"))
agoutiselect_full$noanimal <- ifelse(is.na(agoutiselect_full$sequenceID), 1, 0)

## make sure these rows have all the variables they need for analyses 
# I think easiest way to fill up the NAs is by having a metadata frame to pull info from
metadata <- agoutiselect_full[!duplicated(agoutiselect_full$uniqueloctag), c("uniqueloctag", "deploymentID", "locationName", "tags", "dep_start", "dep_end", "dep_length_hours",
                                                                     "island", "tool_anvil", "identifier")]

for (i in 1:nrow(agoutiselect_full)) {
  if (agoutiselect_full$noanimal[i] == 1) {
    agoutiselect_full$locationName[i] <- metadata$locationName[metadata$uniqueloctag == agoutiselect_full$uniqueloctag[i]]
    agoutiselect_full$tags[i] <- metadata$tags[metadata$uniqueloctag == agoutiselect_full$uniqueloctag[i]]
    agoutiselect_full$dep_start[i] <- metadata$dep_start[metadata$uniqueloctag == agoutiselect_full$uniqueloctag[i]] 
    agoutiselect_full$dep_end[i] <- metadata$dep_end[metadata$uniqueloctag == agoutiselect_full$uniqueloctag[i]] 
    agoutiselect_full$dep_length_hours[i] <- metadata$dep_length_hours[metadata$uniqueloctag == agoutiselect_full$uniqueloctag[i]] 
    agoutiselect_full$island[i] <- metadata$island[metadata$uniqueloctag == agoutiselect_full$uniqueloctag[i]] 
    agoutiselect_full$tool_anvil[i] <- metadata$tool_anvil[metadata$uniqueloctag == agoutiselect_full$uniqueloctag[i]]
    agoutiselect_full$deploymentID[i] <- metadata$deploymentID[metadata$uniqueloctag == agoutiselect_full$uniqueloctag[i]]
  }
}

# for now just set exposure on these days to 24, assuming they dont occur on pick up or deployment days
agoutiselect_full$exposure[agoutiselect_full$noanimal == 1] <- 24
# add month and season
agoutiselect_full$month[agoutiselect_full$noanimal == 1] <- month(agoutiselect_full$seqday[agoutiselect_full$noanimal == 1])
agoutiselect_full$season <- ifelse(agoutiselect_full$month == 12 | agoutiselect_full$month == 1 | agoutiselect_full$month == 2 | agoutiselect_full$month == 3 | 
                                agoutiselect_full$month == 4, "Dry", "Wet") 

# add time variable for different years. So is the date as a numerical variable to look for trend or between-year variation
agoutiselect_full$time <- as.numeric(agoutiselect_full$seqday)
# make a numerical variable of the day of the year (so from 1-365 which day of the year it is)
agoutiselect_full$yrday <- yday(agoutiselect_full$seqday)
# make location a factor
agoutiselect_full$locationfactor <- as.factor(agoutiselect_full$locationName)
# add the year
agoutiselect_full$year <- year(agoutiselect_full$seqday)

#set the NAs in the relevant columns to 0s
agoutiselect_full <- agoutiselect_full %>%
  mutate_at(vars("toolusedurationday", "nr_almendraday", "nr_coconutday", "nr_fruitday", "nr_invertebrateday", "nr_otherday", 
                 "nr_unknownday", "nday"), ~replace_na(.,0))
# this is outdated I think
#selection_seasontools <- c("CEBUS-01-R1", "CEBUS-01-R2", "CEBUS-01-R3", "CEBUS-01-R5", "CEBUS-01-R6",
#                           "CEBUS-02-R1", "CEBUS-02-R2", "CEBUS-02-R3", "CEBUS-02-R4", "CEBUS-02-R5", "CEBUS-02-R6",
#                           "CEBUS-04-R4", "CEBUS-04-R5", "CEBUS-04-R6",
#                           "CEBUS-05-R3", "CEBUS-05-R5", "CEBUS-05-R6", 
#                           "CEBUS-06-R4", "CEBUS-06-R5", "CEBUS-06-R6",
#                           "CEBUS-08-R2", "CEBUS-08-R3", "CEBUS-08-R4", "CEBUS-08-R5", 
#                           "CEBUS-09-R2", "CEBUS-09-R3", "CEBUS-09-R4", "CEBUS-09-R5")
#agoutiselect <- agoutiselect_full[agoutiselect_full$uniqueloctag %in% selection_seasontools,]

agoutiselect <- agoutiselect_full

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


##### MGCV ####
# Things that are important to do now:
# - Understand the AR autocorrelation structure and how to deal with that (see the from the bottom of the heap web pages)
# - compare models using AIC (?)
# - what to do with and how to interpret the k-check indicating it wants more k (if this is the case)

# using mgcv package 
# some notes:
# set bs = "cc" for month and day of the year as you need a cyclic cubic spine, since there should be no discontinuity between january and december
# set knots at 0.5 & 12.5 or at 0.5 and 365.5 as January and December should match at end of January and beginning of December, but can still be different
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
          knots = list(yrday=c(0.5,365.5)), family = ziplss(), method = "REML", select= TRUE)

summary(m2)
draw(m2, overall_uncertainty = TRUE)

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
            knots = list(yrday=c(0.5,365.5), month = c(0.5,12.5)), family = ziplss(), method = "REML", select= TRUE)

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

## prior predictive simulation
# see default prior in brms and what things we can set priors for
get_prior(toolusedurationday ~ s(yrday, bs = "cc", k = 12) + s(locationfactor, bs = "re") + s(yrday, by = locationfactor, bs = "cc", k = 15), knots = list(yrday = c(0.5,365.5)),
          family = hurdle_gamma(), data = agoutiselect)

# simulate student t distribution
library(rethinking)
?rethinking::rstudent
x <- rstudent( n=1e5, nu = 10, mu = 0, sigma = 1 )
dens(x)

x2 <- rstudent( n=1e5, nu = 10, mu = 0, sigma = 2 )
dens(x2 , add=TRUE , col="red")

x3 <- rstudent( n=1e5, nu = 10, mu = 2, sigma = 2 )
dens(x3 , add=TRUE , col="green")

x4 <- rstudent( n=1e5, nu = 3, mu = 0, sigma = 2.5 )
dens(x4 , add=TRUE , col="blue")

x5 <- rstudent( n=1e5, nu = 3.7, mu = 2, sigma = 2.5 )
dens(x5 , add=TRUE , col="yellow")

# could try 
# normal(0,1)
# normal(2,1)
# normal(0,10)
# normal(0,4) 
# normal(0,2)

bm2_prior <- c(prior(normal(0,4), class = sds, coef = s(yrday, by = locationfactor, bs = "cc", k = 15)),
               prior(normal(0,2), class = sds, coef = s(yrday, bs = "cc", k = 12)))


bm_prior <- brm(toolusedurationday ~ s(yrday, bs = "cc", k = 12) + s(locationfactor, bs = "re") + s(yrday, by = locationfactor, bs = "cc", k = 15), 
                knots = list(yrday = c(0.5,365.5)), family = hurdle_gamma(), data = agoutiselect, chain = 4, core = 4, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 12), prior = bm2_prior, sample_prior = "only")


prior_summary(bm_prior)
plot(bm_prior)
summary(bm_prior)
# can do below but takes a long time
# plot(conditional_smooths(bm_prior, spaghetti = TRUE))

# now run model with prior
bm2 <- brm(toolusedurationday ~ s(yrday, bs = "cc", k = 12) + s(locationfactor, bs = "re") + s(yrday, by = locationfactor, bs = "cc", k = 15), 
           knots = list(yrday = c(0.5,365.5)), family = hurdle_gamma(), data = agoutiselect, chain = 4, core = 4, iter = 8000, prior = bm2_prior,
           control = list(adapt_delta = 0.99, max_treedepth = 12))
# saveRDS(bm2, file = "bm2_prior_full.rds")
# bm2 <- readRDS("bm2_full.rds")

summary(bm2)
plot(bm2)
plot(conditional_smooths(bm2))
plot(conditional_effects(bm2))
pp_check(bm2, type = "ecdf_overlay")

# if you want to zoom in past crazy outliers (which I dont have now)
testplot <- plot(conditional_effects(bm2), plot = FALSE)[[3]]
testplot + coord_cartesian(ylim = c(0,300))

#  check for autocorrelation
mcmc_plot(bm2, type = "acf")

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
## hurdle models
# https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/

## BRMS
# https://cran.r-project.org/web/packages/brms/vignettes/brms_distreg.html#zero-inflated-models
# https://www.rensvandeschoot.com/tutorials/brms-started/
# GAMs in BRMS
# https://discourse.mc-stan.org/t/speed-up-for-gams/11203/16
# https://fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/
# https://discourse.mc-stan.org/t/knots-and-basis-dimension-in-brms/12016/3

# Setting priors
# https://paul-buerkner.github.io/brms/reference/set_prior.html
# http://paul-buerkner.github.io/brms/reference/get_prior.html
# https://discourse.mc-stan.org/t/prior-predictive-check-multivariate-model/17220
# https://discourse.mc-stan.org/t/prior-predictive-check-multivariate-model/17220
# https://github.com/paul-buerkner/brms/issues/459
# https://www.magesblog.com/post/2018-08-02-use-domain-knowledge-to-review-prior-predictive-distributions/


## MGCV and Plotting
# https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/family.mgcv.html
# https://mfasiolo.github.io/mgcViz/articles/mgcviz.html#mgcviz-basics
# https://fromthebottomoftheheap.net/2018/10/23/introducing-gratia/
# https://gavinsimpson.github.io/gratia/
# https://cran.r-project.org/web/packages/tidymv/vignettes/plot-smooths.html
# https://stats.stackexchange.com/questions/546378/how-to-plot-gams-on-the-scale-of-the-response-with-univariate-smooths-interacti
# https://cran.r-project.org/web/packages/tidymv/vignettes/predict-gam.html
# https://stackoverflow.com/questions/60161344/is-there-a-way-to-produce-predict-gam-type-terms-values-that-are-not-cen


## Model selection with mgcv
# https://osf.io/wgc4f/wiki/mgcv:%20model%20selection/

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

########## ANALYSIS ON SEQUENCE LEVEL ######

# for question on what they forage on in generally, need agoutisequence dataset that only contains jicaron tool use group
# exclude CEBUS-03 cause it's on same anvil as CEBUS-02
agoutiselect_seq <- subset(agoutisequence_jt, agoutisequence_jt$locationName != "CEBUS-03" & agoutisequence_jt$exposure == 24)
# remove unnecessary datasets
agoutiselect_seq <- agoutiselect_seq[, !names(agoutiselect_seq) %in% c("observationID", "observationType", "cameraSetup",
                                                                       "taxonID", "scientificName", "countNew", "count", "lifeStage",
                                                                       "sex", "behaviour", "individualID", "classificationMethod", "classifiedBy",
                                                                       "classificationTimestamp", "comments.x", "locationID", "setupBy", "cameraInterval",
                                                                       "cameraHeight", "cameraHeading", "flag", "tool_item", "normal_item", "agesexF", "bothforage", 
                                                                       "dep_startday", "dep_endday", "dep_starttime", "dep_endtime", "foraging_item1", "foraging_item2")]

agoutiselect_seq <- droplevels.data.frame(agoutiselect_seq)
unique(agoutiselect_seq$uniqueloctag)

# change variables that need some altering
agoutiselect_seq <- agoutiselect_seq %>%
  mutate(locationfactor = as.factor(locationName),
         yrday = yday(seqday), 
         hour = hour(seq_start)
  )

## if you want only tool use sequences
agoutiselect_seqt <- agoutiselect_seq[which(agoutiselect_seq$tooluse == TRUE),]

hist(agoutiselect_seq$hour)
ftable(agoutiselect_seq$seq_item)

descdist(agoutiselect_seqt$tooluseduration)
testdist2 <- fitdist(agoutiselect_seqt$tooluseduration, "pois")
plot(testdist2)

### temperature

# can get temperature from external source
# or potentially, per day or hour, construct average temperature based on average of camera data
# first have to get out the crazy values (e.g. 56 degrees)
# ?

#### FORAGING ITEMS (PER SEQUENCE) ####
# first consider what they forage on in generally (seasonally)

## multinomial with more information
multifor <- melt(agoutiselect_seq, measure.vars = c("nr_almendra", "nr_coconut", "nr_fruit", "nr_invertebrate"))

# go from count for each type to number of rows for that type
multifor <- multifor %>%
  uncount(value)

# set reference category as almendras
multifor$itemtype <- multifor$variable
# in format for mgcv 
multifor$itemtype2 <- as.numeric(multifor$itemtype) - 1
multifor$sequenceIDF <- as.factor(multifor$sequenceID)
str(multifor)
# mgcv
# see this https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/multinom.html

res_m <- gam(list(itemtype2 ~ s(month, bs ="cc", k = 8) + s(sequenceIDF, bs = "re")  +  s(locationfactor, bs = "re"),
                  ~ s(month, bs = "cc", k = 11) + s(sequenceIDF, bs = "re")  + s(locationfactor, bs = "re"),
                  ~ s(month, bs = "cc", k = 11) + s(sequenceIDF, bs = "re") + s(locationfactor, bs = "re")), 
             data=multifor, family=multinom(K=3), method = "REML", knots = list(month = c(0.5,12.5)))

summary(res_m)
draw(res_m)
gam.check(res_m)

# brms
# smooth version 
res_bm2 <- brm(itemtype ~ s(month, bs ="cc", k = 12) + s(locationfactor, bs = "re") + (1|sequenceIDF), data=multifor, family="categorical", 
              knots = list(month = c(0.5,12.5)), chains=2, cores = 4, backend = "cmdstanr", save_pars = save_pars(all = TRUE),
          iter = 5000, seed = 2222, control = list(adapt_delta = 0.99))

# saveRDS(res_bm2, file = "tide_analysis/ModelRDS/res_bm2_13052022.rds")
# res_bm2 <- readRDS("tide_analysis/ModelRDS/res_bm2_13052022.rds")

summary(res_bm2)
plot(res_bm2)
plot(conditional_smooths(res_bm2, categorical = TRUE))
plot(conditional_effects(res_bm2, categorical = TRUE))

# plot
seasonitem <- plot(conditional_effects(res_bm2, categorical= TRUE), plot = FALSE)[[1]]
seasonitem + labs(y = "Probability", x = "Month") + theme_bw()

locationitem <- plot(conditional_effects(res_bm2, categorical = TRUE), plot = FALSE)[[2]]
locationitem + labs(y = "Probability", x = "Camera location") + theme_bw()

####
## APPROACH 2: Consider change in number of capuchins (or proportion of capuchins in sequence) foraging ####
####
## on each item per sequence. 
# consider only sequences in which capuchins were present (because there is no proportion of capuchins foraging on anything if there are no cpauchins)
# need to get to long format
longitems <- melt(agoutiselect_seq, measure.vars = c("nr_almendra", "nr_coconut", "nr_fruit", "nr_invertebrate"))
longitems <- longitems[which(longitems$n > 0),]
longitems$itemtype <- as.factor(longitems$variable)
longitems$nrforagers <- longitems$value
longitems$propforagers <- longitems$nrforagers/longitems$n
# might need to put sequence in as some kind of random effect? or see if you could make a data list
# where the items type is a matrix of the values of the four different types (like Jake's data?)
longitems$sequenceIDF <- as.factor(longitems$sequenceID)

# subset to when there was foraging at all (because our 0s are biased (detections of capuchins) rather than 'true' zeros)
longitems2 <- longitems[which(longitems$nrforagers >0),]

## Model 1: Only sequences in which there was FORAGING
# poisson is not entirely right family as we don't have 0s, need zero-truncated poisson
resfor_m1 <- gam(nrforagers ~ s(yrday, bs = "cc", k = 15, by = itemtype) + itemtype + s(locationfactor, bs = "re") + offset(log(n)), 
               data = longitems2, family = poisson(), method = "REML", knots = list(yrday = c(0.5, 366.5)))

summary(resfor_m1)
draw(resfor_m1)
gam.check(resfor_m1)

new_data_item <- tidyr::expand(longitems2, nesting(locationfactor, itemtype), yrday = unique(yrday), n = 1)
# need to put n to some kind of constant? 
resfor_m1_pred <- bind_cols(new_data_item,
                           as.data.frame(predict(resfor_m1, newdata = new_data_item, se.fit = TRUE)))

ggplot(resfor_m1_pred, aes(x = yrday, y = exp(fit), group = itemtype, color = itemtype)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary_bin(data = longitems2, aes(y = nrforagers/n), fun = mean, geom = "line", linetype = "dashed") +
  theme_bw() + facet_wrap( ~ itemtype)
## so see reflected here the PROPORTION of capuchins eating a certain resource 
# but only when there is foraging at all 
# and how this fluctuates seasonally

## Model 2: Only sequences in which there were CAPUCHINS (foraging or not)
resfor_m2 <- gam(nrforagers ~ s(yrday, bs = "cc", k = 15, by = itemtype) + itemtype + s(locationfactor, bs = "re") + offset(log(n)), 
               data = longitems, family = poisson(), method = "REML", knots = list(yrday = c(0.5,366.5)))

summary(resfor_m2)
draw(resfor_m2)
gam.check(resfor_m2)

new_data_item2 <- tidyr::expand(longitems, nesting(locationfactor, itemtype), yrday = unique(yrday), n = 1)
# need to put n to some kind of constant? 
resfor_m2_pred <- bind_cols(new_data_item2,
                          as.data.frame(predict(resfor_m2, newdata = new_data_item2, se.fit = TRUE)))

ggplot(resfor_m2_pred, aes(x = yrday, y = exp(fit), group = itemtype, color = itemtype)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary_bin(data = longitems, aes(y = nrforagers/n), fun = mean, geom = "line", linetype = "dashed") +
  theme_bw() + facet_wrap( ~ itemtype)
## so see reflected here the PROPORTION of capuchins eating a certain resource 
# but only when there are capuchins

## brms

## set prior
get_prior(nrforagers ~ s(yrday, bs = "cc", k = 13, by = itemtype) + itemtype + s(locationfactor, bs = "re") + offset(log(n)),
              data = longitems, family = poisson(), knots = list(yrday = c(0.5,366.5)))

# still reconsider this. This is just initial idea. 
bm3_prior <- c(prior(normal(0,4), class = b),
               prior(normal(0,4), class = sds))

res_bm3_prior <- brm(nrforagers ~ s(yrday, bs = "cc", k = 13, by = itemtype) + itemtype + s(locationfactor, bs = "re") + offset(log(n)),
               data = longitems, family = poisson(), knots = list(yrday = c(0.5,366.5)), chains = 2, cores = 4,
               iter = 2000, backend = "cmdstanr", control = list(adapt_delta = 0.99), sample_prior = "only", prior = bm3_prior)

prior_summary(res_bm3_prior)
plot(res_bm3_prior)
summary(res_bm3_prior)

## run model with priors
res_bm3 <- brm(bf(nrforagers ~ s(yrday, bs = "cc", k = 13, by = itemtype) + itemtype + s(locationfactor, bs = "re") + offset(log(n)), decomp = "QR"),
               data = longitems, family = poisson(), knots = list(yrday = c(0.5,366.5)), chains = 3, cores = 4, seed = 1234, init = "0",
               iter = 1000, backend = "cmdstanr", save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))

#saveRDS(res_bm3, "tide_analysis/ModelRDS/res_bm3.rds")
# res_bm3 <- readRDS("tide_analysis/ModelRDS/res_bm3.rds")

summary(res_bm3)
plot(conditional_effects(res_bm3))
plot(conditional_smooths(res_bm3))

#### FORAGING ITEMS (PER DAY) #####
# difference is that now I've added in the zeros, so we could conceivably look at both the absence/presence of foraging on an item
# and how it fluctuates when they do forage on it. 
# use agoutiselect_full dataset
head(agoutiselect_full)

# go to long format
longitemsday <- melt(agoutiselect_full, measure.vars = c("nr_almendraday", "nr_coconutday", "nr_fruitday", "nr_invertebrateday"))

longitemsday$itemtype <- as.factor(longitemsday$variable)
longitemsday$nrforagers <- longitemsday$value
# might need to put sequence in as some kind of random effect? or see if you could make a data list
longitemsday$sequenceIDF <- as.factor(longitemsday$sequenceID)

# Can have number of capuchins seen foraging on item at 
# or have the raw number and the total number per day as an offset. 
hist(longitemsday$nrforagers)
descdist(longitemsday$nrforagers)
plot(fitdist(longitemsday$nrforagers, "pois"))

### Model 1: Raw number of foragers with total number of capuchins as offset, two-stage poisson
# model 0 as dependent on all the normal predictions
# and when 1 also with the offset of nday 
resday_m1 <- gam(list(nrforagers ~ s(yrday, bs = "cc", k = 15, by = itemtype) + itemtype + s(locationfactor, bs = "re") , 
                      ~ s(yrday, bs = "cc", k = 15, by = itemtype) + itemtype + s(locationfactor, bs = "re") + offset(log(nday))), 
               data = longitemsday, family = ziplss(), method = "REML", knots = list(yrday = c(0.5,366.5)))

summary(resday_m1)
draw(resday_m1)
gam.check(resday_m1)

#### TOOL USE AND TIME OF DAY & LOCATIONS #####

## normal not taking age/sex into account
plot(agoutiselect_seqt$hour, agoutiselect_seqt$tooluseduration)
ftable(agoutiselect_seqt$hour)
hist(agoutiselect_seqt$hour)
agoutiselect_seqt$season <- as.factor(agoutiselect_seqt$season)

m1_tuday <- gam(tooluseduration ~ s(hour, k = 10) + s(locationfactor, bs = "re") + n, data = agoutiselect_seqt, family = poisson(),
                method = "REML")

summary(m1_tuday)
plot(m1_tuday)
draw(m1_tuday)
gam.check(m1_tuday)

new_data_tu <- tidyr::expand(agoutiselect_seqt, nesting(locationfactor), hour = unique(hour))
# need to put n to some kind of constant 
m1_tuday_pred <- bind_cols(new_data_tu,
                     as.data.frame(predict(m1_tuday, newdata = new_data_tu, se.fit = TRUE)))

# way 1: all in one plot using the tidymv package
plot_smooths(model = m1_tuday, transform = exp, series = hour) + theme(legend.position = "top")

# same plot as with tidymv package
ggplot(m1_tuday_pred, aes(x = hour, y = exp(fit), group = locationfactor, color = locationfactor)) +
  geom_line() 

ggplot(m1_tuday_pred, aes(x = hour, y = exp(fit), group = locationfactor, color = locationfactor)) +
  geom_line() +
  geom_point(data = agoutiselect_seq, aes(y = tooluseduration)) +
  facet_wrap(~ locationfactor, scales = "free")

# in brms
# need other family. Poisson? normal gamma? something else?
bm1_tuday <- brm(tooluseduration ~ s(hour, k = 12) + s(locationfactor, bs = "re") + n_tooluse, data = agoutiselect_seq,
                 family = poisson, chains = 2, cores = 4, iter  = 3000, control = list(adapt_delta = 0.99, max_treedepth = 12),
                 backend = "cmdstanr")

# saveRDS(bm1_tuday, "tide_analysis/ModelRDS/bm1_tuday_26042022.rds") 
# bm1_tuday <- readRDS("tide_analysis/ModelRDS/bm1_tuday_26042022.rds")

summary(bm1_tuday)
plot(bm1_tuday)
plot(conditional_smooths(bm1_tuday))
plot(conditional_effects(bm1_tuday))

#tool use duration and number of tool users
numbertool <- plot(conditional_effects(bm1_tuday), plot = FALSE)[[1]]
numbertool + labs(y = "Duration of tool use (seconds)", x = "Number of tool users") + theme_bw() +
  geom_point(data = agoutiselect_seqt, aes(y = tooluseduration, x = n_tooluse), inherit.aes = FALSE)

# tool use duration depending on hour of day
hourtool <- plot(conditional_effects(bm1_tuday), plot = FALSE)[[2]]
hourtool + labs(y = "Duration of tool use (seconds)", x = "Hour of day") + theme_bw() +
  stat_summary(data = agoutiselect_seqt, aes(y = tooluseduration, x = hour), fun = mean, geom = "point", inherit.aes = FALSE)

### Model 2: Adding in season/month
# with dry/wet season as by level
agoutiselect_seqt$seasonF <- as.factor(agoutiselect_seqt$season)
m2_tuday <- gam(tooluseduration ~ s(hour, by = seasonF, k = 10) + seasonF + s(locationfactor, bs = "re") + n, data = agoutiselect_seqt, family = poisson(),
                method = "REML")

summary(m2_tuday)
plot(m2_tuday)
draw(m2_tuday)
gam.check(m2_tuday)

# mostly a lot of 1 so keep n at 1
new_data_tu_season <- tidyr::expand(agoutiselect_seqt, nesting(seasonF), hour = unique(hour), locationfactor = unique(locationfactor), n = 1)

m2_pred <- bind_cols(new_data_tu_season,
                      as.data.frame(predict(m2_tuday, newdata = new_data_tu_season, se.fit = TRUE)))

ggplot(m2_pred, aes(x = hour, y = exp(fit), group = seasonF, color = seasonF)) +
  stat_summary(fun = mean, geom = "line") +
  geom_point(data = agoutiselect_seqt, aes(y = tooluseduration)) +
  facet_wrap(~ seasonF, scales = "free")

# with mean duration of tool use per hour
ggplot(m2_pred, aes(x = hour, y = exp(fit), group = seasonF, color = seasonF)) +
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(data = agoutiselect_seqt, aes(y = tooluseduration), fun = mean, geom = "point")

ggplot(agoutiselect_seqt, aes(x = hour, y = tooluseduration, group = seasonF, color = seasonF)) +
  stat_summary(fun = mean, geom = "line")

### Tool use duration by age ###
## reshape to every sequence ID 3 times, one of adult, or for juvenile, one for subadult
# have nr_toolusers and age_toolusers factor
long <- melt(agoutiselect_seqt, measure.vars = c("tu_nAdult", "tu_nSubadult", "tu_nJuvenile"))
long$age <- as.factor(long$variable)
# potentially collapse subadult with adult
long$age2 <- factor(ifelse(long$age == "tu_nSubadult", "tu_nAdult", as.character(long$age)))
long$sequenceIDF <- as.factor(long$sequenceID)
# subset to only when capuchins of this age class where there
long_short <- long[!long$value == 0,]
long_short <- droplevels.data.frame(long_short)

# for now leave out the sequence ID. By since some sequences are repeated now, you should either:
# a) do a separate model per age class
# b) do one big model but in this form (with sequence ID as factor?), accounting for sequence ID repetition:
# testgam <- gam(tooluseduration ~ s(hour, by = age, k = 16) + s(locationfactor, sequenceID, bs = "re") + n:sequenceID, data = long_short, family = poisson(), method = "REML")
# this might require either a better computer to run it on, or running with gamm4 or gamm?
plot(long_short$hour, long_short$tooluseduration)
plot(long_short$hour[which(long_short$age2 == "tu_nAdult")], long_short$tooluseduration[which(long_short$age2 == "tu_nAdult")])
plot(long_short$hour[which(long_short$age2 == "tu_nJuvenile")], long_short$tooluseduration[which(long_short$age2 == "tu_nJuvenile")])

# tool use duration depending on time of day split by adult vs juveniles
testgam <- gam(tooluseduration ~ s(hour, by = age2, k = 16) + age2 + s(locationfactor, bs = "re") + n, data = long_short, family = poisson(),
               method = "REML")

plot(testgam, trans = exp, shift = coef(testgam)[1],
     shade = TRUE, shade.col = "lightblue", pages = 1)
draw(testgam, overall_uncertainty = TRUE)
summary(testgam)
gam.check(testgam)

concurvity(testgam, full = TRUE)
concurvity(testgam, full = FALSE)

plot_smooths(model = testgam, series = hour, comparison = age2, exclude_random = TRUE) + theme(legend.position = "top")
plot_difference(testgam, series = hour, difference = list(age2 = c("tu_nAdult", "tu_nJuvenile")))

## plot on real scale

# mostly a lot of 1 so keep n at 1
new_data_tu_age <- tidyr::expand(long_short, nesting(age2), hour = unique(hour), locationfactor = unique(locationfactor), n = 1)

testgam_pred <- bind_cols(new_data_tu_age,
                           as.data.frame(predict(testgam, newdata = new_data_tu_age, se.fit = TRUE)))
str(testgam_pred)
ggplot(testgam_pred, aes(x = hour, y = exp(fit), group = age2, color = age2)) +
  stat_summary(fun = mean, geom = "line") +
  geom_point(data = long_short, aes(y = tooluseduration)) +
  facet_wrap(~ age2, scales = "free")

# with mean duration of tool use per hour
ggplot(testgam_pred, aes(x = hour, y = exp(fit), group = age2, color = age2)) +
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(data = long_short, aes(y = tooluseduration), fun = mean, geom = "point")

ggplot(long_short, aes(x = hour, y = tooluseduration, group = age2, color = age2)) +
  stat_summary(fun = mean, geom = "line")

# brms
testbm <- brm(tooluseduration ~ s(hour, by = age2, k = 16) + s(locationfactor, bs = "re") + n, data = long_short, family = poisson(),
              cores = 4, chains = 2, iter = 2000, control = list(adapt_delta = 0.99, max_treedepth = 12), backend = "cmdstanr")

## including season
testgam_season <- gam(tooluseduration ~ s(hour, by = interaction(seasonF, age2), k = 16) + age2 + seasonF +
                        s(locationfactor, bs = "re") + n, data = long_short, family = poisson(),
               method = "REML")

plot(testgam_season)
draw(testgam_season, overall_uncertainty = TRUE)
summary(testgam_season)
gam.check(testgam_season)

# mostly a lot of 1 so keep n at 1
new_data_tu_ageseason <- tidyr::expand(long_short, nesting(age2, seasonF), hour = unique(hour), locationfactor = unique(locationfactor), n = 1)

testgam_season_pred <- bind_cols(new_data_tu_ageseason,
                          as.data.frame(predict(testgam_season, newdata = new_data_tu_ageseason, se.fit = TRUE)))

ggplot(testgam_season_pred, aes(x = hour, y = exp(fit), group = age2, color = age2)) +
  stat_summary(fun = mean, geom = "line") +
  geom_point(data = long_short, aes(y = tooluseduration)) +
  facet_wrap(~ season, scales = "free")

# with mean duration of tool use per hour
ggplot(testgam_season_pred, aes(x = hour, y = exp(fit), group = age2, color = age2)) +
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(data = long_short, aes(y = tooluseduration), fun = mean, geom = "point") +
  facet_wrap(~seasonF) + coord_cartesian(ylim = c(0,150))

ggplot(long_short, aes(x = hour, y = tooluseduration, group = age2, color = age2)) +
  stat_summary(fun = mean, geom = "line")

## brms
tuday_as <- brm(tooluseduration ~ s(hour, by = interaction(season, age2), k = 16) + age2 + season +
                  s(locationfactor, bs = "re") + n, data = long_short, family = poisson(), 
                chains = 2, cores = 2, iter = 2000, control = list(adapt_delta = 0.99, max_treedepth = 12), backend = "cmdstanr" )
## maximum treedepth reached in 99% of cases. Need to formulate this differently. Look at how to do 
# interaction by smooth in brms. So 3-way smooth with two factors in brms

# saveRDS(tuday_as, "tide_analysis/ModelRDS/tuday_as_28042022.rds") 
# tuday_as <- readRDS("tide_analysis/ModelRDS/tuday_as_28042022.rds")

summary(tuday_as)
plot(tuday_as)
plot(conditional_smooths(tuday_as))
plot(conditional_effects(tuday_as))
## plots are interesting though! 
# longer tool use duration by adults than juveniles
# longer tool use duration in dry vs wet season
# global tool use hour pattern with peak around 11 and around 20. Dip around 12/13 (hottest part of day?)
# In dry season adults use tool for much longer than juveniles, and longer than adults and juveniles in wet season




# average nr of tool users per hour per age class (only if there was tool use occurring)
plot(long$hour, long$n_tooluse)
plot(long$hour[which(long$age2 == "tu_nAdult")], long$value[which(long$age2 == "tu_nAdult")])
plot(long$hour[which(long$age2 != "tu_nAdult")], long$value[which(long$age2 != "tu_nAdult")])

testgam2 <- gam(value ~ s(hour, by = age2, k = 16) + s(locationfactor, bs = "re"), data = long_short, family = ziP(),
               method = "REML")

plot(testgam2)
draw(testgam2, overall_uncertainty = TRUE)
summary(testgam2)
gam.check(testgam2)
str(long)

# mostly a lot of 1 so keep n at 1
new_data_tu_age2 <- tidyr::expand(long_short, nesting(age2), hour = unique(hour), locationfactor = unique(locationfactor))

testgam2_pred <- bind_cols(new_data_tu_age2,
                          as.data.frame(predict(testgam2, newdata = new_data_tu_age2, se.fit = TRUE)))

ggplot(testgam_pred, aes(x = hour, y = exp(fit), group = age2, color = age2)) +
  stat_summary(fun = mean, geom = "line") +
  geom_point(data = long_short, aes(y = tooluseduration)) +
  facet_wrap(~ age2, scales = "free")

ggplot(testgam2_pred, aes(x = hour, y = exp(fit), group = age2, color = age2)) +
  stat_summary(fun = mean, geom = "line")


#### HOW MANY INDIVIDUALS USE TOOLS #####
plot(agoutiselect_seq$n_tooluse, agoutiselect_seq$n)
ftable(agoutiselect_seq$hour, agoutiselect_seq$n_tooluse)
# feel like proportion of tool using events iwth more than 1 tool users are rarer in morning/evening. How to model this?


# check assumptions
# concurvity (correlation between predictors)
concurvity(m2, full = TRUE)
concurvity(m2, full = FALSE)
# autocorrelation
# STILL NEED TO UNDERSTAND THIS
acf(resid(m2), lag.max = 36, main = "ACF") 
pacf(resid(m2), lag.max = 36, main = "pACF")

