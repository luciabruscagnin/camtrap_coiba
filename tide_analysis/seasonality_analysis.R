## Seasonality of tool use analysis

# first run agouti_cleaning script

# use agoutisequence dataframe that's cleaned on the sequence level
str(agoutisequence)

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

# if tool use in sequence, then sequence duration is tool use duration
agoutisequence$tooluseduration <- ifelse(agoutisequence$tooluse == TRUE, agoutitool$seq_length, 0)
## need to pay attention here, because if seq_length is not available yet (due to exifdata not being pulled) then it will look the same as when there's no tool use. 

# aggregate per day, summing all the tool use durations
aggregate(agoutisequence$tooluseduration, by = list(date = agoutisequence$seq_startday), FUN = sum)
plot(agoutisequence$tooluseduration ~ agoutisequence$month)

## Note:
# Maybe we also need to have a variable where the tool using duration is 0 on days that didn't have tool use..
# so need to make sure we don't lose those sequences
# so maybe put the agoutitool dataset back with the sequence one, or create that variable there. 

#### GAM ####
# useful resource here: https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/

# maybe make date an R Date class (which means it counts from jan 1970 so just have a numerical value) can plot with it
agoutisequence$seqday <- as.Date(agoutisequence$seq_startday)
# add time variable for different years. So is the date as a numerical variable, divided by 1000 to look for trend or between-year variable
agoutisequence$time <- as.numeric(agoutisequence$seqday)/1000
# make temperature numerical
agoutisequence$temperature <- as.numeric(agoutisequence$temperature)
# make location a factor
agoutisequence$locationfactor <- as.factor(agoutisequence$location_name)

# just plots
plot(tooluseduration ~ seqday, data = agoutisequence)
plot(tooluseduration ~ month, data = agoutisequence)

# for now maybe exclude deployments after certain year because data is incomplete (so we have one from mid 2019 randomly, that one goes)
# WHEN WE HAVE CODED REPRESENTATIVE SAMPLE, SELECT THAT HERE
ftable(agoutiselect$dep_startday)
agoutiselect <- agoutisequence[!agoutisequence$dep_startday == "2019-07-25",]
str(agoutiselect)

### trying out (frequentist) gam models
## I THINK WE NEED ZERO-INFLATED MODEL? AND/OR POISSON. LOOK INTO THIS
require("mgcv")
## M1: very easy, just look at tooluseduration depending on month and between years (so time passing by per day)
m1 <- gamm(tooluseduration ~ s(month, bs = "cc", k = 12) + s(time), data = agoutiselect, method = "REML") 
# set cc as you need a cyclic cubic spine, since there should be no discontinuity between january and december
# summarize model and inspect output
summary(m1$gam)
# plot model
plot(m1$gam, all.terms = TRUE, pages = 1)
# can also plot only one effect, e.g. only the month. adding the intercept value and uncertainty
plot(m1$gam, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m1$gam)[1])

layout(matrix(1:2, ncol = 2))
plot(m1$gam, scale = 0)
layout(1)
coef(m1$gam)

# check assumptions
gam.check(m1$gam)
# if significant p-value, means that residuals are not randomly distributed, so are often not enough basis functions. In our case time seems to be linear
# concurvity
concurvity(m1$gam, full = TRUE)
concurvity(m1$gam, full = FALSE)
# see that variables time and month are very related, which makes sense, but isn't good I think. 

# check assumptions
layout(matrix(1:2, ncol =2))
acf(resid(m1$lme), lag.max = 36, main = "ACF") 
pacf(resid(m1$lme), lag.max = 36, main = "pACF")

## M2: add in temperature (now temperature from sequences)
m2 <- gamm(tooluseduration ~ s(month, bs = "cc", k = 12) + s(time) + s(temperature) , data = agoutiselect, method = "REML") 
# set cc as you need a cyclic cubic spine, since there should be no discontinuity between january and december
summary(m2$gam)
layout(matrix(1:3, ncol = 3))
plot(m2$gam, scale = 0)
layout(1)
coef(m2$gam)

# plot model
plot(m2$gam, all.terms = TRUE, pages = 1)
# can also plot only one effect, e.g. only the month. adding the intercept value and uncertainty
plot(m2$gam, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m2$gam)[1])

# check assumptions
gam.check(m2$gam)
# if significant p-value, means that residuals are not randomly distributed, so are often not enough basis functions. In our case time seems to be linear
# concurvity
concurvity(m2$gam, full = TRUE)
concurvity(m2$gam, full = FALSE)
# this seems better, don't fully get it
layout(matrix(1:2, ncol =2))
acf(resid(m2$lme), lag.max = 36, main = "ACF") 
pacf(resid(m2$lme), lag.max = 36, main = "pACF")

## M3: add in the location as a linear factor
# I think time needs many splines now because we have so many holes in the data. Should be better when we have more data
m3 <- gamm(tooluseduration ~ s(month, bs = "cc", k = 12) + s(time) + s(temperature) + locationfactor, data = agoutiselect, method = "REML") 
# set cc as you need a cyclic cubic spine, since there should be no discontinuity between january and december
summary(m3$gam)
layout(matrix(1:3, ncol = 3))
plot(m3$gam, scale = 0)
layout(1)
coef(m3$gam)
plot(m3$gam, all.terms = TRUE, pages = 1)

# check assumptions
gam.check(m3$gam)
# if significant p-value, means that residuals are not randomly distributed, so are often not enough basis functions. In our case time is significant (I think it need smore data, we have holes now)
# concurvity
concurvity(m3$gam, full = TRUE)
concurvity(m3$gam, full = FALSE)

layout(matrix(1:2, ncol =2))
acf(resid(m3$lme), lag.max = 36, main = "ACF") 
pacf(resid(m3$lme), lag.max = 36, main = "pACF")

## M4: include different smooths for different levels of location
# this takes forever to run so probably isn't the right approach!!
m4 <- gamm(tooluseduration ~ s(month, bs = "cc", k = 12, by = locationfactor) + s(time, by = locationfactor) + s(temperature, by = locationfactor) + locationfactor, 
           data = agoutiselect, method = "REML")
plot(m4, pages = 1)

# start easy, per month
# introduce exposure
# built up.



# then match with exposure and other variables we want to have