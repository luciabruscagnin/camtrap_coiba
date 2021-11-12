## Seasonality of tool use analysis

# first run agouti_cleaning script

# use agoutisequence dataframe that's cleaned on the sequence level
str(agoutisequence)

# filter dataset down to cameras that were deployed in the Jicaron tool using groups range
agoutisequence_jt <- agoutisequence[(agoutisequence$tool_site == 1 & agoutisequence$island == "Jicaron"),]

# extract the day of the sequence and also the day of the deployment start and end
# if day of sequence is in day of deployment start or end, then it's time for that. otherwise it's 24 hours
# not too fast but works
agoutisequence_jt$dep_startday <- format(agoutisequence_jt$dep_start, "%Y-%m-%d")
agoutisequence_jt$dep_endday <- format(agoutisequence_jt$dep_end, "%Y-%m-%d")
agoutisequence_jt$seq_startday <- format(agoutisequence_jt$seq_start, "%Y-%m-%d")
agoutisequence_jt$dep_starttime <- hour(agoutisequence_jt$dep_start)
agoutisequence_jt$dep_endtime <- hour(agoutisequence_jt$dep_end)

# maybe too coarse, only on level of hours now 
agoutisequence_jt$exposure <- ifelse(agoutisequence_jt$seq_startday == agoutisequence_jt$dep_startday, 
                                  24-agoutisequence_jt$dep_starttime, 
                                  ifelse(agoutisequence_jt$seq_startday == agoutisequence_jt$dep_endday,
                                         24 - agoutisequence_jt$dep_endtime, 24))

# if tool use in sequence, then sequence duration is tool use duration
agoutisequence_jt$tooluseduration <- ifelse(agoutisequence_jt$tooluse == TRUE, agoutisequence_jt$seq_length, 0)
## need to pay attention here, because if seq_length is not available yet (due to exifdata not being pulled) then it will look the same as when there's no tool use. 

#### GAM ####
# useful resource here: https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/

# maybe make date an R Date class (which means it counts from jan 1970 so just have a numerical value) can plot with it
agoutisequence_jt$seqday <- as.Date(agoutisequence_jt$seq_startday)
# add time variable for different years. So is the date as a numerical variable, divided by 1000 to look for trend or between-year variable
agoutisequence_jt$time <- as.numeric(agoutisequence_jt$seqday)/1000
# make temperature numerical
agoutisequence_jt$temperature <- as.numeric(agoutisequence_jt$temperature)
# make location a factor
agoutisequence_jt$locationfactor <- as.factor(agoutisequence_jt$location_name)

# just plots
plot(tooluseduration ~ seqday, data = agoutisequence_jt)
plot(tooluseduration ~ month, data = agoutisequence_jt)

# aggregate per day, summing all the tool use durations
# DO I NEED to make the data frame on this level? Or can do this with the day?
# would also need to aggregate temperature per day etc if we already jump to this format. Haven't done that yet
agoutiday <- aggregate(agoutisequence_jt$tooluseduration, by = list(seq_startday = agoutisequence_jt$seq_startday), FUN = sum)
names(agoutiday)[names(agoutiday) == "x"] <- "toolusedurationday"
agoutiday2 <- left_join(agoutiday, agoutisequence_jt, by = "seq_startday")
agoutiday2 <- agoutiday2[!duplicated(agoutiday2$seq_startday),]

# for now maybe exclude deployments after certain year because data is incomplete (so we have one from mid 2019 randomly, that one goes)
# WHEN WE HAVE CODED REPRESENTATIVE SAMPLE, SELECT THAT HERE
agoutiselect <- agoutiday2
ftable(agoutiselect$location_name)

## exclude deployment start and end days for now. Discussed with Urs and he thought that if they are only small part of sample makes it easier to exclude them
agoutiselect <- agoutiselect[agoutiselect$exposure == 24, ]
# drop all the columns we don't need
agoutiselect <- agoutiselect[,c("seq_startday", "toolusedurationday", "deployment_id", "location_name", "tags", "dep_start", "dep_end", "dep_length_hours", "month", 
                               "season","island", "exposure", "time", "seqday", "locationfactor")]
agoutiselect <- droplevels.data.frame(agoutiselect)

# look at distribution of response variable
require("fitdistrplus")
descdist(agoutiselect$toolusedurationday)
hist(agoutiselect$toolusedurationday)
# gamma distribution? 
# transform tooluseduration with log?

### trying out (frequentist) gam models
## I THINK WE NEED ZERO-INFLATED MODEL? AND/OR POISSON. LOOK INTO THIS
require("mgcv")
require("brms")

## M1: very easy, just look at tooluseduration depending on month and between years (so time passing by per day)
# now doing zero inflated poisson
# with mgcv
m1 <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time), family = ziP, data = agoutiselect, method = "REML") 
# try with quasipoisson? not sure which family is best
m1c <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time), family = quasipoisson(), data = agoutiselect, method = "REML") 
summary(m1c)
plot(m1c, all.terms = TRUE, pages = 1)
# set cc as you need a cyclic cubic spine, since there should be no discontinuity between january and december
# summarize model and inspect output
summary(m1)
# plot model
plot(m1, all.terms = TRUE, pages = 1)
# can also plot only one effect, e.g. only the month. adding the intercept value and uncertainty
plot(m1, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m1)[1])

# check assumptions
gam.check(m1)
m1$outer.info
# if significant p-value, means that residuals are not randomly distributed, so are often not enough basis functions. In our case time seems to be linear
# concurvity
concurvity(m1, full = TRUE)
concurvity(m1, full = FALSE)
# see that variables time and month are very related, which makes sense, but isn't good I think. 
layout(matrix(1:2, ncol =2))
acf(resid(m1), lag.max = 36, main = "ACF") 
pacf(resid(m1), lag.max = 36, main = "pACF")
layout(1)

## M1a: with just poisson not zero inflated
m1a <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time), family = poisson(), data = agoutiselect, method = "REML")
summary(m1a)
# plot model
plot(m1a, all.terms = TRUE, pages = 1)
# can also plot only one effect, e.g. only the month. adding the intercept value and uncertainty
plot(m1a, select = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m1a)[1])

# check assumptions
gam.check(m1a)
m1a$outer.info
# concurvity
concurvity(m1, full = TRUE)
concurvity(m1, full = FALSE)
# see that variables time and month are very related, which makes sense, but isn't good I think. 
layout(matrix(1:2, ncol =2))
acf(resid(m1), lag.max = 36, main = "ACF") 
pacf(resid(m1), lag.max = 36, main = "pACF")
layout(1)

mk <- gam(toolusedurationday ~ s(time), family = ziP, data = agoutiselect, method = "REML")
summary(mk)

plot(mk, all.terms = TRUE, pages = 1)
## including camera location below, is all not working. too little data? I don't fully get it..
## M2: add in the location as a linear factor
# I think time needs many splines now because we have so many holes in the data. Should be better when we have more data
m2 <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time) + s(locationfactor, bs = "re"), data = agoutiselect, family = ziP, method = "REML") 
summary(m2)
plot(m2, all.terms = TRUE, pages = 1)

# check assumptions
gam.check(m2)
# if significant p-value, means that residuals are not randomly distributed, so are often not enough basis functions. In our case time is significant (I think it need smore data, we have holes now)
# concurvity
concurvity(m2, full = TRUE)
concurvity(m2, full = FALSE)

## M3: include location as an intercept
# this takes forever to run so probably isn't the right approach!!
m3 <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12, by = locationfactor) + s(time, by = locationfactor), family = ziP, 
           data = agoutiselect, method = "REML")
plot(m3, pages = 1)
plot(m3, all.terms =TRUE, pages = 1)


m4 <- gam(toolusedurationday ~ s(month, bs = "cc", k = 12)  + s(time, by = locationfactor, bs = "re"), data = agoutiselect, family = ziP, method = "REML")
summary(m4)
plot(m4, pages = 1)

## BRMS
# with brms which I do not understand at all
# m1b <- brm(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time), family = poisson(), data = agoutiselect, chain = 4, core = 4, control = list(adapt_delta = 0.99))
summary(m1b)
plot(conditional_smooths(m1b))
plot(m1b)
pp_check(m1b)

# with brms and zero inflated poisson
# m1ab <- brm(toolusedurationday ~ s(month, bs = "cc", k = 12) + s(time), family = zero_inflated_poisson(), data = agoutiselect, chain = 4, core = 4, control = list(adapt_delta = 0.99))
summary(m1ab)
plot(conditional_smooths(m1ab))
plot(m1ab)
pp_check(m1ab)

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
