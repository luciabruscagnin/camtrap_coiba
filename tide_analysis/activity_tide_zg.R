## Tool use & tidal cycles + Daily Activity analysis
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

require(tidyr)
require(brms)
require(mgcv)
require(ggplot2)
require(gratia)

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
# for now LIMITED TO JICARON! Need to include Coiba later if we have enough data
agoutisequence_c <- agoutisequence[(agoutisequence$uniqueloctag %in% codeddeployments_total & agoutisequence$island == "Jicaron"),]
agoutisequence_c$hour <- hour(agoutisequence_c$seq_start)
agoutisequence_c$toolusers <- as.factor(agoutisequence_c$tool_site)
agoutisequence_c$locationfactor <- as.factor(agoutisequence_c$locationName)


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

### select #####

# DON'T RUN THE FOLLOWING LINE IF YOU ADDED IN THE HOURS THAT CAMERAS WERENT TRIGGERED
agoutiselect2 <- agoutisequence_c

# exclude deployment pick up and setup days
agoutiselect2$picksetup <- ifelse(agoutiselect2$seqday == date(agoutiselect2$dep_start) | agoutiselect2$seqday == date(agoutiselect2$dep_end), 1, 0)

agoutiselect2 <- agoutiselect2[agoutiselect2$picksetup == 0, c("uniqueloctag", "seqday", "hour", "deploymentID", "locationName", "tags", "capuchin", 
                                                               "n", "seq_start", "seq_end", "seq_length", "temperature", "dep_start", "dep_end", "dep_length_hours",
                                                               "island", "tool_anvil", "tool_site", "exposure", "toolusers", "locationfactor")] # add "noanimal if you added 0's)

### This dataframe still has several sequences per hour, for hours where the camera wasn't triggered it just has a 0 for capuchin count
str(agoutiselect2)
## create variable for nr of capuchins per hour
agoutiselect2$n_hour <- (agoutiselect2$n/agoutiselect2$seq_length) * 3600
agoutiselect2$dayhour <- paste(agoutiselect2$seqday, agoutiselect2$hour, sep = " ")
agoutidayhour <- aggregate(agoutiselect2$n, by = list(dayhour = agoutiselect2$dayhour, uniqueloctag = agoutiselect2$uniqueloctag), FUN = mean)
aggregate(agoutiselect2$seq_length, by = list(dayhour = agoutiselect2$dayhour, uniqueloctag = agoutiselect2$uniqueloctag), FUN = sum)
## CHECK IF THIS WORKS AND IF IT'S NECESSARY
# maybe instead calculate number of capuchins per minute/hour whatever (per sequence using sequencelength offset)
# and then make average of that per day-hour 
# still figure this out, need to do some math (to also incorporate all the minutes/seconds there were no capuchins? unclear)
# or add all sequence lengths and all capuchins per hour and then divide capuchins by total seq length? Think about independence etc and what this means

## GAMs
testdist1 <- fitdist(agoutiselect$toolusedurationday, "gamma", method = "mme")
plot(testdist1)

# only values over 0
testdist2 <- fitdist(agoutiselect$toolusedurationday[agoutiselect$toolusedurationday > 0], "gamma", method = "mme")
plot(testdist2)

hist(agoutiselect2$n)
## a gamma distribution makes most sense to me. Likely hurdle again? 
## hour of day is not cyclic spline, as we have no observations at midnight and early in morning (explained in bottom of the heap youtube)
# use fREML for faster processing

### TOOL USE OVER TIME
## FOR EVERYTHING ON THE SEQUENCE LEVEL: need to add offset of sequence length!!! 
####

# just plots
plot(n ~ hour, data = agoutiselect2[agoutiselect2$toolusers == 1,], main = "Tool users")
plot(n ~ hour, data = agoutiselect2[agoutiselect2$toolusers == 0,], main = "Non-tool users")

# colors for two histograms in one
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

### Tool users vs non tool users
histtool <- hist(agoutiselect2$hour[agoutiselect2$tool_site == 1 & agoutiselect2$capuchin == 1], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histnotool <- hist(agoutiselect2$hour[agoutiselect2$tool_site == 0 & agoutiselect2$capuchin == 1], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histnotool, col = c2, freq = FALSE, main = "Tool users (blue) vs non-tool users (red)", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins", ylim = c(0, 0.3))
plot(histtool, col = c1, freq = FALSE, add = TRUE)

## Model 1: number of capuchins depending on hour of day, everything together
am1_zp <- gam(list(n ~ s(hour, bs = "cc", k = 24), ~ s(hour, bs = "cc", k = 24)), family = ziplss(), data = agoutiselect2, method = "fREML", nthreads = c(4,1), knots = list(hour = c(0,24)))
summary(am1_zp) 
# visualize
plot(am1_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am1_zp) 
ggplot(data.frame(Fitted = fitted(am1_zp),
                  Resid = resid(am1_zp)),
       aes(Fitted, Resid)) + geom_point()

## sequence length as outcome
am1.2_zp <- gam(seq_length ~ s(hour,, k = 8), fmaily = ziP, data = agoutiselect2, method = "fREML",nthreads = c(4,1))
summary(am1.2_zp) 
# visualize
plot(am1.2_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am1.2_zp) 

## presence/absence as outcome
am1_b <- gam(capuchin ~ s(hour), data = agoutiselect2, family = binomial, method = "REML")
summary(am1_b)
plot(am1_b)
str(agoutiselect2)
ggplot(data.frame(Fitted = fitted(am1_b),
                  Resid = resid(am1_b)),
       aes(Fitted, Resid)) + geom_point()

## Model 2: number of capuchins depending on hour of day, by tool use/vs non tool users
am2_zp <- gam(list(n ~ s(hour, bs = "cc", by = toolusers, k = 24) + toolusers, ~ s(hour, bs = "cc", by = toolusers, k = 24) + toolusers),
              family = ziplss(), knots = list(hour = c(0,24)), data = agoutiselect2, method = "REML")
summary(am2_zp) 
# visualize
plot(am2_zp, all.terms = TRUE, pages = 1)
draw(am2_zp)
# check assumptions
gam.check(am2_zp) # k too low? need to find k that works

# with factor smooth instead of by smooth
am2_zpf <- gam(n ~ s(hour, toolusers, bs = "fs"), family = ziP, data = agoutiselect2, method = "REML")
summary(am2_zpf)
plot(am2_zpf, all.terms = TRUE)

## with sequence length instead of number
am2.2_zp <- gam(seq_length ~ s(hour, bs = "cc", by = toolusers) + toolusers, family = ziP, data = agoutiselect2, method = "REML")
summary(am2.2_zp) 
# visualize
plot(am2.2_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am2.2_zp) 

## Model 3: including location as random effect
am3_zp <- gam(n ~ s(hour, by = toolusers) +  toolusers + s(locationfactor, bs = "re"), family = ziP, data = agoutiselect2, method = "fREML")
summary(am3_zp) 
# saveRDS(am3_zp, file = "am3_zp.rds")
# visualize
plot(am3_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am3_zp) 
str(am3_zp)

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

## sequence length instead of number of capuchins
am3.2_zp <- gam(seq_length ~ s(hour, by = toolusers) + toolusers + s(locationfactor, bs = "re"), family = ziP, data = agoutiselect2, method = "REML" )
summary(am3.2_zp) 
# visualize
plot(am3.2_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am3.2_zp, rep = 500) 

## binary
am3_b <- gam(capuchin ~ s(hour, by = toolusers) + toolusers + s(locationfactor, bs = "re"), data = agoutiselect2, family = binomial, method = "REML")
summary(am3_b)
plot(am3_b, all.terms = TRUE, pages = 1)

## Model 4: add temperature
am4_zp <- gam(n ~ s(hour, bs = "cc", by = toolusers) + toolusers + s(locationfactor, bs = "re") + s(temperature), family = ziP, data = agoutiselect2, method = "REML" )
summary(am4_zp) 
# visualize
plot(am4_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(am4_zp) 

##### TIDAL #####

## Need to add in the not observed zero days. Maybe this is better to do for all deployments in the agouti cleaning script?? think about where this makes sense
# this is harder here cause it's not on the day level but on the sequence level and per hour to low tide
# so can't as easily add in the True 0's or differentiate them from the false ones. 
# is it necessary??? Think about this.

#  could do rootogram to see which family is best (gamma? negative binomial? poisson?)
rootogram(tm3.2)

## only taking into account sequences with capuchins?
onlycap <- agoutisequence_c[agoutisequence_c$capuchin == 1,]
onlycap <- onlycap[rowSums(is.na(onlycap)) != ncol(onlycap), ]

descdist(onlycap$n)

hist(onlycap$n)
td1 <- fitdist(onlycap$n, "pois", method = "mle")
plot(td1)

# poisson? gamma? lognormal?

as.matrix(ftable(onlycap$locationfactor, onlycap$toolusers))

## Model 1: n of capuchins per tide difference (abs or not)
# abs
tm1 <- gam(n ~ s(tidedifabs), family = Gamma(link = "log"), data = onlycap, method = "REML" )
summary(tm1) 
# visualize
plot(tm1, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm1) 

# no abs
tm1.2 <- gam(n ~ s(tidedif, bs = "cc"), family = Gamma(link="log"), data = onlycap, method = "REML", knots = list(tidedif = c(-6,6)) )
summary(tm1.2) 
# visualize
plot(tm1.2, all.terms = TRUE, pages = 1)
draw(tm1.2)
# check assumptions
gam.check(tm1.2) 

## Model 2: split by tool use vs non tool users
# abs
tm2 <- gam(n ~ s(tidedifabs, by = toolusers) + toolusers, family = ziP, data = onlycap, method = "REML" )
summary(tm2) 
# visualize
plot(tm2, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm2) 

# no abs
tm2.2 <- gam(n ~ s(tidedif, bs = "cc", by = toolusers) + toolusers, family = poisson, data = onlycap, method = "REML",  knots = list(tidedif = c(-6,6)))
summary(tm2.2) 
# visualize
plot(tm2.2, all.terms = TRUE, pages = 1)
draw(tm2.2)
# check assumptions
gam.check(tm2.2) 

## Model 3: add location of camera as random effect
# abs
tm3_zp <- gam(n ~ s(tidedifabs, by = toolusers) + toolusers + s(locationfactor, bs = "re"), family = ziP, data = onlycap, method = "REML" )
summary(tm3_zp) 
# visualize
plot(tm3_zp, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm3_zp) 

# no abs
tm3 <- gam(n ~ s(tidedif, bs = "cc", by = toolusers) + toolusers + 
             s(locationfactor, bs = "re"), family = poisson, data = onlycap, method = "REML", knots = list(tidedif =c(-6,6)) )
summary(tm3) 
# visualize
plot(tm3, all.terms = TRUE, pages = 1)
# check assumptions
gam.check(tm3) 


# in brms
tbm1 <- brm(n ~ s(tidedif, bs = "cc", by = toolusers) + toolusers + s(locationfactor, bs = "re"), family = poisson,
            data = onlycap, knots = list(tidedif = c(-6,6)), chain = 2, core = 2, iter = 5000, 
            control = list(adapt_delta = 0.99, max_treedepth = 12))


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
tbm1 <- brm(n ~ s(tidedif, bs = "cc", by = toolusers) + toolusers + (1|locationfactor), family = zero_inflated_poisson(), data = onlycap, chain = 4, core = 4, control = list(adapt_delta = 0.99))
# saveRDS(tbm1, file = "tbm1.rds")
summary(tbm1)
plot(conditional_smooths(tbm1))
pp_check(tbm1) # don't get this
plot(tbm1)
pairs(tbm1)

# need to add distance from coast?


## save dataframes we need for analysis as rds

