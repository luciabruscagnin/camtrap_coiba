## Daily Activity analysis + other analyses
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

## Packages required
require(lubridate)
require(brms)
require(lme4)
require(dplyr)
require(ggplot2)

### Camera Inspection ####
# For now overall, but could split into age/sex categories
# have nr of capuchins per sequence inspecting the camera

# get day of deployment (from 1 up) by doing date of sequence - start date deployment?
str(agoutisequence_c)
agoutisequence_c$depdays <- as.numeric(difftime(agoutisequence_c$seq_startday, agoutisequence_c$dep_startday, units = "days"))
agoutisequence_c$depnr <- as.numeric(unlist(regmatches(agoutisequence_c$tags, gregexpr("[[:digit:]]+", agoutisequence_c$tags))))
agoutisequence_c$depnrF <- factor(agoutisequence_c$depnr, order = TRUE)

## only consider sequences with capuchins and until deployment 6
agouti_inspect <- agoutisequence_c[agoutisequence_c$capuchin == 1 & agoutisequence_c$depnr <7,]
# can either use nr of capuchins per sequence inspecting
# or proportion of capuchins per sequence inspecting
agouti_inspect$prop_inspect <- agouti_inspect$n_inspect/agouti_inspect$n
# can also do this in binomial model with "trials" (not sure what exactly this does?)

# NEED TO CHANGE THE DEPLOYMENT NR TO 'NR OF DEPLOYMENTS IN THIS GROUP'. 
# below is a very roundabout way (im sure it can be done in 1/2 lines). But it works!
# the number we have now (depnr) is really just the number of the deployment
depnrs <- aggregate(depnr ~ locationfactor, data = agouti_inspect, FUN = unique)
depnrs$nrdep <- lengths(depnrs$depnr)

agouti_inspect <- agouti_inspect[order(agouti_inspect$locationfactor, agouti_inspect$depnr),]
depnrs$seconddep <- NA
depnrs$thirddep <- NA
depnrs$fourthdep <- NA
depnrs$fifthdep <- NA
depnrs$sixthdep <- NA
for(i in 1:nrow(depnrs)) {
  depnrs$depnr[[i]] <- sort((depnrs$depnr)[[i]])
  depnrs$firstdep[i] <- depnrs$depnr[[i]][1]
  if(depnrs$nrdep[i] > 1) {
  depnrs$seconddep[i] <- depnrs$depnr[[i]][2]
  }
  if(depnrs$nrdep[i] > 2) {
  depnrs$thirddep[i] <- depnrs$depnr[[i]][3]
  }
  if(depnrs$nrdep[i] > 3) {
    depnrs$fourthdep[i] <- depnrs$depnr[[i]][4] 
  }
  if(depnrs$nrdep[i] > 4) {
    depnrs$fifthdep[i] <- depnrs$depnr[[i]][5]
  }
  if(depnrs$nrdep[i] > 5) {
    depnrs$sixthdep[i] <- depnrs$depnr[[i]][6]
  }
}

agouti_inspect$depnr2 <- NA
agouti_inspect <- left_join(agouti_inspect, depnrs[,-2], by  = "locationfactor")
agouti_inspect$depnr2 <- ifelse(agouti_inspect$depnr == agouti_inspect$firstdep, 1,
                                ifelse(agouti_inspect$depnr == agouti_inspect$seconddep, 2,
                                       ifelse(agouti_inspect$depnr == agouti_inspect$thirddep, 3,
                                              ifelse(agouti_inspect$depnr == agouti_inspect$fourthdep, 4,
                                                     ifelse(agouti_inspect$depnr == agouti_inspect$fifthdep, 5,
                                                            ifelse(agouti_inspect$depnr == agouti_inspect$sixthdep, 6,
                                                                   NA))))))

agouti_inspect$depnr2 <- factor(agouti_inspect$depnr2, order = TRUE)

# this is now per specific location, could also consider doing it per area (e.g. if tool users are used 
# to camera's you'd think they wouldn't respond that strongly to a camera being placed later)
# unless the response is also about the specific camera, not about camera traps in general
# can think about this. The old deployment nr with the tag of the deployment captures how much time has passed since the project start
# the new deployment nr captures how many deployments have already been in this specific location. 

agouti_inspect <- agouti_inspect[,c("sequenceID", "locationfactor", "seq_start", "seq_end", "seq_length",
                                    "uniqueloctag", "dep_start", "dep_end", "dep_length_hours", "month",
                                    "season", "island", "tool_anvil", "tool_site", "streambed", "n",
                                    "n_inspect", "depdays", "depnr2", "depnr", "depnrF", "toolusers")]

#### Models ####
# for now only considering sequences with capuchins!

# frequentist attempt (and abandoned)
igm1 <- lmer(n_inspect ~ depdays*toolusers + (1|depnr/locationfactor), data = agoutisequence_c)
summary(igm1)
plot(igm1)

# set normal prior
inspect_prior <- c(prior(normal(0,2), class = b))

## prior simulation (for 4a model)
ibm4a_p <- brm(n_inspect|trials(n) ~ depdays*toolusers*depnr2 + (1|locationfactor), family = "binomial", data = agouti_inspect,
             iter = 2000, chains = 2, cores = 2, backend = "cmdstan", prior = inspect_prior, sample_prior =  "only")

plot(ibm4a_p)

##### IBM1: Model 1, only looking at days since deployment started in interaction with toolusers yes or no #####
# nr of capuchins
ibm1 <- brm(n_inspect ~ depdays*toolusers, family = "poisson", data = agoutisequence_c[agoutisequence_c$capuchin == 1 & agoutisequence_c$depnr < 7,],
    iter = 2000, chains = 2, cores = 2, backend = "cmdstan", prior = inspect_prior)
#saveRDS(ibm1, file ="tide_analysis/ModelRDS/ibm1.rds")
ibm1 <- readRDS("tide_analysis/ModelRDS/ibm1.rds")
summary(ibm1)
plot(conditional_effects(ibm1))

# inspections decrease over time
# non tool users inspect more than tool users

##### IBM2: Model 2, adding nr of deployment (how many deployments have been at this location?) #####
# nr of capuchins
ibm2 <- brm(n_inspect ~ depdays*toolusers + depnr2, family = "poisson", data = agoutisequence_c[agoutisequence_c$capuchin == 1 & agoutisequence_c$depnr < 7,],
            iter = 2000, chains = 2, cores = 2, backend = "cmdstan", prior = inspect_prior)
#saveRDS(ibm3, file ="tide_analysis/ModelRDS/ibm2.rds")
#ibm2 <- readRDS("tide_analysis/ModelRDS/ibm2.rds")
summary(ibm2)
plot(conditional_effects(ibm2))
plot(ibm2)

# prop of capuchins
ibm2a <-brm(n_inspect | trials(n) ~ depdays*toolusers + depnr2, family = "binomial", data = agouti_inspect,
            iter = 2000, chains = 2, cores = 2, backend = "cmdstan", prior = inspect_prior)
#saveRDS(ibm2a, file = "tide_analysis/ModelRDS/ibm2a.rds")
ibm2a <- readRDS("tide_analysis/ModelRDS/ibm2a.rds")
plot(conditional_effects(ibm2a))

# same results as first model
# with addition that there's highest proportion inspecting in first deployment in a location and drops quickly

# still need to add camera location and deployment number (nested)

##### IBM3: Model 3, adding location factor as random effect #####
# nr of capuchins
ibm3 <- brm(n_inspect ~ depdays*toolusers + depnr2 + (1|locationfactor), family = "poisson", data = agouti_inspect,
            iter = 2000, chains = 2, cores = 2, backend = "cmdstan",  prior = inspect_prior)
#saveRDS(ibm3, file ="tide_analysis/ModelRDS/ibm3.rds")
ibm3 <- readRDS("tide_analysis/ModelRDS/ibm3.rds")
summary(ibm3)
plot(conditional_effects(ibm3))

# prop of capuchins
ibm3a <- brm(n_inspect|trials(n) ~ depdays*toolusers + depnr2 + (1|locationfactor), family = "binomial", data = agouti_inspect,
            iter = 2000, chains = 2, cores = 2, backend = "cmdstan",  prior = inspect_prior)
#saveRDS(ibm3a, file ="tide_analysis/ModelRDS/ibm3a.rds")
ibm3a <- readRDS("tide_analysis/ModelRDS/ibm3a.rds")
summary(ibm3a)
mcmc_plot(ibm3a)
plot(conditional_effects(ibm3a))

# all effects still present but less strong

##### IBM4: Model 4, adding location factor as random effect but having other 3 variables as 3-way interaction #####
# proportion
ibm4a <- brm(n_inspect|trials(n) ~ depdays*toolusers*depnr2 + (1|locationfactor), family = "binomial", data = agouti_inspect,
             iter = 2000, chains = 2, cores = 2, backend = "cmdstan",  prior = inspect_prior)
#saveRDS(ibm4a, file ="tide_analysis/ModelRDS/ibm4a.rds")
#ibm4a <- readRDS("tide_analysis/ModelRDS/ibm4a.rds")
summary(ibm4a)
plot(conditional_effects(ibm4a))
mcmc_plot(ibm4a)

# get super many max treedepth warnings and took like 2 hours so might not be right way to construct it
# I think it's because the depnr2 is 1 for most non tool using things. 

### looking at depdays for tool-users non tool users and for number of deployment
ibm5a <- brm(n_inspect|trials(n) ~ depdays*toolusers + depdays*depnr2+ (1|locationfactor), family = "binomial", data = agouti_inspect,
               iter = 2000, chains = 2, cores = 2, backend = "cmdstanr", save_pars = save_pars(all = TRUE))
#ibm5a <- add_criterion(ibm5a, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", ndraws = 2000) 
#saveRDS(ibm5a, file ="tide_analysis/ModelRDS/ibm5a.rds")
#ibm5a <- readRDS("tide_analysis/ModelRDS/ibm5a.rds")
summary(ibm5a)
plot(conditional_effects(ibm5a))
mcmc_plot(ibm5a)
pp_check(ibm5a, ndraws = 100)

# random slope for depdays
ibm5a_r <- brm(n_inspect|trials(n) ~ depdays*toolusers + depdays*depnr2+ (depdays|locationfactor), family = "binomial", data = agouti_inspect,
             iter = 2000, chains = 2, cores = 2, backend = "cmdstanr", save_pars = save_pars(all = TRUE))
ibm5a_r <- add_criterion(ibm5a_r, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", ndraws = 2000) 
#saveRDS(ibm5a_r, file ="tide_analysis/ModelRDS/ibm5a_r.rds")
#ibm5a_r <- readRDS("tide_analysis/ModelRDS/ibm5a_r.rds")
summary(ibm5a_r)
plot(conditional_effects(ibm5a_r))
mcmc_plot(ibm5a_r)
pp_check(ibm5a_r, ndraws = 100)

# this might be the most interesting model. MOST IMPORTANT ONE

tu_inspect <- plot(conditional_effects(ibm5a), plot = FALSE)[[4]]
tu_inspect + labs(y = "Proportion of present capuchins inspecting camera", x = "Days since camera deployment") + theme_bw()

depnr_plot <- plot(conditional_effects(ibm5a), plot = FALSE)[[3]]
depnr_plot + labs(y = "Proportion of present capuchins inspecting camera", x = "Number of consecutive deployments in same location") + theme_bw()

depdaysnr_plot <- plot(conditional_effects(ibm5a), plot = FALSE)[[5]]
depdaysnr_plot + labs(y = "Proportion of present capuchins inspecting camera", x = "Days since camera deployment") + theme_bw()

conditions <- make_conditions(agouti_inspect, vars = c("toolusers"))
plot(conditional_effects(ibm5a, conditions = conditions))

conditions2 <- make_conditions(agouti_inspect, vars = c("depnr2"))
plot(conditional_effects(ibm5a, conditions = conditions2))

ftable(agouti_inspect$island)
# for now dont have enough coiba data (that's only in later deployments)
# so in model is for sure
# camera location
# deployment nr (ordered factor)
# also interaction deployment nr camera location?
# days since deployment
# outcome either nr of capuchins cold or add offset of sequence length (try both)

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

