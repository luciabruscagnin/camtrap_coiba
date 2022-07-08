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
library(ggnewscale)

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

agoutiselect2 <- agoutiselect2[c("deploymentID", "sequenceID", "scientificName", "locationName", "longitude", "latitude", "cameraSetup", "seq_start", 
                                                               "seq_end", "seq_length", "temperature", "dep_start", "dep_end", "dep_length_hours", "month", 
                                                               "season", "island", "tool_anvil", "tool_site", "streambed", "capuchin", "n", "tooluse", "tidedifabs", 
                                                               "tidedif", "tidedif2", "uniqueloctag", "seqday", "hour", "toolusers",  "picksetup", "dataorigin")] # add "noanimal if you added 0's)

# exclude days on which cameras were deployed or picked up (to take away that bias)
agoutiselect_t <- agoutiselect2[(agoutiselect2$picksetup == 0),]
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
# remove only NA rows and cameras more than 50meters from coast
onlycap_tj <- onlycap_tj[which(is.na(onlycap_tj$deploymentID) == FALSE & onlycap_tj$distcoast < 50),]
# new predictor, whether it is the Wet (May-November) or the Dry season (December-April)
onlycap_tj$seasonF <- as.factor(onlycap_tj$season)

## now similar sampling
hist(onlycap_tj$distcoast[onlycap_tj$toolusers == "Non-tool-users"])
hist(onlycap_tj$distcoast[onlycap_tj$toolusers == "Tool-users"])

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

### TIDAL GAMS ####
## outcome variable:
# number of capuchins per sequence
# only Jicaron

## predictors:
# - time until next low tide (tidedif)
# - tool users 1/0, tool using group on Jicaron/Coiba vs not tool using groups
# - location factor, factor for each location
# - distance to coast for each camera in meters
# - season (wet or dry)

#### MGCV #### 

## Model 1: Effect of time to nearest low tide on number of capuchins, split for tool using and non tool using groups and with locationfactor as random effect
###
# non-absolute time to low tide (-6 hours to +6 hours, where 0 is low tide)
tm1 <- gam(n ~ s(tidedif, bs = "cc", by = toolusers, k = 10) + toolusers + 
               s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML", knots = list(tidedif =c(-6,6)) )
summary(tm1) 
plot(tm1, all.terms = TRUE, pages = 1)
gam.check(tm1) 

## Model 2: Including distance to coast in model 1
###

# non-absolute tide difference
tm2 <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
             te(tidedif, distcoast, bs = c("cc", "tp"), by = toolusers, k = c(10,6), m = 1) + toolusers +
             s(locationfactor, bs = "re"), family = Gamma(link = "log"), data = onlycap_tj, method = "REML",
           select = TRUE, knots = list(tidedif =c(-6,6)) )

summary(tm2) 
draw(tm2, pages = 1)
gam.check(tm2) 

### Model 3: Including seasonality
###

## Option A: Split models for tool use and non tool use
# tool using group
tm3 <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
            te(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
            s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], method = "REML",
          select = TRUE, knots = list(tidedif =c(-6,6)) )

summary(tm3)
draw(tm3, pages = 1)
gam.check(tm3) 

## time of day instead of tide difference
tm3_h <- gam(n ~ te(hour, distcoast, bs = c("tp", "tp"), k = c(10, 6)) +
               te(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
               s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], method = "REML",
             select = TRUE)

summary(tm3_h)
draw(tm3_h, pages = 1)
gam.check(tm3_h) 

# looking at non tool users
tm3b <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
            te(tidedif, distcoast, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
            s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], method = "REML",
          select = TRUE, knots = list(tidedif =c(-6,6)) )

summary(tm3b)
draw(tm3b, pages = 1)
gam.check(tm3b) 

## time of day instead of tide difference
tm3b_h <- gam(n ~ te(hour, distcoast, bs = c("tp", "tp"), k = c(10, 6)) +
               te(hour, distcoast, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
               s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], method = "REML",
             select = TRUE)

summary(tm3b_h)
draw(tm3b_h, pages = 1)
gam.check(tm3b_h) 

## Option B: Together in one model with a 4-way interaction
tm4 <-gam(n ~ te(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6)) +
             te(tidedif, distcoast, bs = c("cc", "tp"), by = interaction(seasonF, toolusers), k = c(10,6), m = 1) + toolusers + seasonF + 
             s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj, method = "REML",
           select = TRUE, knots = list(tidedif =c(-6,6)) )

summary(tm4)
plot(tm4, pages = 1)
gam.check(tm4) 

###
## Model 5: Comparing hot and cold low tides to look at temperature 
###

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
## 08.07.2022: now all models are only on tool use and non tool use data within 50 m from coast, and with z-transformed tidedif and distcoast data (and hour). 

## for plotting, create universal colorpalette for the heatmap
inferncol <- viridis_pal(option = "B")(10)
mybreaks <- seq(-0.30, 0.30, length.out = 11)
breaklabel <- function(x){
  labels<- paste0(mybreaks[1:10], "-", mybreaks[2:11])
  labels[1:x]
}

### z-transform 
# standardize distance to coast 
onlycap_tj$distcoast_z <- scale(onlycap_tj$distcoast, center = TRUE, scale = TRUE)
# standardize tidedif
onlycap_tj$tidedif_z <- scale(onlycap_tj$tidedif, center = TRUE, scale = TRUE)
# standardize hour of day
onlycap_tj$hour_z <- scale(onlycap_tj$hour, center = TRUE, scale = TRUE)

meandist <- mean(onlycap_tj$distcoast)
sddist <- sd(onlycap_tj$distcoast)
meantide <- mean(onlycap_tj$tidedif)
sdtide <- sd(onlycap_tj$tidedif)
meanhour <- mean(onlycap_tj$hour)
sdhour <- sd(onlycap_tj$hour)

##### PRIOR ####
# CHECK if these priors still make sense with the z-transformed data
# priors for tidal models
tidal_prior <- c(prior(normal(0, 2), class = Intercept),
               prior(normal(0,2), class = b),
               prior(normal(0,2), class = sds))

# prior simulation
tbm1_prior <- brm(n | trunc(lb=1) ~ t2(tidedif_z, distcoast_z, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
              t2(tidedif_z, distcoast_z, bs = c("cc", "tp"), by = toolusers, k = c(10, 6), m = 1) + toolusers +
              s(locationfactor, bs = "re"), family = poisson(),  knots = list(tidedif =c(-6,6)),  data = onlycap_tj, chain = 2, core = 2, iter = 1000,
              prior = tidal_prior, sample_prior = "only", backend = "cmdstanr")

summary(tbm1_prior)
prior_summary(tbm1_prior)
mcmc_plot(tbm1_prior)

######### run until here to prepare for running or loading models #######

##### MODEL 1: TU AND NTU TOGETHER, NO SEASON ######
## Number of capuchins by tidedif (not absolute) and split by toolusers, with locationfactor as random effect and distance to coast
####
tbm1 <- brm(n  ~ t2(tidedif, distcoast, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
              t2(tidedif, distcoast, bs = c("cc", "tp"), by = toolusers, k = c(10, 6), m = 1) + toolusers +
              s(locationfactor, bs = "re"), family = poisson(),  knots = list(tidedif_z =c(-6,6)),  data = onlycap_tj, 
            chain = 2, core = 2, iter = 3000, save_pars = save_pars(all = TRUE),
            control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

#tbm1 <- add_criterion(tbm1, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99, max_treedepth = 12), backend = "cmdstanr", ndraws = 2000) 
#saveRDS(tbm1, "tide_analysis/ModelRDS/tbm1_z.rds")
# tbm1 <- readRDS("tide_analysis/ModelRDS/tbm1_pois.rds")

mcmc_plot(tbm1,type = "trace")
mcmc_plot(tbm1) #plot posterior intervals
mcmc_plot(tbm1, type = "acf_bar") # autocorrelation
summary(tbm1)

# ce_tbm1 <- readRDS("tide_analysis/ModelRDS/ce_tbm1_prior.rds") # prior for truncated one, poisson for non truncated one
plot(conditional_effects(tbm1))

# cs_tbm1 <- readRDS("tide_analysis/ModelRDS/cs_tbm1_prior.rds")
plot(conditional_smooths(tbm1))

## Checks
pp_check(tbm1, ndraw = 100) 
loo(tbm1)
loo_R2(tbm1)
bayes_R2(tbm1)

# Visualize: Compute posterior predictions with posterior_epred and plot contourplot from that
predict_tbm1_p <- posterior_smooths(tbm1, smooth = 't2(tidedif_z,distcoast_z,bs=c("cc","tp"),by=toolusers,k=c(10,6),m=1)')

# mean of each column is what I'm looking for
tbm1$data$fit_tooltide <- as.numeric(colMedians(predict_tbm1_p))

d1_tu <- with(tbm1$data[tbm1$data$toolusers == "Tool-users",], interp(x = tidedif_z, y = distcoast_z, z = fit_tooltide, duplicate = "mean"))
d1_ntu <-  with(tbm1$data[tbm1$data$toolusers == "Non-tool-users",], interp(x = tidedif_z, y = distcoast_z, z = fit_tooltide, duplicate = "mean"))

d2_tu <- melt(d1_tu$z, na.rm = TRUE)
names(d2_tu) <- c("x", "y", "fit")
d2_tu$tidedif <- d1_tu$x[d2_tu$x] * sdtide + meantide
d2_tu$distcoast <- d1_tu$y[d2_tu$y] *sddist + meandist

d2_ntu <- melt(d1_ntu$z, na.rm = TRUE)
names(d2_ntu) <- c("x", "y", "fit")
d2_ntu$tidedif <- d1_ntu$x[d2_ntu$x] * sdtide + meantide
d2_ntu$distcoast <- d1_ntu$y[d2_ntu$y] *sddist + meandist

d2_tu$toolusers <- "Tool-users"
d2_ntu$toolusers <- "Non-tool-users"

d2_t <- rbind(d2_tu, d2_ntu)
d2_t$toolusers <- factor(d2_t$toolusers, levels = c("Tool-users",  "Non-tool-users"))

# png("tide_analysis/ModelRDS/tuvsntu_pred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/tuvsntu_pred.png", width = 12, height = 6))
ggplot(data = d2_t, aes(x = tidedif, y = distcoast, z = fit)) +
  geom_contour_filled(breaks = mybreaks, show.legend = TRUE) + scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE) + 
  theme_bw() + theme(panel.grid = element_blank()) +  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)") +
  geom_rug(data = onlycap_tj, aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), 
        legend.title = element_text(size =16), axis.text = element_text(size = 14)) + facet_wrap(~toolusers, scales = "free")
# dev.off()

##### MODEL 2 AND 2A: ADDING SEASON, SPLIT BY TU/NTU #####
####
###### Tool users ####
tbm2 <- brm(n ~ t2(tidedif_z, distcoast_z, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
            t2(tidedif_z, distcoast_z, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
            s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], 
          knots = list(tidedif_z =c(-6,6)), chain = 2, core = 2, iter = 3000, save_pars = save_pars(all = TRUE),
          control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2 <- add_criterion(tbm2, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
#saveRDS(tbm2, "tide_analysis/ModelRDS/tbm2_ztransform.rds")
#tbm2 <- readRDS("tide_analysis/ModelRDS/tbm2_ztransform.rds")

mcmc_plot(tbm2,type = "trace")
mcmc_plot(tbm2) #plot posterior intervals
summary(tbm2)

# ce_tbm2 <- readRDS("tide_analysis/ModelRDS/ce_tbm2.rds")
plot(conditional_effects(tbm2))

# cs_tbm2 <- readRDS("tide_analysis/ModelRDS/cs_tbm2.rds")
plot(conditional_smooths(tbm2))

## Checks
pp_check(tbm2, ndraw = 100) 
loo(tbm2)
loo_R2(tbm2)
bayes_R2(tbm2)

## Visualizing
## Option 2: Predicting data
predict_tbm2 <- posterior_smooths(tbm2, smooth = 't2(tidedif_z,distcoast_z,bs=c("cc","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2$data$fit_seasontide <- as.numeric(colMedians(predict_tbm2))

d1_wet <- with(tbm2$data[tbm2$data$seasonF == "Wet",], interp(x = tidedif_z, y = distcoast_z, z = fit_seasontide, duplicate = "mean"))
d1_dry <-  with(tbm2$data[tbm2$data$seasonF == "Dry",], interp(x = tidedif_z, y = distcoast_z, z = fit_seasontide, duplicate = "mean"))

d2_wet <- melt(d1_wet$z, na.rm = TRUE)
names(d2_wet) <- c("x", "y", "fit")
d2_wet$tidedif <- d1_wet$x[d2_wet$x] * sdtide + meantide
d2_wet$distcoast <- d1_wet$y[d2_wet$y] * sddist + meandist

d2_dry <- melt(d1_dry$z, na.rm = TRUE)
names(d2_dry) <- c("x", "y", "fit")
d2_dry$tidedif <- d1_dry$x[d2_dry$x] * sdtide + meantide
d2_dry$distcoast <- d1_dry$y[d2_dry$y] * sddist + meandist

d2_dry$seasonF <- "Dry"
d2_wet$seasonF <- "Wet"

d2 <- rbind(d2_dry, d2_wet)
d2$seasonF <- factor(d2$seasonF, levels = c("Dry", "Wet") )

# png("tide_analysis/ModelRDS/toolusersplot_pred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot_pred.png", width = 12, height = 6))
ggplot(data = d2, aes(x = tidedif, y = distcoast, z = fit)) +
  geom_contour_filled(breaks = mybreaks, show.legend = TRUE) + scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE)  + 
  theme_bw() + theme(panel.grid = element_blank()) +  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), 
        legend.title = element_text(size =16), axis.text = element_text(size=14)) +  facet_wrap(~seasonF)
# dev.off()

## plot of number of capuchins
## number of capuchins per sequence wet vs dry season
ncap <- plot(conditional_effects(tbm2), plot = FALSE)[[1]]
ncap + labs(y = "Average number of capuchins per sequence", x = "Season") + theme_bw() 

###### Non tool users ####
tbm2a <- brm(n  ~ t2(tidedif_z, distcoast_z, bs = c("cc", "tp"), k = c(10, 6), full = TRUE) +
               t2(tidedif_z, distcoast_z, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
               s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], 
             knots = list(tidedif_z =c(-6,6)), chain = 2, core = 2, iter = 3000, save_pars = save_pars(all = TRUE),
             control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2a <- add_criterion(tbm2a, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
# check add criterion, gives error longer object length is not a multiple of shorter object length
# saveRDS(tbm2a, "tide_analysis/ModelRDS/tbm2a_z.rds")
# tbm2a <- readRDS("tide_analysis/ModelRDS/tbm2a_z.rds")

mcmc_plot(tbm2a,type = "trace")
mcmc_plot(tbm2a) #plot posterior intervals
summary(tbm2a)
plot(conditional_smooths(tbm2a))
plot(conditional_effects(tbm2a))

ntoolusersseason <- plot(conditional_effects(tbm2a), plot = FALSE)[[5]]
ntoolusersseason + labs(y = "Average number of capuchins per sequence", x = "Hours before and after nearest low tide (peak of low tide at 0)") + theme_bw() +
  stat_summary_bin(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(y = n, x = tidedif, group = seasonF, color = seasonF), 
                   bins = 12, fun = mean, geom = "point", inherit.aes =  FALSE)

predict_tbm2a <- posterior_smooths(tbm2a, smooth = 't2(tidedif_z,distcoast_z,bs=c("cc","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2a$data$fit_seasontide <- as.numeric(colMedians(predict_tbm2a))

d1a_wet <- with(tbm2a$data[tbm2a$data$seasonF == "Wet",], interp(x = tidedif_z, y = distcoast_z, z = fit_seasontide, duplicate = "mean"))
d1a_dry <-  with(tbm2a$data[tbm2a$data$seasonF == "Dry",], interp(x = tidedif_z, y = distcoast_z, z = fit_seasontide, duplicate = "mean"))

d2a_wet <- melt(d1a_wet$z, na.rm = TRUE)
names(d2a_wet) <- c("x", "y", "fit")
d2a_wet$tidedif <- d1a_wet$x[d2a_wet$x] * sdtide + meantide
d2a_wet$distcoast <- d1a_wet$y[d2a_wet$y] * sddist + meandist

d2a_dry <- melt(d1a_dry$z, na.rm = TRUE)
names(d2a_dry) <- c("x", "y", "fit")
d2a_dry$tidedif <- d1a_dry$x[d2a_dry$x] * sdtide + meantide
d2a_dry$distcoast <- d1a_dry$y[d2a_dry$y] * sddist + meandist

d2a_dry$seasonF <- "Dry"
d2a_wet$seasonF <- "Wet"

d2a <- rbind(d2a_dry, d2a_wet)
d2a$seasonF <- as.factor(d2a$seasonF)

# png("tide_analysis/ModelRDS/nontoolusersplot_pred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/nontoolusersplot_pred.png", width = 12, height = 6))
ggplot(data = d2a, aes(x = tidedif, y = distcoast, z = fit)) +
  geom_contour_filled(breaks = mybreaks, show.legend = TRUE) + scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE)  + 
  theme_bw() + theme(panel.grid = element_blank()) +  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(x = tidedif, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), 
        legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +  facet_wrap(~seasonF, scales = "free")
#dev.off()

##### TIME OF DAY ######
###### Tool users ####
tbm2_h <- brm(n ~ t2(hour_z, distcoast_z, bs = c("tp", "tp"), k = c(10, 6), full = TRUE) +
              t2(hour_z, distcoast_z, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
              s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], 
            chain = 2, core = 2, iter = 3000, save_pars = save_pars(all = TRUE),
            control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2_h <- add_criterion(tbm2_h, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
#saveRDS(tbm2_h, "tide_analysis/ModelRDS/tbm2_hz.rds")
# tbm2_h <- readRDS("tide_analysis/ModelRDS/tbm2_hz.rds")

mcmc_plot(tbm2_h,type = "trace")
mcmc_plot(tbm2_h) #plot posterior intervals
summary(tbm2_h)
plot(conditional_effects(tbm2_h))
plot(conditional_smooths(tbm2_h))

## plot from predicting
predict_tbm2_h <- posterior_smooths(tbm2_h, smooth = 't2(hour_z,distcoast_z,bs=c("tp","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2_h$data$fit_seasonhour <- as.numeric(colMedians(predict_tbm2_h))

d1h_wet <- with(tbm2_h$data[tbm2_h$data$seasonF == "Wet",], interp(x = hour_z, y = distcoast_z, z = fit_seasonhour, duplicate = "mean"))
d1h_dry <-  with(tbm2_h$data[tbm2_h$data$seasonF == "Dry",], interp(x = hour_z, y = distcoast_z, z = fit_seasonhour, duplicate = "mean"))

d2h_wet <- melt(d1h_wet$z, na.rm = TRUE)
names(d2h_wet) <- c("x", "y", "fit")
d2h_wet$hour <- d1h_wet$x[d2h_wet$x] * sdhour + meanhour
d2h_wet$distcoast <- d1h_wet$y[d2h_wet$y] * sddist + meandist

d2h_dry <- melt(d1h_dry$z, na.rm = TRUE)
names(d2h_dry) <- c("x", "y", "fit")
d2h_dry$hour <- d1h_dry$x[d2h_dry$x] * sdhour + meanhour
d2h_dry$distcoast <- d1h_dry$y[d2h_dry$y] * sddist + meandist

d2h_dry$seasonF <- "Dry"
d2h_wet$seasonF <- "Wet"

d2h <- rbind(d2h_dry, d2h_wet)
d2h$seasonF <- factor(d2h$seasonF, levels = c("Dry", "Wet"))

# png("tide_analysis/ModelRDS/toolusersplot_pred_hour.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/toolusersplot_pred_hour.png", width = 12, height = 6))
ggplot(data = d2h, aes(x = hour, y = distcoast, z = fit)) +
  geom_contour_filled(breaks = mybreaks, show.legend = TRUE) + scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE)  +
  theme_bw() + theme(panel.grid = element_blank()) +  labs(x = "Hour of the day", y = "Distance to coast (m)") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = hour, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), 
        legend.text =  element_text(size = 16), legend.title = element_text(size =16)) +  facet_wrap(~seasonF)
#dev.off()

###### Non tool users ####
tbm2a_h <- brm(n ~ t2(hour_z, distcoast_z, bs = c("tp", "tp"), k = c(10, 6), full = TRUE) +
                t2(hour_z, distcoast_z, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1) + seasonF +
                s(locationfactor, bs = "re"), family = poisson(), data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], 
              chain = 2, core = 2, iter = 5000, save_pars = save_pars(all = TRUE),
              control = list(adapt_delta = 0.99), backend = "cmdstanr", prior = tidal_prior)

# tbm2a_h <- add_criterion(tbm2a_h, c("loo", "loo_R2", "bayes_R2"), moment_match = TRUE, control = list(adapt_delta = 0.99), backend = "cmdstanr", ndraws = 2000) 
# saveRDS(tbm2a_h, "tide_analysis/ModelRDS/tbm2a_hz.rds")
# tbm2a_h <- readRDS("tide_analysis/ModelRDS/tbm2a_hz.rds")

mcmc_plot(tbm2a_h,type = "trace")
mcmc_plot(tbm2a_h) #plot posterior intervals
summary(tbm2a_h)
plot(conditional_effects(tbm2a_h))
plot(conditional_smooths(tbm2a_h))

## plot from predicting
predict_tbm2a_h <- posterior_smooths(tbm2a_h, smooth = 't2(hour_z,distcoast_z,bs=c("tp","tp"),by=seasonF,k=c(10,6),m=1)')
# mean of each column is what I'm looking for
tbm2a_h$data$fit_seasonhour <- as.numeric(colMedians(predict_tbm2a_h))

d1ha_wet <- with(tbm2a_h$data[tbm2a_h$data$seasonF == "Wet",], interp(x = hour_z, y = distcoast_z, z = fit_seasonhour, duplicate = "mean"))
d1ha_dry <-  with(tbm2a_h$data[tbm2a_h$data$seasonF == "Dry",], interp(x = hour_z, y = distcoast_z, z = fit_seasonhour, duplicate = "mean"))

d2ha_wet <- melt(d1ha_wet$z, na.rm = TRUE)
names(d2ha_wet) <- c("x", "y", "fit")
d2ha_wet$hour <- d1ha_wet$x[d2ha_wet$x] * sdhour + meanhour
d2ha_wet$distcoast <- d1ha_wet$y[d2ha_wet$y] * sddist + meandist

d2ha_dry <- melt(d1ha_dry$z, na.rm = TRUE)
names(d2ha_dry) <- c("x", "y", "fit")
d2ha_dry$hour <- d1ha_dry$x[d2ha_dry$x] * sdhour + meanhour
d2ha_dry$distcoast <- d1ha_dry$y[d2ha_dry$y] * sddist + meandist

d2ha_dry$seasonF <- "Dry"
d2ha_wet$seasonF <- "Wet"

d2ha <- rbind(d2ha_dry, d2ha_wet)
d2ha$seasonF <- factor(d2ha$seasonF, levels = c("Dry", "Wet"))

# png("tide_analysis/ModelRDS/nontoolusersplot_pred_hourred.png", width = 12, height = 6, units = 'in', res = 300)
# setEPS(postscript(file = "tide_analysis/ModelRDS/nontoolusersplot_pred_hourred.png", width = 12, height = 6))
ggplot(data = d2ha, aes(x = hour, y = distcoast, z = fit)) +
  geom_contour_filled(breaks = mybreaks, show.legend = TRUE) + scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE)  +
  theme_bw() + theme(panel.grid = element_blank()) + labs(x = "Hour of the day", y = "Distance to coast (m)") +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(x = hour, y = distcoast), alpha = 0.05, inherit.aes = FALSE) + 
  theme(strip.text.x = element_text(size = 20), axis.title = element_text(size = 20), legend.text =  element_text(size = 16), 
        legend.title = element_text(size =16)) +  facet_wrap(~seasonF, scales = "free")
# dev.off()


### DERIVATIVES OF GAMS #########
#### FUNCTIONS ####

## run function below (now copied from Shauhin's script on 23.06.2022)
deriv_plot <- function (model, dimensions = 1, by = FALSE, term, main, eps, response = NULL, spaghetti=FALSE, rug = TRUE, confidence=95,output){
  require(dplyr)
  require(ggplot2)
  require(plotly)
  require(brms)
  
  ###model must be a brms model object
  ###dimensions should be the number of variables in your spline
  ###term is a character string of the smooth term, same syntax as used in the model
  ###main is a character string (or vector of characters equal to dimensions) of the predictor variable, must not be wrapped in a smooth function
  ###eps is the amount to offset the original data (or a vector of offsets equal to dimensions), to be differenced from original to calculate slope
  ###response is an optional character string indicating the response variable to use, only relevant in the multivariate case
  ###confidence is the confidence level used to calculate the posterior intervals
  ###The desired name of the resulting ggplot object 
  Response=response
  if(is.null(Response)){
    Response=model$formula$resp
  }
  
  if(length(names(model$data))>6){
    model$data=model$data[,c(1:6)]
  }
  
  upper=(50+(confidence/2))/100
  lower=(50-(confidence/2))/100
  
  newdat=model$data
  newdat_b=model$data
  newdat_c=model$data
  newdat_d=model$data
  
  ##for 2D smooth finite difference aprox something like this
  ##fxy(x,y)~(f(x+eps_h,y+eps_k)-f(x+eps_h,y-eps_k)-f(x-eps_h,y+eps_k)+f(x-eps_h,y-eps_k))/(4*eps_h*eps*k)
  if(dimensions > 1) {
    if(length(eps)==1){
      eps[2]=eps[1]
    }
    for(i in 1:dimensions) {
      newdat[,which(names(newdat)==main[i])]=newdat[,which(names(newdat)==main[i])]+eps[i] # h + K
      newdat_b[,which(names(newdat_b)==main[i])]=newdat_b[,which(names(newdat_b)==main[i])]-eps[i] # -h - K
      
    }
    
    
    #h - k
    newdat_c[,which(names(newdat_c)==main[1])]=newdat_c[,which(names(newdat_c)==main[1])]+eps[1] 
    newdat_c[,which(names(newdat_c)==main[2])]=newdat_c[,which(names(newdat_c)==main[2])]-eps[2] 
    
    #-h + k
    newdat_d[,which(names(newdat_d)==main[1])]=newdat_d[,which(names(newdat_d)==main[1])]-eps[1]  
    newdat_d[,which(names(newdat_d)==main[2])]=newdat_d[,which(names(newdat_d)==main[2])]+eps[2] 
    
    
  } else{
    newdat[,which(names(newdat)==main)]=newdat[,which(names(newdat)==main)]+eps
  } 
  
  if(dimensions > 1){
    #dir=posterior_smooths(model, smooth = term, resp=response)
    dir2=posterior_smooths(model, smooth = term, resp=response, newdata = newdat)
    dir2_b=posterior_smooths(model, smooth = term, resp=response, newdata = newdat_b)
    dir2_c=posterior_smooths(model, smooth = term, resp=response, newdata = newdat_c)
    dir2_d=posterior_smooths(model, smooth = term, resp=response, newdata = newdat_d)
    
    dir_model=(dir2-dir2_c-dir2_d+dir2_b)/(4*prod(eps))
    
    mean_der <- apply(dir_model,MARGIN = 2,FUN = mean)
    lower_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = lower)
    upper_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = upper)
    
    
    der_data = cbind(mean_der, lower_der, upper_der)
    
    for(i in 1:length(main)) {
      der_data = cbind(der_data, model$data[,which(names(model$data)==main[i])])
    }
    der_data <- as.data.frame(der_data)
    
    colnames(der_data)=c("mean","lower","upper", main[1:length(main)])
    
    if(is.null(by)==TRUE) {
      interpdat <- with(der_data, akima::interp(x = der_data[,4], y = der_data[,5], z = mean, duplicate = "mean"))
      interpdat2 <- reshape2::melt(interpdat$z, na.rm = TRUE)
      names(interpdat2) <- c("x", "y", "dir")
      interpdat2$main1 <- interpdat$x[interpdat2$x]
      interpdat2$main2 <- interpdat$y[interpdat2$y]
      interpdat_low <- with(der_data, akima::interp(x = der_data[,4], y = der_data[,5], z = lower, duplicate = "mean"))
      interpdat2_low <- reshape2::melt(interpdat_low$z, na.rm = TRUE)
      names(interpdat2_low) <- c("x", "y", "dir")
      interpdat2_low$main1 <- interpdat_low$x[interpdat2_low$x]
      interpdat2_low$main2 <- interpdat_low$y[interpdat2_low$y]
      interpdat_high <- with(der_data, akima::interp(x = der_data[,4], y = der_data[,5], z = upper, duplicate = "mean"))
      interpdat2_high <- reshape2::melt(interpdat_high$z, na.rm = TRUE)
      names(interpdat2_high) <- c("x", "y", "dir")
      interpdat2_high$main1 <- interpdat_high$x[interpdat2_high$x]
      interpdat2_high$main2 <- interpdat_high$y[interpdat2_high$y]
      interpdat2$upper=interpdat2_high$dir
      interpdat2$lower=interpdat2_low$dir
      interpdat2$threshold=0
    } else {
      # add by column to der_data
      # for now only set up for by variable with TWO LEVELS and in quite explicit/roundabout way
      der_data = cbind(der_data, model$data[,which(names(model$data)==by)])
      colnames(der_data)=c("mean","lower","upper", main[1:length(main)], by)
      
      # factor level 1
      der_data_by1 <- der_data[which(der_data[,6] == levels(der_data[,6])[1]),]
      interpdat_a <- with(der_data_by1, akima::interp(x = der_data_by1[,4], y = der_data_by1[,5], z = mean, duplicate = "mean"))
      interpdat_a2 <- reshape2::melt(interpdat_a$z, na.rm = TRUE)
      names(interpdat_a2) <- c("x", "y", "dir")
      interpdat_a2$main1 <- interpdat_a$x[interpdat_a2$x]
      interpdat_a2$main2 <- interpdat_a$y[interpdat_a2$y]
      interpdat_a_low <- with(der_data_by1, akima::interp(x = der_data_by1[,4], y = der_data_by1[,5], z = lower, duplicate = "mean"))
      interpdat_a2_low <- reshape2::melt(interpdat_a_low$z, na.rm = TRUE)
      names(interpdat_a2_low) <- c("x", "y", "dir")
      interpdat_a2_low$main1 <- interpdat_a_low$x[interpdat_a2_low$x]
      interpdat_a2_low$main2 <- interpdat_a_low$y[interpdat_a2_low$y]
      interpdat_a_high <- with(der_data_by1, akima::interp(x = der_data_by1[,4], y = der_data_by1[,5], z = upper, duplicate = "mean"))
      interpdat_a2_high <- reshape2::melt(interpdat_a_high$z, na.rm = TRUE)
      names(interpdat_a2_high) <- c("x", "y", "dir")
      interpdat_a2_high$main1 <- interpdat_a_high$x[interpdat_a2_high$x]
      interpdat_a2_high$main2 <- interpdat_a_high$y[interpdat_a2_high$y]
      interpdat_a2$upper=interpdat_a2_high$dir
      interpdat_a2$lower=interpdat_a2_low$dir
      interpdat_a2$threshold=0
      assign(paste(output, "1", sep = "_"),interpdat_a2, envir = parent.frame())
      
      
      # factor level 2
      der_data_by2 <- der_data[which(der_data[,6] == levels(der_data[,6])[2]),]
      interpdat_b <- with(der_data_by2, akima::interp(x = der_data_by2[,4], y = der_data_by2[,5], z = mean, duplicate = "mean"))
      interpdat_b2 <- reshape2::melt(interpdat_b$z, na.rm = TRUE)
      names(interpdat_b2) <- c("x", "y", "dir")
      interpdat_b2$main1 <- interpdat_b$x[interpdat_b2$x]
      interpdat_b2$main2 <- interpdat_b$y[interpdat_b2$y]
      interpdat_b_low <- with(der_data_by2, akima::interp(x = der_data_by2[,4], y = der_data_by2[,5], z = lower, duplicate = "mean"))
      interpdat_b2_low <- reshape2::melt(interpdat_b_low$z, na.rm = TRUE)
      names(interpdat_b2_low) <- c("x", "y", "dir")
      interpdat_b2_low$main1 <- interpdat_b_low$x[interpdat_b2_low$x]
      interpdat_b2_low$main2 <- interpdat_b_low$y[interpdat_b2_low$y]
      interpdat_b_high <- with(der_data_by2, akima::interp(x = der_data_by2[,4], y = der_data_by2[,5], z = upper, duplicate = "mean"))
      interpdat_b2_high <- reshape2::melt(interpdat_b_high$z, na.rm = TRUE)
      names(interpdat_b2_high) <- c("x", "y", "dir")
      interpdat_b2_high$main1 <- interpdat_b_high$x[interpdat_b2_high$x]
      interpdat_b2_high$main2 <- interpdat_b_high$y[interpdat_b2_high$y]
      interpdat_b2$upper=interpdat_b2_high$dir
      interpdat_b2$lower=interpdat_b2_low$dir
      interpdat_b2$threshold=0
      assign(paste(output, "2", sep = "_"),interpdat_b2, envir = parent.frame())
      
    }
    
    if(is.null(by)==TRUE){
      axx <- list(
        title = names(model$data)[3]
      )
      
      axy <- list(
        title = names(model$data)[4]
      )
      
      
      p <- plot_ly(interpdat2, x=~main1, y=~main2, 
                   z=~dir, intensity = ~dir,type="mesh3d") %>% 
        add_mesh(x=~main1, y=~main2, 
                 z=~upper, intensity = ~upper, opacity=0.30) %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~lower, intensity = ~lower, opacity=0.30)  %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~threshold, intensity = ~threshold, colorscale='Hot' )
      p=p%>% hide_colorbar()
      p <- p %>% layout(title = "Derivative",
                        scene = list(xaxis=axx, yaxis=axy,
                                     aspectmode='cube'))
      assign(output,p, envir = parent.frame())
      return(p)
    } else{ 
      axx <- list(
        title = names(model$data)[3]
      )
      
      axy <- list(
        title = names(model$data)[4]
      )
      
      p1 <- plot_ly(interpdat_a2, x=~main1, y=~main2, 
                    z=~dir, intensity = ~dir, scene= 'scene1', type="mesh3d") %>% 
        add_mesh(x=~main1, y=~main2, 
                 z=~upper, intensity = ~upper, opacity=0.30) %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~lower, intensity = ~lower, opacity=0.30)  %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~threshold, intensity = ~threshold, colorscale='Hot' )
      p1=p1%>% hide_colorbar()
      p1 <- p1 %>% layout(annotations = list(x = 0.2 , y = 0.95, text = paste(by, levels(der_data[,6])[1], sep = ": "),
                                             showarrow = F, xref='paper', yref='paper', font = list(size = 15)), showlegend = FALSE) 
      
      p2 <- plot_ly(interpdat_b2, x=~main1, y=~main2, 
                    z=~dir, intensity = ~dir, scene= 'scene2', type="mesh3d") %>% 
        add_mesh(x=~main1, y=~main2, 
                 z=~upper, intensity = ~upper, opacity=0.30) %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~lower, intensity = ~lower, opacity=0.30)  %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~threshold, intensity = ~threshold, colorscale='Hot' )
      p2=p2%>% hide_colorbar()
      p2 <- p2 %>% layout(annotations = list(x = 0.2 , y = 0.95, text = paste(by, levels(der_data[,6])[2], sep = ": "),
                                             showarrow = F, xref='paper', yref='paper', font = list(size = 15)), showlegend = FALSE) 
      
      pp <- subplot(p1, p2)
      pp <- pp %>% layout(title = paste("Derivative at confidence", confidence, sep = " "),
                          scene = list(xaxis=axx, yaxis=axy,
                                       aspectmode='cube'),
                          scene2 = list(xaxis=axx, yaxis=axy,
                                        aspectmode='cube'))
      assign(output,pp, envir = parent.frame())
      return(pp) 
    }
    
    
    
  } else{
    newdat=model$data
    newdat[,which(names(newdat)==main)]=newdat[,which(names(newdat)==main)]+eps
    dir=posterior_smooths(model, smooth = term, resp=response)
    dir2=posterior_smooths(model, smooth = term, resp=response, newdata = newdat)
    
    dir_model=(dir2-dir)/eps
    
    mean_der <- apply(dir_model,MARGIN = 2,FUN = mean)
    lower_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = lower)
    upper_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = upper)
    
    der_data=data.frame(mean_der) %>%
      cbind(lower_der) %>%
      cbind(upper_der) %>%
      cbind(model$data[,which(names(model$data)==main)])
    colnames(der_data)=c("mean","lower","upper","main")
    
    
    der_data$Significance=NA
    der_data$Significance[which(sign(der_data$lower)<0&sign(der_data$upper)<0)]="Significant"
    der_data$Significance[which(sign(der_data$lower)>0&sign(der_data$upper)>0)]="Significant"
    der_data$Significance[which(sign(der_data$lower)!=sign(der_data$upper))]="Not Significant"
    #sigranges=tapply(der_data$main,as.factor(der_data$Significance),range)
    
    der_data$Significance=NA
    der_data$Significance[which(sign(der_data$lower)<0&sign(der_data$upper)<0)]=-1
    der_data$Significance[which(sign(der_data$lower)>0&sign(der_data$upper)>0)]=1
    der_data$Significance[which(sign(der_data$lower)!=sign(der_data$upper))]=0
    #der_data=der_data[with(der_data, order(der_data[,4], der_data[,5])),]
    der_data$siglab <- with(rle(der_data$Significance), rep(cumsum(lengths >= 1),lengths))
    
    
    if(length(which(der_data$Significance!=0))==0){
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug,errorbar_args = list(alpha=0.1),plot=FALSE)
      if(is.null(response)){
        index=which(names(model_plot)==paste(main,sep=""))
      }else{
        index=which(names(model_plot)==paste(response,".",response,"_",main,sep=""))
      }    
      model_est <- as.data.frame(model_plot[[index]][[1]])
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug,errorbar_args = list(alpha=0.1),plot=FALSE)[[index]]
      
      index2=which(names(model_est)==main)
      colnames(model_est)[index2]="Main"
      
      model_plot2=model_plot+
        geom_line(data=model_est,aes(Main,estimate__,color=I("black")),size=1)+
        ylab(Response)+xlab(main)+
        theme_classic()+ guides(color="none")
      assign(output,model_plot2, envir = parent.frame())
      return(model_plot2)
      
    } else{
      der_data_SIG=der_data[which(der_data$Significance!=0),]
      
      sigranges=tapply(der_data_SIG$main,as.factor(der_data_SIG$siglab),range, na.rm=T)
      
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug,errorbar_args = list(alpha=0.1),plot=FALSE)
      if(is.null(response)){
        index=which(names(model_plot)==paste(main,sep=""))
      }else{
        index=which(names(model_plot)==paste(response,".",response,"_",main,sep=""))
      }    
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug, errorbar_args = list(alpha=0.1),plot=FALSE)[[index]]
      
      model_est <- as.data.frame(model_plot[[1]])
      model_est$Sig=NA
      model_est$Sig2=NA
      model_est$Sig2[which(model_est$Sig==0)]=.8
      model_est$Sig2[which(model_est$Sig==1)]=1.5
      index2=which(names(model_est)==main)
      colnames(model_est)[index2]="Main"
      
      
      for(i in 1:nrow(model_est)){
        for(j in 1:length(sigranges)){
          if(model_est$Main[i]>=sigranges[[j]][1] & model_est$Main[i]<sigranges[[j]][2]){
            model_est$Sig[i]=1
          }
        }
        
        
      }
      model_est$Sig[-which(model_est$Sig==1)]=0
      if(length(which(model_est$Sig==1))==0){
        model_est$Sig=0
      }
      model_plot2=model_plot+ 
        geom_line(data=model_est,aes(Main,estimate__,color=(Sig)),size=1)+
        scale_color_gradient2(low="black", mid="black",high="cyan" )+
        ylab(Response)+xlab(main)+
        theme_classic()+ guides(color="none")
      
      assign(output,model_plot2, envir = parent.frame())
      output2=gsub("_plot", "", output)
      output2=paste("VOI",output2,sep="_")
      if(length(which(model_est$Sig==1))>0){
        VOIdat=model_est[which(model_est$Sig==1),]
        assign(output2,VOIdat, envir = parent.frame())
      }
    }
    return(model_plot2)
    
  }
  
}

## function for z-transformed data
deriv_plot_ztransformed <- function (model, dimensions = 1, by = FALSE, term, main, eps, response = NULL, spaghetti=FALSE, rug = TRUE, confidence=95,output, meanmain, sdmain){
  require(dplyr)
  require(ggplot2)
  require(plotly)
  require(brms)
  
  ###model must be a brms model object
  ###dimensions should be the number of variables in your spline
  ###term is a character string of the smooth term, same syntax as used in the model
  ###main is a character string (or vector of characters equal to dimensions) of the predictor variable, must not be wrapped in a smooth function
  ###eps is the amount to offset the original data (or a vector of offsets equal to dimensions), to be differenced from original to calculate slope
  ###response is an optional character string indicating the response variable to use, only relevant in the multivariate case
  ###confidence is the confidence level used to calculate the posterior intervals
  ###The desired name of the resulting ggplot object 
  ## meanmain is a vector of the means of your main predictor variable(s)
  ## sdmain is a vector of the means of your main predictor variable(s)
  Response=response
  if(is.null(Response)){
    Response=model$formula$resp
  }
  
  if(length(names(model$data))>6){
    model$data=model$data[,c(1:6)]
  }
  
  upper=(50+(confidence/2))/100
  lower=(50-(confidence/2))/100
  
  newdat=model$data
  newdat_b=model$data
  newdat_c=model$data
  newdat_d=model$data
  
  ##for 2D smooth finite difference aprox something like this
  ##fxy(x,y)~(f(x+eps_h,y+eps_k)-f(x+eps_h,y-eps_k)-f(x-eps_h,y+eps_k)+f(x-eps_h,y-eps_k))/(4*eps_h*eps*k)
  if(dimensions > 1) {
    if(length(eps)==1){
      eps[2]=eps[1]
    }
    for(i in 1:dimensions) {
      newdat[,which(names(newdat)==main[i])]=newdat[,which(names(newdat)==main[i])]+eps[i] # h + K
      newdat_b[,which(names(newdat_b)==main[i])]=newdat_b[,which(names(newdat_b)==main[i])]-eps[i] # -h - K
      
    }
    
    
    #h - k
    newdat_c[,which(names(newdat_c)==main[1])]=newdat_c[,which(names(newdat_c)==main[1])]+eps[1] 
    newdat_c[,which(names(newdat_c)==main[2])]=newdat_c[,which(names(newdat_c)==main[2])]-eps[2] 
    
    #-h + k
    newdat_d[,which(names(newdat_d)==main[1])]=newdat_d[,which(names(newdat_d)==main[1])]-eps[1]  
    newdat_d[,which(names(newdat_d)==main[2])]=newdat_d[,which(names(newdat_d)==main[2])]+eps[2] 
    
    
  } else{
    newdat[,which(names(newdat)==main)]=newdat[,which(names(newdat)==main)]+eps
  } 
  
  if(dimensions > 1){
    #dir=posterior_smooths(model, smooth = term, resp=response)
    dir2=posterior_smooths(model, smooth = term, resp=response, newdata = newdat)
    dir2_b=posterior_smooths(model, smooth = term, resp=response, newdata = newdat_b)
    dir2_c=posterior_smooths(model, smooth = term, resp=response, newdata = newdat_c)
    dir2_d=posterior_smooths(model, smooth = term, resp=response, newdata = newdat_d)
    
    dir_model=(dir2-dir2_c-dir2_d+dir2_b)/(4*prod(eps))
    
    mean_der <- apply(dir_model,MARGIN = 2,FUN = mean)
    lower_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = lower)
    upper_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = upper)
    
    
    der_data = cbind(mean_der, lower_der, upper_der)
    
    for(i in 1:length(main)) {
      der_data = cbind(der_data, model$data[,which(names(model$data)==main[i])])
    }
    der_data <- as.data.frame(der_data)
    
    colnames(der_data)=c("mean","lower","upper", main[1:length(main)])
    
    if(is.null(by)==TRUE) {
      interpdat <- with(der_data, akima::interp(x = der_data[,4], y = der_data[,5], z = mean, duplicate = "mean"))
      interpdat2 <- reshape2::melt(interpdat$z, na.rm = TRUE)
      names(interpdat2) <- c("x", "y", "dir")
      interpdat2$main1 <- interpdat$x[interpdat2$x]
      interpdat2$main2 <- interpdat$y[interpdat2$y]
      interpdat_low <- with(der_data, akima::interp(x = der_data[,4], y = der_data[,5], z = lower, duplicate = "mean"))
      interpdat2_low <- reshape2::melt(interpdat_low$z, na.rm = TRUE)
      names(interpdat2_low) <- c("x", "y", "dir")
      interpdat2_low$main1 <- interpdat_low$x[interpdat2_low$x]
      interpdat2_low$main2 <- interpdat_low$y[interpdat2_low$y]
      interpdat_high <- with(der_data, akima::interp(x = der_data[,4], y = der_data[,5], z = upper, duplicate = "mean"))
      interpdat2_high <- reshape2::melt(interpdat_high$z, na.rm = TRUE)
      names(interpdat2_high) <- c("x", "y", "dir")
      interpdat2_high$main1 <- interpdat_high$x[interpdat2_high$x]
      interpdat2_high$main2 <- interpdat_high$y[interpdat2_high$y]
      interpdat2$upper=interpdat2_high$dir
      interpdat2$lower=interpdat2_low$dir
      interpdat2$threshold=0
    } else {
      # add by column to der_data
      # for now only set up for by variable with TWO LEVELS and in quite explicit/roundabout way
      der_data = cbind(der_data, model$data[,which(names(model$data)==by)])
      colnames(der_data)=c("mean","lower","upper", main[1:length(main)], by)
      ## transform back to real scale for z-transformed variables
      der_data[,which(names(der_data)==main[1])] <- der_data[,which(names(der_data)==main[1])] * sdmain[1] + meanmain[1]
      der_data[,which(names(der_data)==main[2])] <- der_data[,which(names(der_data)==main[2])] * sdmain[2] + meanmain[2]
      
      # factor level 1
      der_data_by1 <- der_data[which(der_data[,6] == levels(der_data[,6])[1]),]
      interpdat_a <- with(der_data_by1, akima::interp(x = der_data_by1[,4], y = der_data_by1[,5], z = mean, duplicate = "mean"))
      interpdat_a2 <- reshape2::melt(interpdat_a$z, na.rm = TRUE)
      names(interpdat_a2) <- c("x", "y", "dir")
      interpdat_a2$main1 <- interpdat_a$x[interpdat_a2$x]
      interpdat_a2$main2 <- interpdat_a$y[interpdat_a2$y]
      interpdat_a_low <- with(der_data_by1, akima::interp(x = der_data_by1[,4], y = der_data_by1[,5], z = lower, duplicate = "mean"))
      interpdat_a2_low <- reshape2::melt(interpdat_a_low$z, na.rm = TRUE)
      names(interpdat_a2_low) <- c("x", "y", "dir")
      interpdat_a2_low$main1 <- interpdat_a_low$x[interpdat_a2_low$x]
      interpdat_a2_low$main2 <- interpdat_a_low$y[interpdat_a2_low$y]
      interpdat_a_high <- with(der_data_by1, akima::interp(x = der_data_by1[,4], y = der_data_by1[,5], z = upper, duplicate = "mean"))
      interpdat_a2_high <- reshape2::melt(interpdat_a_high$z, na.rm = TRUE)
      names(interpdat_a2_high) <- c("x", "y", "dir")
      interpdat_a2_high$main1 <- interpdat_a_high$x[interpdat_a2_high$x]
      interpdat_a2_high$main2 <- interpdat_a_high$y[interpdat_a2_high$y]
      interpdat_a2$upper=interpdat_a2_high$dir
      interpdat_a2$lower=interpdat_a2_low$dir
      interpdat_a2$threshold=0
      assign(paste(output, "1", sep = "_"),interpdat_a2, envir = parent.frame())
      
      
      # factor level 2
      der_data_by2 <- der_data[which(der_data[,6] == levels(der_data[,6])[2]),]
      interpdat_b <- with(der_data_by2, akima::interp(x = der_data_by2[,4], y = der_data_by2[,5], z = mean, duplicate = "mean"))
      interpdat_b2 <- reshape2::melt(interpdat_b$z, na.rm = TRUE)
      names(interpdat_b2) <- c("x", "y", "dir")
      interpdat_b2$main1 <- interpdat_b$x[interpdat_b2$x]
      interpdat_b2$main2 <- interpdat_b$y[interpdat_b2$y]
      interpdat_b_low <- with(der_data_by2, akima::interp(x = der_data_by2[,4], y = der_data_by2[,5], z = lower, duplicate = "mean"))
      interpdat_b2_low <- reshape2::melt(interpdat_b_low$z, na.rm = TRUE)
      names(interpdat_b2_low) <- c("x", "y", "dir")
      interpdat_b2_low$main1 <- interpdat_b_low$x[interpdat_b2_low$x]
      interpdat_b2_low$main2 <- interpdat_b_low$y[interpdat_b2_low$y]
      interpdat_b_high <- with(der_data_by2, akima::interp(x = der_data_by2[,4], y = der_data_by2[,5], z = upper, duplicate = "mean"))
      interpdat_b2_high <- reshape2::melt(interpdat_b_high$z, na.rm = TRUE)
      names(interpdat_b2_high) <- c("x", "y", "dir")
      interpdat_b2_high$main1 <- interpdat_b_high$x[interpdat_b2_high$x]
      interpdat_b2_high$main2 <- interpdat_b_high$y[interpdat_b2_high$y]
      interpdat_b2$upper=interpdat_b2_high$dir
      interpdat_b2$lower=interpdat_b2_low$dir
      interpdat_b2$threshold=0
      assign(paste(output, "2", sep = "_"),interpdat_b2, envir = parent.frame())
      
    }
    
    if(is.null(by)==TRUE){
      axx <- list(
        title = names(model$data)[3]
      )
      
      axy <- list(
        title = names(model$data)[4]
      )
      
      
      p <- plot_ly(interpdat2, x=~main1, y=~main2, 
                   z=~dir, intensity = ~dir,type="mesh3d") %>% 
        add_mesh(x=~main1, y=~main2, 
                 z=~upper, intensity = ~upper, opacity=0.30) %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~lower, intensity = ~lower, opacity=0.30)  %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~threshold, intensity = ~threshold, colorscale='Hot' )
      p=p%>% hide_colorbar()
      p <- p %>% layout(title = "Derivative",
                        scene = list(xaxis=axx, yaxis=axy,
                                     aspectmode='cube'))
      assign(output,p, envir = parent.frame())
      return(p)
    } else{ 
      axx <- list(
        title = names(model$data)[3]
      )
      
      axy <- list(
        title = names(model$data)[4]
      )
      
      p1 <- plot_ly(interpdat_a2, x=~main1, y=~main2, 
                    z=~dir, intensity = ~dir, scene= 'scene1', type="mesh3d") %>% 
        add_mesh(x=~main1, y=~main2, 
                 z=~upper, intensity = ~upper, opacity=0.30) %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~lower, intensity = ~lower, opacity=0.30)  %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~threshold, intensity = ~threshold, colorscale='Hot' )
      p1=p1%>% hide_colorbar()
      p1 <- p1 %>% layout(annotations = list(x = 0.2 , y = 0.95, text = paste(by, levels(der_data[,6])[1], sep = ": "),
                                             showarrow = F, xref='paper', yref='paper', font = list(size = 15)), showlegend = FALSE) 
      
      p2 <- plot_ly(interpdat_b2, x=~main1, y=~main2, 
                    z=~dir, intensity = ~dir, scene= 'scene2', type="mesh3d") %>% 
        add_mesh(x=~main1, y=~main2, 
                 z=~upper, intensity = ~upper, opacity=0.30) %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~lower, intensity = ~lower, opacity=0.30)  %>%
        add_mesh(x=~main1, y=~main2, 
                 z=~threshold, intensity = ~threshold, colorscale='Hot' )
      p2=p2%>% hide_colorbar()
      p2 <- p2 %>% layout(annotations = list(x = 0.2 , y = 0.95, text = paste(by, levels(der_data[,6])[2], sep = ": "),
                                             showarrow = F, xref='paper', yref='paper', font = list(size = 15)), showlegend = FALSE) 
      
      pp <- subplot(p1, p2)
      pp <- pp %>% layout(title = paste("Derivative at confidence", confidence, sep = " "),
                          scene = list(xaxis=axx, yaxis=axy,
                                       aspectmode='cube'),
                          scene2 = list(xaxis=axx, yaxis=axy,
                                        aspectmode='cube'))
      assign(output,pp, envir = parent.frame())
      return(pp) 
    }
    
    
    
  } else{
    newdat=model$data
    newdat[,which(names(newdat)==main)]=newdat[,which(names(newdat)==main)]+eps
    dir=posterior_smooths(model, smooth = term, resp=response)
    dir2=posterior_smooths(model, smooth = term, resp=response, newdata = newdat)
    
    dir_model=(dir2-dir)/eps
    
    mean_der <- apply(dir_model,MARGIN = 2,FUN = mean)
    lower_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = lower)
    upper_der <- apply(dir_model,MARGIN = 2,FUN = quantile, prob = upper)
    
    der_data=data.frame(mean_der) %>%
      cbind(lower_der) %>%
      cbind(upper_der) %>%
      cbind(model$data[,which(names(model$data)==main)])
    colnames(der_data)=c("mean","lower","upper","main")
    
    
    der_data$Significance=NA
    der_data$Significance[which(sign(der_data$lower)<0&sign(der_data$upper)<0)]="Significant"
    der_data$Significance[which(sign(der_data$lower)>0&sign(der_data$upper)>0)]="Significant"
    der_data$Significance[which(sign(der_data$lower)!=sign(der_data$upper))]="Not Significant"
    #sigranges=tapply(der_data$main,as.factor(der_data$Significance),range)
    
    der_data$Significance=NA
    der_data$Significance[which(sign(der_data$lower)<0&sign(der_data$upper)<0)]=-1
    der_data$Significance[which(sign(der_data$lower)>0&sign(der_data$upper)>0)]=1
    der_data$Significance[which(sign(der_data$lower)!=sign(der_data$upper))]=0
    #der_data=der_data[with(der_data, order(der_data[,4], der_data[,5])),]
    der_data$siglab <- with(rle(der_data$Significance), rep(cumsum(lengths >= 1),lengths))
    
    
    if(length(which(der_data$Significance!=0))==0){
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug,errorbar_args = list(alpha=0.1),plot=FALSE)
      if(is.null(response)){
        index=which(names(model_plot)==paste(main,sep=""))
      }else{
        index=which(names(model_plot)==paste(response,".",response,"_",main,sep=""))
      }    
      model_est <- as.data.frame(model_plot[[index]][[1]])
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug,errorbar_args = list(alpha=0.1),plot=FALSE)[[index]]
      
      index2=which(names(model_est)==main)
      colnames(model_est)[index2]="Main"
      
      model_plot2=model_plot+
        geom_line(data=model_est,aes(Main,estimate__,color=I("black")),size=1)+
        ylab(Response)+xlab(main)+
        theme_classic()+ guides(color="none")
      assign(output,model_plot2, envir = parent.frame())
      return(model_plot2)
      
    } else{
      der_data_SIG=der_data[which(der_data$Significance!=0),]
      
      sigranges=tapply(der_data_SIG$main,as.factor(der_data_SIG$siglab),range, na.rm=T)
      
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug,errorbar_args = list(alpha=0.1),plot=FALSE)
      if(is.null(response)){
        index=which(names(model_plot)==paste(main,sep=""))
      }else{
        index=which(names(model_plot)==paste(response,".",response,"_",main,sep=""))
      }    
      model_plot=plot(conditional_effects(model,spaghetti=spaghetti),rug = rug, errorbar_args = list(alpha=0.1),plot=FALSE)[[index]]
      
      model_est <- as.data.frame(model_plot[[1]])
      model_est$Sig=NA
      model_est$Sig2=NA
      model_est$Sig2[which(model_est$Sig==0)]=.8
      model_est$Sig2[which(model_est$Sig==1)]=1.5
      index2=which(names(model_est)==main)
      colnames(model_est)[index2]="Main"
      
      
      for(i in 1:nrow(model_est)){
        for(j in 1:length(sigranges)){
          if(model_est$Main[i]>=sigranges[[j]][1] & model_est$Main[i]<sigranges[[j]][2]){
            model_est$Sig[i]=1
          }
        }
        
        
      }
      model_est$Sig[-which(model_est$Sig==1)]=0
      if(length(which(model_est$Sig==1))==0){
        model_est$Sig=0
      }
      model_plot2=model_plot+ 
        geom_line(data=model_est,aes(Main,estimate__,color=(Sig)),size=1)+
        scale_color_gradient2(low="black", mid="black",high="cyan" )+
        ylab(Response)+xlab(main)+
        theme_classic()+ guides(color="none")
      
      assign(output,model_plot2, envir = parent.frame())
      output2=gsub("_plot", "", output)
      output2=paste("VOI",output2,sep="_")
      if(length(which(model_est$Sig==1))>0){
        VOIdat=model_est[which(model_est$Sig==1),]
        assign(output2,VOIdat, envir = parent.frame())
      }
    }
    return(model_plot2)
    
  }
  
}

## function for extracting ranges of derivative at one side of 0
deriv_ranges <- function(der_data_50_1, der_data_50_2, der_data_70_1, der_data_70_2, der_data_90_1, der_data_90_2, factorlevels, modelname, seventy = TRUE, ninety = TRUE){
  # supply all derivative dataframes
  # levels of factor
  # name of model
  
  der_data_50_1$Significance <- ifelse(sign(der_data_50_1$lower) <0 & sign(der_data_50_1$upper)<0 | sign(der_data_50_1$lower) >0 & sign(der_data_50_1$upper)>0, 1, 0)
  der_data_50_2$Significance <- ifelse(sign(der_data_50_2$lower) <0 & sign(der_data_50_2$upper)<0 | sign(der_data_50_2$lower) >0 & sign(der_data_50_2$upper)>0, 1, 0)
  der_data_50_1$factor <- factorlevels[1]
  der_data_50_2$factor <- factorlevels[2]
  der_data_50_1$confidence <- 50
  der_data_50_2$confidence <- 50 
  
  if(seventy == TRUE){
    der_data_70_1$Significance <- ifelse(sign(der_data_70_1$lower) <0 & sign(der_data_70_1$upper)<0 | sign(der_data_70_1$lower) >0 & sign(der_data_70_1$upper)>0, 1, 0)
    der_data_70_2$Significance <- ifelse(sign(der_data_70_2$lower) <0 & sign(der_data_70_2$upper)<0 | sign(der_data_70_2$lower) >0 & sign(der_data_70_2$upper)>0, 1, 0)
    der_data_70_1$factor <- factorlevels[1]
    der_data_70_2$factor <- factorlevels[2]
    der_data_70_1$confidence <- 70
    der_data_70_2$confidence <- 70 
  }
  if(ninety==TRUE){
    der_data_90_1$Significance <- ifelse(sign(der_data_90_1$lower) <0 & sign(der_data_90_1$upper)<0 | sign(der_data_90_1$lower) >0 & sign(der_data_90_1$upper)>0, 1, 0)
    der_data_90_2$Significance <- ifelse(sign(der_data_90_2$lower) <0 & sign(der_data_90_2$upper)<0 | sign(der_data_90_2$lower) >0 & sign(der_data_90_2$upper)>0, 1, 0)
    der_data_90_1$factor <- factorlevels[1]
    der_data_90_2$factor <- factorlevels[2]
    der_data_90_1$confidence <- 90
    der_data_90_2$confidence <- 90
  }
  
  if(seventy==FALSE & ninety == FALSE){
    der_data <- rbind(der_data_50_1, der_data_50_2)
  }
  
  if(seventy == TRUE & ninety == FALSE){
    der_data <- rbind(der_data_50_1, der_data_50_2, der_data_70_1, der_data_70_2)
  }
  
  if(seventy == TRUE & ninety == TRUE){
    der_data <- rbind(der_data_50_1, der_data_50_2, der_data_70_1, der_data_70_2, der_data_90_1, der_data_90_2)
  }
  
  assign(paste(modelname, "overlay", sep = "_"), der_data, envir = parent.frame())
  
}

#### APPLYING DERIVATIVES TO BRMS MODELS ####

##### TIDES #####
###### Tbm1: TU vs NTU #####
## 50 confidence
deriv_plot_ztransformed(tbm1, dimensions = 2, by = c("toolusers"), term = 't2(tidedif_z, distcoast_z, bs = c("cc", "tp"), by = toolusers, k = c(10, 6), m = 1)', 
                        main = c("tidedif_z", "distcoast_z"), eps = 0.001, confidence = 50, output = "derivplot_tbm1_50", 
                        meanmain = c(meantide, meandist), sdmain = c(sdtide, sddist))
## CHECK IF: no regions that are reliably on one side of 0, already at such low confidence, so no need to proceed (also not with ranges plot)
# means that the effect is not really supported

###### Tbm2: TU #####
## 50 confidence
deriv_plot_ztransformed(tbm2, dimensions = 2, by = c("seasonF"), term = 't2(tidedif_z, distcoast_z, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("tidedif_z", "distcoast_z"), eps = 0.001, confidence = 50, output = "derivplot_tbm2season_50",
           meanmain = c(meantide, meandist), sdmain = c(sdtide, sddist))
# save the resulting data frames as rds so I don't have to keep running this
#saveRDS(derivplot_tbm2season_50_1, file = "tide_analysis/ModelRDS/derivplot_tbm2season_50_1.rds")
#saveRDS(derivplot_tbm2season_50_2, file = "tide_analysis/ModelRDS/derivplot_tbm2season_50_2.rds")
derivplot_tbm2season_50_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_50_1.rds")
derivplot_tbm2season_50_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_50_2.rds")

## 70 confidence
deriv_plot_ztransformed(tbm2, dimensions = 2, by = c("seasonF"), term = 't2(tidedif_z, distcoast_z, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("tidedif_z", "distcoast_z"), eps = 0.001, confidence = 70, output = "derivplot_tbm2season_70",
           meanmain = c(meantide, meandist), sdmain = c(sdtide, sddist))
#saveRDS(derivplot_tbm2season_70_1, file = "tide_analysis/ModelRDS/derivplot_tbm2season_70_1.rds")
#saveRDS(derivplot_tbm2season_70_2, file = "tide_analysis/ModelRDS/derivplot_tbm2season_70_2.rds")
derivplot_tbm2season_70_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_70_1.rds")
derivplot_tbm2season_70_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_70_2.rds")

## 90 confidence
deriv_plot_ztransformed(tbm2, dimensions = 2, by = c("seasonF"), term = 't2(tidedif_z, distcoast_z, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("tidedif_z", "distcoast_z"), eps = 0.001, confidence = 90, output = "derivplot_tbm2season_90",
           meanmain = c(meantide, meandist), sdmain = c(sdtide, sddist))
#saveRDS(derivplot_tbm2season_90_1, file = "tide_analysis/ModelRDS/derivplot_tbm2season_90_1.rds")
#saveRDS(derivplot_tbm2season_90_2, file = "tide_analysis/ModelRDS/derivplot_tbm2season_90_2.rds")
derivplot_tbm2season_90_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_90_1.rds")
derivplot_tbm2season_90_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2season_90_2.rds")

#### Making overlay plot
deriv_ranges(derivplot_tbm2season_50_1, derivplot_tbm2season_50_2, derivplot_tbm2season_70_1, derivplot_tbm2season_70_2, derivplot_tbm2season_90_1, derivplot_tbm2season_90_2, 
             factorlevels = c("Dry", "Wet"), modelname <- "tbm2", seventy = TRUE, ninety = TRUE)

tbm2_merge <- left_join(d2, tbm2_overlay, by = c("tidedif" = "main1", "distcoast" = "main2", "seasonF" = "factor"))

# 70 % confidence, still put alpha of rug lower if you are exporting to picture
# color on gray
ggplot() +
  geom_contour_filled(data = tbm2_merge, aes(x = tidedif, y = distcoast, z = fit)) + scale_fill_grey() +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = tidedif, y = distcoast),alpha = 1, inherit.aes = FALSE) + 
  new_scale_fill() + 
  geom_contour_filled(data = tbm2_merge[tbm2_merge$confidence == 70 & tbm2_merge$Significance == 1,], aes(x = tidedif, y = distcoast, z = fit)) + 
  scale_fill_viridis(option = "inferno", discrete = TRUE) + facet_wrap(~seasonF) + theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change nr of capuchins") +
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12))

# color on lighter color
ggplot() +
  geom_contour_filled(data = tbm2_merge, breaks = mybreaks, show.legend = TRUE, aes(x = tidedif, y = distcoast, z = fit), alpha = 0.7) +
  scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE)+
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = tidedif, y = distcoast),alpha = 1, inherit.aes = FALSE) + 
  new_scale_fill() + 
  geom_contour_filled(data = tbm2_merge[tbm2_merge$confidence == 70 & tbm2_merge$Significance == 1,], breaks = mybreaks, show.legend = TRUE, aes(x = tidedif, y = distcoast, z = fit)) + 
  scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE) + facet_wrap(~seasonF) + theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hours until and after nearest low tide (=0)", y = "Distance to coast (m)", fill = "Change nr of capuchins") +
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12))

###### Tbm2a: NTU ####
## 50 confidence
deriv_plot_ztransformed(tbm2a, dimensions = 2, by = c("seasonF"), term = 't2(tidedif_z, distcoast_z, bs = c("cc", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("tidedif_z", "distcoast_z"), eps = 0.001, confidence = 50, output = "derivplot_tbm2aseason_50",
           meanmain = c(meantide, meandist), sdmain = c(sdtide, sddist))

### CHECK IF no regions that are reliably on one side of 0, already at such low confidence, so no need to proceed
# means that the effect is not really supported

##### HOUR OF DAY ####
###### Tbm2_h: TU ####
# 50 confidence
deriv_plot_ztransformed(tbm2_h, dimensions = 2, by = c("seasonF"), term = 't2(hour_z, distcoast_z, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour_z", "distcoast_z"), eps = 0.001, confidence = 50, output = "derivplot_tbm2hseason_50",
           meanmain = c(meanhour, meandist), sdmain = c(sdhour, sddist))
#saveRDS(derivplot_tbm2hseason_50_1, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_50_1.rds")
#saveRDS(derivplot_tbm2hseason_50_2, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_50_2.rds")
derivplot_tbm2hseason_50_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_50_1.rds")
derivplot_tbm2hseason_50_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_50_2.rds")

# 70 confidence
deriv_plot_ztransformed(tbm2_h, dimensions = 2, by = c("seasonF"), term = 't2(hour_z, distcoast_z, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour_z", "distcoast_z"), eps = 0.001, confidence = 70, output = "derivplot_tbm2hseason_70",
           meanmain = c(meanhour, meandist), sdmain = c(sdhour, sddist))
#saveRDS(derivplot_tbm2hseason_70_1, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_70_1.rds")
#saveRDS(derivplot_tbm2hseason_70_2, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_70_2.rds")
derivplot_tbm2hseason_70_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_70_1.rds")
derivplot_tbm2hseason_70_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_70_2.rds")

# 90 confidence
deriv_plot_ztransformed(tbm2_h, dimensions = 2, by = c("seasonF"), term = 't2(hour_z, distcoast_z, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour_z", "distcoast_z"), eps = 0.001, confidence = 90, output = "derivplot_tbm2hseason_90",
           meanmain = c(meanhour, meandist), sdmain = c(sdhour, sddist))
#saveRDS(derivplot_tbm2hseason_90_1, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_90_1.rds")
#saveRDS(derivplot_tbm2hseason_90_2, file = "tide_analysis/ModelRDS/derivplot_tbm2hseason_90_2.rds")
derivplot_tbm2hseason_90_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_90_1.rds")
derivplot_tbm2hseason_90_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2hseason_90_2.rds")

#### Making overlay plot
deriv_ranges(derivplot_tbm2hseason_50_1, derivplot_tbm2hseason_50_2, derivplot_tbm2hseason_70_1, derivplot_tbm2hseason_70_2, derivplot_tbm2hseason_90_1, derivplot_tbm2hseason_90_2, 
             factorlevels = c("Dry", "Wet"), modelname <- "tbm2_h", seventy = TRUE, ninety = TRUE)

tbm2_h_merge <- left_join(d2, tbm2_h_overlay, by = c("hour" = "main1", "distcoast" = "main2", "seasonF" = "factor"))

# 70 % confidence, still put alpha of rug lower if you are exporting to picture
# color on gray
ggplot() +
  geom_contour_filled(data = tbm2_h_merge, aes(x = hour, y = distcoast, z = fit)) + scale_fill_grey() +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = hour, y = distcoast),alpha = 1, inherit.aes = FALSE) + 
  new_scale_fill() + 
  geom_contour_filled(data = tbm2_h_merge[tbm2_h_merge$confidence == 70 & tbm2_h_merge$Significance == 1,], aes(x = hour, y = distcoast, z = fit)) + 
  scale_fill_viridis(option = "inferno", discrete = TRUE) + facet_wrap(~seasonF) + theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hour of day", y = "Distance to coast (m)", fill = "Change nr of capuchins") +
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12))

# color on lighter color
ggplot() +
  geom_contour_filled(data = tbm2_h_merge, breaks = mybreaks, show.legend = TRUE, aes(x = hour, y = distcoast, z = fit), alpha = 0.7) +
  scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE)+
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], aes(x = hour, y = distcoast),alpha = 1, inherit.aes = FALSE) + 
  new_scale_fill() + 
  geom_contour_filled(data = tbm2_h_merge[tbm2_h_merge$confidence == 70 & tbm2_h_merge$Significance == 1,], breaks = mybreaks, show.legend = TRUE, aes(x = hour, y = distcoast, z = fit)) + 
  scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE) + facet_wrap(~seasonF) + theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hour of day", y = "Distance to coast (m)", fill = "Change nr of capuchins") +
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12))

###### Tbm2a_h: NTU  ####
# 50 confidence
deriv_plot_ztransformed(tbm2a_h, dimensions = 2, by = c("seasonF"), term = 't2(hour_z, distcoast_z, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour_z", "distcoast_z"), eps = 0.001, confidence = 50, output = "derivplot_tbm2ahseason_50",
           meanmain = c(meanhour, meandist), sdmain = c(sdhour, sddist))
#saveRDS(derivplot_tbm2ahseason_50_1, file = "tide_analysis/ModelRDS/derivplot_tbm2ahseason_50_1.rds")
#saveRDS(derivplot_tbm2ahseason_50_2, file = "tide_analysis/ModelRDS/derivplot_tbm2ahseason_50_2.rds")
derivplot_tbm2ahseason_50_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2ahseason_50_1.rds")
derivplot_tbm2ahseason_50_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2ahseason_50_2.rds")

# 70 confidence
deriv_plot_ztransformed(tbm2a_h, dimensions = 2, by = c("seasonF"), term = 't2(hour_z, distcoast_z, bs = c("tp", "tp"), by = seasonF, k = c(10,6), m = 1)',
           main = c("hour_z", "distcoast_z"), eps = 0.001, confidence = 70, output = "derivplot_tbm2ahseason_70",
           meanmain = c(meanhour, meandist), sdmain = c(sdhour, sddist))
#saveRDS(derivplot_tbm2ahseason_70_1, file = "tide_analysis/ModelRDS/derivplot_tbm2ahseason_70_1.rds")
#saveRDS(derivplot_tbm2ahseason_70_2, file = "tide_analysis/ModelRDS/derivplot_tbm2ahseason_70_2.rds")
derivplot_tbm2ahseason_70_1 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2ahseason_70_1.rds")
derivplot_tbm2ahseason_70_2 <- readRDS("tide_analysis/ModelRDS/derivplot_tbm2ahseason_70_2.rds")

#### Making overlay plot
deriv_ranges(derivplot_tbm2ahseason_50_1, derivplot_tbm2ahseason_50_2, derivplot_tbm2ahseason_70_1, derivplot_tbm2ahseason_70_2, 
             factorlevels = c("Dry", "Wet"), modelname <- "tbm2_ah", seventy = TRUE, ninety = FALSE)

tbm2_ah_merge <- left_join(d2, tbm2_ah_overlay, by = c("hour" = "main1", "distcoast" = "main2", "seasonF" = "factor"))

# 70 % confidence, still put alpha of rug lower if you are exporting to picture
# color on gray
ggplot() +
  geom_contour_filled(data = tbm2_ah_merge, aes(x = hour, y = distcoast, z = fit)) + scale_fill_grey() +
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(x = hour, y = distcoast),alpha = 1, inherit.aes = FALSE) + 
  new_scale_fill() + 
  geom_contour_filled(data = tbm2_h_merge[tbm2_ah_merge$confidence == 70 & tbm2_ah_merge$Significance == 1,], aes(x = hour, y = distcoast, z = fit)) + 
  scale_fill_viridis(option = "inferno", discrete = TRUE) + facet_wrap(~seasonF) + theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hour of day", y = "Distance to coast (m)", fill = "Change nr of capuchins") +
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12))

# color on lighter color
ggplot() +
  geom_contour_filled(data = tbm2_ah_merge, breaks = mybreaks, show.legend = TRUE, aes(x = hour, y = distcoast, z = fit), alpha = 0.7) +
  scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE)+
  geom_rug(data = onlycap_tj[onlycap_tj$toolusers == "Non-tool-users",], aes(x = hour, y = distcoast),alpha = 1, inherit.aes = FALSE) + 
  new_scale_fill() + 
  geom_contour_filled(data = tbm2_ah_merge[tbm2_ah_merge$confidence == 70 & tbm2_ah_merge$Significance == 1,], breaks = mybreaks, show.legend = TRUE, aes(x = hour, y = distcoast, z = fit)) + 
  scale_fill_manual(values = inferncol, name = "Change nr of capuchins", drop = FALSE) + facet_wrap(~seasonF) + theme_bw() + theme(panel.grid = element_blank())  +
  labs(x = "Hour of day", y = "Distance to coast (m)", fill = "Change nr of capuchins") +
  theme(strip.text.x = element_text(size = 12), axis.title = element_text(size = 14), legend.text =  element_text(size = 12), plot.title = element_text(size = 14),
        legend.title = element_text(size =12), axis.text = element_text(size=12))


#### DESCRIPTIVES ####
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

## number of capuchins per sequence
# did we see capuchins on all cameras
agoutiselect_tj <- agoutiselect_t[agoutiselect_t$island == "Jicaron" & agoutiselect_t$locationfactor != "CEBUS-03",]
table(agoutiselect_tj$capuchin, agoutiselect_tj$uniqueloctag)
mean(onlycap_tj$n[which(onlycap_tj$toolusers == "Tool-users")])
max(onlycap_tj$n)

## reduced sample
tidaldays_r <- onlycap_tj[onlycap_tj$dataorigin == "agoutidata",]
tidaldays_r$dayloc <-  paste(tidaldays_r$locationfactor, tidaldays_r$seqday, sep = " ")
tidaldays2_r <- tidaldays_r[!duplicated(tidaldays_r$dayloc),]

# make overview of deployments we have and their start and end days
locations_tr <- data.frame(uniqueloctag = unique(onlycap_tj$uniqueloctag[which(onlycap_tj$dataorigin == "agoutidata")])) 
locations_tr <- left_join(locations_tr, onlycap_tj[,c("uniqueloctag", "dep_start", "dep_end", "locationfactor", "toolusers")], by = "uniqueloctag")
locations_tr <- locations_tr[!duplicated(locations_tr$uniqueloctag),]
# take time off and keep just date variable
locations_tr$dep_startday <- as.Date(format(locations_tr$dep_start, "%Y-%m-%d"))
locations_tr$dep_endday <- as.Date(format(locations_tr$dep_end, "%Y-%m-%d"))
# calculate days in each deployment (round up)
locations_tr$dep_days <- ceiling(difftime(locations_tr$dep_end, locations_tr$dep_start, units = c("days")))
# number of rows in the tidaldays2 dataframe (so how many days we have)
for (i in 1:nrow(locations_tr)) {
  locations_tr$nrow[i] <- nrow(tidaldays2_r[tidaldays2_r$uniqueloctag == locations_tr$uniqueloctag[i],])
}

locations_tr2 <- aggregate(locations_tr$dep_days, list(locationfactor  = locations_tr$locationfactor, toolusers = locations_tr$toolusers), FUN = sum)

sum(locations_tr$dep_days[locations_tr$toolusers == "Non-tool-users"])
sum(locations_tr2$x)

# How many locations
nrow(locations_tr2)
ftable(locations_tr2$toolusers)
# Average number of trapping days per location
summary(as.numeric(locations_tr2$x))
# average number of trapping days per deployment
summary(as.numeric(locations_tr$dep_days))
# number of deployments per location
max(as.matrix(ftable(locations_tr$locationfactor)))
mean(as.matrix(ftable(locations_tr$locationfactor)))




########## TEMPERATURE ########
# First need to identify cameras that are definitely wrong
# per deployment, plot temperature per hour of the day with color of the camera location
plot(onlycap_tj$seq_start, onlycap_tj$temperature)

ggplot(data = onlycap_tj[str_detect(onlycap_tj$uniqueloctag, "R6") == TRUE,], aes(x = seq_start, y = temperature, group = locationfactor, color = locationfactor)) +
  geom_point()
# in R1: Survey CEBUS-01-01 pops out with very high values (~45 degrees)
# in R2: Everything seems alright and pretty consistent
# in R3: Also seems alright and consistent
# in R4: CEBUS-09 goes up to 50 degrees a few times, also SURVEY-CEBUS-15-04 goes up a few times
# in R5: CEBUS-01 seems broken (goes up to 50 degrees). CEBUS-04 has spike in 50's once.
# in R6: CEBUS-04 goes above 40 a few times, but not very far

# variation in cameras within one day
ggplot(data = onlycap_tj[onlycap_tj$seqday == "2018-02-18",], aes(x = hour, y = temperature, group = locationfactor, color = locationfactor)) +
  geom_point()

# does distance to coast matter a lot?
# consider temperature  vs distance to coast (see if it changes within hour)
ggplot(data = onlycap_tj[onlycap_tj$seqday == "2018-02-18",], aes(x = hour, y = temperature, color = distcoast)) +
  geom_point()
ggplot(data = onlycap_tj[str_detect(onlycap_tj$uniqueloctag, "R1") == TRUE,], aes(x = hour, y = temperature, color = distcoast)) +
  geom_point()
# doesn't seem to matter that much

# difference jicaron tool use and non tool use
ggplot(data = onlycap_tj[str_detect(onlycap_tj$uniqueloctag, "R6") == TRUE,], aes(x = hour, y = temperature, color = toolusers)) +
  geom_point()

## Filter out wrong ones, then get to average temperature per seqday per hour (for sequences that are present)
# R1
onlycap_tj[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-01-01-R1", c("seqday","hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-01-01-R1",], aes(x = hour, y = temperature)) +  geom_point()
# is likely due to camera being in the direct sun, is at hotter part of the day
# but 2017-06-01  at  7 being 37 degrees is likely wrong 

# R4
onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-09-R4",c("hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-09-R4",], aes(x = hour, y = temperature)) +  geom_point()
onlycap_tj[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4",c("seqday", "hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4",], aes(x = hour, y = temperature)) +  geom_point()
# seems to be that at 2018-07-05 at 8 AM the estimates are all very high (although at 7 they are around 26 degrees, now suddenly around 30-40). Take average of 7 AM?

# R5
onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-01-R5", c("seqday", "hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-01-R5",], aes(x = hour, y = temperature)) +  geom_point()
# seems that on 2018-09-07 have many of the over 40 values, also in the 50s. Fix by setting over 40's to NA?

onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-04-R5", c("seqday", "hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-04-R5",], aes(x = hour, y = temperature)) +  geom_point()

# R6
onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-04-R6", c("seqday", "hour", "temperature")]
ggplot(data = onlycap_tj[onlycap_tj$uniqueloctag == "CEBUS-04-R6",], aes(x = hour, y = temperature)) +  geom_point()

### Filter out obviously wrong ones

# create new temperature variable, set wrong cameras to NA and wrong temperatures (>40) to NA
onlycap_tj$temp <- ifelse(onlycap_tj$temperature > 40, NA, onlycap_tj$temperature)
onlycap_tj$temp[onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4" & onlycap_tj$seqday == "2018-07-05" & hour == 8,]
# correct the time at 2018-07-05 in SURVEY-CEBUS-15-04-R4
onlycap_tj$temp[which(onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4" & onlycap_tj$seqday == "2018-07-05" & onlycap_tj$hour == 8)] <-
  median(onlycap_tj$temp[which(onlycap_tj$uniqueloctag == "SURVEY-CEBUS-15-04-R4" & onlycap_tj$seqday == "2018-07-05" & onlycap_tj$hour == 7)])

### OPTION 1: Run in this form (or impute data). Keep variation
tidetemp_m1 <- gam(n ~ te(tidedif, distcoast, bs = c("tp", "tp"), k = c(10, 6)) +
                     te(tidedif, distcoast, temp, bs = c("tp", "tp"), k = c(10,6), m = 1) +
                     s(locationfactor, bs = "re"), family = poisson, data = onlycap_tj[onlycap_tj$toolusers == "Tool-users",], method = "REML",
                   select = TRUE)

summary(tidetemp_m1)
draw(tidetemp_m1)

### OPTION 2: Take average temperature of all cameras active at that date and hour (lose variation)


# aggregate by seqday, hour, temperature (mean)


# replace NA's with temp at same hour on previous day

# how to deal with Claudio's data? 

# add temp2 to dataframe by leftjoining on seqday + hour









# standardize within cameras vs get average per region (e.g. tool users) or whole island even?





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

