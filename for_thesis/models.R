#agoutisequence_c contains also observations without capuchin detection
library(dplyr)
setwd("C:/Users/Lucia/Desktop/IRT3 - thesis/camtrap_coiba-main (github)/camtrap_coiba-main")
agoutisequence_c<-read.csv("agoutisequence_c.csv")
str(agoutisequence_c)

###clean agoutisequence_c, keep only columns with relevant information
#specify the columns to keep
columns_to_keep <- c(
  "observationID", "deploymentID", "sequenceID", 
  "cameraSetup", "scientificName", "count", "sex", "behaviour", 
  "individualID", "locationID", 
  "locationName", "tags", "seq_start", "seq_end", "seq_length", 
  "mediatype", "captureMethod", "dep_start", "dep_end", 
  "dep_length_hours", "month", "season", "longitude", "latitude", 
  "island", "side", "tool_anvil", "tool_site", "streambed", 
  "capuchin", "agesex", "n", "name", "nAdult", "nSubadult", 
  "nJuvenile", "nFemales", "nMales", "n_inspect", "tooluse", 
  "n_tooluse", "seqday", "dep_startday", "dep_endday", 
  "seq_startday", "dep_starttime", "dep_endtime", "exposure", 
  "hour", "toolusers", "locationfactor", "depdays", "depnr", 
  "islandside", "picksetup", "uniqueloctag"
)
agoutisequence_c <- agoutisequence_c[, columns_to_keep]


#I have 160 deployments
unique(agoutisequence_c$deploymentID)

###subset only observations with capuchin detection
with_capuchins<-agoutisequence_c[agoutisequence_c$capuchin==1,]
ordered_with_capuchin <- with_capuchins %>%
  arrange(seq_start)

###create a new column date, extracting date from seq_start
#convert the 'seq_start' column to a datetime format
library(lubridate)
ordered_with_capuchin$seq_start <- ymd_hms(ordered_with_capuchin$seq_start)
#create a new column 'date' containing only the date part
ordered_with_capuchin$date <- as.Date(ordered_with_capuchin$seq_start)

###detections/day per each unique location per day
ordered_with_capuchin <- ordered_with_capuchin %>%
  group_by(uniqueloctag, date) %>%
  summarise(detections_per_day = n()) %>%
  left_join(ordered_with_capuchin, by = c("uniqueloctag", "date"))
#I have now the ordered_with_capuchin showing the number of detections per location per day

###inspection per each unique location per day
ordered_with_capuchin <- ordered_with_capuchin %>%
  group_by(uniqueloctag, date) %>%
  mutate(insp_day = sum(n_inspect == 1))

###now I want a new column with inpections/detections
ordered_with_capuchin <- ordered_with_capuchin %>%
  group_by(date, uniqueloctag) %>%
  mutate(inspection_rate = insp_day / sum(capuchin == 1))

###not sure everything worked fine I need to clean my db a bit
capuchin_cleaned <- ordered_with_capuchin[, c("date","observationID", "deploymentID", "sequenceID", 
                                              "cameraSetup", "sex", "behaviour", 
                                              "individualID", "locationID", 
                                              "tags", "seq_start", "seq_end",
                                              "month", "season", "longitude", "latitude", 
                                              "island", "side", "tool_anvil", "tool_site", "streambed", 
                                              "capuchin", "agesex", "nAdult", "nSubadult", 
                                              "nJuvenile", "nFemales", "nMales", "n_inspect", "tooluse", 
                                              "seqday", "dep_startday", "dep_endday", 
                                              "exposure", "hour", "toolusers", "locationfactor", "depdays", "depnr", 
                                              "islandside", "picksetup", "uniqueloctag", "detections_per_day", 
                                              "insp_day", "inspection_rate")]
###database on which I can run the models
#capuchin_cleaned (only with capuchin detections)



#which are the unique locations? 124 different locations
unique(agoutisequence_c$locationID)

#for CEBUS-01
#plot number of inspections over time
#filter data for locationName == "CEBUS-01" and behavior == "BS: inspection"
as.factor(agoutisequence_c$locationName)
cebus_inspection_data_cebus01 <- agoutisequence_c %>%
  filter(locationName == "CEBUS-01" & n_inspect ==1)
unique(cebus_inspection_data_cebus01$seqday)


####from "activity_other.R"
require(lubridate)
require(brms)
require(lme4)
require(dplyr)
require(ggplot2)
require(fitdistrplus)
require(mgcv)
require(gratia)

### Camera Inspection ####
# For now overall, but could split into age/sex categories
# have nr of capuchins per sequence inspecting the camera

# get day of deployment (from 1 up) by doing date of sequence - start date deployment?
str(agoutisequence_c)
#create a column where there is the difference of the start day of the sequence and the start day of the deployment
#the sequence can start on the deployment start day or later
agoutisequence_c$depdays <- as.numeric(difftime(agoutisequence_c$seq_startday, agoutisequence_c$dep_startday, units = "days"))
#extract depnr from the tag, which is its numerical part 
agoutisequence_c$depnr <- as.numeric(unlist(regmatches(agoutisequence_c$tags, gregexpr("[[:digit:]]+", agoutisequence_c$tags))))
agoutisequence_c$depnrF <- factor(agoutisequence_c$depnr, order = TRUE)

## only consider sequences with capuchins, all deployments?
#tags, I have 12 tags, R9 is missing and one is "Caution | R11"
unique(agoutisequence_c$tags)
#depnr is from the tags, I have numbers from 1 to 12, 9 is missing
unique(agoutisequence_c$depnr)

agouti_inspect <- agoutisequence_c[agoutisequence_c$capuchin == 1,]
# can either use nr of capuchins per sequence inspecting
# or proportion of capuchins per sequence inspecting (over detections?)
agouti_inspect$prop_inspect <- agouti_inspect$n_inspect/agouti_inspect$n


# NEED TO CHANGE THE DEPLOYMENT NR TO 'NR OF DEPLOYMENTS IN THIS GROUP'. 
# below is a very roundabout way (im sure it can be done in 1/2 lines). But it works!
# the number we have now (depnr) is really just the number of the deployment
#for each unique locationfactor I have the nr (extracted from tags) of the deployments
depnrs <- aggregate(depnr ~ locationfactor, data = agouti_inspect, FUN = unique)
#I calculate the nr of different deployments per each locationfactor (CEBUS 02,..), I can have up to 7 different deployments
depnrs$nrdep <- lengths(depnrs$depnr)

#sort the agouti_inspect dataframe by locationfactor and depnr
agouti_inspect <- agouti_inspect[order(agouti_inspect$locationfactor, agouti_inspect$depnr),]
# Create new columns for up to 7 deployments in depnrs and initialize them to NA
depnrs$seconddep <- NA
depnrs$thirddep <- NA
depnrs$fourthdep <- NA
depnrs$fifthdep <- NA
depnrs$sixthdep <- NA
depnrs$seventhdep <- NA
for(i in 1:nrow(depnrs)) {
  depnrs$depnr[[i]] <- sort((depnrs$depnr)[[i]])
  #assign the first deployment to the firstdep column
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
  if(depnrs$nrdep[i] > 6) {
    depnrs$seventhdep[i] <- depnrs$depnr[[i]][7]}
}
agouti_inspect$depnr2 <- NA
#left join "agouti_inspect" with "depnrs", excluding the second column (the one specifing the vector with the nr of the deployments)
agouti_inspect <- left_join(agouti_inspect, depnrs[,-2], by  = "locationfactor")
#???create a new column "depnr2" in agouti_inspect so that if a deployment's number (depnr) matches the first deployment number (firstdep) within its group, it is assigned the position or value 1 in the depnr2 column.
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

#? the new deployment nr captures how many deployments have already been in this specific location. 

agouti_inspect <- agouti_inspect[,c("sequenceID", "locationfactor", "seq_start", "seq_end", "seq_length",
                                    "uniqueloctag", "dep_start", "dep_end", "dep_length_hours", "month",
                                    "season", "island", "tool_anvil", "tool_site", "streambed", "n",
                                    "n_inspect", "depdays", "depnr2", "depnr", "depnrF", "toolusers")]

#### Models ####
# for now only considering sequences with capuchins!

library(brms)
# set normal prior
inspect_prior <- c(prior(normal(0,2), class = b))

## prior simulation (for 4a model)
#warning message: rows containing NAs were excluded from the model,cannot plot it
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