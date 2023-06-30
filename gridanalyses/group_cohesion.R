## Investigating group cohesion in grid data of fixed anvil tool-using vs non-tool-using groups 
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

# start with agouticlean which is cleaned, not yet aggregated to sequence level or excluded incomplete deployments
# start with the agoutisequence_c dataframe that's cleaned and aggregated to the sequence level 
# alternatively have agoutiselect2 data that is on level of day-hour with zero's added in

## Packages required
library(stringr)
library(ggplot2)
library(mgcv)
#library(gratia)
library(reshape2)
library(asnipe)
library(igraph)
library(activity)
library(lme4)
library(brms)
library(dplyr)
library(geodist)
library(sna)
library(assortnet)

## Notes for analyses:

# Need to find some way to incorporate detection distance in the models, see how it varies between the cameras. 
# --> have this information on Kobo, can add it to the csv with camera information? --> this is incomplete
# will need to find out how to calculate detection distance from camera images

##### DIAGNOSTICS ####

## Subset to only grid cameras
# observation level
gridclean <- agouticlean[which(str_detect(agouticlean$locationName, "TU") == TRUE),]
ftable(gridclean$locationName)
# sequence level
gridsequence <- agoutisequence_c[which(str_detect(agoutisequence_c$locationName, "TU") == TRUE),]
ftable(gridsequence$locationName)

## exclude grid cameras that are blank
# NTU-151 and TU-168 have only blanks, TU-152 was pointed at the ground and therefore had mostly blanks from the start. For now exclude these three
gridclean_c <- gridclean[! gridclean$locationName %in% c("NTU-151", "TU-168", "TU-152"),]
gridclean_c$gridtype <- ifelse(str_detect(gridclean_c$locationName, "NTU") == TRUE, "NTU", "TU")

gridsequence_c <-  gridsequence[! gridsequence$locationName %in% c("NTU-151", "TU-168", "TU-152"),]
gridsequence_c$gridtype <- ifelse(str_detect(gridsequence_c$locationName, "NTU") == TRUE, "NTU", "TU")
gridsequence_c <- droplevels.data.frame(gridsequence_c)
#saveRDS(gridclean_c, "gridanalyses/RDS/gridclean_c.RDS")
#saveRDS(gridsequence_c, "gridanalyses/RDS/gridsequence_c.RDS")

## Did we have at least one capuchin detection at all remaining cameras?
ftable(gridsequence_c[which(gridsequence_c$capuchin == 1),]$locationName)

## any weird time issues?
hist(gridsequence_c[which(gridsequence_c$capuchin == 1),]$hour)
# all looks fine

# filter down to only capuchin detections
gridseq_oc <- gridsequence_c[gridsequence_c$capuchin == 1,]
gridseq_oc$gridtype <- as.factor(gridseq_oc$gridtype)
gridseq_oc <- droplevels.data.frame(gridseq_oc)

###### Exposure, how many trapping nights TU vs NTU ####
## How many camera trapping days
griddays <- gridsequence_c
griddays$dayloc <- paste(griddays$locationfactor, griddays$seqday, sep = " ")
griddays2 <- griddays[!duplicated(griddays$dayloc),]

# make overview of deployments we have and their start and end days
gridlocations_t <- data.frame(uniqueloctag = unique(gridsequence_c$uniqueloctag)) 
gridlocations_t <- left_join(gridlocations_t, gridsequence_c[,c("uniqueloctag", "dep_start", "dep_end", "locationfactor", "gridtype")], by = "uniqueloctag")
gridlocations_t <- gridlocations_t[!duplicated(gridlocations_t$uniqueloctag),]
# take time off and keep just date variable
gridlocations_t$dep_startday <- as.Date(gridlocations_t$dep_start, tz = "America/Panama", "%Y-%m-%d")
gridlocations_t$dep_endday <- as.Date(gridlocations_t$dep_end, tz = "America/Panama", "%Y-%m-%d")
# calculate days in each deployment (round up)
gridlocations_t$dep_days <- ceiling(difftime(gridlocations_t$dep_end, gridlocations_t$dep_start, units = c("days")))
# number of rows in the griddays2 dataframe (so how many days we have)
for (i in 1:nrow(gridlocations_t)) {
  gridlocations_t$nrow[i] <- nrow(griddays2[griddays2$uniqueloctag == gridlocations_t$uniqueloctag[i],])
}

gridlocations_t2 <- aggregate(gridlocations_t$dep_days, list(locationfactor  = gridlocations_t$locationfactor, gridtype = gridlocations_t$gridtype), FUN = sum)

sum(gridlocations_t$dep_days[gridlocations_t$gridtype == "NTU"])
sum(gridlocations_t$dep_days[gridlocations_t$gridtype == "TU"])
## so comparable number of trapping days, but more in NTU than TU grid

# How many locations
ftable(gridlocations_t2$gridtype) # one more location in NTU grid
# Average number of trapping days per location
summary(as.numeric(gridlocations_t2$x[gridlocations_t2$gridtype == "NTU"]))
summary(as.numeric(gridlocations_t2$x[gridlocations_t2$gridtype == "TU"]))

# slightly longer deployments in NTU grid than TU grid, but no dramatic differences

###### Detection of other animals vs capuchin detections ####
head(gridsequence_c)

# can do per camera, nr of capuchin detections, nr of other animal detections
# then calculate ratio and have overall ratio NTU-TU?

# first try just visually

# capuchin is easy, that is the variable "capuchin"  1 or 0
# make other animal column
nocaps <- gridsequence_c[gridsequence_c$capuchin == 0,]
# options are
# pick/up deployment sequence
# humans (homo sapiens) 
# unknown
# basically if scientific name is not homo sapiens or blank then yes
gridspecies <- subset(gridsequence_c, gridsequence_c$scientificName != " Homo sapiens" & gridsequence_c$scientificName != "")
gridspecies$species <- ifelse(gridspecies$scientificName == "Cebus imitator", "Capuchin", 
                              ifelse(gridspecies$scientificName == "Dasyprocta coibae", "Agouti",
                                     ifelse(gridspecies$scientificName == "Rattus rattus", "Rat",
                                            ifelse(gridspecies$scientificName == "Alouatta palliata", "Howler",
                                                   ifelse(gridspecies$scientificName %in% c("Leptotila battyi", "Leptotila cassini"), "Pigeon",
                                                          ifelse(gridspecies$scientificName %in% c("Iguana iguana", "Holcosus quadrilineatus", "Clelia clelia"), "Reptile", "Other bird"))))))
ggplot(data = gridspecies, aes(x = species, group = gridtype, fill = gridtype)) + geom_histogram(stat = "count", position = "dodge") +  
  stat_count(binwidth = 1, geom = "text", aes(label = after_stat(count), group = gridtype), position = position_dodge(width = 1)) + theme_bw()

# so seems like at NTU grid there were many (many!) more agouti sightings, more pigeons, more rats. In general more animals that were not capuchins. 
# this is not yet offset to the number of observation days (which is slightly higher in NTU)
# so this is literally just the number of sequences with 1 or more animals present of each species (type)

# information overload, per camera
ggplot(data = gridspecies, aes(x = species, group = gridtype, fill = gridtype)) + geom_histogram(stat = "count", position = "dodge") +  
  stat_count(binwidth = 1, geom = "text", aes(label = after_stat(count), group = gridtype), position = position_dodge(width = 1)) + theme_bw() + facet_wrap(~locationName)

###### Detection distance ####

# load in gridadditionalinfo csv file
# see if average detection distance is similar between TU and NTU grid


##### Are we capturing one NTU group? ####
# max group size seen
NTUgridseq <- gridsequence_c[gridsequence_c$gridtype == "NTU",]
NTUgridseq[which(NTUgridseq$n == max(NTUgridseq$n)),]
# in max group size, see 4 adult females, 5 adult males, 5 juveniles (of which one infant), 2 subadult males

# look at supposed group composition (max number of adult males and adult females seen in one sequence and how many we have IDed)
max(NTUgridseq$nAF) # max of 4 adult females (have identified 5)
max(NTUgridseq$nAM) # max of 4 adult males (have identified 5, potentially 6)
max(NTUgridseq$nJU) # max of 6 juveniles
max(NTUgridseq$nSM) # max of 2 subadult males ( have identified 2, maybe 3)

# co-occurrence of identifiable individuals (SNA network)
# look at who occurs together in the same sequence. get nodes and see who has not been seen with anyone. 
# step 1: working with gridclean dataframe, need to turn the IDstrings into the ID key codes we use for clarity.
# need to get the file that links strings back to names. Asked Agouti for it
# until then just work with the ID strings as they are

# step 2: per sequence, get some kind of dyadic information of who was seen with whom
# make dataframe with individual variation
gridagesex <- gridclean_c[,c("name","lifeStage", "sex", "gridtype")]
gridagesex <- gridagesex[! is.na(gridagesex$name) == TRUE & ! duplicated(gridagesex$name),]
NTUgridagesex <- gridagesex[gridagesex$gridtype == "NTU",]
# for now just add real names in manually, later use key file
NTUgridagesex$col <- ifelse(NTUgridagesex$sex == "male", "lightblue", "pink")
NTUgridagesex$col <- ifelse(NTUgridagesex$lifeStage == "adult", NTUgridagesex$col, "lightgreen")
NTUgridagesex <- NTUgridagesex[order(NTUgridagesex$name),]

# I think data format needs to be sequenceID/individualID
# go to only NTU grid data and only sequence ID and individual ID (when individual ID was known)
NTUassoc <- gridclean_c[gridclean_c$gridtype == "NTU" & ! is.na(gridclean_c$name) == TRUE, c("sequenceID", "name")]

# then go from long to wide?
NTUassoc_w <- dcast(NTUassoc, sequenceID ~ name)
NTUassoc_w2 <- NTUassoc_w
NTUassoc_w2[is.na(NTUassoc_w2) == FALSE] <- 1
NTUassoc_w2$sequenceID <- NTUassoc_w$sequenceID
NTUassoc_w2[is.na(NTUassoc_w2)] <- 0
NTUassoc_w2[,2:15] <- as.numeric(unlist(NTUassoc_w2[,2:15]))
rownames(NTUassoc_w2) <- NTUassoc_w2$sequenceID
NTUassoc_w2 <- NTUassoc_w2[,-1]
## now we have a dataframe with all associations (whenever individuals were seen together in the same sequence) in GBI (group by individual) format
# use this with asnipe package to get a network 
adj.m <- get_network(NTUassoc_w2, association_index = "SRI")
assoc.g <- graph_from_adjacency_matrix(adj.m, "undirected", weighted = T)
plot(assoc.g, edge.width = E(assoc.g)$weight*100)

net_NTU <- graph.adjacency(adj.m, mode = "undirected", weighted = TRUE, diag = FALSE)
plot(net_NTU, vertex.color = NTUgridagesex$col, edge.width = E(assoc.g)$weight*100)
coms_NTU <- fastgreedy.community(net_NTU) #identify communities
NTUgridagesex$COM <- membership(coms_NTU) #assign membership of communities
plot(net_NTU, vertex.color =NTUgridagesex$col, edge.with = 20*E(net_NTU)$weight^2, mark.groups = coms_NTU)

# largely appears to be one group, but I will still do a double-check of the IDs and try to identify more individuals in the big group sightings

### SNA TU GROUP ##########
## just out of interest, social network of TU group (based on all data)
agoutiseq_jt <- agouticlean[agouticlean$tool_site == 1 & agouticlean$island == "Jicaron" & agouticlean$capuchin == 1,]
TUassoc <- agoutiseq_jt[, c("sequenceID", "name")]

# make dataset with all unique individuals and their age-sex
inds <- agoutiseq_jt[!duplicated(agoutiseq_jt$name), c("name", "lifeStage", "sex")]
inds <- inds[-1,]
inds$col <- NA
inds$col[which(inds$sex == "male"  & inds$lifeStage == "adult")] <- "lightblue"
inds$col[which(inds$sex == "female" & inds$lifeStage == "adult")] <- "pink"
inds$col[which(inds$sex == "male"  & inds$lifeStage != "adult")] <- "lightgreen"
inds$col[which(inds$sex == "female" & inds$lifeStage != "adult")] <- "purple"

inds <- inds[order(inds$name),]

# then go from long to wide?
TUassoc_w <- dcast(TUassoc, sequenceID ~ name)
TUassoc_w2 <- TUassoc_w
TUassoc_w2$sequenceID <- TUassoc_w$sequenceID
TUassoc_w2[,2:21] <- as.numeric(unlist(TUassoc_w2[,2:21]))
rownames(TUassoc_w2) <- TUassoc_w2$sequenceID
TUassoc_w2 <- TUassoc_w2[,-c(1,21)]

# try to remove individuals that are rarely seen (less than 10 times now)
TUassoc_w3 <- TUassoc_w2[,colSums(TUassoc_w2) > 9]
# but need to consider better cutoff here

inds2 <- inds[inds$name %in% colnames(TUassoc_w3),]

## now we have a dataframe with all associations (whenever individuals were seen together in the same sequence) in GBI (group by individual) format
# use this with asnipe package to get a network 
adj.m_TU <- get_network(TUassoc_w2, association_index = "SRI")
net <- graph.adjacency(adj.m_TU, mode = "undirected", weighted = TRUE, diag = FALSE)
plot(net, vertex.color = inds$col, edge.width = E(assoc.g_TU)$weight*200)
coms <- fastgreedy.community(net) #identify communities
inds$COM <- membership(coms) #assign membership of communities
plot(net, vertex.color =inds$col, edge.with = 20*E(net)$weight^2, mark.groups = coms)

## with less seen individuals removed
adj.m_TU2 <- get_network(TUassoc_w3, association_index = "SRI")
net2 <- graph.adjacency(adj.m_TU2, mode = "undirected", weighted = TRUE, diag = FALSE)
plot(net2, vertex.color = inds2$col, edge.width = E(net2)$weight*200)
coms2 <- fastgreedy.community(net2) #identify communities
inds2$COM <- membership(coms2) #assign membership of communities
plot(net2, vertex.color =inds2$col, edge.width = 2000*E(net2)$weight^2, mark.groups = coms2)

## which individuals are more central?
inds$BINARY.DEGREE <- sna::degree(adj.m_TU, ignore.eval = TRUE)
inds$WEIGHTED.DEGREE <- sna::degree(adj.m_TU, ignore.eval=FALSE) 
# ABE is most central individual, followed by older females (OLG and BEA) 

###### Trap shyness ####
# inspired by McCarthy et al 2018
# can look at 1: if party size increases over time/if individuals are more likely to be spotted over time (only first is feasible with our data I think)
# depdays is days since camera was deployed
ggplot(data = gridseq_oc) + geom_smooth(aes(x = depdays, y = n, group = gridtype, color = gridtype))

# will be simple gam maybe if we expect not linear
ts_gam1 <- gam(n ~ s(depdays, by = gridtype) + s(locationfactor, bs = "re"), data = gridseq_oc, family = "poisson")
summary(ts_gam1)
draw(ts_gam1)

# linear
ts_m1 <- glmer(n ~ depdays*gridtype + (1|locationfactor), data = gridseq_oc, family = "poisson")
summary(ts_m1)
plot(ts_m1)

predicted_values<- modelr::data_grid(gridseq_oc, depdays, gridtype, locationfactor) %>% 
  modelr::add_predictions(ts_m1)
predicted_values %>% 
  ggplot(aes(depdays, pred, color = locationfactor))+
  geom_line()+
  geom_point(data = gridseq_oc, aes(depdays, n, color = locationfactor)) + facet_wrap(~gridtype)

# seems to be something going on but not a clear linear pattern, more non-linear. And not sure if this is due to trap shyness or other (seasonal) components

# 2: are you more likely to spot capuchins at a camera trap the longer it has been out? So this is then a model where per day you have 1 capuchin 0 no capuchin and time of days since cam deployment
# for this use the dataset with 0s in. 
head(agoutiselect2)
gridday <- agoutiselect2[str_detect(agoutiselect2$locationName, "TU") == TRUE, c("seqday", "locationfactor", "capuchin", "dep_start", "noanimal") ]
gridday$dep_startday <- format(gridday$dep_start, "%Y-%m-%d")
gridday$depdays <- as.numeric(difftime(gridday$seqday, gridday$dep_startday, units = "days"))
gridday$gridtype <- as.factor(ifelse(str_detect(gridday$locationfactor, "NTU") == TRUE, "NTU", "TU"))
gridday <- droplevels.data.frame(gridday)
#saveRDS(gridday, "gridanalyses/RDS/gridday.RDS")

ts_gam2 <- gam(capuchin ~ s(depdays, by = gridtype), data = gridday, family = binomial()) 
summary(ts_gam2)
plot(ts_gam2)


# linear
ts_m2 <- glmer(capuchin ~ depdays*gridtype + (gridtype|locationfactor), data = gridday, family = binomial)
summary(ts_m2)
library("emmeans")

# probably need to rescale depdays
# not sure if locationfactor needs to be nested in gridtype or not

ts_bm2 <- brm(capuchin ~ depdays*gridtype + (1|locationfactor), data = gridday, family = bernoulli(), iter = 1000, chain = 2, core = 2, backend = "cmdstanr")
#saveRDS(ts_bm2, "gridanalyses/RDS/ts_bm2.RDS")
plot(conditional_effects(ts_bm2))

# seems like (without cameras in) probability of capuchins is higher later in deployments

##### DAILY ACTIVITY PATTERN #####
# first just visually, what time of day do we see activity of capuchins? 
# colors for two histograms in one
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

### Tool users vs non tool users
histTU <- hist(gridseq_oc$hour[gridseq_oc$gridtype == "TU"], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)
histNTU <- hist(gridseq_oc$hour[gridseq_oc$gridtype == "NTU"], breaks = seq(from = 0, to = 24, by = 1), xlim = c(0, 24), freq = FALSE)

plot(histNTU, col = c2, freq = FALSE, main = "Tool users (blue) vs non-tool users (red)", xlab = "Time of Day", ylab = "Proportion of sequences with capuchins", ylim = c(0, 0.12))
plot(histTU, col = c1, freq = FALSE, add = TRUE)

# in general TU group appears to be active more later in the afternoon
# all early morning and late evening activity is from the TU group
# look at this closer
nightowls <- gridseq_oc[gridseq_oc$hour < 5 | gridseq_oc$hour > 19,]
# all of these cameras seem to have the correct time set. So this means we truly have a capuchin detection at midnight and one at 4 AM!

## still make proper daily activity model of this, look at Lester's paper for example code etc
# following this  vignette https://bookdown.org/c_w_beirne/wildCo-Data-Analysis/activity.html

## I think the right data format is what agouti gives you not aggregated to a sequence
# so each sequence gets repeated for each individual sighting
# use data with only capuchins in it
head(gridseq_oc)
# relevant columns are the gridtype (TU or NTU)
# seq_start time, which is when the observation occurred

# have various ways of representing time
# the best and most relevant to me, seems to be solar time, which uses coordinates of observations to determine sunrise and sunset
# then the activity is classified as being during the day or during the night
gridseq_oc$timestamp <- ymd_hms(gridseq_oc$seq_start, tz = "America/Panama")

tmp <- solartime(gridseq_oc$timestamp,
                 gridseq_oc$latitude,
                 gridseq_oc$longitude,
                 tz = -5,
                 format = "%Y-%m-%d %H:%M:%S")

gridseq_oc$solar <- tmp$solar
gridseq_oc$clock <- tmp$clock

plot(gridseq_oc$solar, gridseq_oc$clock)

# compare TU to NTU
# TU
act_m1 <- fitact(gridseq_oc$solar[gridseq_oc$gridtype == "TU"], sample = "model", reps = 100) # need to use 1000 reps
#saveRDS(act_m1, "gridanalyses/RDS/act_m1.RDS")
#act_m1 <- readRDS("gridanalyses/RDS/act_m1.RDS")
plot(act_m1)
act_m1@act[1] * 24
# this means they spend 0.38 * 24 = 9 hours per day active

# NTU
act_m2 <- fitact(gridseq_oc$solar[gridseq_oc$gridtype == "NTU"], sample = "model", reps = 100) # need to use at least 1000 reps
#saveRDS(act_m2, "gridanalyses/RDS/act_m2.RDS")
plot(act_m2)
act_m2@act[1] * 24
# this means they spend 0.38 * 24 = 10 hours per day active


# plot both together on same axis
plot(act_m1, yunit="density", data="none", las=1, lwd=2,
     tline=list(lwd=2), # Thick line 
     cline=list(lty=0)) # Supress confidence intervals

plot(act_m2, yunit="density", data="none", add=TRUE, 
     tline=list(col="red", lwd=2),
     cline=list(lty=0))

legend("topright", c("TU", "NTU"), col=1:2, lty=1, lwd=2)

# overlap between the two
compareCkern(act_m1, act_m2, reps = 100)
# 0.897366727 lot of overlap

# just out of curiosity, if we do the same on all the Jicaron TU data (not the grid)
jtonly <- agouticlean[agouticlean$island == "Jicaron" & agouticlean$tool_site == 1 & str_detect(agouticlean$locationName, "TU") == FALSE & agouticlean$capuchin == 1,]
jtonly$timestamp <- ymd_hms(jtonly$seq_start, tz = "America/Panama")

tmp2 <- solartime(jtonly$timestamp,
                 jtonly$latitude,
                 jtonly$longitude,
                 tz = -5,
                 format = "%Y-%m-%d %H:%M:%S")

jtonly$solar <- tmp2$solar
jtonly$clock <- tmp2$clock

# TU based on all data
# Note: this is biased because this contains both individuals passing through and using tools, esp the latter really retriggers the camera
# I was just curious, cant use it in this form. 
act_m3 <- fitact(jtonly$solar, sample = "model", reps = 100) # need to use 1000 reps
#saveRDS(act_m3, "gridanalyses/RDS/act_m3.RDS")
plot(act_m3)
act_m3@act[1] * 24

# plot TU grid and non-grid together on same axis
plot(act_m3, yunit="density", data="none", las=1, lwd=2,
     tline=list(lwd=2), # Thick line 
     cline=list(lty=0)) # Supress confidence intervals

plot(act_m1, yunit="density", data="none", add=TRUE, 
     tline=list(col="red", lwd=2),
     cline=list(lty=0))

legend("topleft", c("Non grid", "Grid"), col=1:2, lty=1, lwd=2)

# make activity for anvil cameras vs random vs streambed vs grid. 
jtonly$locationtype <- as.factor(ifelse(jtonly$tool_anvil == 1, "anvil", 
                                              ifelse(jtonly$streambed == 1, "streambed", "random")))
# plot them all together! 
# that will be interesting. 

# anvil
act_m4 <- fitact(jtonly$solar[jtonly$locationtype == "anvil"], sample = "model", reps = 100) # need to use 1000 reps
#saveRDS(act_m4, "gridanalyses/RDS/act_m4.RDS")
plot(act_m4)
act_m4@act[1] * 24

# streambed
act_m5 <- fitact(jtonly$solar[jtonly$locationtype == "streambed"], sample = "model", reps = 100) # need to use 1000 reps
#saveRDS(act_m5, "gridanalyses/RDS/act_m5.RDS")
plot(act_m5)
act_m5@act[1] * 24

# random
act_m6 <- fitact(jtonly$solar[jtonly$locationtype == "random"], sample = "model", reps = 100) # need to use 1000 reps
#saveRDS(act_m6, "gridanalyses/RDS/act_m6.RDS")
plot(act_m6)
act_m6@act[1] * 24

# plot TU grid and non-grid anvil/stream/random together on same axis
plot(act_m1, yunit="density", data="none", las=1, lwd=2,
     tline=list(lwd=2), # Thick line 
     cline=list(lty=0),
     ylim = c(0,0.18))# Supress confidence intervals

plot(act_m4, yunit="density", data="none", add=TRUE, 
     tline=list(col="green", lwd=2),
     cline=list(lty=0))

plot(act_m5, yunit="density", data="none", add=TRUE, 
     tline=list(col="blue", lwd=2),
     cline=list(lty=0))

plot(act_m6, yunit="density", data="none", add=TRUE, 
     tline=list(col="purple", lwd=2),
     cline=list(lty=0))

legend("topleft", c("Grid", "Anvil", "Streambed", "Random"), col=c("black", "green", "blue", "purple"), lty=1, lwd=2)

# are all surprisingly similar! 

##### PARTY SIZE ####

###### 1: Does the number of capuchins per sequence differ between TU and NTU grid? ####
# account for camera and detection distance (if we have it)
mean(gridseq_oc$n[gridseq_oc$gridtype == "NTU"])
mean(gridseq_oc$n[gridseq_oc$gridtype == "TU"])
## need to model this obvs, some kind of poisson?

gs_m1 <- glmer(n ~ gridtype + (1|gridtype:locationfactor), offset = log(dep_length_hours), data = gridseq_oc, family = poisson)
summary(gs_m1)
# but since there's likely this nonlinear daily relationship, might need to do gam instead
# also if trap shyness is a thing, should probably include number of days since first deployment

head(gridseq_oc)
## GAM I want to do below that looks at time of day/number of capuchins also does comparison of mean group size between TU and NTU! so maybe can be one model?

###### 2. Does the number of capuchins per sequence fluctuate depending on the hour of day, and does this relationship differ between TU and NTU ####
# use a gam
## hour of day is not cyclic spline, as we have no observations at midnight and early in morning (explained in bottom of the heap youtube)
grid_gam1 <- gam(n ~ s(hour, by = gridtype) + gridtype, offset = log(dep_length_hours), data = gridseq_oc, method = "REML", family = poisson())

summary(grid_gam1)
draw(grid_gam1)
# This simplest model seems to suggest for NTU there is no real nonlinear relationship (slight trend).
# if anything, is lower party size in morning and then constant throughout day until evening
# for TU, see higher party size in mornings and evenings, lower throughout the day

gam.check(grid_gam1)
# seems to still be a lot of unexplained variation we are not capturing
# probably we'd need to deal with the heavy 1 inflation to understand this model well
# it's now all being flattened by the many 1s sprinkled everywhere
# maybe we should include the 0s too? 

# including locationfactor as a random effect
grid_gam2 <- gam(n ~ s(hour, by = gridtype) + gridtype +  s(locationfactor, bs = "re"), offset = log(dep_length_hours), data = gridseq_oc, method = "REML", family = poisson())

summary(grid_gam2)
draw(grid_gam2)
gam.check(grid_gam2)


## Need to think further on how to model this. Is poisson appropriate without 0s? should be 1-inflated. Once I'm satisfied with it could take it to brms

## brms
ps_bm1 <- brm(n ~ s(hour, by = gridtype) + gridtype +  s(locationfactor, bs = "re") + offset(log(dep_length_hours)), data = gridseq_oc, family = poisson(), iter = 2000, chain = 2, core = 2, backend = "cmdstanr")
summary(ps_bm1)
plot(conditional_smooths(ps_bm1))


##### PARTY COMPOSITION ####
# try zero-inflated poisson number of adult  females in a sequence (in dataframe only capuchin detections. TU vs NTU)
# same for adult males
# then combine the likelihood functions into one model? 
pc_bm1 <- brm(nAF ~ gridtype + offset(log(dep_length_hours)) + (1|locationfactor), data = gridseq_oc, family = zero_inflated_poisson(link = "log", link_zi = "logit"), iter = 2000, chain = 2, core = 2, backend = "cmdstanr")
summary(pc_bm1)
plot(conditional_effects(pc_bm1))
pp_check(pc_bm1)
hypothesis(pc_bm1, "Intercept > Intercept + gridtypeTU")


# significantly higher number of females per sequence in NTU than TU grid

#males
pc_bm2 <- brm(nAM ~ gridtype + offset(log(dep_length_hours)) + (1|locationfactor), data = gridseq_oc, family = zero_inflated_poisson(link = "log", link_zi = "logit"), iter = 2000, chain = 2, core = 2, backend = "cmdstanr")
plot(conditional_effects(pc_bm2))
pp_check(pc_bm2)

# also higher number of males per sequence in NTU than TU grid

#combining?
pc_bm3 <- brm(nAF ~ gridtype*nAM + offset(log(dep_length_hours)) + (1|locationfactor), data = gridseq_oc, family = zero_inflated_poisson(link = "log", link_zi = "logit"), iter = 2000, chain = 2, core = 2, backend = "cmdstanr")
plot(conditional_effects(pc_bm3))
pp_check(pc_bm3)
summary(pc_bm3)
hypothesis(pc_bm3, "Intercept > Intercept + gridtypeTU")

##### CO-OCCURRENCES ####

## Step 1: Generate distance matrix showing distance between each camera per grid
TUgridcams <- gridseq_oc[!duplicated(gridseq_oc$locationfactor) & gridseq_oc$gridtype == "TU", c("locationfactor", "latitude", "longitude")]
TUgridcams <- TUgridcams[order(TUgridcams$locationfactor),]
TUdistmat <- geodist::geodist(TUgridcams)
rownames(TUdistmat) <- TUgridcams$locationfactor
colnames(TUdistmat) <- TUgridcams$locationfactor
TUdistmat

NTUgridcams <- gridseq_oc[!duplicated(gridseq_oc$locationfactor) & gridseq_oc$gridtype == "NTU", c("locationfactor", "latitude", "longitude")]
NTUgridcams <- NTUgridcams[order(NTUgridcams$locationfactor),]
NTUdistmat <- geodist::geodist(NTUgridcams)
rownames(NTUdistmat) <- NTUgridcams$locationfactor
colnames(NTUdistmat) <- NTUgridcams$locationfactor
NTUdistmat

### Using all TU data (not just grid), looking for co-occurrences within 60 seconds >150 m away
# exclude sequences with unfamiliar individuals (from grid) and CEBUS-03 (duplicate)
agoutiseq_jt <- agoutisequence_c[agoutisequence_c$capuchin == 1 & agoutisequence_c$island == "Jicaron" & agoutisequence_c$tool_site == 1 & !agoutisequence_c$locationfactor == "CEBUS-03",]
unfamiliars <- agouticlean$sequenceID[which(str_detect(agouticlean$comments.x, "unfamiliar") == TRUE & agouticlean$tool_site == 1 & agouticlean$island == "Jicaron")]
agoutiseq_jt <- agoutiseq_jt[! agoutiseq_jt$sequenceID %in% unfamiliars,]

# make distance matrix for all cameras
TUcams <- agoutiseq_jt[!duplicated(agoutiseq_jt$locationfactor), c("locationfactor", "latitude", "longitude")]
TUcams <- TUcams[order(TUcams$locationfactor),]
TUdistmat_all <- geodist::geodist(TUcams)
rownames(TUdistmat_all) <- TUcams$locationfactor
colnames(TUdistmat_all) <- TUcams$locationfactor

i <-  13295
# make blank co-occurrence info data frame? 
agoutiseq_jt$cooccurrence <- 0
agoutiseq_jt$cooc_ID <- NA

cooccurrences <- data.frame(cooc_ID = "seqid", seqstart = NA, seqday = NA, cam1 = NA, cam2 = NA, cam3 =NA, distcam12 = 0, distcam13 = 0, nrseq = 0, nrcap_1 = 0, nrcap_2 = 0, nrcap_3 = 0,
                            nAdult_1 = 0, nAdult_2 = 0, nAdult_3 = 0, nSubadult_1 = 0, nSubadult_2 = 0, nSubadult_3 = 0, nJuvenile_1 = 0, nJuvenile_2 = 0,
                            nJuvenile_3 = 0, nUU_1 = 0, nUU_2 = 0, nUU_3 = 0, tooluse_1 = NA, tooluse_2 = NA, tooluse_3 = NA)

for (i in 1:nrow(agoutiseq_jt)) {
  ## at beginning have some kind of check if the sequenceID is already in the co-occurence dataframe, if so can skip everything
  if(sum(str_detect(cooccurrences$cooc_ID, paste(agoutiseq_jt$sequenceID[i]))) == 0) {
    dist <- as.data.frame(subset(TUdistmat_all, rownames(TUdistmat_all) %in% agoutiseq_jt$locationfactor[i])) 
    cand_locs <- colnames(dist[,dist > 150]) # make list of candidate locations for co-occurrence (>150 m away)
    # filter to sequence that are at candidate location and on same day as sequence we're looking at 
    cand_seq <- agoutiseq_jt[agoutiseq_jt$locationfactor %in% cand_locs & agoutiseq_jt$seqday == agoutiseq_jt$seqday[i], c("sequenceID", "locationfactor", "seqday", "seq_start", "seq_end", "n", "nAdult", "nJuvenile","nSubadult", "nUU", "tooluse")]
    dist_m <- melt(dist)
    cand_seq$locationfactor <- as.character(cand_seq$locationfactor)
    dist_m$variable <- as.character(dist_m$variable)
    cand_seq <- left_join(cand_seq, dist_m, by = c("locationfactor" = "variable"))
    # see if there are any co-occurrences
    # if there is anything, then extract information from those sequences, both add to agoutiseq_jt dataframe, and to co-occurrence dataframe?
    if(nrow(cand_seq) > 0) {
      if(min(abs(difftime(agoutiseq_jt$seq_start[i], cand_seq$seq_start, unit = "s"))) < 60) {
        cand_seq$dtime <- difftime(agoutiseq_jt$seq_start[i], cand_seq$seq_start, unit = "s")
        cand_seq_t <- cand_seq[abs(cand_seq$dtime) < 60,]
        cand_seq_t <- cand_seq_t[!duplicated(cand_seq_t$locationfactor),]
        if(nrow(cand_seq_t) > 0) {
          agoutiseq_jt$cooccurrence <- 1
          agoutiseq_jt$cooc_ID[i] <- ifelse(nrow(cand_seq_t) == 1, paste(agoutiseq_jt$sequenceID[i], cand_seq_t$sequenceID[1], sep = ","),
                                            paste(agoutiseq_jt$sequenceID[i], cand_seq_t$sequenceID[1], cand_seq_t$sequenceID[2], sep = ","))
          cooccurrences[nrow(cooccurrences) +1,] <- c(agoutiseq_jt$cooc_ID[i], paste(agoutiseq_jt$seq_start[i]), paste(agoutiseq_jt$seqday[i]), paste(agoutiseq_jt$locationfactor[i]), 
                                                      paste(cand_seq_t$locationfactor[1]), paste(cand_seq_t$locationfactor[2]), cand_seq_t$value[1], cand_seq_t$value[2], nrow(cand_seq_t), agoutiseq_jt$n[i], 
                                                      cand_seq_t$n[1], cand_seq_t$n[2], agoutiseq_jt$nAdult[i], cand_seq_t$nAdult[1], cand_seq_t$nAdult[2],agoutiseq_jt$nSubadult[i], cand_seq_t$nSubadult[1], 
                                                      cand_seq_t$nSubadult[2], agoutiseq_jt$nJuvenile[i], cand_seq_t$nJuvenile[1], cand_seq_t$nJuvenile[2], agoutiseq_jt$nUU[i], cand_seq_t$nUU[1], 
                                                      cand_seq_t$nUU[2], paste(agoutiseq_jt$tooluse[i]), paste(cand_seq_t$tooluse[1]), paste(cand_seq_t$tooluse[2]))
        }
      }
    }
  }
  print(i)
}

cooccurrences <- cooccurrences[-1,]
cooccurrences[,7:22] <- as.numeric(unlist(cooccurrences[,7:22]))
#saveRDS(cooccurrences, "gridanalyses/RDS/cooccurrences.RDS")
#cooccurrences <- readRDS("gridanalyses/RDS/cooccurrences.RDS")

sum(cooccurrences$tooluse_1 == TRUE | cooccurrences$tooluse_2 == TRUE)
summary(cooccurrences$nrcap_1)
summary(cooccurrences$nrcap_2)
allcapnrs <- c(cooccurrences$nrcap_1, cooccurrences$nrcap_2)
sum(cooccurrences$nrcap_1 == 1 | cooccurrences$nrcap_2 == 1)
cooccurrences$ratio <- cooccurrences$nrcap_1/(cooccurrences$nrcap_1 + cooccurrences$nrcap_2)
summary(cooccurrences$ratio)

cooccurrences$tooluse_any <- factor(ifelse(cooccurrences$tooluse_1 == "TRUE" | cooccurrences$tooluse_2 == "TRUE" | cooccurrences$tooluse_3 == "TRUE", "TRUE", "FALSE"))

hist(hour(cooccurrences$seqstart))
ggplot(data = cooccurrences, aes(x = hour(cooccurrences$seqstart), fill = tooluse_any)) + geom_histogram(bins = 12, col = "black") + facet_wrap(~tooluse_any) + scale_x_continuous(breaks = 6:18) +
  theme_bw() + xlab("Hour")
max(unique(hour(cooccurrences$seqstart)))
sum(cooccurrences$nJuvenile_1 > 0 | cooccurrences$nJuvenile_2 > 0)
sum(cooccurrences$nAdult_1 > 0 | cooccurrences$nAdult_2 > 0)
sum(cooccurrences$nSubadult_1 > 0 | cooccurrences$nSubadult_2 > 0)

ggplot(data = cooccurrences, aes(x = month(cooccurrences$seqstart), fill = as.factor(year(cooccurrences$seqstart)))) + geom_histogram(bins = 12, col = "black")  + theme_bw() + scale_x_continuous(breaks = 0:12) +
  labs(fill = "Year") + xlab("Month")

hist(cooccurrences$distcam12)
summary(cooccurrences$distcam12)
agoutiseq_jt$year <- year(agoutiseq_jt$seq_start)
length(unique(agoutiseq_jt$locationfactor[which(agoutiseq_jt$year == "2022")]))
