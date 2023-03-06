## Detailed tool use analyses -- Cleaning Script
## MPI-AB; Z Goldsborough

## Script how to clean BORIS output data

## packages needed
library(stringr)
library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)
require(tidybayes)
library(fitdistrplus)


## NOTE: add code about catching when anvil type switches (e.g. if it's wooden anvil at comment seq_start, or whe nthey switch back to stone/wood)

# load tsv file with aggregated BORIS output
dettools <- read.csv("detailedtools/ZGdetailedtoolscoding.tsv", sep = "\t")
# Meredith's csv
#dettools <- read.csv("detailedtools/EXP-ANV-01-R11_MKWC.csv")

# sort so that observations from the same video are clustered together and it's chronological
dettools <- dettools[order(dettools$Observation.id),]

# remove unnecessary columns and rename the ones we keep
dettools_r <- data.frame("videoID" = dettools$Observation.id, "codingdate" = dettools$Observation.date,
                         "medianame" = dettools$Media.file, "videolength" = dettools$Total.length, "coder" = 
                           dettools$Coder.ID, "subjectID" = dettools$Subject, "behavior" = dettools$Behavior,
                         "modifiers" = dettools$Modifiers, "starttime" = dettools$Start..s., "comment" = dettools$Comment.start)

dettools_r <- dettools_r[!dettools_r$videoID == "femaletooluse1",]

## creature unique sequence ID that is same for sequences continuing across multiple videos
## and different for 2 or 3 sequences in the same video
### NOTE: it is crucial that seq_start is the first thing and seq_end the last thing in each sequence!!!! 

# change seq_end to seq_cont if it has the continue modifier
dettools_r$behavior[which(str_detect(dettools_r$modifiers, "cont") == TRUE)] <- "seqcont"

# assign unique sequence ID to each sequence (Location + Coder + Ascending number)
# create ascending number.
curseq <- 1
cache <- 1
dettools_r$seqnumber <- NA

for (i in 1:nrow(dettools_r)) {
  dettools_r$seqnumber[i] <- curseq
  
  if(dettools_r$behavior[i] == "seqend") {
    cache <- curseq
    dettools_r$seqnumber[i] <- curseq
    curseq <- NA
  }
  if(dettools_r$behavior[i] == "seqstart") {
  curseq <- cache + 1
  dettools_r$seqnumber[i] <- curseq
  cache <- NA
  }

}

# combine with location and date to get unique seq_ID
dettools_r$location <- ifelse(str_detect(dettools_r$medianame, "EXP-ANV") == TRUE, "EXP-ANV-01", "CEBUS-02")
dettools_r$mediadate <- sapply(str_split(dettools_r$medianame, "__"), '[', 2)
dettools_r$sequenceID <- paste(dettools_r$location, dettools_r$mediadate, dettools_r$seqnumber, sep = "_" )

## pull out all modifiers per sequence
# make dataframe with sequence_level information and populate it, then left_join at the end
seqdat <- data.frame(sequenceID = unique(dettools_r$sequenceID))

# what item is being consumed
seqdat$item[which(seqdat$sequenceID == dettools_r[dettools_r$behavior == "seqstart",]$sequenceID)] <-  dettools_r[dettools_r$behavior == "seqstart",]$modifiers[which(seqdat$sequenceID == dettools_r[dettools_r$behavior == "seqstart",]$sequenceID)]

# hammerstone information
hammers <- dettools_r[dettools_r$behavior == "hammerstone",]
hammers$h_startloc <- sapply(str_split(hammers$modifiers, "[|]"), '[', 1)
hammers$h_endloc <- sapply(str_split(hammers$modifiers, "[|]"), '[', 4)
hammers$h_endloc <- ifelse(hammers$h_endloc == "None",
                           lead(hammers$h_endloc, order_by = hammers$sequenceID),
                           hammers$h_endloc)
hammers <- hammers[which(hammers$h_startloc != "None"),]
hammers$nrhammers <- sapply(str_split(hammers$modifiers, "[|]"), '[', 3)
hammers$nrhammers[which(hammers$nrhammers == "None")] <- 0
# check that the next line works correctly to identify unmarked and unknown hammerstones
hammers$hammerID <- ifelse(sapply(str_split(hammers$modifiers, "[|]"), '[', 2) != "None", sapply(str_split(hammers$modifiers, "[|]"), '[', 2), hammers$comment)
hammers <- hammers[,c("sequenceID", "h_startloc", "h_endloc", "nrhammers", "hammerID")]

seqdat <- left_join(seqdat, hammers, "sequenceID")

# sequence end information
seqendings <- dettools_r[dettools_r$behavior == "seqend" | dettools_r$behavior == "seqcont",]
# outcome
seqendings$outcome <- sapply(str_split(seqendings$modifiers, "[|]"), '[', 1)
# displacement
seqendings$displacement <- sapply(str_split(seqendings$modifiers, "[|]"), '[', 3)
# social attention
seqendings$socatt <- sapply(str_split(seqendings$modifiers, "[|]"), '[', 4)
# scrounging
seqendings$scrounging <- sapply(str_split(seqendings$modifiers, "[|]"), '[', 2)

# filter out seqcont ones after making sure their information is included
seqendings <- seqendings[!seqendings$outcome == "seqcont", c("sequenceID", "outcome", "displacement", "socatt", "scrounging")]

seqdat <- left_join(seqdat, seqendings, "sequenceID")

# I think that is all the sequence specific info, attach this to the main dataframe
dettools_r2 <- left_join(dettools_r, seqdat, "sequenceID")

# specify pound type
dettools_r2$poundtype <- ifelse(dettools_r2$behavior == "pound", sapply(str_split(dettools_r2$modifiers, "[|]"), '[', 1), NA)
# specify one-footed yes/no
dettools_r2$onefoot <- ifelse(dettools_r2$behavior == "pound", str_detect(dettools_r2$modifiers, "1foot"), NA)
# specify overhead yes/no
dettools_r2$overhead <- ifelse(dettools_r2$behavior == "pound", str_detect(dettools_r2$modifiers, "overhead"), NA)
# specify one-handed yes/no
dettools_r2$onehand <- ifelse(dettools_r2$behavior == "pound", str_detect(dettools_r2$modifiers, "1hand"), NA)
# specify tail-support yes/no
dettools_r2$tailsupport <- ifelse(dettools_r2$behavior == "pound", str_detect(dettools_r2$modifiers, "tailsupport"), NA)

# specify mistake type
dettools_r2$mistaketype <- ifelse(dettools_r2$behavior == "misstrike", dettools_r2$modifiers, NA)
# specify repositioning type
dettools_r2$repostype <- ifelse(dettools_r2$behavior == "reposit", dettools_r2$modifiers, NA)

# will have to do when there's a hammerswitch that hammerID changes until seq_end (use for loop for it like above)
currenthammerID <- dettools_r2$hammerID[1]

for (i in 1:nrow(dettools_r2)) {
  dettools_r2$hammerID2[i] <- currenthammerID
   if(dettools_r2$behavior[i] == "hammerswitch") {
    currenthammerID <- ifelse(sapply(str_split(dettools_r2$modifiers[i], "[|]"), '[', 2) != "None", sapply(str_split(dettools_r2$modifiers[i], "[|]"), '[', 2), dettools_r2$comment[i])
    dettools_r2$hammerID2[i] <- currenthammerID
  }

  if(dettools_r2$behavior[i] == "seqstart") {
    currenthammerID <- dettools_r2$hammerID[i]
    dettools_r2$hammerID2[i] <- currenthammerID
  }
}

# extract location of hammerstone they switched to 
dettools_r2$h_switchloc <- ifelse(dettools_r2$behavior == "hammerswitch", sapply(str_split(dettools_r2$modifiers, "[|]"), '[', 1), NA)

## incorporate what type of anvil it is
# default is stone, if it was wood we made a comment at the seqstart
seqdat$anviltype <- ifelse(seqdat$sequenceID %in% dettools_r2$sequenceID[which(dettools_r2$behavior == "seqstart" & str_detect(dettools_r2$comment, "wood") == TRUE)], "wood", "stone")
# attach this to main dataframe
dettools_r2 <- left_join(dettools_r2, seqdat[,c("sequenceID", "anviltype")], "sequenceID")

# will have to do when there's an anvilswitch that anviltype changes until seq_end (use for loop for it like above)
currentanviltype <- dettools_r2$anviltype[1]

for (i in 1:nrow(dettools_r2)) {
  dettools_r2$anviltype2[i] <- currentanviltype
  if(dettools_r2$behavior[i] == "anvilswitch") {
    currentanviltype <- dettools_r2$modifiers[i]
    dettools_r2$anviltype2[i] <- currentanviltype
  }
  
  if(dettools_r2$behavior[i] == "seqstart") {
    currentanviltype <- dettools_r2$anviltype[i]
    dettools_r2$anviltype2[i] <- currentanviltype
  }
}

## so anviltype2 and hammerID2 are the correct variables


# calculate duration 
# need to extract the start and end time of the videos
dettools_r2$videostart <- as.POSIXct(paste(sapply(str_split(dettools_r2$videoID, "__"), '[', 2), sapply(str_split(dettools_r2$videoID, "__"), '[', 3), sep = " "), tz = "America/Panama", format = "%Y-%m-%d %H-%M-%S") 
dettools_r2$videoend <- dettools_r2$videostart + 60

# make flag if it's a split sequence or not
# then calculate duration and  start/end time differently depending on whether it is a split sequence or not. 
for (i in 1:nrow(seqdat)) {
  seqdat$split[i] <- ifelse("seqcont" %in% dettools_r2$behavior[which(dettools_r2$sequenceID== seqdat$sequenceID[i])], TRUE, FALSE)
  seqdat$seqstart[i] <- dettools_r2$starttime[which(dettools_r2$sequenceID == seqdat$sequenceID[i] & dettools_r2$behavior == "seqstart")]
  
  # if sequence is within one video it is just the time of seqstart and seqend
  if( seqdat$split[i] == FALSE) { 
  seqdat$seqend[i] <- dettools_r2$starttime[which(dettools_r2$sequenceID == seqdat$sequenceID[i] & dettools_r2$behavior == "seqend")]
  seqdat$seqduration[i] <- seqdat$seqend[i] - seqdat$seqstart[i]
  }
  
  # if sequence is split over consecutive videos, we use the real time (the start time of the video + timestamp of seqstart and seqend) to calculate the duration
  # this also includes the time when the camera was not active but retriggering! (usually around 10-30 seconds)
  if (seqdat$split[i] == TRUE) { 
  realstart <- unique(dettools_r2$videostart[which(dettools_r2$sequenceID == seqdat$sequenceID[i] & dettools_r2$behavior == "seqstart")] + seqdat$seqstart[i])  
  realend <- unique(dettools_r2$videostart[which(dettools_r2$sequenceID == seqdat$sequenceID[i] & dettools_r2$behavior == "seqend")] + 
                        dettools_r2$starttime[which(dettools_r2$sequenceID == seqdat$sequenceID[i] & dettools_r2$behavior == "seqend")])
  seqdat$seqduration[i] <- as.numeric(difftime(realend, realstart, unit = "secs"))
  seqdat$seqend[i] <- seqdat$seqstart[i] + seqdat$seqduration[i]
  }
}

# left join again to get this sequence information in 
dettools_r2 <- left_join(dettools_r2, seqdat[, c("sequenceID", "seqduration")], "sequenceID")
# sequence duration only makes sense as a metric of efficiency if the outcome was indeed that the item was opened, so will need to filter on that when doing these analyses (via the outcome variable)

# nr of pounds
poundsonly <- dettools_r2[dettools_r2$behavior == "pound",]
nr_pounds <- poundsonly %>%
  dplyr::count(sequenceID)
colnames(nr_pounds) <- c("sequenceID", "n_pounds")

dettools_r2 <- left_join(dettools_r2, nr_pounds, "sequenceID")

# nr of misstrikes
missonly <- dettools_r2[dettools_r2$behavior == "misstrike",]
nr_miss <- missonly %>%
  dplyr::count(sequenceID)
colnames(nr_miss) <- c("sequenceID", "n_miss")

dettools_r2 <- left_join(dettools_r2, nr_miss, "sequenceID")
dettools_r2$n_miss[which(is.na(dettools_r2$n_miss) == TRUE)] <- 0

# nr of repositions
reponly <- dettools_r2[dettools_r2$behavior == "reposit",]
nr_repos <- reponly %>%
  dplyr::count(sequenceID)
colnames(nr_repos) <- c("sequenceID", "n_reposit")

dettools_r2 <- left_join(dettools_r2, nr_repos, "sequenceID")
dettools_r2$n_reposit[which(is.na(dettools_r2$n_reposit) == TRUE)] <- 0

## add age/sex to IDS
# load in file with capuchin age and sexes. KEEP THIS UP TO DATE
capID <- read.csv("detailedtools/capuchinIDs.csv", sep = ",")
dettools_r2 <- left_join(dettools_r2, capID, by = c("subjectID" = "ID"))

# fill in age sex for unidentified individuals
dettools_r2$Age[which(is.na(dettools_r2$Age) == TRUE)] <- ifelse(str_detect(dettools_r2$subjectID[which(is.na(dettools_r2$Age) == TRUE)], "juvenile") == TRUE, "Juvenile", 
                                                                 ifelse(str_detect(dettools_r2$subjectID[which(is.na(dettools_r2$Age) == TRUE)], "adult") == TRUE, "Adult",
                                                                 ifelse(str_detect(dettools_r2$subjectID[which(is.na(dettools_r2$Age) == TRUE)], "subadult") == TRUE, "Subadult", "Unknown"))) 
dettools_r2$Sex[which(is.na(dettools_r2$Sex) == TRUE)] <- ifelse(str_detect(dettools_r2$subjectID[which(is.na(dettools_r2$Sex) == TRUE)], "male") == TRUE, "Male", 
                                                                 ifelse(str_detect(dettools_r2$subjectID[which(is.na(dettools_r2$Sex) == TRUE)], "female") == TRUE, "Female", "Unknown")) 
# have some extra  spaces that snuck in
dettools_r2$Age <- str_trim(dettools_r2$Age)
dettools_r2$hammerID2 <- str_trim(dettools_r2$hammerID2)


# sequence level dataframe (only for analyzing things like nr of pounds/duration. things that are fixed per sequence)
detseq <- dettools_r2[!duplicated(dettools_r2$sequenceID),]
detseq$split <- seqdat$split[which(detseq$sequenceID == seqdat$sequenceID)]

#saveRDS(detseq, "detailedtools/RDS/detseq.rds")

####### INITIAL ANALYSES/MESSING AROUND ########
## move this to a different script later

### IF SEQUENCE IS SPLIT then we can "reliably" still look at the duration of the sequence in seconds (tho likely overestimation?)
# but to be safe probably shouldnt include these when looking at number of pounds as we def miss at least 1 maybe more pounds in between
# maybe exclude them in general if we have enough data, think about it
# definitely include whether it was split yes/no as a random effect/fixed effect 

## add itemtype as well 

# Age differences in efficiency (duration of sequence)
# filter to only opened sequences
detseq_o <- detseq[detseq$outcome == "opened",]

boxplot(detseq_o$seqduration ~ detseq_o$Age)

# age as ordered factor?
detseq_o$age_of <- factor(detseq_o$Age, ordered = TRUE, levels = c("Juvenile", "Subadult", "Adult"))

ggplot(detseq_o, aes(x=age_of, y=seqduration)) + 
  geom_violin()

descdist(detseq_o$seqduration)

testdist1.1 <- fitdist(detseq_o$seqduration, "norm")
plot(testdist1.1)

testdist1.2 <- fitdist(detseq_o$seqduration, "gamma")
plot(testdist1.2)


m_e1 <- brm(seqduration ~ Age + item + split, data = detseq_o, iter = 1000, chain = 2, core = 2, backend = "cmdstanr", family = "gamma")
#saveRDS(m_e1, "detailedtools/RDS/m_e1.rds")
summary(m_e1)
mcmc_plot(m_e1)
pp_check(m_e1)
plot(conditional_effects(m_e1))

# make violin plot
m_type_pred <- m_e1 %>% 
  epred_draws(newdata = tibble(item = detseq_o$item,
                               Age = detseq_o$Age,
                               split = detseq_o$split))

# age difference in duration to open item
ggplot(data = m_type_pred, aes(x = Age, y = .epred)) + geom_violin(aes(color = Age, fill = Age), alpha = 0.4) + ylim(0,50) +
  stat_summary(detseq_o, inherit.aes = FALSE, mapping=aes(x = Age, y = seqduration, color = Age), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Age", y = "Seconds required to open item") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 

# item difference in duration to open item
ggplot(data = m_type_pred, aes(x = item, y = .epred)) + geom_violin(aes(color = item, fill = item), alpha = 0.4) + ylim(0,100) +
  stat_summary(detseq_o, inherit.aes = FALSE, mapping=aes(x = item, y = seqduration, color = item), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Item type", y = "Seconds required to open item") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 

## including interaction of age and item (once we have more reliable data for each item type)
#m_e1b <- brm(seqduration ~ age_of + item + age_of*item + split, data = detseq_o, iter = 1000, chain = 2, core = 2, backend = "cmdstanr", family = "gamma")
plot(conditional_effects(m_e1b))


## Age differences in efficiency (number of pounds)
boxplot(detseq_o$n_pounds ~ detseq_o$Age)
ggplot(detseq_o, aes(x=Age, y=n_pounds)) + 
  geom_violin()

descdist(detseq_o$n_pounds)

testdist2.1 <- fitdist(detseq_o$n_pounds, "pois")
plot(testdist2.1)

# for now exclude split sequences as we likely missed pounds off camera
m_e2 <- brm(n_pounds ~ Age + item, data = detseq_o[which(detseq_o$split == FALSE),], family = "poisson", iter = 1000, chain = 2, core = 2, backend = "cmdstanr")
#saveRDS(m_e2, "detailedtools/RDS/m_e2.rds")
summary(m_e2)
pp_check(m_e2)
mcmc_plot(m_e2)
plot(conditional_effects(m_e2))

# make violin plot
m_type_pred2 <- m_e2 %>% 
  epred_draws(newdata = tibble(item = detseq_o$item,
                               Age = detseq_o$Age))

# age difference in duration to open item
ggplot(data = m_type_pred2, aes(x = Age, y = .epred)) + geom_violin(aes(color = Age, fill = Age), alpha = 0.4) +
  stat_summary(detseq_o, inherit.aes = FALSE, mapping=aes(x = Age, y = n_pounds, color = Age), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Age", y = "Number of pounds required to open item") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 

# item difference in duration to open item
ggplot(data = m_type_pred2, aes(x = item, y = .epred)) + geom_violin(aes(color = item, fill = item), alpha = 0.4) +
  stat_summary(detseq_o, inherit.aes = FALSE, mapping=aes(x = item, y = n_pounds, color = item), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Item type", y = "Number of pounds required to open item") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 

### FROM HERE ON OUT NEED TO CHANGE TO NOT USING DETSEQ_O BUT JUST DETSEQ. 
# doesnt need to just be opened sequences but can do all?
range(detseq_o$n_pounds[which(str_detect(detseq_o$item, "almendra") == TRUE & detseq_o$Age == "Juvenile")])
mean(detseq_o$n_pounds[which(str_detect(detseq_o$item, "almendra") == TRUE & detseq_o$Age == "Juvenile")])


## Age differences in efficiency (number of mistakes)
boxplot(detseq_o$n_miss ~ detseq_o$Age)
ggplot(detseq_o, aes(x=Age, y=n_miss)) + 
  geom_violin()

descdist(detseq_o$n_miss)
testdist3.1 <- fitdist(detseq_o$n_miss, "pois")
plot(testdist3.1)


m_e3 <- brm(n_miss ~ Age, data = detseq_o, family = "poisson", iter = 2000, chain = 2, core = 2, backend = "cmdstanr", control = list(adapt_delta = 0.99))
#saveRDS(m_e3, "detailedtools/RDS/m_e3.rds")
summary(m_e3)
pp_check(m_e3)
plot(conditional_effects(m_e3))

# make violin plot
m_type_pred3 <- m_e3 %>% 
  epred_draws(newdata = tibble(Age = detseq_o$Age))

# age difference in duration to open item
ggplot(data = m_type_pred3, aes(x = Age, y = .epred)) + geom_boxplot(aes(color = Age, fill = Age), alpha = 0.4) +
  stat_summary(detseq_o, inherit.aes = FALSE, mapping=aes(x = Age, y = n_miss, color = Age), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Age", y = "Average number of mistakes per tool use sequence") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 

# individual variation in how many mistakes are made
knownids <- data.frame(ID = c("TER", "SPT", "BAL", "TOM", "PEA", "SMG", "ZIM", "MIC", "LAR"))
for (i in 1:nrow(knownids)) {
  knownids$nrow[i] <- nrow(detseq_o[which(detseq_o$subjectID == knownids$ID[i]),])
}

detseq_o2 <- left_join(detseq_o[which(detseq_o$subjectID %in% knownids$ID),], knownids, by = c("subjectID" = "ID"))

ggplot(detseq_o2, aes(x=subjectID, y=n_miss, color = Age, fill = Age)) + 
  geom_violin(alpha = 0.4) + geom_text(aes(y = 7.5, x = subjectID, label = nrow)) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  labs(x = "Age", y = "Average number of mistakes per tool use sequence") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 

## Age differences in efficiency (number of repositions)
boxplot(detseq_o$n_reposit ~ detseq_o$Age)
ggplot(detseq_o, aes(x=Age, y=n_reposit)) + 
  geom_violin()


m_e4 <- brm(n_reposit ~ Age, data = detseq_o, family = "poisson", iter = 2000, chain = 2, core = 2, backend = "cmdstanr")
#saveRDS(m_e4, "detailedtools/RDS/m_e4.rds")
summary(m_e4)
mcmc_plot(m_e4)
pp_check(m_e4)
plot(conditional_effects(m_e4b))

# make violin plot
m_type_pred4 <- m_e4 %>% 
  epred_draws(newdata = tibble(Age = detseq_o$Age))

# age difference in duration to open item
ggplot(data = m_type_pred4, aes(x = Age, y = .epred)) + geom_violin(aes(color = Age, fill = Age), alpha = 0.4) +
  stat_summary(detseq_o, inherit.aes = FALSE, mapping=aes(x = Age, y = n_reposit, color = Age), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Age", y = "Average number of repositions per sequence") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 

# individual variation

ggplot(detseq_o2, aes(x=subjectID, y=n_reposit, color = Age, fill = Age)) + 
  geom_violin(alpha = 0.4) + geom_text(aes(y = 9.5, x = subjectID, label = nrow)) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  labs(x = "Age", y = "Average number of repositions per tool use sequence") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 


## still account for individual ID (when we can)


# some exploration just messing around

library(reshape2)
melt_detseq <- melt(detseq_o2, measure.vars = c("n_pounds", "n_miss", "n_reposit"))

ggplot(melt_detseq) + geom_violin(aes(y = value, x = variable, color = variable, fill = variable), alpha = 0.4) +
  stat_summary(melt_detseq, inherit.aes = FALSE, mapping=aes(x = variable, y = value, color = variable), geom = "point", fun = "mean",
               size = 4) +
  facet_wrap(~factor(subjectID, levels = c("BAL", "PEA", "TER",  "ZIM", "LAR", "MIC", "SPT", "SMG", "TOM"))) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  labs(x = "Age", y = "Average number of repositions per sequence") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 

# relationship between nr of pounds and how long the sequence lasts
ggplot(detseq_o[detseq_o$split == FALSE,], aes(y = seqduration, x = n_pounds, color = Age, shape = Age)) + geom_point(size = 3) + geom_smooth() + 
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  labs(y = "Seconds needed to open item", x = "Number of pounds needed to open item") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 
