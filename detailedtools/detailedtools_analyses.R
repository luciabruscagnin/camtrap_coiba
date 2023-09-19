## Detailed tool use analyses -- Analyses
## MPI-AB; Z Goldsborough

## Analysing efficiency, technique and selectivity in tool use

## packages needed
library(stringr)
library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)
library(tidybayes)
library(fitdistrplus)

## NOTES ###
# If we have enough data, ideally exclude sequences that are split across multiple videos (split == TRUE)
# Similarly, if we have enough data with identified individuals, only work on that dataset so individual can be a random effect

### Load datasets, cleaned in detailedtools.R script ####
#detseq <- readRDS("detailedtools/RDS/detseq.rds")
#dettools_r2 <- readRDS("detailedtools/RDS/dettools_r2.rds")

# for most analyses, will only look at sequences that were successful (so outcome = opened)
# also filter the items to only the most abundant ones (for now)
ftable(detseq$item)
detseq_o <- detseq[detseq$outcome == "opened" & detseq$item %in% c("almendrabrown", "almendragreen", "almendraunknown", "almendrared"),]

### Diagnostics #### 

# first let's examine some general things, and see whether we have good inter-rater agreement and how much data we have
ggplot(detseq_o, aes(x=coder, y=n_pounds)) + 
  geom_violin()

# now looking only at EXP-ANV-01-R11 as that is the only one fully coded
# later could look at all sites/times
# for this plot exclude really rare items
ggplot(detseq[detseq$deployment == "R11" & detseq$location == "EXP-ANV-01" & 
                detseq$item %in% c("almendrabrown", "almendragreen", "almendraunknown", "almendrared"),], 
       aes(x = mediadate, fill = item)) + geom_histogram() + theme_bw() + facet_wrap(~item)

### Efficiency ####
## Comparing efficiency between age classes on opened sequences, multiple measures
# 1. Sequence duration (seconds) 
# 2. Number of pounds to open item 
# 3. Number of misstrikes
# 4. Number of repositions

## Other things that likely affect these variables are:
# The specific item being processed
# The type of anvil (wood or stone)
# The hammerstone (when we have reliable IDs, include hammerstone ID as random effect)
# Identity of tool user (when we only use subset with identified individuals, random effect)
# Whether sequence is split across two videos or not (with pounds miss some, durations are longer)


### IF SEQUENCE IS SPLIT then we can "reliably" still look at the duration of the sequence in seconds (though likely overestimation?)
# but to be safe probably shouldnt include these when looking at number of pounds as we miss at least 1 (maybe more) pounds in between
# maybe exclude them in general if we have enough data, think about it
# definitely include whether it was split yes/no as a random effect/fixed effect 

nrow(detseq_o)
# so at the moment have 1341 sequences
# filter to the best of the best (where individuals are known), of course should still check as many unidentified ones as possible
# also exclude splits
detseq_oi <- detseq_o[!detseq_o$subjectID %in% c("adultmale", "subadultmale", "juvenileunknown") & detseq_o$split == FALSE,]
# then end up with 1199 sequences

#### 1. Sequence duration #####

## determine distribution, what family is best?
descdist(detseq_oi$seqduration)
hist(detseq_oi$seqduration)

testdist1.1 <- fitdist(detseq_oi$seqduration, "norm")
plot(testdist1.1)

testdist1.2 <- fitdist(detseq_oi$seqduration, "gamma")
plot(testdist1.2)

# Gamma appears best (for now)

## Model 1: Duration depending on age, including item, anviltype, and individual ID as random effect
m_e1 <- brm(seqduration ~ Age + item*anviltype + (1|subjectID), data = detseq_oi, iter = 1000, chain = 2, core = 2, backend = "cmdstanr", family = "gamma")
# saving model if you dont want to have to run it again
#saveRDS(m_e1, "detailedtools/RDS/m_e1.rds")
#m_e1 <- readRDS("detailedtools/RDS/m_e1.rds")

# diagnostics
summary(m_e1)
mcmc_plot(m_e1)
pp_check(m_e1)
plot(conditional_effects(m_e1))

# visualize
# make violin plot
m_type_pred <- m_e1 %>% 
  epred_draws(newdata = tibble(item = detseq_oi$item,
                               Age = detseq_oi$Age,
                               anviltype = detseq_oi$anviltype,
                               subjectID = detseq_oi$subjectID))

# age difference in duration to open item
ggplot(data = m_type_pred, aes(x = Age, y = .epred)) + geom_violin(aes(color = Age, fill = Age), alpha = 0.4) + ylim(0,50) +
  stat_summary(detseq_o, inherit.aes = FALSE, mapping=aes(x = Age, y = seqduration, color = Age), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Age", y = "Seconds required to open item") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) + facet_wrap(~anviltype)

# item difference in duration to open item
ggplot(data = m_type_pred, aes(x = item, y = .epred)) + geom_violin(aes(color = item, fill = item), alpha = 0.4) + ylim(0,100) +
  stat_summary(detseq_o, inherit.aes = FALSE, mapping=aes(x = item, y = seqduration, color = item), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Item type", y = "Seconds required to open item") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) + facet_wrap(~anviltype)

#### 2. Number of pounds #####
descdist(detseq_oi$n_pounds)

testdist2.1 <- fitdist(detseq_oi$n_pounds, "pois")
plot(testdist2.1)

# Model 2: Number of pounds depending on age, including item, anviltype, and individual ID as random effect
m_e2 <- brm(n_pounds ~ Age + item*anviltype + (1|subjectID), data = detseq_oi, family = "poisson", iter = 1000, chain = 2, core = 2, backend = "cmdstanr")
# save and load model
# saveRDS(m_e2, "detailedtools/RDS/m_e2.rds")
# m_e2 <- readRDS("detailedtools/RDS/m_e2.rds")

# diagnostics
summary(m_e2)
pp_check(m_e2)
mcmc_plot(m_e2)
plot(conditional_effects(m_e2))

# to test hypotheses
hypothesis(m_e2, "Intercept  > Intercept + anviltypewood", alpha = 0.05)
hypothesis(m_e2, "Intercept  < Intercept + AgeJuvenile", alpha = 0.05)

# visualizing
# make violin plot
m_type_pred2 <- m_e2 %>% 
  epred_draws(newdata = tibble(item = detseq_oi$item,
                               Age = detseq_oi$Age,
                               anviltype = detseq_oi$anviltype,
                               subjectID = detseq_oi$subjectID))

# age difference in number of pounds to open item
ggplot(data = m_type_pred2, aes(x = Age, y = .epred)) + geom_violin(aes(color = Age, fill = Age), alpha = 0.4) +
  stat_summary(detseq_o, inherit.aes = FALSE, mapping=aes(x = Age, y = n_pounds, color = Age), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Age", y = "Number of pounds required to open item") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) + facet_wrap(~anviltype)

# item difference in number of pounds to open item
ggplot(data = m_type_pred2, aes(x = item, y = .epred)) + geom_violin(aes(color = item, fill = item), alpha = 0.4) +
  stat_summary(detseq_o[which(detseq_o$item %in% c("almendrabrown", "almendragreen")),], inherit.aes = FALSE, mapping=aes(x = item, y = n_pounds, color = item), geom = "point", fun = "mean",
               size = 4) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Item type", y = "Number of pounds required to open item") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 

#### 3. Number of mistakes #####
descdist(detseq_oi$n_miss)
testdist3.1 <- fitdist(detseq_oi$n_miss, "pois")
plot(testdist3.1)

# Model 3: Number of misstrikes depending on age, item, anviltype and subjectID as random effect
m_e3 <- brm(n_miss ~ Age + item*anviltype + (1|subjectID), data = detseq_o, family = "poisson", iter = 1000, chain = 2, core = 2, backend = "cmdstanr", control = list(adapt_delta = 0.99))
# saving and loading model
# saveRDS(m_e3, "detailedtools/RDS/m_e3.rds")
# m_e3 <- readRDS("detailedtools/RDS/m_e3.rds")

# diagnostics
summary(m_e3)
mcmc_plot(m_e3)
pp_check(m_e3)
plot(conditional_effects(m_e3))

# make violin plot
m_type_pred3 <- m_e3 %>% 
  epred_draws(newdata = tibble(Age = detseq_oi$Age,
                               item = detseq_oi$item,
                               anviltype = detseq_oi$anviltype,
                               subjectID = detseq_oi$subjectID))

# age difference in misstrikes
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
# only take individuals with enough sequences observed
ftable(detseq_oi$subjectID)
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

#### 4. Number of repositions #####

# Model 4: Number of repositions depending on age, item, anviltype and subject ID as random effect
m_e4 <- brm(n_reposit ~ Age + item*anviltype + (1|subjectID), data = detseq_oi, family = "poisson", iter = 1000, chain = 2, core = 2, backend = "cmdstanr")
# saving and loading model
# saveRDS(m_e4, "detailedtools/RDS/m_e4.rds")
# readRDS("detailedtools/RDS/m_e4.rds")

# diagnostics
summary(m_e4)
mcmc_plot(m_e4)
pp_check(m_e4)
plot(conditional_effects(m_e4))

# make violin plot
m_type_pred4 <- m_e4 %>% 
  epred_draws(newdata = tibble(Age = detseq_oi$Age,
                               item = detseq_oi$item,
                               anviltype = detseq_oi$anviltype,
                               subjectID = detseq_oi$subjectID))

# age difference in number of repositions
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


## FROM HERE ON OUT DID NOT CLEAN YET ### 

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

# improvement over time?
head(detseq_o2)
ggplot(detseq_o2[!detseq_o2$subjectID == "JOE" & detseq_o2$location == "EXP-ANV-01",]) + geom_smooth(aes(x = videostart, y = n_miss, color = "n_miss")) + geom_smooth(aes(x = videostart, y = n_pounds, color = "n_pounds")) + 
  geom_smooth(aes(x = videostart, y = n_reposit,  color = "n_repositions"))  + facet_wrap(~subjectID, scales = "free")  + theme_bw() + scale_color_manual("", breaks = c("n_miss", "n_pounds", "n_repositions"),
                                                                                                                                                          values = c("red", "blue", "green"))
# would probably have to be some kind of GAM, that also includes other things that affects these things (item, anviltype)
# either work with actual number or do maybe a julian day or something? and then split by ID? 

ggplot(detseq_o2[detseq_o2$split == FALSE,]) + geom_smooth(aes(x = videostart, y = seqduration)) + facet_wrap(~subjectID, scales = "free") + theme_bw() 
ftable(detseq_o2$seqduration)


#### HAMMER TIME #####

ggplot(detseq_o, aes(x=coder, y=n_pounds)) + 
  geom_violin()

library(stringr)
ftable(detseq$location[which(detseq$item %in% c("almendrabrown", "almendragreen", "almendraunknown", "almendrared"))])
