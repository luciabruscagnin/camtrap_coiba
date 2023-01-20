## Sex bias in tool use
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

# start with the agoutisequence dataframe that's cleaned and aggregated to the sequence level 
# alternatively have agoutiselect2 data that is on level of day-hour with zero's added in

## packages
require(brms)
require(emmeans)
require(lme4)
require(tidybayes)
require(modelr)
library(ggthemes)
require(dplyr)
require(ggplot2)
require(mgcv)
require(gratia)
require(reshape2)

## Questions/Hypotheses

# H1 : Females are less terrestrial than males
## P1a: see less adult females on camera traps than adult males (and juveniles?)
## P1b: females especially being unlikely to be on the ground in open spaces such as in streams and near the coast

# H2: Females are rarely observed tool-using because of within-group competition
## P2a: Displacements at anvils are common
## P2b: See female tool use (if we do at all) at opportunistic anvils e.g. in streambeds
## P2c: Females will frequently be observed to scrounge on food opened by others
## P2d: see less females at anvils because they are outcompeted?

# H3: Female capuchins may use tools on different food items than males
## P3a: If we see tool use by females, is more likely to be on snails and crabs than almendras
## P3b: Isotope analyses of hair would show sex differences in diet

# H4: Infant carrying limits tool use
## P4a: If we see females using tools, they are not carrying dorsal infants
## P4b: Juveniles of both sexes use tools equally
## Case of Joker with howlers not using tools with howler and using tools without
#???

## Analyses to do to test these predictions
## subset to only jicaron tool-site data

head(agoutisequence_c)
agoutiseq_jt <- agoutisequence_c[which(agoutisequence_c$tool_site == 1 & agoutisequence_c$island == "Jicaron"),]

agoutiseq_jt$locationtype <- as.factor(ifelse(agoutiseq_jt$tool_anvil == 1, "anvil", 
                                              ifelse(agoutiseq_jt$streambed == 1, "streambed", "random")))
agoutiseq_jt$locationtype <- relevel(agoutiseq_jt$locationtype, ref = "random")

# look at whether there is seasonality in the birthpeak. When do we mostly see adult females with infants? Maybe consider ratio
agoutiseq_jt$year <- year(agoutiseq_jt$seqday)
agoutiseq_jt$yrday <- yday(agoutiseq_jt$seqday)
agoutiseq_jt$week <- week(agoutiseq_jt$seqday)

ggplot(data = agoutiseq_jt, aes(x = month, y = nAF_infant/nAF, col = as.factor(year))) + stat_summary(geom =  "smooth", fun = "mean")   + theme_bw()
ggplot(data = agoutiseq_jt, aes(x = week, y = nAF_infant/nAF, col = as.factor(year))) + stat_summary(geom =  "smooth", fun = "mean") + facet_wrap(~year)  + theme_bw()


s_gam1 <- gam(nAF_infant/nAF ~ s(yrday, bs = "cc") + factor(year) + s(month, bs = "cc"), data = agoutiseq_jt)
plot(s_gam1)
summary(s_gam1)

# differentiate adult females with and without infants
agoutiseq_jt$nAF_noinfant <- agoutiseq_jt$nAF - agoutiseq_jt$nAF_infant

#### H1 : Females are less terrestrial than males in general #####
## P1a: see less adult females on camera traps than adult males (and juveniles?)
## P1b: females especially being unlikely to be on the ground in open spaces such as in streams and near the coast

# Attempt one, using only sequences with capuchins in and comparing female/male ratios at the three different location types
# only sequences with capuchins
agoutiseq_jto <- agoutiseq_jt[agoutiseq_jt$capuchin == 1,]
agoutiseq_jto <- droplevels.data.frame(agoutiseq_jto)

## still need to add in all the grids as random cameras (have way fewer random cams than other locations now)
ftable(agoutiseq_jto$locationtype)

# so when capuchins are present, what is the ratio of adult females to adult males at the three different location types?
# what can affect this ratio is: locationfactor, locationtype, season (?)

model2 <- glmer(cbind(nAM, nAF) ~ locationtype + (1|locationfactor), data = agoutiseq_jto, family = binomial)
summary(model2)
em2 <- emmeans(model2, "locationtype")
summary(em2, type = "response")

# is ratio of females with and without infants different at different locations?
model2b <- glmer(cbind(nAF_noinfant, nAF_infant) ~ locationtype + (1|locationfactor), data = agoutiseq_jto, family = binomial)
summary(model2b)
em2b <- emmeans(model2b, "locationtype")
summary(em2b, type = "response")
# no 

agoutiseq_jto$Nadults <- agoutiseq_jto$nAF + agoutiseq_jto$nAM

## stil need to set appropriate priors

s_bm1 <- brm(nAF | trials(Nadults) ~ locationtype + (1|locationfactor), data = agoutiseq_jto, family = binomial, iter = 1000, chain = 2, core = 2, backend = "cmdstanr")
#saveRDS(s_bm1, "tide_analysis/ModelRDS/s_bm1.RDS")
#s_bm1 <- readRDS("tide_analysis/ModelRDS/s_bm1.RDS")
summary(s_bm1)
mcmc_plot(s_bm1)

plot(conditional_effects(s_bm1))

m_type_pred <- s_bm1 %>% 
  epred_draws(newdata = tibble(Nadults = agoutiseq_jto$Nadults,
                               locationtype = agoutiseq_jto$locationtype,
                               locationfactor = agoutiseq_jto$locationfactor)) %>% 
  mutate(.epred_prop = .epred/Nadults) # change to proportion

ggplot(data = m_type_pred, aes(x = locationtype, y = .epred_prop)) + geom_violin(aes(color = locationtype, fill = locationtype), alpha = 0.4) + 
  stat_summary(agoutiseq_jto, inherit.aes = FALSE, mapping=aes(x = locationtype, y = nAF/Nadults, color = locationtype), geom = "point", fun = "mean",
               size = 4) + 
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "black", size = 1, alpha = 0.5) +
  scale_y_continuous(lim = c(0,1)) +
  guides(color = "none", fill = "none") +
  labs(x = "Locationtype", y = "Female:male ratio") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 


## ratio of females with infants to females without
s_bm1b <- brm(nAF_infant | trials(nAF) ~ locationtype + (1|locationfactor), data = agoutiseq_jto, family = binomial, iter = 1000, chain = 2, core = 2, backend = "cmdstanr")
#saveRDS(s_bm1b, "tide_analysis/ModelRDS/s_bm1b.RDS")
#s_bm1b <- readRDS("tide_analysis/ModelRDS/s_bm1b.RDS")
summary(s_bm1b)
mcmc_plot(s_bm1b)

plot(conditional_effects(s_bm1b))

m_type_predb <- s_bm1b %>% 
  epred_draws(newdata = tibble(nAF = agoutiseq_jto$nAF,
                               locationtype = agoutiseq_jto$locationtype,
                               locationfactor = agoutiseq_jto$locationfactor)) %>% 
  mutate(.epred_prop = .epred/nAF) # change to proportion

ggplot(data = m_type_predb, aes(x = locationtype, y = .epred_prop)) + geom_violin(aes(color = locationtype, fill = locationtype), alpha = 0.4) + 
  stat_summary(agoutiseq_jto, inherit.aes = FALSE, mapping=aes(x = locationtype, y = nAF_infant/nAF, color = locationtype), geom = "point", fun = "mean",
               size = 4) + 
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "black", size = 1, alpha = 0.5) +
  scale_y_continuous(lim = c(0,1)) +
  guides(color = "none", fill = "none") +
  labs(x = "Locationtype", y = "Ratio females with infants:females without infants") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 


## include month to account for seasonality
## gam with month by locationtype
s_gam1 <- gam(nAF/Nadults ~ s(month, by = locationtype, bs = "cc", k = 12) + s(locationfactor, bs= "re"), weights = Nadults, data = agoutiseq_jto, family = binomial())
plot(s_gam1)

plot(s_gam1)

## potentially could include distance to coast of cameras (so distcoast*locationtype)


### CONCLUSION: adult females and adult males are seen just as frequently at random cameras, but at streambeds and anvils males are much more frequent
## females are not less likely to be on the ground

#### H2: Females are rarely observed tool-using because of within-group competition ####
## P2a: Displacements at anvils are common
ftable(agoutiseq_jto$displacement[which(agoutiseq_jto$locationtype == "anvil")])
## of the 14 300 sequences at anvils, displacement only occurred in 95 of them 

ftable(agoutiseq_jto$displacement[which(agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$n > 1)])
## even if you filter to only sequences with more than 1 individual in, still see only 95 displacements on ~6300 sequences
# so no, displacements at anvils are actually rare

## additionally, if you look at who is being displaced
dis_vics <- as.data.frame(as.matrix(ftable(agoutiseq_jto$victim_as)))
dis_vics$juvenile <- dis_vics$`juvenile male` + dis_vics$`juvenile unknown`
dis_vics$subadult <- dis_vics$`subadult male`+dis_vics$`subadult unknown`

dis_vics <- melt(dis_vics[, c("adult female", "adult male", "subadult", "juvenile", "unknown")])
dis_vics$prop <- dis_vics$value / 94

cbbPalette <- c("#CC79A7",  "#56B4E9", "#E69F00", "#009E73", "#F0E442" )

ggplot(dis_vics, aes(x = variable, y = value, fill = variable)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  labs( x = "Age-sex class", y = "Number of times being displaced", fill = "Age-sex class" ) +   scale_fill_manual(values = cbbPalette) +
theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) 

4/95 * 100 # only in 4% of the cases was a female being displaced, mostly juveniles are being displaced

## just for fun, who is the aggressor
dis_agg <- as.data.frame(as.matrix(ftable(agoutiseq_jto$aggressor_as)))
dis_agg$juvenile <- dis_agg$`juvenile male` + dis_agg$`juvenile unknown`

dis_agg <- melt(dis_agg[, c("adult female", "adult male", "subadult male", "juvenile", "unknown")])
dis_agg$prop <- dis_agg$value / 94

ggplot(dis_agg, aes(x = variable, y = value, fill = variable)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  labs( x = "Age-sex class", y = "Number of times displacing others", fill = "Age-sex class" ) +   scale_fill_manual(values = cbbPalette) +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) 

ftable(agoutiseq_jto$nAF[which(agoutiseq_jto$locationtype == "anvil")]) # really hardly even see adult females at anvils

## CONCLUSION: so no, displacements at anvils are not common, and females are not often displaced. 
# in the vast majority of the cases, tool users are "alone" at the anvil

## P2b: See female tool use (if we do at all) at opportunistic anvils e.g. in streambeds
# only have one case of female tool use so far

## P2c: Females will frequently be observed to scrounge on food opened by others
agoutiseq_jto$scrounging <- ifelse(rowSums(agoutiseq_jto[,c("sc_nAF", "sc_nAM", "sc_nAU", 
                                                            "sc_nJF","sc_nJM","sc_nJU", "sc_nSF",
                                                            "sc_nSM", "sc_nSU", "sc_nUF", "sc_nUM",
                                                            "sc_nUU")])> 0 & agoutiseq_jto$n_tooluse >0, 1, 0) 

ftable(agoutiseq_jto$sc_nAF[which(agoutiseq_jto$scrounging == 1)])

## of ~103 sequences with scrounging observed, in 8 of them were adult females present
## only saw scrounging by adult females 2x at an anvil, so atm not really worthy of a further analysis (though if we'd want to do one, see method below)
agoutiseq_jto$nUnknown <- agoutiseq_jto$nUF + agoutiseq_jto$nUM + agoutiseq_jto$nUU
agoutiseq_jto$sc_nJuvenile <- agoutiseq_jto$sc_nJF + agoutiseq_jto$sc_nJM + agoutiseq_jto$sc_nJU
agoutiseq_jto$sc_nSubadult <- agoutiseq_jto$sc_nSF + agoutiseq_jto$sc_nSM + agoutiseq_jto$sc_nSU
agoutiseq_jto$sc_nUnknown <- agoutiseq_jto$sc_nUF + agoutiseq_jto$sc_nUM + agoutiseq_jto$sc_nUU

# nr of scrounging by adult females
scrounge_jto <-  colSums(agoutiseq_jto[agoutiseq_jto$scrounging == 1, c("sc_nAF", "sc_nAM", 
                                                                        "sc_nSubadult", "sc_nJuvenile", 
                                                                        "sc_nUnknown")])

scrounging <- data.frame(agesex = c("adult female", "adult male", "subadult", "juvenile", "unknown"), 
                         n_scrounge = scrounge_jto)

# add opportunity for scrounging
## nr of sequences that X was present and Y was tool using
# females that means nAF > 0 & n_tooluse >0. 
# juveniles /adults/subadults could be the tool users themselves. So it means nJuvenile >0, n_tooluse >0, and either nJuvenile >1,nadultmale >0 or nsubadult >0
scrounging$n_opp[which(scrounging$agesex == "adult female")] <- nrow(agoutiseq_jto[agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$nAF > 0 & agoutiseq_jto$n_tooluse > 0,])
scrounging$n_opp[which(scrounging$agesex == "adult male")] <- nrow(agoutiseq_jto[agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$nAM > 0 & agoutiseq_jto$n_tooluse > 0 &
                                                                                   rowSums(agoutiseq_jto[,c("nAM", "nSubadult", "nJuvenile")]) > 1,])
scrounging$n_opp[which(scrounging$agesex == "subadult")] <- nrow(agoutiseq_jto[agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$nSubadult > 0 & agoutiseq_jto$n_tooluse > 0 &
                                                                                   rowSums(agoutiseq_jto[,c("nAM", "nSubadult", "nJuvenile")]) > 1,])
scrounging$n_opp[which(scrounging$agesex == "juvenile")] <- nrow(agoutiseq_jto[agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$nJuvenile > 0 & agoutiseq_jto$n_tooluse > 0 &
                                                                                   rowSums(agoutiseq_jto[,c("nAM", "nSubadult", "nJuvenile")]) > 1,])
scrounging$n_opp[which(scrounging$agesex == "unknown")] <- nrow(agoutiseq_jto[agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$nUnknown > 0 & agoutiseq_jto$n_tooluse > 0 &
                                                                                 rowSums(agoutiseq_jto[,c("nAM", "nSubadult", "nJuvenile")]) > 1,])

scrounging$prop <- scrounging$n_scrounge / scrounging$n_opp
scrounging$agesex <- factor(scrounging$agesex, levels = c("adult female", "adult male","subadult", "juvenile", "unknown")) 

ggplot(scrounging, aes(x = agesex, y = prop, fill = agesex)) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=paste(n_scrounge, n_opp, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  labs( x = "Age-sex class", y = "Proportion of opportunities spent scrounging", fill = "Age-sex class" ) +   scale_fill_manual(values = cbbPalette) +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) 


## right now it is number of scroungers / nr of sequences thye could have scrounged in. 
## i think we dont often have multiple scroungers per sequence, but neater way to do this would be nr of sequences with scrounging by this age sex class/ sequences with opportunity
## or alternatively, nr of scroungers/ nr of this age sex class that could have been scroungers 
#(so colsums of the subset above I think, though then how do you rule out if it's 2 juveniles and one is tool using not to count them both?)
# this is imperfect but the error is minor so eh. All preliminary. 


## consumption of anvil debris
agoutiseq_jto$anvildebris <- ifelse(rowSums(agoutiseq_jto[,c("ad_nAF", "ad_nAM", "ad_nAU", 
                                                             "ad_nJF","ad_nJM","ad_nJU", "ad_nSF",
                                                             "ad_nSM", "ad_nSU", "ad_nUF", "ad_nUM",
                                                             "ad_nUU")])> 0, 1, 0) 

# nr of consumption of anvil debris
agoutiseq_jto$ad_nJuvenile <- agoutiseq_jto$ad_nJF + agoutiseq_jto$ad_nJM + agoutiseq_jto$ad_nJU
agoutiseq_jto$ad_nSubadult <- agoutiseq_jto$ad_nSF + agoutiseq_jto$ad_nSM + agoutiseq_jto$ad_nSU
agoutiseq_jto$ad_nUnknown <- agoutiseq_jto$ad_nUF + agoutiseq_jto$ad_nUM + agoutiseq_jto$ad_nUU

anvildebris_jto <-  colSums(agoutiseq_jto[agoutiseq_jto$anvildebris == 1, c("ad_nAF", "ad_nAM", 
                                                                        "ad_nSubadult", "ad_nJuvenile", 
                                                                        "ad_nUnknown")])

# add opportunity for consuming anvil debris (total nr of this age/sex class observed)
opp_jto <-  colSums(agoutiseq_jto[agoutiseq_jto$locationtype == "anvil", c("nAF", "nAM", 
                                                                           "nSubadult", "nJuvenile", 
                                                                           "nUnknown")])

anvildebris <- data.frame(agesex = c("adult female", "adult male", "subadult", "juvenile", "unknown"), 
                         n_debris = anvildebris_jto, n_opp = opp_jto)

anvildebris$prop <- anvildebris$n_debris / anvildebris$n_opp
anvildebris$agesex <- factor(anvildebris$agesex, levels = c("adult female", "adult male","subadult", "juvenile", "unknown")) 

ggplot(anvildebris, aes(x = agesex, y = prop, fill = agesex)) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=paste(n_debris, n_opp, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  labs( x = "Age-sex class", y = "Proportion of opportunities spent consuming anvil debris", fill = "Age-sex class" ) +   scale_fill_manual(values = cbbPalette) +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) 



# of the 1180 sequences with foraging on anvil debris, 48 times were by adult females

# can go to long format to have n of anvil debris foragers and then agesex factor
library(reshape2)

agoutiseq_jto$ad_nJuvenile <- agoutiseq_jto$ad_nJF + agoutiseq_jto$ad_nJM + agoutiseq_jto$ad_nJU

## make it proportions of the age/sex class present that is scrounging
# so different question, asks: If adult females are present at an avil, how many are consuming anvil debris
# and how does this proportion compare to other age/sex classes
agoutiseq_jto$ad_pAF <- agoutiseq_jto$ad_nAF/agoutiseq_jto$nAF
agoutiseq_jto$ad_pAM <- agoutiseq_jto$ad_nAM/agoutiseq_jto$nAM
agoutiseq_jto$ad_pJuvenile <- agoutiseq_jto$ad_nJuvenile/agoutiseq_jto$nJuvenile

ftable(agoutiseq_jto$ad_pAF)
ftable(agoutiseq_jto$ad_pAM)
ftable(agoutiseq_jto$ad_pJuvenile)

long_debris <- melt(agoutiseq_jto[agoutiseq_jto$locationtype == "anvil",], measure.vars = c("ad_pAF", "ad_pAM", "ad_pJuvenile"))
long_debris$value[which(long_debris$value >1)] <- 1
long_debris$agesex2 <- as.factor(long_debris$variable)

ggplot(long_debris) + geom_violin(aes(x = agesex2, y = value))

model3 <- glm(value ~ agesex2, data = long_debris, family = binomial())
summary(model3)
em3 <- emmeans(model3, "agesex2")
summary(em3, type = "response")

# bad analysis, but conclusion is that  females are LESS likely to be scrounging on anvil debris than other age sex classes, when they are present

## MAKE BETTER MODELS AND VISUALIZATIONS OF THIS
# maybe number of sequences foraging on anvil debris/number of sequences present? so simple dataframe with 
# number of sequences foraging on anvil debris and number of sequences present, factor is agesex class


## P2d: see less females at anvils because they are outcompeted?

### Scrounging rates of adult females compared to juveniles and adult males (at tool use anvils)

### Anvil debris consumption of adult females compared to juveniles and adult males

### Displacement rates at anvils and who is being displaced


#### H3: Females have different diet than males ####

# use dataset per day with 0's in
head(agoutiselect2)
agoutiselect2_jt <- agoutiselect2[agoutiselect2$island == "Jicaron" & agoutiselect2$tool_site == 1,]










