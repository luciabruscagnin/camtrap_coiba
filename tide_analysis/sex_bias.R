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

#### H1 : Females are less terrestrial than males in general #####
## P1a: see less adult females on camera traps than adult males (and juveniles?)
## P1b: females especially being unlikely to be on the ground in open spaces such as in streams and near the coast

# Attempt one, using only sequences with capuchins in and comparing female/male ratios at the three different location types
head(agoutisequence_c)
agoutiseq_jt <- agoutisequence_c[which(agoutisequence_c$tool_site == 1 & agoutisequence_c$island == "Jicaron"),]

agoutiseq_jt$locationtype <- as.factor(ifelse(agoutiseq_jt$tool_anvil == 1, "anvil", 
                                              ifelse(agoutiseq_jt$streambed == 1, "streambed", "random")))
agoutiseq_jt$locationtype <- relevel(agoutiseq_jt$locationtype, ref = "random")

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

agoutiseq_jto$Nadults <- agoutiseq_jto$nAF + agoutiseq_jto$nAM

## stil need to set appropriate priors

s_bm1 <- brm(nAF | trials(Nadults) ~ locationtype + (1|locationfactor), data = agoutiseq_jto, family = binomial, iter = 1000, chain = 2, core = 2, backend = "cmdstanr")
#saveRDS(s_bm1, "tide_analysis/ModelRDS/s_bm1.RDS")
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
  theme_clean() +
  theme_bw(base_family = "ArcherPro Book")

## include month to account for seasonality
## gam with month by locationtype
s_gam1 <- gam(nAF/Nadults ~ s(month, by = locationtype, bs = "cc", k = 12) + s(locationfactor, bs= "re"), weights = Nadults, data = agoutiseq_jto, family = binomial())
plot(s_gam1)
require(gratia)
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
ftable(agoutiseq_jto$victim_as)
4/95 * 100 # only in 4% of the cases was a female being displaced, mostly juveniles are being displaced

ftable(agoutiseq_jto$nAF[which(agoutiseq_jto$locationtype == "anvil")]) # really hardly even see adult females at anvils

## CONCLUSION: so no, displacements at anvils are not common, and females are not often displaced. 
# in the vast majority of the cases, tool users are "alone" at the anvil
# need to make nice visualizations of this. (stacked bar graphs?)

## P2b: See female tool use (if we do at all) at opportunistic anvils e.g. in streambeds
# only have one case of female tool use so far

## P2c: Females will frequently be observed to scrounge on food opened by others
agoutiseq_jto$scrounging <- ifelse(rowSums(agoutiseq_jto[,c("sc_nAF", "sc_nAM", "sc_nAU", 
                                                            "sc_nJF","sc_nJM","sc_nJU", "sc_nSF",
                                                            "sc_nSM", "sc_nSU", "sc_nUF", "sc_nUM",
                                                            "sc_nUU")])> 0, 1, 0) 
ftable(agoutiseq_jto$sc_nAF[which(agoutiseq_jto$scrounging == 1)])
## of ~103 sequences with scrounging observed, in 8 of them were adult females present
## only saw scrounging by adult females 2x at an anvil, so atm not really worthy of a further analysis (though if we'd want to do one, see method below)

## consumption of anvil debris
agoutiseq_jto$anvildebris <- ifelse(rowSums(agoutiseq_jto[,c("ad_nAF", "ad_nAM", "ad_nAU", 
                                                             "ad_nJF","ad_nJM","ad_nJU", "ad_nSF",
                                                             "ad_nSM", "ad_nSU", "ad_nUF", "ad_nUM",
                                                             "ad_nUU")])> 0, 1, 0) 
ftable(agoutiseq_jto$agesexF[which(agoutiseq_jto$anvildebris == 1)])
ftable(agoutiseq_jto$ad_nAF[which(agoutiseq_jto$anvildebris == 1)])

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












