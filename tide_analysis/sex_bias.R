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
require(tidyverse)
require(marginaleffects)
require(broom.mixed)
require(janitor)
require(easystats)

## Questions/Hypotheses

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

### Preparation #####
## Create two datasets:
# 1. subset to only Jicaron tool-site data
agoutiseq_jt <- agoutisequence_c[which(agoutisequence_c$tool_site == 1 & agoutisequence_c$island == "Jicaron"),]

# exclude days on which cameras were deployed or picked up (to take away that bias)
# since not all cameras ran until pick up, need to extract the actual days we were there
picksetupdays2 <- unique(agoutiseq_jt$seqday[agoutiseq_jt$cameraSetup ==  "True"])
agoutiseq_jt$picksetup <- ifelse(agoutiseq_jt$seqday %in% picksetupdays2 , 1, 0)
# did not do this now because then we only have 80 displacements vs 94. discuss what we want
#agoutiseq_jt <- agoutiseq_jt[(agoutiseq_jt$picksetup == 0),]

# 2. subset to only Coiba tool-site data
agoutiseq_ct <- agoutisequence_c[which(agoutisequence_c$tool_site == 1 & agoutisequence_c$island == "Coiba"),]
# exclude pick/set up days
picksetupdays3 <- unique(agoutiseq_ct$seqday[which(agoutiseq_ct$cameraSetup ==  "True")] )
agoutiseq_ct$picksetup <- ifelse(agoutiseq_ct$seqday %in% picksetupdays3 , 1, 0)
#agoutiseq_ct <- agoutiseq_ct[(agoutiseq_ct$picksetup == 0),]

### H1: Females are physically incapable of tool use ####

# female tool use events on Jicaron
tooluse <- agoutiseq_jt[which(agoutiseq_jt$tooluse == "TRUE"),]
sum(tooluse$tu_nAM)
sum(tooluse$tu_nJuvenile)
sum(tooluse$tu_nSubadult)

# tool use events on Coiba
tooluse_c <- agoutiseq_ct[which(agoutiseq_ct$tooluse == "TRUE"),]
sum(tooluse_c$tu_nAM)
sum(tooluse_c$tu_nJuvenile)
sum(tooluse_c$tu_nSubadult)


### H2 : Females are less terrestrial than males ####

## Jicaron prep
# Use only sequences with capuchins in and comparing female/male ratios at the three different location types
agoutiseq_jt$locationtype <- as.factor(ifelse(agoutiseq_jt$tool_anvil == 1, "anvil", 
                                              ifelse(agoutiseq_jt$streambed == 1, "streambed", "random")))
agoutiseq_jt$locationtype <- relevel(agoutiseq_jt$locationtype, ref = "random")
# differentiate adult females with and without infants
agoutiseq_jt$nAF_noinfant <- agoutiseq_jt$nAF - agoutiseq_jt$nAF_infant
agoutiseq_jt$Nadults <- agoutiseq_jt$nAF + agoutiseq_jt$nAM

# only sequences with adult capuchins and excluding when we didnt sex any of the adults
agoutiseq_jto <- agoutiseq_jt[agoutiseq_jt$capuchin == 1,]
agoutiseq_jto1 <- agoutiseq_jt[agoutiseq_jt$Nadults > 0 & (agoutiseq_jt$nAF >0 | agoutiseq_jt$nAM >0),] # CHECK do we want this or also include sequences with capuchins, without adults?
agoutiseq_jto1 <- droplevels.data.frame(agoutiseq_jto1)

## add distance to coast (dist2coast.R script for getting distance per camera)
# only available for Jicaron
dist2coast_all <- read.csv("tide_analysis/allcams_gps.csv", header = TRUE)
dist2coast_all <- dist2coast_all[ , c(2,5)]

agoutiseq_jto1 <- left_join(agoutiseq_jto1, dist2coast_all, by = c("locationfactor" = "camera_id"))
# standardize distance to coast
agoutiseq_jto1$distcoast_z <- scale(agoutiseq_jto1$distcoast, center = TRUE, scale = TRUE)

# so when capuchins are present, what is the ratio of adult females to adult males at the three different location types?
# what can affect this ratio is: locationfactor, locationtype, distance from coast

## Coiba prep
# Use only sequences with capuchins in and comparing female/male ratios at the three different location types
# differentiate adult females with and without infants
agoutiseq_ct$nAF_noinfant <- agoutiseq_ct$nAF - agoutiseq_ct$nAF_infant
agoutiseq_ct$Nadults <- agoutiseq_ct$nAF + agoutiseq_ct$nAM

# only sequences with adult capuchins
# also dropping sequences that only contain unsexed adults (so no adults recognized as males or females) 
# could potentially do imputation of missing data https://github.com/rmcelreath/rethinking see here about discrete imputation
agoutiseq_cto <- agoutiseq_ct[agoutiseq_ct$capuchin == 1,]
agoutiseq_cto1 <- agoutiseq_ct[agoutiseq_ct$Nadults > 0 & (agoutiseq_ct$nAF >0 | agoutiseq_ct$nAM >0),] 
agoutiseq_cto1 <- droplevels.data.frame(agoutiseq_cto1)

## need to set appropriate priors
sexbias_prior <- c(prior(normal(0, 1), class = Intercept),
                   prior(normal(0,1), class = b),
                   prior(exponential(1), class = sd))

# prior predictive simulation
sbm1_prior <- brm(nAF | trials(Nadults) ~ locationtype*distcoast_z +  (1|locationfactor), data = agoutiseq_jto1, family = binomial, 
                  prior = sexbias_prior, sample_prior = "only", iter = 1000, chain = 2, core = 2, backend = "cmdstanr")

summary(sbm1_prior)
prior_summary(sbm1_prior)
mcmc_plot(sbm1_prior)
plot(sbm1_prior)


### P1a: see less adult females on camera traps than adult males 
### P1b: females are especially unlikely to be on the ground in open spaces such as in streams and near the coast 

#### Model 1: on Jicaron, comparing female to male sex ratio at different locationtypes #######
s_bm1 <- brm(nAF | trials(Nadults) ~ locationtype*distcoast +  (1|locationfactor), data = agoutiseq_jto1, family = binomial, 
             prior = sexbias_prior, iter = 3000, chain = 3, core = 3, backend = "cmdstanr", save_pars = save_pars(all = TRUE))
# s_bm1 <- add_criterion(s_bm1, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", ndraws = 3000) 
# saveRDS(s_bm1, "tide_analysis/ModelRDS/s_bm1.RDS")
# s_bm1 <- readRDS("tide_analysis/ModelRDS/s_bm1.RDS")
summary(s_bm1)
mcmc_plot(s_bm1)

## Checks
pp_check(s_bm1, ndraw = 100) 
loo(s_bm1)
loo_R2(s_bm1)
round(bayes_R2(s_bm1),2)

plot(s_bm1)
plot(conditional_effects(s_bm1, re_formula = NULL)) # re_formula = NULL to include random effects

hypothesis(s_bm1, "Intercept > Intercept + locationtypeanvil", alpha = 0.05)
hypothesis(s_bm1, "Intercept > Intercept + locationtypestreambed")
hypothesis(s_bm1, "Intercept +locationtypeanvil < Intercept + locationtypestreambed")

hypothesis(s_bm1, "distcoast < distcoast + locationtypeanvil:distcoast")
hypothesis(s_bm1, "distcoast < distcoast + locationtypestreambed:distcoast")
hypothesis(s_bm1, "distcoast + locationtypeanvil:distcoast < distcoast + locationtypestreambed:distcoast")

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

round(logit2prob(-0.01),2)

# to help with reporting results
report(s_bm1)

## set viridis color palette
scales::viridis_pal(end = 0.8)(3) 
cols <- c( "#440154FF", "#7AD151FF", "#2A788EFF" )

## Figure 1: violin plot of sex ratio across location types
m_type_pred <- s_bm1 %>% 
  epred_draws(newdata = tibble(Nadults = agoutiseq_jto1$Nadults,
                               locationtype = agoutiseq_jto1$locationtype,
                               locationfactor = agoutiseq_jto1$locationfactor,
                               distcoast = agoutiseq_jto1$distcoast)) %>% 
  mutate(.epred_prop = .epred/Nadults)

#png("tide_analysis/ModelRDS/s_bm1_ratios.png", width = 8, height = 7, units = 'in', res = 300)
ggplot(data = m_type_pred, aes(x = locationtype, y = .epred_prop)) + 
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "black", linewidth = 1, alpha = 0.5) +
  geom_hline(yintercept = 0.55, linetype = "dashed", color = "black", linewidth = 1, alpha = 0.5) +
  geom_violin(aes(color = locationtype, fill = locationtype), alpha = 0.4) + 
  stat_summary(agoutiseq_jto1, inherit.aes = FALSE, mapping=aes(x = locationtype, y = nAF/Nadults, color = locationtype), geom = "point", fun = "mean",
               size = 4) + 
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  scale_y_continuous(lim = c(0,1)) +
  guides(color = "none", fill = "none") +
  labs(x = "Locationtype", y = "Female:male ratio") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 
#dev.off()

### Marginal effects plots
# want to get to marginal effects (average across all cameras) instead of conditional effect (effect at average camera)
# pull out parameters that we care about
r_fit <- s_bm1 %>%
  tidy() %>%
  split(~term)

B0 <- r_fit$`(Intercept)`$estimate # average sex ratio across all sequences in random cams
B1 <- r_fit$locationtypeanvil$estimate # effect of anvil on sex ratio across all sequences
B2 <- r_fit$locationtypestreambed$estimate # effect of streambed across all sequences
B3 <- r_fit$distcoast$estimate # effect of distcoast on random cams
B4 <- r_fit$`locationtypeanvil:distcoast`$estimate # interaction anvil and distcoast
B5 <- r_fit$`locationtypestreambed:distcoast`$estimate #interaction streambed and distcoast
sigma_0 <- r_fit$`sd__(Intercept)`$estimate # between-camera variability of sex ratio 

# visualize camera-specific offsets 
#png("tide_analysis/ModelRDS/s_bm1_cameras.png", width = 8, height = 7, units = 'in', res = 300)
s_bm1 %>%
  linpred_draws(tibble(locationfactor = agoutiseq_jto1$locationfactor,
                       locationtype = agoutiseq_jto1$locationtype,
                       Nadults = agoutiseq_jto1$Nadults,
                       distcoast = agoutiseq_jto1$distcoast)) %>%
  ungroup() %>% 
  mutate(locationfactor = fct_reorder(factor(locationfactor), .linpred, .fun = mean)) %>%
  ggplot(aes(x = logit2prob(.linpred), y = locationfactor, color = locationtype)) +
  scale_color_manual(values = cols) +
  geom_vline(xintercept = logit2prob(B0), color = cols[1], linewidth = 1) +
  geom_vline(xintercept = logit2prob(B0+B1), color = cols[2], linewidth = 1) +
  geom_vline(xintercept = logit2prob(B0+B2), color = cols[3], linewidth = 1) +
  stat_pointinterval() +
  theme_bw() +
  labs(x = "Model estimation of female:male ratio", y = "Camera location", color = "Location type") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
#dev.off()

## use marginal effects to marginalize across existing random effects
marginal_preds <- predictions(
  s_bm1,
  newdata = datagrid(locationtype = c("random", "anvil", "streambed"),
                     locationfactor = unique),
  by = "locationtype",
  re_formula = NULL
) %>% 
  posteriordraws()

## this is the first violin plot but then differently
p_marginal_preds <- marginal_preds %>% 
  ggplot(aes(x = draw, fill = locationtype)) +
  stat_halfeye(alpha = 0.5) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values= cols) +
  labs(x = "Sex ratio female to male", y = "Density", fill = "Location type",
       title = "Marginal population-level means",
       subtitle = "Random effects averaged / marginalized / integrated") +
  theme_bw()
p_marginal_preds

## distance to coast interaction
# this is assuming each camera can be at each distance at each locationtype (so also simulating distant anvil cameras)

# random
marginal_preds2 <- predictions(
  s_bm1,
  newdata = datagrid(locationtype = "random",
                     locationfactor = unique,
                     distcoast = unique),
  by = "distcoast",
  re_formula = NULL
) %>% 
  posteriordraws()

p_random <- ggplot(data = marginal_preds2, aes(x = distcoast)) +
  geom_point(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "random"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF/Nadults),  col = cols[1], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "random"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF/Nadults), col = cols[1], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill = cols[1], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, col = cols[1]) +
  labs(subtitle="Random cameras",
       y = "Female:male ratio", x = NULL) +
  guides(colour="none", fill="none") + theme_bw() + theme(
    axis.title.y = element_text(size = 18),
    plot.subtitle = element_text(size = 18),
    axis.text = element_text(size = 16))
p_random

# anvil
marginal_preds3 <- predictions(
  s_bm1,
  newdata = datagrid(locationtype = "anvil",
                     locationfactor = unique,
                     distcoast = unique),
  by = "distcoast",
  re_formula = NULL
) %>% 
  posteriordraws()

head(marginal_preds3)

p_anvil <- ggplot(data = marginal_preds3, aes(x = distcoast)) +
  geom_point(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "anvil"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF/Nadults), colour = cols[2], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "anvil"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF/Nadults), colour = cols[2], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high)  , fill = cols[2], colour = cols[2], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, colour = cols[2]) +
  labs(subtitle="Anvil cameras",
         y=NULL, x ="Distance from coast (m)") +
  guides(colour="none", fill="none") + theme_bw() +  theme(axis.title.y=element_blank(),
                                                           axis.text.y=element_blank(),
                                                           axis.ticks.y=element_blank(),
                                                           axis.title.x = element_text(size = 18),
                                                           plot.subtitle = element_text(size = 18),
                                                           axis.text = element_text(size = 16))
p_anvil

# streambed
marginal_preds4 <- predictions(
  s_bm1,
  newdata = datagrid(locationtype = "streambed",
                     locationfactor = unique,
                     distcoast = unique),
  by = "distcoast",
  re_formula = NULL
) %>% 
  posteriordraws()

p_streambed <- ggplot(data = marginal_preds4, aes(x = distcoast)) +
  geom_point(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "streambed"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF/Nadults), colour = cols[3], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "streambed"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF/Nadults), colour = cols[3], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high)  , fill = cols[3], colour = cols[3], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, colour = cols[3]) + 
  labs(subtitle="Streambed cameras", y=NULL, x = NULL, shape = "data_") +
  theme_bw() +  theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                plot.subtitle = element_text(size = 18),
                axis.text = element_text(size = 16)) 
p_streambed

#png("tide_analysis/ModelRDS/s_bm1_interaction.png", width = 11, height = 7, units = 'in', res = 300)
plot(p_random + p_anvil + p_streambed)
#dev.off()

#### Model 1coiba: on Coiba, ratio of adult females to adult males (only streambeds) #####
s_bm1coiba <- brm(nAF | trials(Nadults) ~ 1 + (1|locationfactor), data = agoutiseq_cto1, family = binomial, 
             iter = 3000, chain = 3, core = 3, backend = "cmdstanr", save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
# s_bm1coiba <- add_criterion(s_bm1coiba, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", control = list(adapt_delta = 0.99), ndraws = 3000) 
# saveRDS(s_bm1coiba, "tide_analysis/ModelRDS/s_bm1coiba.RDS")
#s_bm1coiba <- readRDS("tide_analysis/ModelRDS/s_bm1coiba.RDS")
summary(s_bm1coiba)
mcmc_plot(s_bm1coiba)

## Checks
pp_check(s_bm1coiba, ndraw = 100) 
loo(s_bm1coiba)
loo_R2(s_bm1coiba)
round(bayes_R2(s_bm1coiba),2)

round(logit2prob(1.74),2)

plot(s_bm1coiba)

report(s_bm1coiba)

## Plot for supplements of sex ratio at each camera. 
r_fitc <- s_bm1coiba %>%
  tidy() %>%
  split(~term)

B0c <- r_fitc$`(Intercept)`$estimate # average sex ratio across all sequences 
sigma_0c <- r_fitc$`sd__(Intercept)`$estimate # between-camera variability of sex ratio 

#png("tide_analysis/ModelRDS/s_bm1coiba_cameras.png", width = 8, height = 7, units = 'in', res = 300)
s_bm1coiba %>%
  linpred_draws(tibble(locationfactor = agoutiseq_cto1$locationfactor,
                       Nadults = agoutiseq_cto1$Nadults)) %>%
  ungroup() %>% 
  mutate(locationfactor = fct_reorder(factor(locationfactor), .linpred, .fun = mean)) %>%
  ggplot(aes(x = logit2prob(.linpred), y = locationfactor)) +
  geom_vline(xintercept = logit2prob(B0c), color = cols[3], linewidth = 1) +
  stat_pointinterval(color = cols[3]) +
  theme_bw() +
  labs(x = "Model estimation of female:male ratio", y = "Camera location", color = "Location type") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
#dev.off()

#### Model 2: ratio of females with infants to females without #########
agoutiseq_jto2 <- agoutiseq_jto1[which(agoutiseq_jto1$nAF >0),]

s_bm1b <- brm(nAF_infant | trials(nAF) ~ locationtype*distcoast + (1|locationfactor), data = agoutiseq_jto2, family = binomial, 
              prior = sexbias_prior,  iter = 3000, chain = 3, core = 3, backend = "cmdstanr", save_pars = save_pars(all = TRUE))
# s_bm1b <- add_criterion(s_bm1b, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", ndraws = 3000) 
#saveRDS(s_bm1b, "tide_analysis/ModelRDS/s_bm1b.RDS")
#s_bm1b <- readRDS("tide_analysis/ModelRDS/s_bm1b.RDS")
summary(s_bm1b)
mcmc_plot(s_bm1b)

## Checks
pp_check(s_bm1b, ndraw = 100) 
loo(s_bm1b)
loo_R2(s_bm1b)
bayes_R2(s_bm1b)

round(logit2prob(-0.28),2)

plot(conditional_effects(s_bm1b))

hypothesis(s_bm1b, "Intercept > Intercept + locationtypeanvil", alpha = 0.05)
hypothesis(s_bm1b, "Intercept > Intercept + locationtypestreambed", alpha = 0.05)
hypothesis(s_bm1b, "Intercept + locationtypestreambed > Intercept + locationtypeanvil", alpha = 0.05)

report(s_bm1b)

# plots
m_type_predb <- s_bm1b %>% 
  epred_draws(newdata = tibble(nAF = agoutiseq_jto2$nAF,
                               locationtype = agoutiseq_jto2$locationtype,
                               locationfactor = agoutiseq_jto2$locationfactor,
                               distcoast = agoutiseq_jto2$distcoast)) %>% 
  mutate(.epred_prop = .epred/nAF) # change to proportion

#png("tide_analysis/ModelRDS/s_bm1b_ratios.png", width = 8, height = 7, units = 'in', res = 300)
ggplot(data = m_type_predb, aes(x = locationtype, y = .epred_prop)) + geom_violin(aes(color = locationtype, fill = locationtype), alpha = 0.4) + 
  stat_summary(agoutiseq_jto2, inherit.aes = FALSE, mapping=aes(x = locationtype, y = nAF_infant/nAF, color = locationtype), geom = "point", fun = "mean",
               size = 4) + 
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  scale_y_continuous(lim = c(0,1)) +
  guides(color = "none", fill = "none") +
  labs(x = "Locationtype", y = "Ratio females with infants:females without infants") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 
#dev.off()

# distance to coast marginal effects plot
r_fit <- s_bm1b %>%
  tidy() %>%
  split(~term)

B0_b <- r_fit$`(Intercept)`$estimate # average infant across all sequences in random cams
B1_b <- r_fit$locationtypeanvil$estimate # effect of anvil on sex ratio across all sequences
B2_b <- r_fit$locationtypestreambed$estimate # effect of streambed across all sequences
sigma_0b <- r_fit$`sd__(Intercept)`$estimate # between-camera variability of sex ratio 

# visualize camera-specific offsets 
#png("tide_analysis/ModelRDS/s_bm1b_cameras.png", width = 8, height = 7, units = 'in', res = 300)
s_bm1b %>%
  linpred_draws(tibble(locationfactor = agoutiseq_jto2$locationfactor,
                       locationtype = agoutiseq_jto2$locationtype,
                       nAF = agoutiseq_jto2$nAF,
                       distcoast = agoutiseq_jto2$distcoast)) %>%
  ungroup() %>% 
  mutate(locationfactor = fct_reorder(factor(locationfactor), .linpred, .fun = mean)) %>%
  ggplot(aes(x = logit2prob(.linpred), y = locationfactor, color = locationtype)) +
  scale_color_manual(values = cols) +
  geom_vline(xintercept = logit2prob(B0_b), color = cols[1], linewidth = 1) +
  geom_vline(xintercept = logit2prob(B0_b+B1_b), color = cols[2], linewidth = 1) +
  geom_vline(xintercept = logit2prob(B0_b+B2_b), color = cols[3], linewidth = 1) +
  stat_pointinterval() +
  theme_bw() +
  labs(x = "Model estimation of female with infants:female without infants ratio", y = "Camera location", color = "Location type") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
#dev.off()

## visualize distance to coast interaction
# marginal effects plots
marginal_predsb <- predictions(
  s_bm1b,
  newdata = datagrid(locationtype = c("random", "anvil", "streambed"),
                     locationfactor = unique),
  by = "locationtype",
  re_formula = NULL
) %>% 
  posteriordraws()

## this is the first violin plot but then differently
p_marginal_predsb <- marginal_predsb %>% 
  ggplot(aes(x = draw, fill = locationtype)) +
  stat_halfeye(alpha = 0.5) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values= cols) +
  labs(x = "Ratio females with infants:females without infants", y = "Density", fill = "Location type",
       title = "Marginal population-level means",
       subtitle = "Random effects averaged / marginalized / integrated") +
  theme_bw()
p_marginal_predsb

## if we do it per locationtype 
# this is assuming each camera can be at each distance at each locationtype (so also simulating distant anvil cameras)
marginal_preds2b <- predictions(
  s_bm1b,
  newdata = datagrid(locationtype = "random",
                     locationfactor = unique,
                     distcoast = unique),
  by = "distcoast",
  re_formula = NULL
) %>% 
  posteriordraws()

p_randomb <- ggplot(data = marginal_preds2b, aes(x = distcoast)) +
  geom_point(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "random"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF_infant/nAF),  col = cols[1], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "random"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF_infant/nAF), col = cols[1], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill = cols[1], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, col = cols[1]) +
  labs(subtitle="Random cameras",
       y = "Ratio females with infants:females without infants", x = NULL) +
  guides(colour="none", fill="none") + theme_bw() + theme(
    axis.title.y = element_text(size = 18),
    plot.subtitle = element_text(size = 18),
    axis.text = element_text(size = 16))
p_randomb

# anvil
marginal_preds3b <- predictions(
  s_bm1b,
  newdata = datagrid(locationtype = "anvil",
                     locationfactor = unique,
                     distcoast = unique),
  by = "distcoast",
  re_formula = NULL
) %>% 
  posteriordraws()

p_anvilb <- ggplot(data = marginal_preds3b, aes(x = distcoast)) +
  geom_point(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "anvil"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF_infant/nAF), colour = cols[2], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "anvil"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF_infant/nAF), colour = cols[2], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high)  , fill = cols[2], colour = cols[2], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, colour = cols[2]) +
  labs(subtitle="Anvil cameras",
       y=NULL, x ="Distance from coast (m)") +
  guides(colour="none", fill="none") + theme_bw() +  theme(axis.title.y=element_blank(),
                                                           axis.text.y=element_blank(),
                                                           axis.ticks.y=element_blank(),
                                                           axis.title.x = element_text(size = 18),
                                                           plot.subtitle = element_text(size = 18),
                                                           axis.text = element_text(size = 16))
p_anvilb

# streambed
marginal_preds4b <- predictions(
  s_bm1b,
  newdata = datagrid(locationtype = "streambed",
                     locationfactor = unique,
                     distcoast = unique),
  by = "distcoast",
  re_formula = NULL
) %>% 
  posteriordraws()

p_streambedb <- ggplot(data = marginal_preds4b, aes(x = distcoast)) +
  geom_point(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "streambed"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF_infant/nAF), colour = cols[3], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "streambed"),], inherit.aes = FALSE, aes(x = distcoast, y = nAF_infant/nAF), colour = cols[3], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high)  , fill = cols[3], colour = cols[3], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, colour = cols[3]) + 
  labs(subtitle="Streambed cameras", y=NULL, x = NULL, shape = "data_") +
  theme_bw() +  theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      plot.subtitle = element_text(size = 18),
                      axis.text = element_text(size = 16)) 
p_streambedb

#png("tide_analysis/ModelRDS/s_bm1b_interaction.png", width = 11, height = 7, units = 'in', res = 300)
plot(p_randomb + p_anvilb + p_streambedb)
#dev.off()

#### Model 2coiba: ratio of females with infants to females without on Coiba (only streambed) #########
agoutiseq_cto2 <- agoutiseq_cto1[which(agoutiseq_cto1$nAF >0),]

s_bm1b_coiba <- brm(nAF_infant | trials(nAF) ~ 1 + (1|locationfactor), data = agoutiseq_cto2, family = binomial, 
             iter = 3000, chain = 3, core = 3, backend = "cmdstanr", save_pars = save_pars(all = TRUE))
# s_bm1b_coiba <- add_criterion(s_bm1b_coiba, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", ndraws = 3000) 
# saveRDS(s_bm1b_coiba, "tide_analysis/ModelRDS/s_bm1b_coiba.RDS")
# s_bm1b_coiba <- readRDS("tide_analysis/ModelRDS/s_bm1b_coiba.RDS")
summary(s_bm1b_coiba)
mcmc_plot(s_bm1b_coiba)

## Checks
pp_check(s_bm1b_coiba, ndraw = 100) 
loo(s_bm1b_coiba)
loo_R2(s_bm1b_coiba)
round(bayes_R2(s_bm1b_coiba),2)

round(logit2prob(-2.45),2)

report(s_bm1b_coiba)

# marginal effects plot
r_fit <- s_bm1b_coiba %>%
  tidy() %>%
  split(~term)

B0_bc <- r_fit$`(Intercept)`$estimate # average infant across all sequences in random cams
sigma_0bc <- r_fit$`sd__(Intercept)`$estimate # between-camera variability of sex ratio 

# visualize camera-specific offsets 
#png("tide_analysis/ModelRDS/s_bm1bcoiba_cameras.png", width = 8, height = 7, units = 'in', res = 300)
s_bm1b_coiba %>%
  linpred_draws(tibble(locationfactor = agoutiseq_cto2$locationfactor,
                       nAF = agoutiseq_cto2$nAF)) %>%
  ungroup() %>% 
  mutate(locationfactor = fct_reorder(factor(locationfactor), .linpred, .fun = mean)) %>%
  ggplot(aes(x = logit2prob(.linpred), y = locationfactor)) +
  geom_vline(xintercept = logit2prob(B0_bc), color = cols[3], linewidth = 1) +
  stat_pointinterval(col = cols[3]) +
  theme_bw() +
  labs(x = "Model estimation of female with infants:female without infants ratio", y = "Camera location") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
#dev.off()

### H3: Females are rarely observed tool-using because of within-group competition ####
## P3a: Displacements at anvils are common
ftable(agoutiseq_jto$displacement[which(agoutiseq_jto$locationtype == "anvil")])
## of the 14 300 sequences at anvils, displacement only occurred in 94 of them 

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

head(dis_agg)
dis_graph <- rbind(dis_agg, dis_vics)
dis_graph$type <- c(rep("aggressor", 5), rep("victim", 5))
dis_opp <- as.data.frame(ftable(agoutiseq_jto$agesex[which(agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$n > 1)]))
# extract opportunity from above either manually or automatically

dis_graph$opportunity <- c(dis_opp$Freq[1], dis_opp$Freq[2], 
                           (dis_opp$Freq[6] + dis_opp$Freq[7]+dis_opp$Freq[8]),
                             (dis_opp$Freq[4] + dis_opp$Freq[5]), 0 ,rep("",5))
#exclude unknown
dis_graph <- dis_graph[-c(5,10),]
dis_graph$variable[3] <-"subadult"
dis_graph$agesex <- factor(dis_graph$variable, levels = c("adult female", "adult male", "subadult", "juvenile"))

ggplot(dis_graph, aes(x = agesex, y = value, group = type, fill = type)) + geom_bar(stat = "identity", position = position_dodge()) + 
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  geom_text(aes(label = opportunity), position = position_dodge(width = 0), vjust = -0.5, fontface = "italic" ) +
  labs( x = "Age-sex class", y = "Number of times observed", fill = "Role in displacement" ) +   scale_fill_grey() +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size=10)) 

## CONCLUSION: so no, displacements at anvils are not common, and females are not often displaced. 
# in the vast majority of the cases, tool users are "alone" at the anvil
# when are females alone at anvils?
ftable(agoutiseq_jto$tooluse[which(agoutiseq_jto$nAF > 0 & agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$nAM == 0)])
# 761 observations of a single adult female at an anvil without adult males there or other adult females
# 814 observations of adult females at anvils without adult males there: 712 of times no tool use either, 102 times tool use by juveniles

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

head(scrounging)

#log odds instead of real scale
scrounging$logodds <- log(scrounging$prop/(1-scrounging$prop))

ggplot(scrounging, aes(x = agesex, y = logodds, fill = agesex)) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=paste(n_scrounge, n_opp, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  labs( x = "Age-sex class", y = "Log-odds of proportion of opportunities spent scrounging", fill = "Age-sex class" ) +   scale_fill_manual(values = cbbPalette) +
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

#log odds instead of real scale
anvildebris$logodds <- log(anvildebris$prop/(1-anvildebris$prop))

ggplot(anvildebris, aes(x = agesex, y = logodds, fill = agesex)) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=paste(n_debris, n_opp, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  labs( x = "Age-sex class", y =  "Log-odds of proportion of opportunities spent consuming anvil debris", fill = "Age-sex class" ) +   scale_fill_manual(values = cbbPalette) +
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



############################ CODE GRAVEYARD DON'T RUN ###############

# look at whether there is seasonality in the birthpeak. When do we mostly see adult females with infants? Maybe consider ratio
agoutiseq_jt$year <- year(agoutiseq_jt$seqday)
agoutiseq_jt$yrday <- yday(agoutiseq_jt$seqday)
agoutiseq_jt$week <- week(agoutiseq_jt$seqday)

ggplot(data = agoutiseq_jt, aes(x = month, y = nAF_infant/nAF, col = as.factor(year))) + stat_summary(geom =  "smooth", fun = "mean")   + theme_bw()
ggplot(data = agoutiseq_jt, aes(x = week, y = nAF_infant/nAF, col = as.factor(year))) + stat_summary(geom =  "smooth", fun = "mean") + facet_wrap(~year)  + theme_bw()


s_gam1 <- gam(nAF_infant/nAF ~ s(yrday, bs = "cc") + factor(year) + s(month, bs = "cc"), data = agoutiseq_jt)
plot(s_gam1)
summary(s_gam1)

#### H5: Females have different diet than males ####
ftable(agoutiseq_ct$tool_item[which(agoutiseq_ct$tu_nAF >0)])


# use agouticlean dataset
head(agouticlean)

# this is repurposed code from agouti_cleaning script. Are doing the same thing as before but now separately for adult males and females.

#subset agouti gross dataset to only adult male and adult female sightings
# only jicaron and tool users
# drop out uncoded deployments
agouti_mf <- subset(agoutigross, (agoutigross$agesex == "adult male" | agoutigross$agesex == "adult female") & agoutigross$island == "Jicaron" & agoutigross$tool_site == 1 & 
                      agoutigross$uniqueloctag %in% codeddeployments_total)
agouti_mf <- droplevels.data.frame(agouti_mf)
# now per sequence we want:
# nr of adult males and females eating almendra, coconut, fruit, invertebrate, other, unknown. --> still need to make
# nr of adult males and females present (nAF and nAM)

### APPROACH 1: get sequence-level variable of what is mostly being processed in that sequence
# also get number of capuchins consuming each item per sequence 
# tool items
agouti_mf_tools <- agouti_mf[agouti_mf$tooluse == TRUE,]
tool_items_mf <- as.data.frame(as.matrix(ftable(agouti_mf_tools$sequenceID, agouti_mf_tools$tool_item)))
# set unknown tool use to 1 for all rows, so that when there are two things being processed (e.g. one unknown, one almendra) you keep the almendra
# this is crude solution but works
tool_items_mf$unknown <- 1
tool_items_mf$seq_toolitem_mf <- colnames(tool_items_mf)[max.col(tool_items_mf,ties.method="first")]  
# now gets the most frequently processed item in that sequence. If they tie it takes the first one first (so almendra, then coconut etc)
tool_items_mf$sequenceID <- rownames(tool_items_mf)
tool_items_mf2 <- tool_items_mf[,c("sequenceID", "seq_toolitem_mf")]

agouti_mf <- left_join(agouti_mf, tool_items_mf2, "sequenceID")

# normal items
agouti_mf_foraging <- agouti_mf[str_detect(agouti_mf$behaviour, "\\bF:"),]
normal_items_mf <- as.data.frame(as.matrix(ftable(agouti_mf_foraging$sequenceID, agouti_mf_foraging$normal_item)))
# set unknown foraging to 1 for all rows, so that when there are two things being processed (e.g. one unknown, one almendra) you keep the almendra
# this is crude solution but works
normal_items_mf$unknown <- 1
normal_items_mf$seq_normalitem_mf <- colnames(normal_items_mf)[max.col(normal_items_mf,ties.method="first")]  
# now gets the most frequently processed item in that sequence. If they tie it takes the first one first (so almendra, then coconut etc)
normal_items_mf$sequenceID <- rownames(normal_items_mf)
normal_items_mf2 <- normal_items_mf[,c("sequenceID", "seq_normalitem_mf")]

agouti_mf <- left_join(agouti_mf, normal_items_mf2, "sequenceID")

# see whether they both forage with and without tools and create variable with most information
# so what's being foraged irrespective of with or without tools
agouti_mf$bothforage <- ifelse(is.na(agouti_mf$seq_toolitem_mf) == FALSE & is.na(agouti_mf$seq_normalitem_mf) == FALSE, 1, 0)

# how many cases in which they forage with and without tools
unique(agouti_mf$behaviour[which(agouti_mf$bothforage == 1)])

# make general item one, if they both use tools and forage normally, tools take precedent
agouti_mf$seq_item <- ifelse(agouti_mf$bothforage == 0 & is.na(agouti_mf$seq_toolitem_mf) == FALSE, agouti_mf$seq_toolitem_mf,
                               ifelse(agouti_mf$bothforage == 0 & is.na(agouti_mf$seq_normalitem_mf) == FALSE, agouti_mf$seq_normalitem_mf,
                                      ifelse(agouti_mf$bothforage == 1 & agouti_mf$seq_toolitem_mf == "unknown", agouti_mf$seq_normalitem_mf,
                                             ifelse(agouti_mf$bothforage == 1 & agouti_mf$seq_toolitem_mf != "unknown", agouti_mf$seq_toolitem_mf, NA))))

# simplify to categories that overlap
agouti_mf$seq_item <- ifelse(str_detect(agouti_mf$seq_item, "crab"), "crab", agouti_mf$seq_item)

### APPROACH 2: get number of capuchins foraging certain resource per sequence for each itemtype
# for how many capuchins are normal item and tool item the same?
agouti_mf[which(agouti_mf$normal_item == agouti_mf$tool_item),]
# wouldn't want to inflate this
agouti_mf$foraging_item1 <- agouti_mf$normal_item
agouti_mf$foraging_item2 <- agouti_mf$tool_item
agouti_mf$foraging_item2[which(agouti_mf$normal_item == agouti_mf$tool_item)] <- NA

agouti_mf[which(is.na(agouti_mf$foraging_item1) == FALSE & is.na(agouti_mf$foraging_item2) == FALSE),]

# combine both tool_items and normal_items (if they are not the same)
# sum normal and tool items. This means that if a capuchin eats two different things they are counted twice
# so if 4 out of 5 capuchins eat insects in a sequence, still 3 could be eating almendras (cause they eat several things at once)
# this seems fair. Can also do this otherwise. 

# now do separately for males and females
agouti_mf_allforaging_f <- agouti_mf[str_detect(agouti_mf$behaviour, "F:") == TRUE & agouti_mf$agesex == "adult female",]
agouti_mf_allforaging_m <- agouti_mf[str_detect(agouti_mf$behaviour, "F:") == TRUE & agouti_mf$agesex == "adult male",]


d1_f <- as.data.frame(as.matrix(ftable(agouti_mf_allforaging_f$sequenceID, agouti_mf_allforaging_f$foraging_item1)))
d1_m <- as.data.frame(as.matrix(ftable(agouti_mf_allforaging_m$sequenceID, agouti_mf_allforaging_m$foraging_item1)))
d2_f <- as.data.frame(as.matrix(ftable(agouti_mf_allforaging_f$sequenceID, agouti_mf_allforaging_f$foraging_item2)))
d2_m <- as.data.frame(as.matrix(ftable(agouti_mf_allforaging_m$sequenceID, agouti_mf_allforaging_m$foraging_item2)))
d1_f$sequenceID <- rownames(d1_f)
d2_f$sequenceID <- rownames(d2_f)
d1_m$sequenceID <- rownames(d1_m)
d2_m$sequenceID <- rownames(d2_m)

all_items_f <- full_join(d1_f, d2_f, by = "sequenceID")
all_items_m <- full_join(d1_m, d2_m, by = "sequenceID")

# only make the four categories now
all_items_f$nr_almendra_f <- all_items_f$almendra.x + all_items_f$almendra.y
all_items_f$nr_coconut_f <- all_items_f$coconut
all_items_f$nr_fruit_f <- all_items_f$fruit
all_items_f$nr_invertebrate_f <- all_items_f$crab + all_items_f$insect
all_items_f$nr_other_f <- all_items_f$other
all_items_f$nr_unknown_f <- all_items_f$unknown.x + all_items_f$unknown.y

all_items_f2 <- all_items_f[,c("sequenceID", "nr_almendra_f", "nr_coconut_f", "nr_fruit_f", "nr_invertebrate_f",
                           "nr_other_f", "nr_unknown_f")]

agouti_mf <- left_join(agouti_mf, all_items_f2, "sequenceID")

all_items_m$nr_almendra_m <- all_items_m$almendra.x + all_items_m$almendra.y
all_items_m$nr_coconut_m <- all_items_m$coconut.x + all_items_m$coconut.y
all_items_m$nr_fruit_m <- all_items_m$fruit.x + all_items_m$fruit.y + all_items_m$palm
all_items_m$nr_invertebrate_m <- all_items_m$hecrab + all_items_m$hwcrab + all_items_m$insect.x + 
  all_items_m$insect.y + all_items_m$snail + all_items_m$crab
all_items_m$nr_other_m <- all_items_m$other.x + all_items_m$other.y
all_items_m$nr_unknown_m <- all_items_m$unknown.x + all_items_m$unknown.y

all_items_m2 <- all_items_m[,c("sequenceID", "nr_almendra_m", "nr_coconut_m", "nr_fruit_m", "nr_invertebrate_m",
                           "nr_other_m", "nr_unknown_m")]
str(all_items_m2)

agouti_mf <- left_join(agouti_mf, all_items_m2, "sequenceID")

# drop to sequence level
agouticlean_mf <- agouti_mf[, !names(agouti_mf) %in% c("timestamp","mediaID", "classificationConfidence", "coordinateUncertainty", "start", "end", "cameraID", "cameraModel", "baitUse", "featureType",
                                                        "timestampIssues", "cameraTilt", "session", "array", "habitat", "X_id.y", "X_id.x", "comments.y", "time", "comment_item", "foraging_item1", "foraging_item2",
                                                        "countNew", "cameraHeading")]

# drop duplicated sequences
agoutisequence_mf <- agouticlean_mf[!duplicated(agouticlean_mf$sequenceID),]

agoutimelt_mf <- melt(agoutisequence_mf, measure.vars = c("nr_almendra_m", "nr_coconut_m", "nr_fruit_m", "nr_invertebrate_m", 
                                         "nr_almendra_f", "nr_coconut_f", "nr_fruit_f", "nr_invertebrate_f"))
head(agoutimelt_mf)

agoutimelt_mf$adultsex <- ifelse(agoutimelt_mf$variable %in% c("nr_almendra_m", "nr_coconut_m", "nr_fruit_m", "nr_invertebrate_m"), "male", "female")
head(agoutimelt_mf)
agoutimelt_mf$value[which(is.na(agoutimelt_mf$value) == TRUE)] <- 0
agoutimelt_mf$itemtype <-  sapply(str_split(agoutimelt_mf$variable, "_"), '[', 2)
agoutimelt_mf$yrday <- yday(agoutimelt_mf$seq_start)
agoutimelt_mf$npresent <- ifelse(agoutimelt_mf$adultsex == "male", agoutimelt_mf$nAM, agoutimelt_mf$nAF)
agoutimelt_mf$locationfactor <- as.factor(agoutimelt_mf$locationName)
agoutimelt_mf$value[which(agoutimelt_mf$value > agoutimelt_mf$npresent)] <- agoutimelt_mf$npresent[which(agoutimelt_mf$value > agoutimelt_mf$npresent)]
agoutimelt_mf$valueop <- agoutimelt_mf$npresent - agoutimelt_mf$value

for_m1 <- glmer(cbind(value, valueop) ~ adultsex*itemtype+ (1|locationfactor), data = agoutimelt_mf, family = binomial)
summary(for_m1)
emmip(for_m1, adultsex ~ itemtype)
em_form1 <- emmeans(for_m1, ~ adultsex * itemtype)
summary(em_form1, type = "response")

resfor_gam1 <- gam(value ~ s(yrday, bs = "cc", k = 15, by = interaction(itemtype, adultsex)) + itemtype + adultsex + s(locationfactor, bs = "re") + offset(npresent), 
                 data = agoutimelt_mf, family = poisson(), method = "REML", knots = list(yrday = c(0.5,366.5)))
#saveRDS(resfor_gam1, "tide_analysis/ModelRDS/resfor_gam1.RDS")
summary(resfor_gam1)
draw(resfor_gam1)

gam.check(resfor_gam1)


agoutimelt_mf$locationtype <- as.factor(ifelse(agoutimelt_mf$tool_anvil == 1, "anvil", 
                                              ifelse(agoutimelt_mf$streambed == 1, "streambed", "random")))

# proportion of individuals foraging on a given item out of those present
# so if 3 males are present, how many of them are eating almendra, coconut etc
# look per itemtype sex interaction
# taking into account that there are different camera locations of different types
# can them compare proportion of diet taken up by each item for the two sexes

agoutimelt_mf$locationtype <- as.factor(ifelse(agoutimelt_mf$tool_anvil == 1, "anvil", 
                                              ifelse(agoutimelt_mf$streambed == 1, "streambed", "random")))
agoutimelt_mf$locationtype <- relevel(agoutimelt_mf$locationtype, ref = "random")

## CHECK THIS WITH BRENDAN! ESPECIALLY THE RANDOM EFFECT STRUCTURE. Do you need to include sex in the random effect? like account for fact that males specifically would be eating a lot of almendras at anvils
for_bm1 <- brm(value | trials(npresent) ~ adultsex*itemtype + (1|locationtype:locationfactor), data = agoutimelt_mf, family = binomial, iter = 1000, chain = 2, core = 2, backend = "cmdstanr")
#saveRDS(for_bm1, "tide_analysis/ModelRDS/for_bm1.RDS")
summary(for_bm1)
mcmc_plot(for_bm1)
plot(conditional_effects(for_bm1))
mcmc_plot(for_bm1,type = "trace")
mcmc_plot(for_bm1, type = "acf_bar") # autocorrelation
pp_check(for_bm1, ndraw = 100) 

# potentially still drop the 0 npresent 0 observations for  females when only males are present and vice versa
# dont care about when no one is present for the category, so drop all npresent == 0
agoutimelt_mf_r <- agoutimelt_mf[!agoutimelt_mf$npresent == 0,]

for_bm2 <- brm(value | trials(npresent) ~ adultsex*itemtype + (1|locationtype:locationfactor), data = agoutimelt_mf_r, family = binomial, iter = 1000, chain = 2, core = 2, backend = "cmdstanr")
#saveRDS(for_bm2, "tide_analysis/ModelRDS/for_bm2.RDS")
#for_bm2 <- readRDS("tide_analysis/ModelRDS/for_bm2.RDS")
summary(for_bm2)
mcmc_plot(for_bm2)
plot(conditional_effects(for_bm2))
mcmc_plot(for_bm2,type = "trace")
mcmc_plot(for_bm2, type = "acf_bar") # autocorrelation
pp_check(for_bm2, ndraw = 100) 

hypothesis(for_bm2, "Intercept + itemtypecoconut = Intercept + adultsexmale:itemtypecoconut", alpha = 0.05)
hypothesis(for_bm2, "Intercept + itemtypefruit = Intercept + adultsexmale:itemtypefruit", alpha = 0.05)
hypothesis(for_bm2, "Intercept + itemtypeinvertebrate = Intercept + adultsexmale:itemtypeinvertebrate", alpha = 0.05)


dietplot <- plot(conditional_effects(for_bm2), plot = FALSE)[[3]]

dietplot  + theme_bw() + 
  stat_summary(data = agoutimelt_mf_r, inherit.aes = FALSE, aes(x = adultsex, y = value/npresent, fill = itemtype), 
               geom = "point", fun = "mean", size = 2, shape = 24, alpha = 0.5) +
  labs(y = "Proportion of capuchins in sequence foraging on item", x = "Sex of adult capuchin") +
  theme( strip.text.x = element_text(size = 12), 
        axis.title = element_text(size = 14), legend.text =  element_text(size = 10), 
        legend.title = element_text(size =12), axis.text = element_text(size = 12))

sum(agoutisequence_mf$nAF)
sum(agoutisequence_mf$nr_almendra_f, na.rm = TRUE)
sum(agoutisequence_mf$nr_invertebrate_f, na.rm = TRUE)
sum(agoutisequence_mf$nr_coconut_f, na.rm = TRUE)
sum(agoutisequence_mf$nr_fruit_f, na.rm = TRUE)
?stat_summary
# can go to per day if we want to include the 0's. (see script from seasonality analysis script)
# for more crude analysis on all adult male/female sightings


agouticlean[which(str_detect(agouticlean$behaviour, "Infant") == TRUE & 
              str_detect(agouticlean$behaviour,  "TAF") == TRUE),]
head(agouticlean)
