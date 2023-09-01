# Supplemental code for "Male-biased stone tool use by wild white-faced capuchins (Cebus capucinus imitator)

# Zoë Goldsborough, Margaret Crofoot, Brendan Barrett
# 2023

# setwd("~/GitHub/camtrap_coiba/sexbias")

## Packages required
require(brms)
require(tidybayes)
require(modelr)
require(ggthemes)
require(dplyr)
require(ggplot2)
require(report)
require(reshape2)
require(tidyverse)
require(marginaleffects)
require(broom.mixed)

# Load datasets of capuchin detections for tool-using group on Jicarón (agoutiseq_jt) and Coiba (agoutiseq_ct)
agoutiseq_jt <- read.csv("agoutiseq_jt.csv", header = TRUE, stringsAsFactors = FALSE)
agoutiseq_ct <- read.csv("agoutiseq_ct.csv", header = TRUE, stringsAsFactors = FALSE)
# every row is a sequence

#### PREPARATION #### 

# locationtype (anvil, random, streambed) as a factor, with random as reference
agoutiseq_jt$locationtype <- as.factor(agoutiseq_jt$locationtype)
agoutiseq_jt$locationtype <- relevel(agoutiseq_jt$locationtype,  ref = "random")

# location factor as factor
agoutiseq_jt$locationfactor <- as.factor(agoutiseq_jt$locationfactor)
agoutiseq_ct$locationfactor <- as.factor(agoutiseq_ct$locationfactor)

## need to set appropriate priors for models
sexbias_prior <- c(prior(normal(0, 1), class = Intercept),
                   prior(normal(0,1), class = b),
                   prior(exponential(1), class = sd))

# for models, for going from logit scale to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

## set viridis color palette for plots
scales::viridis_pal(end = 0.8)(3) 
cols <- c( "#440154FF", "#7AD151FF", "#2A788EFF" )

### H1: PHYSICAL ABILITY ####

# female tool use events on Jicaron
tooluse <- agoutiseq_jt[which(agoutiseq_jt$tooluse == "TRUE"),]
sum(tooluse$tu_nAM)
sum(tooluse$tu_nJuvenile)
sum(tooluse$tu_nSubadult)
sum(tooluse$tu_nAF)

# tool use events on Coiba
tooluse_c <- agoutiseq_ct[which(agoutiseq_ct$tooluse == "TRUE"),]
sum(tooluse_c$tu_nAM)
sum(tooluse_c$tu_nJuvenile)
sum(tooluse_c$tu_nSubadult)
sum(tooluse_c$tu_nAF)

### H2 : RISK AVERSION ####
##### Model 1: Comparing ratio of adult females to adult males ######
###### Jicarón island #######

# filter to only sequences with adult capuchins and exclude when we didnt sex any of the adults
agoutiseq_jto <- agoutiseq_jt[agoutiseq_jt$capuchin == 1,]
agoutiseq_jto1 <- agoutiseq_jt[agoutiseq_jt$Nadults > 0 & (agoutiseq_jt$nAF >0 | agoutiseq_jt$nAM >0),] 
agoutiseq_jto1 <- droplevels.data.frame(agoutiseq_jto1)

# when capuchins are present, what is the ratio of adult females to adult males at the three different location types?
# what can affect this ratio is: locationfactor, locationtype, distance from coast

# prior predictive simulation (using model 1)
sbm1_prior <- brm(nAF | trials(Nadults) ~ locationtype*distcoast_z +  (1|locationfactor), data = agoutiseq_jto1, family = binomial, 
                  prior = sexbias_prior, sample_prior = "only", iter = 1000, chain = 2, core = 2, backend = "cmdstanr")

summary(sbm1_prior)
prior_summary(sbm1_prior)
mcmc_plot(sbm1_prior)
plot(sbm1_prior)

# Model 1
s_bm1 <- brm(nAF | trials(Nadults) ~ locationtype*distcoast_z +  (1|locationfactor), data = agoutiseq_jto1, family = binomial, 
             prior = sexbias_prior, iter = 3000, chain = 3, core = 3, backend = "cmdstanr", save_pars = save_pars(all = TRUE), seed = 1245)

# to add loo, loo_r2 and bayes_R2
s_bm1 <- add_criterion(s_bm1, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", ndraws = 3000)

# saving and loading model after it ran, change to location where you'd want to save the object
# saveRDS(s_bm1, "ModelRDS/s_bm1.RDS")
# s_bm1 <- readRDS("ModelRDS/s_bm1.RDS")

# Diagnostics
mcmc_plot(s_bm1)
pp_check(s_bm1, ndraw = 100) 
loo(s_bm1)
loo_R2(s_bm1)
round(bayes_R2(s_bm1),2)
plot(s_bm1)

# Evaluating results
summary(s_bm1)
# converting estimates to probabilities
round(logit2prob(0.26-1.11),2)

plot(conditional_effects(s_bm1, re_formula = NULL)) # re_formula = NULL to include random effects

hypothesis(s_bm1, "Intercept > Intercept + locationtypeanvil", alpha = 0.05)
hypothesis(s_bm1, "Intercept > Intercept + locationtypestreambed")
hypothesis(s_bm1, "Intercept +locationtypeanvil < Intercept + locationtypestreambed")

hypothesis(s_bm1, "distcoast < distcoast + locationtypeanvil:distcoast")
hypothesis(s_bm1, "distcoast < distcoast + locationtypestreambed:distcoast")
hypothesis(s_bm1, "distcoast + locationtypeanvil:distcoast < distcoast + locationtypestreambed:distcoast")

# to help with reporting results
report(s_bm1)

## Figure 1: Violin plot of sex ratio across location types
m_type_pred <- s_bm1 %>% 
  epred_draws(newdata = tibble(Nadults = agoutiseq_jto1$Nadults,
                               locationtype = agoutiseq_jto1$locationtype,
                               locationfactor = agoutiseq_jto1$locationfactor,
                               distcoast_z = agoutiseq_jto1$distcoast_z)) %>% 
  mutate(.epred_prop = .epred/Nadults)

# for saving as PNG can uncomment line below and "dev.off" line.
#png("ModelRDS/s_bm1_ratios.png", width = 8, height = 7, units = 'in', res = 300)
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
  labs(x = "Location type", y = "Female:male ratio") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 
#dev.off()

### Marginal effects plots
# want to get to marginal effects (average across all cameras) instead of conditional effect (effect at average camera)
# pull out parameters 
r_fit <- s_bm1 %>%
  tidy() %>%
  split(~term)

B0 <- r_fit$`(Intercept)`$estimate # average sex ratio across all sequences in random cams
B1 <- r_fit$locationtypeanvil$estimate # effect of anvil on sex ratio across all sequences
B2 <- r_fit$locationtypestreambed$estimate # effect of streambed across all sequences
B3 <- r_fit$distcoast_z$estimate # effect of distcoast on random cams
B4 <- r_fit$`locationtypeanvil:distcoast_z`$estimate # interaction anvil and distcoast
B5 <- r_fit$`locationtypestreambed:distcoast_z`$estimate #interaction streambed and distcoast
sigma_0 <- r_fit$`sd__(Intercept)`$estimate # between-camera variability of sex ratio 

## Supplemental Figure S1: Visualize camera-specific offsets 
#png("ModelRDS/s_bm1_cameras.png", width = 9, height = 9, units = 'in', res = 300)
s_bm1 %>%
  linpred_draws(tibble(locationfactor = agoutiseq_jto1$locationfactor,
                       locationtype = agoutiseq_jto1$locationtype,
                       Nadults = agoutiseq_jto1$Nadults,
                       distcoast_z = agoutiseq_jto1$distcoast_z)) %>%
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

## Figure 2: Visualizing distance to coast interaction
# this is assuming each camera can be at each distance at each locationtype (so also simulating distant anvil cameras)

# make x-axis labels for distance to coast on real scale instead of z-score scale. want 0, 100, 200, 300
meandist <- mean(agoutiseq_jt$distcoast)
sddist <- sd(agoutiseq_jt$distcoast)
labelsdistcoast <- c(0, 100, 200, 300)
zdistcoast <- (labelsdistcoast - meandist)/sddist

# make each figure separately for each locationtype and then combine
# Random
marginal_preds2 <- predictions(
  s_bm1,
  newdata = datagrid(locationtype = "random",
                     locationfactor = unique,
                     distcoast_z = unique),
  by = "distcoast_z",
  re_formula = NULL
) %>% 
  posteriordraws()

p_random <- ggplot(data = marginal_preds2, aes(x = distcoast_z)) +
  geom_point(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "random"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF/Nadults),  col = cols[1], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "random"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF/Nadults), col = cols[1], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill = cols[1], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, col = cols[1]) +
  labs(subtitle="Random cameras",
       y = "Female:male ratio", x = NULL) +
  scale_x_continuous(breaks = zdistcoast,
                     labels = as.character(labelsdistcoast)) +
  guides(colour="none", fill="none") + theme_bw() + theme(
    axis.title.y = element_text(size = 18),
    plot.subtitle = element_text(size = 18),
    axis.text = element_text(size = 16))
p_random

# Anvil
marginal_preds3 <- predictions(
  s_bm1,
  newdata = datagrid(locationtype = "anvil",
                     locationfactor = unique,
                     distcoast_z = unique),
  by = "distcoast_z",
  re_formula = NULL
) %>% 
  posteriordraws()

p_anvil <- ggplot(data = marginal_preds3, aes(x = distcoast_z)) +
  geom_point(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "anvil"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF/Nadults), colour = cols[2], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "anvil"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF/Nadults), colour = cols[2], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high)  , fill = cols[2], colour = cols[2], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, colour = cols[2]) +
  labs(subtitle="Anvil cameras",
       y=NULL, x ="Distance from coast (m)") +
  scale_x_continuous(breaks = zdistcoast,
                     labels = as.character(labelsdistcoast)) +
  guides(colour="none", fill="none") + theme_bw() +  theme(axis.title.y=element_blank(),
                                                           axis.text.y=element_blank(),
                                                           axis.ticks.y=element_blank(),
                                                           axis.title.x = element_text(size = 18),
                                                           plot.subtitle = element_text(size = 18),
                                                           axis.text = element_text(size = 16))
p_anvil

# Streambed
marginal_preds4 <- predictions(
  s_bm1,
  newdata = datagrid(locationtype = "streambed",
                     locationfactor = unique,
                     distcoast_z = unique),
  by = "distcoast_z",
  re_formula = NULL
) %>% 
  posteriordraws()

p_streambed <- ggplot(data = marginal_preds4, aes(x = distcoast_z)) +
  geom_point(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "streambed"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF/Nadults), colour = cols[3], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto1[which(agoutiseq_jto1$locationtype == "streambed"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF/Nadults), colour = cols[3], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high)  , fill = cols[3], colour = cols[3], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, colour = cols[3]) + 
  labs(subtitle="Streambed cameras", y=NULL, x = NULL, shape = "data_") +
  scale_x_continuous(breaks = zdistcoast,
                     labels = as.character(labelsdistcoast)) +
  theme_bw() +  theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      plot.subtitle = element_text(size = 18),
                      axis.text = element_text(size = 16)) 
p_streambed

#png("ModelRDS/s_bm1_interaction.png", width = 11, height = 7, units = 'in', res = 300)
plot(p_random + p_anvil + p_streambed)
#dev.off()

###### Coiba island #######

# Preparation
# Use only sequences with capuchins in and comparing female/male ratios at the three different location types
# differentiate adult females with and without infants
agoutiseq_ct$nAF_noinfant <- agoutiseq_ct$nAF - agoutiseq_ct$nAF_infant
agoutiseq_ct$Nadults <- agoutiseq_ct$nAF + agoutiseq_ct$nAM

# only sequences with adult capuchins
# also dropping sequences that only contain unsexed adults (so no adults recognized as males or females) 
agoutiseq_cto <- agoutiseq_ct[agoutiseq_ct$capuchin == 1,]
agoutiseq_cto1 <- agoutiseq_ct[agoutiseq_ct$Nadults > 0 & (agoutiseq_ct$nAF >0 | agoutiseq_ct$nAM >0),] 
agoutiseq_cto1 <- droplevels.data.frame(agoutiseq_cto1)

# Model 1-coiba: 
s_bm1coiba <- brm(nAF | trials(Nadults) ~ 1 + (1|locationfactor), data = agoutiseq_cto1, family = binomial, 
                  iter = 3000, chain = 3, core = 3, backend = "cmdstanr", save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99), seed = 1245)

# to add loo, loo_r2 and bayes_R2
s_bm1coiba <- add_criterion(s_bm1coiba, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", control = list(adapt_delta = 0.99), ndraws = 3000) 

# Saving and loading model after it ran. Change to location where you'd want to save the object
# saveRDS(s_bm1coiba, "ModelRDS/s_bm1coiba.RDS")
# s_bm1coiba <- readRDS("ModelRDS/s_bm1coiba.RDS")

# Diagnostics
mcmc_plot(s_bm1coiba)
pp_check(s_bm1coiba, ndraw = 100) 
loo(s_bm1coiba)
loo_R2(s_bm1coiba)
round(bayes_R2(s_bm1coiba),2)
plot(s_bm1coiba)

# Evaluating results
summary(s_bm1coiba)
round(logit2prob(1.70),2)
report(s_bm1coiba)

## Supplemental Figure S2: Plot of sex ratio at each camera. 
r_fitc <- s_bm1coiba %>%
  tidy() %>%
  split(~term)

B0c <- r_fitc$`(Intercept)`$estimate # average sex ratio across all sequences 
sigma_0c <- r_fitc$`sd__(Intercept)`$estimate # between-camera variability of sex ratio 

#png("ModelRDS/s_bm1coiba_cameras.png", width = 8, height = 7, units = 'in', res = 300)
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

##### Model 1: Comparing ratio of adult females with infants to adult females without infants ######
###### Jicarón island #######

# Preparation
agoutiseq_jto2 <- agoutiseq_jto1[which(agoutiseq_jto1$nAF >0),]
# make month a factor (include month as a random effect because there is a seasonal birth peak that affects infant ratio)
agoutiseq_jto2$monthF <- as.factor(agoutiseq_jto2$month)

# Model 2:
s_bm1b <- brm(nAF_infant | trials(nAF) ~ locationtype*distcoast_z + (1|locationfactor) + (1|monthF), data = agoutiseq_jto2, family = binomial, 
              prior = sexbias_prior,  iter = 3000, chain = 3, core = 3, backend = "cmdstanr", save_pars = save_pars(all = TRUE), seed = 1245)

# to add loo, loo_R2 and bayes_R2
s_bm1b <- add_criterion(s_bm1b, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", ndraws = 3000) 

# Saving and loading model after it ran. Change to location where you'd want to save the object
# saveRDS(s_bm1b, "ModelRDS/s_bm1b.RDS")
# s_bm1b <- readRDS("ModelRDS/s_bm1b.RDS")

# Diagnostics
mcmc_plot(s_bm1b)
pp_check(s_bm1b, ndraw = 100) 
loo(s_bm1b)
loo_R2(s_bm1b)
round(bayes_R2(s_bm1b),2)
plot(s_bm1b)

# Evaluating results
summary(s_bm1b)
round(logit2prob(-1.34+0.86),2)
plot(conditional_effects(s_bm1b))

hypothesis(s_bm1b, "Intercept > Intercept + locationtypeanvil", alpha = 0.05)
hypothesis(s_bm1b, "Intercept > Intercept + locationtypestreambed", alpha = 0.05)
hypothesis(s_bm1b, "Intercept + locationtypestreambed > Intercept + locationtypeanvil", alpha = 0.05)

report(s_bm1b)

# Visualizing results
## Figure 3: comparing female with infant:female without infant ratio between locationtypes
m_type_predb <- s_bm1b %>% 
  epred_draws(newdata = tibble(nAF = agoutiseq_jto2$nAF,
                               locationtype = agoutiseq_jto2$locationtype,
                               locationfactor = agoutiseq_jto2$locationfactor,
                               distcoast_z = agoutiseq_jto2$distcoast_z,
                               monthF = agoutiseq_jto2$monthF)) %>% 
  mutate(.epred_prop = .epred/nAF) # change to proportion

#png("ModelRDS/s_bm1b_ratios.png", width = 8, height = 7, units = 'in', res = 300)
ggplot(data = m_type_predb, aes(x = locationtype, y = .epred_prop)) + geom_violin(aes(color = locationtype, fill = locationtype), alpha = 0.4) + 
  stat_summary(agoutiseq_jto2, inherit.aes = FALSE, mapping=aes(x = locationtype, y = nAF_infant/nAF, color = locationtype), geom = "point", fun = "mean",
               size = 4) + 
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  scale_y_continuous(lim = c(0,1)) +
  guides(color = "none", fill = "none") +
  labs(x = "Location type", y = "Ratio females with infants:females without infants") +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 14)) 
#dev.off()

# Supplemental Figure S3: Camera specific offsets
r_fit <- s_bm1b %>%
  tidy() %>%
  split(~term)

B0_b <- r_fit$`(Intercept)`$estimate # average infant across all sequences in random cams
B1_b <- r_fit$locationtypeanvil$estimate # effect of anvil on sex ratio across all sequences
B2_b <- r_fit$locationtypestreambed$estimate # effect of streambed across all sequences
sigma_0b1 <- r_fit$`sd__(Intercept)`$estimate[1] # between-camera variability of sex ratio 
sigma_0b2 <- r_fit$`sd__(Intercept)`$estimate[2] # between-camera variability of sex ratio 

#png("ModelRDS/s_bm1b_cameras.png", width = 8, height = 9, units = 'in', res = 300)
s_bm1b %>%
  linpred_draws(tibble(locationfactor = agoutiseq_jto2$locationfactor,
                       locationtype = agoutiseq_jto2$locationtype,
                       nAF = agoutiseq_jto2$nAF,
                       distcoast_z = agoutiseq_jto2$distcoast_z,
                       monthF = agoutiseq_jto2$monthF)) %>%
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

# Supplemental Figure S4: Visualize variation between months
#png("ModelRDS/s_bm1b_month.png", width = 8, height = 8, units = 'in', res = 300)
s_bm1b %>%
  linpred_draws(tibble(locationfactor = agoutiseq_jto2$locationfactor,
                       locationtype = agoutiseq_jto2$locationtype,
                       nAF = agoutiseq_jto2$nAF,
                       distcoast_z = agoutiseq_jto2$distcoast_z,
                       monthF = agoutiseq_jto2$monthF)) %>%
  ungroup() %>% 
  ggplot(aes(x = logit2prob(.linpred), y = monthF, color = locationtype)) +
  scale_color_manual(values = cols) +
  stat_pointinterval() + facet_wrap(~locationtype) + 
  theme_bw() +
  labs(x = "Model estimation of female with infants:female without infants ratio", y = "Month", color = "Location type") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
#dev.off()

## Figure 4: Visualize distance to coast interaction

# this is assuming each camera can be at each distance at each locationtype (so also simulating distant anvil cameras)
# Random
marginal_preds2b <- predictions(
  s_bm1b,
  newdata = datagrid(locationtype = "random",
                     locationfactor = unique,
                     distcoast_z = unique),
  by = "distcoast_z",
  re_formula = NULL
) %>% 
  posteriordraws()

p_randomb <- ggplot(data = marginal_preds2b, aes(x = distcoast_z)) +
  geom_point(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "random"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF_infant/nAF),  col = cols[1], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "random"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF_infant/nAF), col = cols[1], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill = cols[1], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, col = cols[1]) +
  labs(subtitle="Random cameras",
       y = "Ratio females with infants:females without infants", x = NULL) +
  scale_x_continuous(breaks = zdistcoast,
                     labels = as.character(labelsdistcoast)) +
  guides(colour="none", fill="none") + theme_bw() + theme(
    axis.title.y = element_text(size = 18),
    plot.subtitle = element_text(size = 18),
    axis.text = element_text(size = 16))
p_randomb

# Anvil
marginal_preds3b <- predictions(
  s_bm1b,
  newdata = datagrid(locationtype = "anvil",
                     locationfactor = unique,
                     distcoast_z = unique),
  by = "distcoast_z",
  re_formula = NULL
) %>% 
  posteriordraws()

p_anvilb <- ggplot(data = marginal_preds3b, aes(x = distcoast_z)) +
  geom_point(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "anvil"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF_infant/nAF), colour = cols[2], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "anvil"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF_infant/nAF), colour = cols[2], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high)  , fill = cols[2], colour = cols[2], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, colour = cols[2]) +
  labs(subtitle="Anvil cameras",
       y=NULL, x ="Distance from coast (m)") +
  scale_x_continuous(breaks = zdistcoast,
                     labels = as.character(labelsdistcoast)) +
  guides(colour="none", fill="none") + theme_bw() +  theme(axis.title.y=element_blank(),
                                                           axis.text.y=element_blank(),
                                                           axis.ticks.y=element_blank(),
                                                           axis.title.x = element_text(size = 18),
                                                           plot.subtitle = element_text(size = 18),
                                                           axis.text = element_text(size = 16))
p_anvilb

# Streambed
marginal_preds4b <- predictions(
  s_bm1b,
  newdata = datagrid(locationtype = "streambed",
                     locationfactor = unique,
                     distcoast_z = unique),
  by = "distcoast_z",
  re_formula = NULL
) %>% 
  posteriordraws()

p_streambedb <- ggplot(data = marginal_preds4b, aes(x = distcoast_z)) +
  geom_point(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "streambed"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF_infant/nAF), colour = cols[3], alpha = 0.3) +
  stat_summary(data = agoutiseq_jto2[which(agoutiseq_jto2$locationtype == "streambed"),], inherit.aes = FALSE, aes(x = distcoast_z, y = nAF_infant/nAF), colour = cols[3], 
               geom = "point", fun = "mean", size = 4, shape = 17, alpha = 0.5) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high)  , fill = cols[3], colour = cols[3], alpha=0.3) +
  geom_line(aes(y=estimate), linewidth = 1, colour = cols[3]) + 
  labs(subtitle="Streambed cameras", y=NULL, x = NULL, shape = "data_") +
  scale_x_continuous(breaks = zdistcoast,
                     labels = as.character(labelsdistcoast)) +
  theme_bw() +  theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      plot.subtitle = element_text(size = 18),
                      axis.text = element_text(size = 16)) 
p_streambedb

#png("ModelRDS/s_bm1b_interaction.png", width = 11, height = 7, units = 'in', res = 300)
plot(p_randomb + p_anvilb + p_streambedb)
#dev.off()

###### Coiba island #######
# Preparation
agoutiseq_cto2 <- agoutiseq_cto1[which(agoutiseq_cto1$nAF >0),]
agoutiseq_cto2$monthF <- as.factor(agoutiseq_cto2$month)

# Model 2-coiba:
s_bm1b_coiba <- brm(nAF_infant | trials(nAF) ~ 1 + (1|locationfactor), data = agoutiseq_cto2, family = binomial, 
                    iter = 3000, chain = 3, core = 3, backend = "cmdstanr", save_pars = save_pars(all = TRUE), seed = 1245)

# to add loo, loo_R2 and bayes_R2
s_bm1b_coiba <- add_criterion(s_bm1b_coiba, c("loo", "loo_R2", "bayes_R2"), reloo = TRUE, backend = "cmdstanr", ndraws = 3000) 

# Saving and loading model after it ran. Change to location where you'd want to save the object
# saveRDS(s_bm1b_coiba, "ModelRDS/s_bm1b_coiba.RDS")
# s_bm1b_coiba <- readRDS("ModelRDS/s_bm1b_coiba.RDS")

# Diagnostics
mcmc_plot(s_bm1b_coiba)
pp_check(s_bm1b_coiba, ndraw = 100) 
loo(s_bm1b_coiba)
loo_R2(s_bm1b_coiba)
round(bayes_R2(s_bm1b_coiba),2)

# Evaluating results
summary(s_bm1b_coiba)
round(logit2prob(-4.67),2)

report(s_bm1b_coiba)

# Supplemental Figure S5: Visualize camera specific offsets
r_fit <- s_bm1b_coiba %>%
  tidy() %>%
  split(~term)

B0_bc <- r_fit$`(Intercept)`$estimate # average infant across all sequences in random cams
sigma_0bc <- r_fit$`sd__(Intercept)`$estimate # between-camera variability of sex ratio 

#png("ModelRDS/s_bm1bcoiba_cameras.png", width = 8, height = 7, units = 'in', res = 300)
s_bm1b_coiba %>%
  linpred_draws(tibble(locationfactor = agoutiseq_cto2$locationfactor,
                       nAF = agoutiseq_cto2$nAF), allow_new_levels = TRUE) %>%
  ungroup() %>% 
  mutate(locationfactor = fct_reorder(factor(locationfactor), .linpred, .fun = mean)) %>%
  ggplot(aes(x = logit2prob(.linpred), y = locationfactor)) +
  geom_vline(xintercept = logit2prob(B0_bc), color = cols[3], linewidth = 1) +
  stat_pointinterval(col = cols[3]) +
  theme_bw() +
  labs(x = "Model estimation of female with infants:female without infants ratio", y = "Camera location") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
#dev.off()

### H3: COMPETITION ####

## Are displacements at anvils common?
ftable(agoutiseq_jto$displacement[which(agoutiseq_jto$locationtype == "anvil")])
## of the 12 300 sequences at anvils, displacement only occurred in 92 of them 
ftable(agoutiseq_jto$displacement[which(agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$n > 1)])
## even if you filter to only sequences with more than 1 individual in, still see only 92 displacements on ~5200 sequences

# on Coiba we saw no displacements at all
ftable(agoutiseq_cto$displacement)
ftable(agoutiseq_cto$displacement[which(agoutiseq_cto$n > 1)])

## Who is being displaced?
dis_vics <- as.data.frame(as.matrix(ftable(agoutiseq_jto$victim_as)))
dis_vics$juvenile <- dis_vics$`juvenile male` + dis_vics$`juvenile unknown`
dis_vics$subadult <- dis_vics$`subadult male`+dis_vics$`subadult unknown`
dis_vics <- melt(dis_vics[, c("adult female", "adult male", "subadult", "juvenile", "unknown")])
dis_vics$prop <- dis_vics$value / 92

## who is the displacer?
dis_agg <- as.data.frame(as.matrix(ftable(agoutiseq_jto$aggressor_as)))
dis_agg$juvenile <- dis_agg$`juvenile male` + dis_agg$`juvenile unknown`
dis_agg$subadult <- dis_agg$`subadult male`
dis_agg <- melt(dis_agg[, c("adult female", "adult male", "subadult", "juvenile", "unknown")])
dis_agg$prop <- dis_agg$value / 92

dis_graph <- rbind(dis_agg, dis_vics)
dis_graph$type <- c(rep("aggressor", 5), rep("victim", 5))

# Opportunities for displacement: when an age-sex class is present in a sequence with other individuals
dis_graph$opportunity <- NA
dis_graph$opportunity[dis_graph$variable == "adult female"] <-  nrow(agoutiseq_jto[which(agoutiseq_jto$nAF > 0 & agoutiseq_jto$n > 1 & agoutiseq_jto$locationtype == "anvil"),])
dis_graph$opportunity[dis_graph$variable == "adult male"] <-  nrow(agoutiseq_jto[which(agoutiseq_jto$nAM > 0 & agoutiseq_jto$n > 1 & agoutiseq_jto$locationtype == "anvil"),])
dis_graph$opportunity[dis_graph$variable == "subadult"] <-  nrow(agoutiseq_jto[which(agoutiseq_jto$nSubadult > 0 & agoutiseq_jto$n > 1 & agoutiseq_jto$locationtype == "anvil"),])
dis_graph$opportunity[dis_graph$variable == "juvenile"] <-  nrow(agoutiseq_jto[which(agoutiseq_jto$nJuvenile > 0 & agoutiseq_jto$n > 1 & agoutiseq_jto$locationtype == "anvil"),])
dis_graph$opportunity[dis_graph$variable == "unknown"] <-  nrow(agoutiseq_jto[which(agoutiseq_jto$nUU > 0 & agoutiseq_jto$n > 1 & agoutiseq_jto$locationtype == "anvil"),])

dis_graph
dis_graph <- dis_graph[-c(5,10),] #exclude unknown age/sex
dis_graph$opportunity[which(dis_graph$type == "victim")] <- NA
dis_graph$agesex <- factor(dis_graph$variable, levels = c("adult female", "adult male", "subadult", "juvenile"))

## Figure 5: Visualization of role in displacement events per agesex class
#png("ModelRDS/displacement.png", width = 8, height = 6, units = 'in', res = 300)
ggplot(dis_graph, aes(x = agesex, y = value, group = type, fill = type)) + geom_bar(stat = "identity", position = position_dodge()) + 
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  geom_text(stat = "unique", aes(label = opportunity), position = position_dodge(width = 0), vjust = -0.5, fontface = "italic" ) +
  labs( x = "Age-sex class", y = "Number of displacement events", fill = "Role in displacement" ) +   scale_fill_grey() +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size=12),
                     legend.text = element_text(size=11)) 
#dev.off()

# further opportunity: when are females alone at anvils?
ftable(agoutiseq_jto$tooluse[which(agoutiseq_jto$nAF > 0 & agoutiseq_jto$locationtype == "anvil" & agoutiseq_jto$nAM == 0)])

### H4: SCROUNGING ####

###### Scrounging during tool use #######
agoutiseq_jto$scrounging <- ifelse(rowSums(agoutiseq_jto[,c("sc_nAF", "sc_nAM", "sc_nAU", 
                                                            "sc_nJF","sc_nJM","sc_nJU", "sc_nSF",
                                                            "sc_nSM", "sc_nSU", "sc_nUF", "sc_nUM",
                                                            "sc_nUU")])> 0 & agoutiseq_jto$n_tooluse >0, 1, 0) 
ftable(agoutiseq_jto$sc_nAF[which(agoutiseq_jto$scrounging == 1)])
## of ~96 sequences with scrounging observed, in 2 of them were adult females present
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


###### Scrounging on anvil debris #######
agoutiseq_jto$anvildebris <- ifelse(rowSums(agoutiseq_jto[,c("ad_nAF", "ad_nAM", "ad_nAU", 
                                                             "ad_nJF","ad_nJM","ad_nJU", "ad_nSF",
                                                             "ad_nSM", "ad_nSU", "ad_nUF", "ad_nUM",
                                                             "ad_nUU")])> 0, 1, 0) 
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
                          n_scrounge = anvildebris_jto, n_opp = opp_jto)

anvildebris$prop <- anvildebris$n_scrounge / anvildebris$n_opp
anvildebris$agesex <- factor(anvildebris$agesex, levels = c("adult female", "adult male","subadult", "juvenile", "unknown")) 

# bind both types of scrounging together
scr_graph <- rbind(scrounging, anvildebris)
scr_graph$type <- c(rep("scrounge", 5), rep("debris", 5))
#scr_graph <- scr_graph[-c(5,10),] #exclude unknown age/sex
scr_graph <- droplevels.data.frame(scr_graph)
scr_graph$type <- factor(scr_graph$type, levels = c("scrounge", "debris"))

## Figure version of Table 2
#png("ModelRDS/scrounging.png", width = 9, height = 7, units = 'in', res = 300)
ggplot(scr_graph, aes(x = agesex, y = prop, group = type, fill = type)) + geom_bar(stat = "identity", position = position_dodge()) + 
  geom_text(aes(label=paste(n_scrounge, n_opp, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  labs( x = "Age-sex class", y = "Number of times observed", fill = "Type of scrounging" ) +   scale_fill_grey() +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size=12),
                     legend.text = element_text(size=11)) 
#dev.off()

### H5: DIET ####
# what do females on Coiba consume?
ftable(agoutiseq_ct$tool_item[which(agoutiseq_ct$tu_nAF >0)])

# tool use in streambeds on Jicaron
tooluse_s <- agoutiseq_jt[which(agoutiseq_jt$tooluse == "TRUE" & agoutiseq_jt$locationtype == "streambed"),]
sum(tooluse_s$tu_nAM)
sum(tooluse_s$tu_nJuvenile)
sum(tooluse_s$tu_nSubadult)
sum(tooluse_s$tu_nAF)

### DESCRIPTIVES ####
###### Jicarón island #######

## How many camera trapping days
jicdays <- agoutiseq_jt
jicdays$dayloc <- paste(jicdays$locationfactor, jicdays$seqday, sep = " ")
jicdays2 <- jicdays[!duplicated(jicdays$dayloc),]

# make overview of deployments we have and their start and end days
locations_t <- data.frame(uniqueloctag = unique(agoutiseq_jt$uniqueloctag)) 
locations_t <- left_join(locations_t, agoutiseq_jt[,c("uniqueloctag", "dep_start", "dep_end", "locationfactor", "locationtype")], by = "uniqueloctag")
locations_t <- locations_t[!duplicated(locations_t$uniqueloctag),]
# take time off and keep just date variable
locations_t$dep_startday <- as.Date(locations_t$dep_start, tz = "America/Panama", "%Y-%m-%d")
locations_t$dep_endday <- as.Date(locations_t$dep_end, tz = "America/Panama", "%Y-%m-%d")
# calculate days in each deployment (round up)
locations_t$dep_days <- ceiling(difftime(locations_t$dep_end, locations_t$dep_start, units = c("days")))
# number of rows in the jicdays2 dataframe (so how many days we have)
for (i in 1:nrow(locations_t)) {
  locations_t$nrow[i] <- nrow(jicdays2[jicdays2$uniqueloctag == locations_t$uniqueloctag[i],])
}

locations_t2 <- aggregate(locations_t$dep_days, list(locationfactor  = locations_t$locationfactor, locationtype = locations_t$locationtype), FUN = sum)

# trapping days per location
sum(locations_t$dep_days[locations_t$locationtype == "streambed"])
sum(locations_t2$x) # total trapping days on Jicarón island

# How many different locations (several consecutive cameras can be placed in one location)
nrow(locations_t2)
ftable(locations_t2$locationtype)
# Average number of trapping days per location
summary(as.numeric(locations_t2$x))
# average number of trapping days per deployment
summary(as.numeric(locations_t$dep_days))
# number of deployments per location
max(as.matrix(ftable(locations_t$locationfactor)))
mean(as.matrix(ftable(locations_t$locationfactor)))
as.matrix(ftable(locations_t$locationfactor))

###### Coiba island #######
coidays <- agoutiseq_ct
coidays$dayloc <- paste(coidays$locationfactor, coidays$seqday, sep = " ")
coidays2 <- coidays[!duplicated(coidays$dayloc),]

# make overview of deployments we have and their start and end days
locations_ct <- data.frame(uniqueloctag = unique(agoutiseq_ct$uniqueloctag)) 
locations_ct <- left_join(locations_ct, agoutiseq_ct[,c("uniqueloctag", "dep_start", "dep_end", "locationfactor")], by = "uniqueloctag")
locations_ct <- locations_ct[!duplicated(locations_ct$uniqueloctag),]
# take time off and keep just date variable
locations_ct$dep_startday <- as.Date(locations_ct$dep_start, tz = "America/Panama", "%Y-%m-%d")
locations_ct$dep_endday <- as.Date(locations_ct$dep_end, tz = "America/Panama", "%Y-%m-%d")
# calculate days in each deployment (round up)
locations_ct$dep_days <- ceiling(difftime(locations_ct$dep_end, locations_ct$dep_start, units = c("days")))
# number of rows in the coidays2 dataframe (so how many days we have)
for (i in 1:nrow(locations_ct)) {
  locations_ct$nrow[i] <- nrow(coidays2[coidays2$uniqueloctag == locations_ct$uniqueloctag[i],])
}

locations_ct2 <- aggregate(locations_ct$dep_days, list(locationfactor  = locations_ct$locationfactor), FUN = sum)

sum(locations_ct2$x)

# How many locations
nrow(locations_ct2)
# Average number of trapping days per location
summary(as.numeric(locations_ct2$x))
# average number of trapping days per deployment
summary(as.numeric(locations_ct$dep_days))
# number of deployments per location
max(as.matrix(ftable(locations_ct$locationfactor)))
mean(as.matrix(ftable(locations_ct$locationfactor)))