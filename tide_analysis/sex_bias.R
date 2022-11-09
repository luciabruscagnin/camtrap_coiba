## Sex bias in tool use
## MPI-AB; Z Goldsborough

## STEP 1: Run "agouti_cleaning.R" script and its dependencies (1. "exiftempseq_cleaning.R" 2. "tide_cleaning.R")

# start with the agoutisequence dataframe that's cleaned and aggregated to the sequence level 
# alternatively have agoutiselect2 data that is on level of day-hour with zero's added in

## Questions/Hypotheses

# H1 : Females are less terrestrial than males
## P1a: see less females on camera traps than males and juveniles
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
# need to either have repeated rows with nr and then factor for adult male/female etc


head(agoutisequence_c)

agoutiseq_jt <- agoutisequence_c[which(agoutisequence_c$tool_site == 1 & agoutisequence_c$island == "Jicaron"),]

agoutiseq_jt$locationtype <- as.factor(ifelse(agoutiseq_jt$streambed == 1, "streambed", 
                                    ifelse(agoutiseq_jt$tool_anvil == 1, "anvil", "random")))

### Comparison of male/female detection rates at anvils vs streambed vs random cams vs grid cameras
ggplot(agoutiseq_jt, aes(x = locationtype, y = nAF)) + geom_violin()
ggplot(agoutiseq_jt, aes(x = locationtype, y = nAM)) + geom_violin()

## need to do this differently and turn it into rates rather than pure counts. Think about it? 
head(agoutiseq_jt)


### Scrounging rates of adult females compared to juveniles and adult males (at tool use anvils)
### Anvil debris consumption of adult females compared to juveniles and adult males

### Displacement rates at anvils and who is being displaced




