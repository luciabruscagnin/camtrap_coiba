library(lubridate)
library(RColorBrewer)
seq <-  read.csv("agouti_output/202012211221-coiba-tool-use-sequences.csv")
obs <- read.csv("agouti_output/202012211220-coiba-tool-use-observations.csv")
beh <- read.csv("agouti_output/202012211220-coiba-tool-use-behaviours.csv")
ind <- read.csv("agouti_output/202012211222-coiba-tool-use-individuals.csv")
str(obs)
str(beh)
str(seq)
str(ind)
unique(obs$observer)
d <- obs[obs$location_label=="CEBUS-02" & obs$scientificName=="Homo",]
d <- d[c("observation_id","scientificName","individual" , "observation_notes" , "behaviours" , "observer" , "sequence_start" , "originalFilename","location_label")]
str(d)
d$sequence_start
d$datetime <- ymd_hms(d$sequence_start , tz="America/Panama")

d$tdiff_sec <- d$datetime - min(d$datetime) #time difffernce in seconds from earliest observation in this subset
d$tdiff_min <- d$tdiff_sec/(60)
d$tdiff_hours <- d$tdiff_min/60 
d$tdiff_days <- d$tdiff_hours/24

dhours(x=d$tdiff_sec)
dhours(d$datetime - min(d$datetime))

str(d)
d <- droplevels(d)
d$observer_index <- as.integer(d$observer)
plot(d$tdiff_days,d$individual , col=d$observer_index)

ind$individual <- ind$id
d <- merge(d, ind[, c("name", "individual")], by="individual" , all.x = TRUE) #keeps values from d w/ no individuals
d <- droplevels(d)
d$individual_index <- as.integer(d$name.x) # convert to integer
d$individual_index <-ifelse(is.na(d$individual_index)==TRUE , 0 , d$individual_index) #turn nas where no tool id is there to 0

plot(d$tdiff_days,d$individual_index , col=d$observer_index , yaxt='n' )
axis(2, at=d$individual_index,labels=d$name.x, col.axis="red", las=2)
axis(2, at=0,labels="No ID", las=2)

f <- c(1,2,3)
f <- (1-(f/2))*0.5

d$individual_index_jitter <- d$individual_index + f[d$observer_index]
col.pal <- brewer.pal(3,"Set1")#set color pallete
pch.pal <- c(14,19,21)
plot(d$tdiff_days,d$individual_index_jitter , col=col.pal[d$observer_index] , yaxt='n' )
axis(2, at=d$individual_index,labels=d$name.x, col.axis="red", las=2)
axis(2, at=0,labels="No ID", las=2)
legend("topleft", inset=.01 , c("Brendan" , "Meredith" , "Tamara") , fill=col.pal[unique(d$observer_index)])
