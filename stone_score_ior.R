library(lubridate)
library(RColorBrewer)

seq <-  read.csv("agouti_output/202012211221-coiba-tool-use-sequences.csv")
obs <- read.csv("agouti_output/202012211220-coiba-tool-use-observations.csv")
beh <- read.csv("agouti_output/202102081715-coiba-tool-use-behaviours.csv")
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

str(d)
d <- droplevels(d)
d$observer_index <- as.integer(as.factor(d$observer))
plot(d$tdiff_days,as.integer(as.factor(d$individual)) , col=d$observer_index)

ind$individual <- ind$id #create new column to merge
d <- merge(d, ind[, c("name", "individual")], by="individual" , all.x = TRUE) #keeps values from d w/ no individuals
d <- droplevels(d)
d$individual_index <- as.integer(as.factor(d$name)) # convert to integer
d$individual_index <-ifelse(is.na(d$individual_index)==TRUE , 0 , d$individual_index) #turn nas where no tool id is there to 0


###lets add behaviors
beh2 <- beh[, c("name", "id")]
unique(beh$id)
unique(nchar(d$behaviours))
#extract subelements of behaviors into own columns
#i could make this a loop, but life is too short
d$beh_ssh_1 <- ifelse( nchar(d$behaviours)>2 , substr(d$behaviours, 2, 37) , NA ) ##first behavior
d$beh_ssh_2 <- ifelse( nchar( d$behaviours)>38 , substr(d$behaviours, 2+37, 37*2 ) , NA ) ##2 behav
d$beh_ssh_3 <- ifelse( nchar(d$behaviours)>75 , substr(d$behaviours, 2+37*2, 37*3 ) , NA ) ##3 behav
d$beh_ssh_4 <- ifelse( nchar(d$behaviours)>112 , substr(d$behaviours, 2+37*3, 37*4 ) , NA ) ##4 behav
unique(d[c("beh_ssh_1" , "beh_ssh_2")])

#unique behaviors across all 4 columns of behaviors
beh_index_key <- as.data.frame(cbind (
  sort(unique(c(d$beh_ssh_1,d$beh_ssh_2,d$beh_ssh_3,d$beh_ssh_4))) , 
  as.integer(as.factor(sort(unique(c(d$beh_ssh_1,d$beh_ssh_2,d$beh_ssh_3,d$beh_ssh_4)))))
  ))
beh_index_key[,1] <- as.character(beh_index_key[,1])
beh_index_key[,2] <- as.integer(beh_index_key[,2])

#match using key
d$beh_1_index <- match(d$beh_ssh_1, beh_index_key[,"V1"])
d$beh_2_index <- match(d$beh_ssh_2, beh_index_key[,"V1"])
d$beh_3_index <- match(d$beh_ssh_3, beh_index_key[,"V1"])
d$beh_4_index <- match(d$beh_ssh_4, beh_index_key[,"V1"])
beh_ss <- beh[c("id","name")]

d <- merge( d, beh_ss, by.x = "beh_ssh_1", by.y = "id" , all.x=TRUE )
colnames(d)[ncol(d)] <- "beh_1"
d <- merge( d, beh_ss, by.x = "beh_ssh_2", by.y = "id" , all.x=TRUE )
colnames(d)[ncol(d)] <- "beh_2"
d <- merge( d, beh_ss, by.x = "beh_ssh_3", by.y = "id" , all.x=TRUE )
colnames(d)[ncol(d)] <- "beh_3"
d <- merge( d, beh_ss, by.x = "beh_ssh_4", by.y = "id" , all.x=TRUE )
colnames(d)[ncol(d)] <- "beh_4"


plot(d$tdiff_days,d$individual_index , col=d$observer_index , yaxt='n' )
axis(2, at=d$individual_index,labels=d$name.x, col.axis="red", las=2)
axis(2, at=0,labels="No ID", las=2)

f <- c(1,2,3)
f <- (1-(f/2))*0.5

d$individual_index_jitter <- d$individual_index + f[d$observer_index] #jitter for viz comp
col.pal <- brewer.pal(3,"Set1")#set color pallete

#ploit with no behavioral comparisons
plot(d$tdiff_days,d$individual_index_jitter , col=col.pal[d$observer_index] , yaxt='n' , ylab='' )
axis(2, at=d$individual_index,labels=d$name.x, col.axis="red", las=2)
axis(2, at=0,labels="No ID", las=2)
legend("topleft", inset=.01 , c("Brendan" , "Meredith" , "Tamara") , fill=col.pal[unique(d$observer_index)])
#plot w/ unique poiunts per behavior

#ab line from tool on anvil to tool off anvil
beh_index_key2 <-  merge( beh_index_key, beh_ss, by.x = "V1", by.y = "id" )
beh_index_key2$pch <- c(19,0,NA,NA,NA,1,NA,NA,15)

plot(d$tdiff_days,d$individual_index_jitter , col=col.pal[d$observer_index] , yaxt='n' , ylab='' , pch=beh_index_key2$pch[d$beh_1_index] , cex=0.7)
points(d$tdiff_days,d$individual_index_jitter , col=col.pal[d$observer_index] , yaxt='n' , ylab='' , pch=beh_index_key2$pch[d$beh_2_index] , cex=0.7)
points(d$tdiff_days,d$individual_index_jitter , col=col.pal[d$observer_index] , yaxt='n' , ylab='' , pch=beh_index_key2$pch[d$beh_3_index] , cex=0.7)
points(d$tdiff_days,d$individual_index_jitter , col=col.pal[d$observer_index] , yaxt='n' , ylab='' , pch=beh_index_key2$pch[d$beh_4_index] , cex=0.7)
axis(2, at=d$individual_index,labels=d$name.x, col.axis="red", las=2)
axis(2, at=0,labels="No ID", las=2)
legend("topleft", inset=.01 , c("Brendan" , "Meredith" , "Tamara") , fill=col.pal[unique(d$observer_index)])
#need per obs behavior column
d[,26:29]
d$bring_tool <-d$tool_on_anvil <- d$take_tool <- d$tool_off_anvil <- 0
beh_index_key2
# 
#   for(j in 1:nrow(d)){
#     for (i in which(names(d)=="beh_1"):which( names(d)=="beh_4")){
#     # d$bring_tool[j] <- ifelse( d[j,i]=="*HS: Bring tool" , 1 , d$bring_tool[j])
#     # d$take_tool[j] <- ifelse( d[j,i]=="*HS: Take tool" , 1 , d$take_tool[j])
#     d$tool_on_anvil[j] <- ifelse( d[j,i]=="*HS: Tool on anvil" , 1 , d$tool_on_anvil[j])
#     # d$tool_off_anvil[j] <- ifelse( d[j,i]=="*HS: Tool off anvil" , 1 , d$tool_off_anvil[j])
#   }
# }


d$tool_on_anvil <- ifelse(d["beh_1"]=="*HS: Tool on anvil" |
                            d["beh_2"]=="*HS: Tool on anvil" |
                            d["beh_3"]=="*HS: Tool on anvil"|
                            d["beh_4"]=="*HS: Tool on anvil", 1 ,0)

d$tool_off_anvil <- ifelse(d["beh_1"]=="*HS: Tool off anvil" |
                            d["beh_2"]=="*HS: Tool off anvil" |
                            d["beh_3"]=="*HS: Tool off anvil"|
                            d["beh_4"]=="*HS: Tool off anvil", 1 ,0 )

d$bring_tool <- ifelse(d["beh_1"]=="*HS: Bring tool" |
                             d["beh_2"]=="*HS: Bring tool" |
                             d["beh_3"]=="*HS: Bring tool" |
                             d["beh_4"]=="*HS: Bring tool" , 1 ,0 )

d$take_tool <- ifelse(d["beh_1"]=="*HS: Take tool" |
                         d["beh_2"]=="*HS: Take tool" |
                         d["beh_3"]=="*HS: Take tool" |
                         d["beh_4"]=="*HS: Take tool" , 1 ,0 )

d$tool_off_anvil[which(is.na(d$tool_off_anvil)== TRUE )] <- 0
d$tool_on_anvil[which(is.na(d$tool_on_anvil)== TRUE )] <- 0
d$bring_tool[which(is.na(d$bring_tool)== TRUE )] <- 0
d$take_tool[which(is.na(d$take_tool)== TRUE )] <- 0

tmin <- min(d$tdiff_days[d$individual_index==1 & d$bring_tool==1 | d$tool_on_anvil==1 & d$observer_index==1])
tmax <- max(d$tdiff_days[d$individual_index==1 & d$take_tool==1 | d$tool_off_anvil==1 & d$observer_index==1])

for (obs in 1:max(d$observer_index)){
  for (toolid in min(d$individual_index):max(d$individual_index) ){
  tmin <- min(d$tdiff_days[d$individual_index==toolid & d$bring_tool==1 | d$tool_on_anvil==1 & d$observer_index==obs])
  tmax <- max(d$tdiff_days[d$individual_index==toolid & d$take_tool==1 | d$tool_off_anvil==1 & d$observer_index==obs])
  segments(tmin,toolid,tmax,toolid)# we might want to exclude some behaviors 
  }
}

tmin <- min(d$tdiff_days[d$individual_index==6 & d$bring_tool==1 | d$tool_on_anvil==1 & d$observer_index==1])
tmax <- max(d$tdiff_days[d$individual_index==6 & d$take_tool==1 | d$tool_off_anvil==1 & d$observer_index==1])

tmin <- min(d$tdiff_days[d$individual_index_jitter==1 & d$bring_tool==1 | d$tool_on_anvil==1 & d$observer_index==1])
tmax <- max(d$tdiff_days[d$individual_index_jitter==1 & d$take_tool==1 | d$tool_off_anvil==1 & d$observer_index==1])
segments(tmin,1,tmax,1)# we might want to exclude some behaviors 
unique(d$beh_1)
min(d$tdiff_days[])
#####something else to see what are most common species for AI
sort(table(obs$scientificName))

write.csv(d , "tool behav excerpts.csv")
