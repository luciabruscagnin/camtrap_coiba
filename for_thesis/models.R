#agoutisequence_c contains also observations without capuchin detection
library(dplyr)
library(lme4)
library(devtools)
library(marginaleffects)
library(ggeffects)
library(ggplot2)
library(tidyverse)
setwd("C:/Users/Lucia/Desktop/IRT3 - thesis/camtrap_coiba-main (github)/camtrap_coiba-main")
agoutisequence_c<-read.csv("agoutisequence_c.csv")
str(agoutisequence_c)

###clean agoutisequence_c, keep only columns with relevant information
#specify the columns to keep
columns_to_keep <- c(
  "observationID", "deploymentID", "sequenceID", 
  "cameraSetup", "scientificName", "count", "sex", "behaviour", 
  "individualID", "locationID", 
  "locationName", "tags", "seq_start", "seq_end", "seq_length", 
  "mediatype", "captureMethod", "dep_start", "dep_end", 
  "dep_length_hours", "month", "season", "longitude", "latitude", 
  "island", "side", "tool_anvil", "tool_site", "streambed", 
  "capuchin", "agesex", "n", "name", "nAdult", "nSubadult", 
  "nJuvenile", "nFemales", "nMales", "n_inspect", "tooluse", 
  "n_tooluse", "seqday", "dep_startday", "dep_endday", 
  "seq_startday", "dep_starttime", "dep_endtime", "exposure", 
  "hour", "toolusers", "locationfactor", "depdays", "depnr", 
  "islandside", "picksetup", "uniqueloctag"
)
agoutisequence_c <- agoutisequence_c[, columns_to_keep]


#I have 160 deployments
unique(agoutisequence_c$deploymentID)

###subset only observations with capuchin detection
with_capuchins<-agoutisequence_c[agoutisequence_c$capuchin==1,]
ordered_with_capuchin <- with_capuchins %>%
  arrange(seq_start)

###create a new column date, extracting date from seq_start
#convert the 'seq_start' column to a datetime format
library(lubridate)
ordered_with_capuchin$seq_start <- ymd_hms(ordered_with_capuchin$seq_start)
#create a new column 'date' containing only the date part
ordered_with_capuchin$date <- as.Date(ordered_with_capuchin$seq_start)

###detections/day per each unique location per day
ordered_with_capuchin <- ordered_with_capuchin %>%
  group_by(uniqueloctag, date) %>%
  summarise(detections_per_day = n()) %>%
  left_join(ordered_with_capuchin, by = c("uniqueloctag", "date"))
#I have now the ordered_with_capuchin showing the number of detections per location per day

###inspection per each unique location per day
ordered_with_capuchin <- ordered_with_capuchin %>%
  group_by(uniqueloctag, date) %>%
  mutate(insp_day = sum(n_inspect == 1))

###now I want a new column with inpections/detections
ordered_with_capuchin <- ordered_with_capuchin %>%
  group_by(date, uniqueloctag) %>%
  mutate(inspection_rate = insp_day / sum(capuchin == 1))

###not sure everything worked fine I need to clean my db a bit
capuchin_cleaned <- ordered_with_capuchin[, c("date","observationID", "deploymentID", "sequenceID", 
                                              "cameraSetup", "sex", "behaviour", 
                                              "individualID", "locationID", 
                                              "tags", "seq_start", "seq_end",
                                              "month", "season", "longitude", "latitude", 
                                              "island", "side", "tool_anvil", "tool_site", "streambed", 
                                              "capuchin", "agesex", "nAdult", "nSubadult", 
                                              "nJuvenile", "nFemales", "nMales", "n_inspect", "tooluse", 
                                              "seqday", "dep_startday", "dep_endday", 
                                              "exposure", "hour", "toolusers", "locationfactor", "depdays", "depnr", 
                                              "islandside", "picksetup", "uniqueloctag", "detections_per_day", 
                                              "insp_day", "inspection_rate")]
###database on which I can run the models
#capuchin_cleaned (only with capuchin detections)



#which are the unique locations? 124 different locations
unique(agoutisequence_c$locationID)

#for CEBUS-01
#plot number of inspections over time
#filter data for locationName == "CEBUS-01" and behavior == "BS: inspection"
as.factor(agoutisequence_c$locationName)
cebus_inspection_data_cebus01 <- agoutisequence_c %>%
  filter(locationName == "CEBUS-01" & n_inspect ==1)
unique(cebus_inspection_data_cebus01$seqday)

plot(capuchin_cleaned$inspection_rate ~ capuchin_cleaned$date)
capuchin_cleaned

ccr1<-capuchin_cleaned[which(capuchin_cleaned$tags=="R1"),]
str(ccr1)
for (i in sort(unique(ccr1$locationID))){
  plot(inspection_rate ~ date , data=ccr1[ccr1$locationID==i,])
}
unique(ccr1$locationID)
#I have 15 different locationsID in R1, so 15 plots

###inspection_rate against date of R1###
Sys.setlocale("LC_TIME", "en_US.UTF-8") #set language to english
plot(inspection_rate ~ date , data=ccr1, main = "R1") 
#Is the linear model appropriate? Let's check the assumptions.
m1 <- lm(inspection_rate ~ date , data=ccr1) #it is bad because the date seems to have a positive effect on the inspection rate, this is due to the fact that it does not consider 0s (no inspections) in between 
#1. Linearity: we are far from linearity, a lm is not appropriate
lines(lowess(ccr1$date, ccr1$inspection_rate), col = "red") #the lowess line highlights the absence of linearity
plot(m1, which = 1)
#2. Zero inflation
#count the number of zeros, there are 178/940 are 0s.
zero_count <- sum(ccr1$inspection_rate == 0)
#histogram of the distribution of Inspection rate
hist(ccr1$inspection_rate, breaks = 20, main = "Distribution of Inspection Rate")
#3. Normality of residuals
qqnorm(residuals(m1))
qqline(residuals(m1))
shapiro.test(residuals(m1)) #low p-value, almost 0, support agains H0 of linearity, the residuals from the lm are NOT normally distributed

###Let's start with the models
##I still use the simplest model as a starting point, so the lm
###m1
m1 <- lm(inspection_rate ~ date , data=ccr1) #it is bad because the date seems to have a positive effect on the inspection rate, this is due to the fact that it does not consider 0s (no inspections) in between 
summary(m1) #bad do not do, we have already eexplained why
###m2, not completed yet
m2 <- glm((insp_day ~ date , data=ccr1 , weights=ccr1$)) #put the weights as the total number of capuchin detections per location per day
summary(m2)
###m3
m3 <- glm(insp_day ~ date, data=ccr1, family = "poisson") #start with this
summary(m3)
plot(m3)
plot_predictions(m3, condition = "date") #does not seem to work
##I found another code to plot predictions(https://stats.stackexchange.com/questions/487947/plotting-predicted-values-with-lmer)
#add predicted values to the original dataset
ccr1 <- ccr1 %>%
  rowwise() %>%
  mutate(pred = predict(m3, newdata = data.frame(date = date), type = "response"))
#plot predictions
m3 <- glm(insp_day ~ date, data=ccr1, family = "poisson") #start with this
library(ggplot2)
ccr1 %>%
  ggplot(aes(date, pred, color = uniqueloctag)) +
  geom_line() +
  geom_point(aes(date, insp_day, color = uniqueloctag)) +
  labs(title = "Predicted Values by uniqueloctag",
       x = "Date",
       y = "Predicted Value") +
  theme_minimal()

###m3v, in this model the slope is different for each unique loctag
m3v <- glmer(insp_day ~ date + (1|uniqueloctag), data=ccr1, family = "poisson")
summary(m3v)
plot(m3v)
##plot predictions using the same code as before
#add predicted values to the original dataset
ccr1 <- ccr1 %>%
  rowwise() %>%
  mutate(pred = predict(m3v, newdata = data.frame(date = date, uniqueloctag = uniqueloctag), type = "response"))
#plot predictions
library(ggplot2)
ccr1 %>%
  ggplot(aes(date, pred, color = uniqueloctag)) +
  geom_line() +
  geom_point(aes(date, insp_day, color = uniqueloctag)) +
  labs(title = "Predicted Values by uniqueloctag",
       x = "Date",
       y = "Predicted Value") +
  theme_minimal()
###m4
m4 <- glmer(insp_day ~ date + tool_site + (1|uniqueloctag), data=ccr1, family = "poisson")
summary(m4)
plot(m4)
##plot predictions, always same code
ccr1 <- ccr1 %>%
  rowwise() %>%
  mutate(pred = predict(m4, newdata = data.frame(date = date, tool_site = tool_site, uniqueloctag = uniqueloctag), type = "response"))
library(ggplot2)
ccr1 %>%
  ggplot(aes(date, pred, color = uniqueloctag)) +
  geom_line() +
  geom_point(aes(date, insp_day, color = uniqueloctag)) +
  labs(title = "Predicted Values by uniqueloctag",
       x = "Date",
       y = "Predicted Value") +
  theme_minimal()
##m4 but distinguish tool site from non-tool
ccr1 <- ccr1 %>%
  rowwise() %>%
  mutate(pred = predict(m4, newdata = data.frame(date = date, tool_site = tool_site, uniqueloctag = uniqueloctag), type = "response"))
#plot predictions with different linetypes for tool_site levels
library(ggplot2)
ccr1 %>%
  ggplot(aes(date, pred, color = uniqueloctag, linetype = factor(tool_site))) +
  geom_line() +
  geom_point(aes(date, insp_day, color = uniqueloctag, shape = factor(tool_site)), size = 2) +
  labs(title = "Predicted Values by uniqueloctag",
       x = "Date",
       y = "Predicted Value") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_shape_manual(values = c(16, 17)) +
  theme_minimal()
unique(ccr1$tool_site) #1 and 0, so solid and 16 (circles) is for tool-using, dashed and 17 (triangles) is for non-tool using
###m4v, date effect depends on the tool or non-tool
m4v <- glmer(insp_day ~ date * tool_site + (1|uniqueloctag), data=ccr1, family = "poisson")
summary(m4v)
plot(m4v)
##plot predictions
ccr1 <- ccr1 %>%
  rowwise() %>%
  mutate(pred = predict(m4v, newdata = data.frame(date = date, tool_site = tool_site, uniqueloctag = uniqueloctag), type = "response"))
library(ggplot2)
ccr1 %>%
  ggplot(aes(date, pred, color = uniqueloctag, linetype = factor(tool_site))) +
  geom_line() +
  geom_point(aes(date, insp_day, color = uniqueloctag, shape = factor(tool_site)), size = 2) +
  labs(title = "Predicted Values by uniqueloctag",
       x = "Date",
       y = "Predicted Value") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_shape_manual(values = c(16, 17)) +
  theme_minimal()

####plot main fixed effect per tool or non-tool against the data
##I have started doing it for m3v but I am a bit unsure on what I am plotting
###m3v
summary(m3v)
ranef(m3v) #I don't get this
library(ggplot2)

#create a new dataset with date and predicted values for plotting
plot_data_m3v <- expand.grid(
  date = seq(min(ccr1$date), max(ccr1$date), by = "days"),
  uniqueloctag = unique(ccr1$uniqueloctag),
  tool = c(0, 1)
)
#add predicted values to the new dataset
plot_data_m3v$pred <- predict(m3v, newdata = plot_data_m3v, type = "response")
#plot main fixed effect of tool, I can see that per all the locations the inpection rate is higher for tool-users(?)
ggplot(plot_data_m3v, aes(date, pred, color = factor(tool))) +
  geom_line() +
  geom_point(aes(shape = factor(tool), fill = factor(tool)), size = 2) +
  labs(title = "Main Fixed Effect of Tool",
       x = "Date",
       y = "Predicted Value",
       color = "Tool") +
  scale_shape_manual(values = c(16, 17)) +  # Shapes for 0 and 1
  theme_minimal()

##add slopes, the shapes make the graph difficult to read
#plot main fixed effect per tool with separate slopes for each uniqueloctag. The blue slope hides the one for the non-tool users.
ggplot(plot_data_m3v, aes(date, pred, color = factor(tool))) +
  geom_line() +
  geom_jitter(aes(shape = factor(tool)), size = 2, position = position_jitter(width = 0.2), show.legend = FALSE) +
  geom_smooth(aes(group = interaction(uniqueloctag, tool), method = "lm", se = FALSE)) +
  labs(title = "Main Fixed Effect of Tool",
       x = "Date",
       y = "Predicted Value",
       color = "Tool") +
  scale_shape_manual(values = c(16, 17)) +  # Shapes for 0 and 1
  facet_wrap(~ uniqueloctag, scales = "free") +  # Separate slopes for each uniqueloctag
  theme_minimal()

#trying to solve it
#reorder levels of the "tool" variable
plot_data_m3v$tool <- factor(plot_data_m3v$tool, levels = c(1, 0))

#plot main fixed effect per tool with separate colors for each uniqueloctag
ggplot(plot_data_m3v, aes(date, pred, color = factor(tool))) +
  geom_line(size = 1.5) +  # Increased line thickness
  geom_smooth(aes(group = interaction(uniqueloctag), linetype = factor(tool), color = factor(tool)), method = "lm", se = FALSE, size = 1.5) +
  labs(title = "Main Fixed Effect of Tool",
       x = "Date",
       y = "Predicted Value",
       color = "Tool") +
  scale_color_manual(values = c("darkblue", "red")) +  # Colors for tool 1 and tool 0
  facet_wrap(~ uniqueloctag, scales = "free") +  # Separate panels for each uniqueloctag
  theme_minimal()

##the shapes overlap...I am thinking how to represent it
ggplot(plot_data_m3v, aes(date, pred, color = factor(uniqueloctag), linetype = factor(tool))) +
  geom_line() +
  geom_point(aes(shape = factor(tool)), size = 2) +
  labs(title = "Main Fixed Effect of Tool",
       x = "Date",
       y = "Predicted Value",
       color = "Uniqueloctag",
       linetype = "Tool") +
  scale_shape_manual(values = c(16, 17)) +  # Shapes for 0 and 1
  theme_minimal()

###that's what I have done so far





