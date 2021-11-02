## Seasonality of tool use analysis

# first run agouti_cleaning script

# use agoutisequence dataframe that's cleaned on the sequence level
str(agoutisequence)

# extract the day of the sequence and also the day of the deployment start and end
# if day of sequence is in day of deployment start or end, then it's time for that. otherwise it's 24 hours
# not too fast but works
agoutisequence$dep_startday <- format(agoutisequence$dep_start, "%Y-%m-%d")
agoutisequence$dep_endday <- format(agoutisequence$dep_end, "%Y-%m-%d")
agoutisequence$seq_startday <- format(agoutisequence$seq_start, "%Y-%m-%d")
agoutisequence$dep_starttime <- hour(agoutisequence$dep_start)
agoutisequence$dep_endtime <- hour(agoutisequence$dep_end)

# maybe too coarse, only on level of hours now 
agoutisequence$exposure <- ifelse(agoutisequence$seq_startday == agoutisequence$dep_startday, 
                                  24-agoutisequence$dep_starttime, 
                                  ifelse(agoutisequence$seq_startday == agoutisequence$dep_endday,
                                         24 - agoutisequence$dep_endtime, 24))

# if tool use in sequence, then sequence duration is tool use duration
# extract sequences with tool use
agoutitool <- agoutisequence[agoutisequence$tooluse == "TRUE",]
agoutitool$tooluseduration <- agoutitool$seq_length
## need to pay attention here, because if seq_length is not available yet (due to exifdata not being pulled) then it will look the same as when there's no tool use. 
## maybe only do this after subsetting for only the tool use sequences?

# aggregate per day, summing all the tool use durations
aggregate(agoutitool$tooluseduration, by = list(date = agoutitool$seq_startday), FUN = sum)

# then match with exposure and other variables we want to have