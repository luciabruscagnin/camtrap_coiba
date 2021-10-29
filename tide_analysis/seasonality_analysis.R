## Seasonality of tool use analysis

# first run agouti_cleaning script

# use agoutisequence dataframe that's cleaned on the sequence level
str(agoutisequence)

# if tool use in sequence, then sequence duration is tool use duration
agoutisequence$tooluseduration <- ifelse(agoutisequence$tooluse == TRUE, agoutisequence$seq_length, NA)
## need to pay attention here, because if seq_length is not available yet (due to exifdata not being pulled) then it will look the same as when there's no tool use. 
## maybe only do this after subsetting for only the tool use sequences?

# extract the day of the sequence
# also the day of the deployment start and end
# if day of sequence is in day of deployment start or end, then it's time for that 
# otherwise it's 24 hours
# maybe do this on deployment info level? Calculating these variables and then matching to the agoutisequence day of sequence