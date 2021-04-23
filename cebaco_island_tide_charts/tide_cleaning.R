# Cleaning tidal data
require(stringr)
require(lubridate)
require(magrittr)

tidalgross <- read.csv("cebaco_island_tide_charts/csvs/2017_01_Cebaco.csv", skip = 9, header =  TRUE)
head(tidalgross)

# remove second and ninth column filled with weird symbols
# CHECK IF THATS THE SAME IN EACH FILE
tidalgross <- tidalgross[,-c(2,9)]
# renames columns
colnames(tidalgross) <- c("DAY", "SUN", "TIDE_1", "TIDE_2", "TIDE_3", "TIDE_4", "COEFFICIENT")

# remove white rows at the bottom
tidalgross <- tidalgross[!apply(tidalgross == "", 1, all),]

#### DAY, MONTH, YEAR ####
# first need to create month and year column from the data in the bottom of the excel
# number of days of the month can vary so want to select last column to be sure
# year = part after comma
tidalgross$YEAR <- sapply(str_split(tidalgross$DAY[nrow(tidalgross)], ","), '[', 2)

# month = between "CEBACO ISLAND" and comma (can likely do this in one step too)
tidalgross$MONTH <- sapply(str_split(tidalgross$DAY[nrow(tidalgross)], ","), '[', 1)
tidalgross$MONTH <- sapply(str_split(tidalgross$MONTH, "\n"), '[', 2)

# cut unnecessary bottom part of
tidalgross <- tidalgross[-c(nrow(tidalgross), (nrow(tidalgross)-1)),]

# keep only day number
tidalgross$DAY <- substring(tidalgross$DAY, 1, 1)
tidalgross$DAY[nrow(tidalgross)]

#### SUNRISE AND SUNSET #####
# remove all non-alphanumeric characters and extract one time
tidalgross$SUN <- str_replace_all(tidalgross$SUN, "[^[:alnum:]]", " ")

# Sunrise
# could not incorporate line below in pipe
tidalgross$SUNRISE <- sapply(str_split(tidalgross$SUN, "h "), '[', 1)
tidalgross$SUNRISE <- tidalgross$SUNRISE %>%
  str_trim() %>%
  str_replace(" ", ":") %>%
  paste("0",., sep="") # don't have it as date or time object currently, add 0 for 24h clock consistency

#alternatively, first few steps nested below
#tidalgross$SUNRISE <- str_replace((str_trim(sapply(str_split(tidalgross$SUN, "h "), '[', 1))), " ", ":")  

# Sunset
tidalgross$SUNSET <- str_replace((str_trim(sapply(str_split(tidalgross$SUN, "h "), '[', 2))), " ", ":")  
tidalgross$SUNSET <- tidalgross$SUNSET %>%
  substr(., 1, nchar(.)-2) %>%
  str_trim() %>%
  str_replace(" ", ":")

#### TIME & HEIGHT OF TIDES #####
head(tidalgross)
tidalgross$TIDE_1_HEIGHT <- sapply(str_split(tidalgross$TIDE_1, "\\n"), '[', 2)
tidalgross$TIDE_1_HEIGHT <- substr(tidalgross$TIDE_1_HEIGHT, 1, nchar(tidalgross$TIDE_1_HEIGHT)-4) 
# check if for all heights it's the last 4 characters
tidalgross$TIDE_1_TIME <- sapply(str_split(tidalgross$TIDE_1, "\\n"), '[', 1)
tidalgross$TIDE_1_TIME <- tidalgross$TIDE_1_TIME %>%
  substr(., 1, nchar(.)-2)

# the below step works for the first tide, but second tide has numbers above 10 too. 
# use if statement to only append 0 if first 2 character is under 10? is difficult. 
# maybe just remove the 0 as it's not really necessary for understanding the time (also not for R I think)
paste("0",., sep="")

# seems like you can't have a time column without the date. So consider making the sunset and sunrise columns contain the date too?
# then could combine month year day column with sunrise or sunset column to create this
# maybe something to keep for the end?
# or keep it as character as it is (but now you can't really calculate with it)

strptime(tidalgross$SUNSET, format = "%H:%M")
?as.POSIXct

# can loop time and height of tides operation over the different numbers of the tides?
#something like i in 1:4, TIDE_[i], TIDE_[i]_HEIGHT etc. Then do all 4 at once
# resolve when there's no 4th tide? add NA?

# after everything is clean try it with a few different datasets to see if things go wrong
# then write loop