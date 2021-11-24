# CLEANING TIDAL CSVS
# Used packages
require(stringr)
require(lubridate) # not used yet, but necessary if we potentially want to use date objects
require(magrittr)
require(rlist)
require(reshape2)

# check if working directory is set okay
setwd("~/Git/camtrap_coiba")

# make list of all tidal csvs in folder
tidal_files <- list.files(path = "cebaco_island_tide_charts/csvs")

# function to read tidal csvs with parameters we want
read.csv_tidal <- function(x) {
  read.csv(file = paste("cebaco_island_tide_charts/csvs/", x, sep = ""), header = FALSE, blank.lines.skip = FALSE)
}

# apply to all of our csvs
tidal_data <- lapply(tidal_files, read.csv_tidal)

#### FUNCTION TO CLEAN THE TIDAL DATA #####
# tidalgross = your terribly formatted tidal dataset
TideClean <- function(tidalgross) {
  
  ##### GENERAL CLEAN-UP #####
  # some files seem to have a 10th column with only NAs. Remove that one if it's there 
  tidalgross <- tidalgross[, colSums(is.na(tidalgross)) != nrow(tidalgross)]
  # remove second and ninth column filled with weird symbols
  tidalgross <- tidalgross[, -c(2,9)]
  # renames columns to things we can actually use
  colnames(tidalgross) <- c("DAY", "SUN", "TIDE_1", "TIDE_2", "TIDE_3", "TIDE_4", "COEFFICIENT")
  
  #### DAY, MONTH, YEAR ####
  # now we need to do some witchery to find the month/year, which can be in two places
  # first want to get rid of blank rows
  # set blank values to NA
  tidalgross[tidalgross == ""] <- NA
  # remove any rows that are NA
  tidalgross <- tidalgross[rowSums(is.na(tidalgross[ , 0:ncol(tidalgross)])) < ncol(tidalgross), ]
  
  # Option 1: month year is at the top of the csv, in row 3
  # make this value a new column (if there's no month year there, this will be the "DAY" title)
  tidalgross$MONTHYEAR <- tidalgross$DAY[3]
  # remove unnecessary rows at top (this shouldn't vary but might vary with new csvs?)
  tidalgross <- tidalgross[-c(1:4),]
  
  # Option 2: month year is at the bottom of the excel, either after "CEBACO ISLAND" or on its own
  # so if the MONTHYEAR column just says "DAY", use this bottom row
  # Otherwise use the data from the MONTHYEAR column
  if(sum(tidalgross$MONTHYEAR == "DAY") == nrow(tidalgross)) { 
    # remove the CEBACO ISLAND part (as this is not in all of the csvs)
    tidalgross$DAY[nrow(tidalgross)] <- str_replace(tidalgross$DAY[nrow(tidalgross)], "CEBACO ISLAND\n", "")
    # day and year are separated by comma
    tidalgross$YEAR <- sapply(str_split(tidalgross$DAY[nrow(tidalgross)], ","), '[', 2)
    tidalgross$MONTH <- sapply(str_split(tidalgross$DAY[nrow(tidalgross)], ","), '[', 1) 
  } else { 
    tidalgross$YEAR <- sapply(str_split(tidalgross$MONTHYEAR, ","), '[', 2) 
    tidalgross$MONTH <- sapply(str_split(tidalgross$MONTHYEAR, ","), '[', 1)
  } 
  
  # cut unnecessary bottom part of and remove MONTHYEAR (column 8)
  tidalgross <- tidalgross[-c(nrow(tidalgross), (nrow(tidalgross)-1)), -8]
  # keep only day number (day of the week is always 3 characters)
  tidalgross$DAY <- substr(tidalgross$DAY, 1, nchar(tidalgross$DAY)-3)
  # remove rows that have a blank value for DAY to get all dataframes equally trimmed
  tidalgross <- tidalgross[!is.na(tidalgross$DAY), ]
  
  #### SUNRISE AND SUNSET #####
  # remove all non-alphanumeric characters and extract one time
  tidalgross$SUN <- str_replace_all(tidalgross$SUN, "[^[:alnum:]]", " ")
  
  # Sunrise
  # could not incorporate line below in pipe, so do that first
  tidalgross$SUNRISE <- sapply(str_split(tidalgross$SUN, "h "), '[', 1)
  tidalgross$SUNRISE <- tidalgross$SUNRISE %>%
    str_trim() %>%
    str_replace(" ", ":") %>%
    paste("0",., sep="") # don't have it as date or time object currently, add 0 for 24h clock consistency
  
  # Sunset
  tidalgross$SUNSET <- str_replace((str_trim(sapply(str_split(tidalgross$SUN, "h "), '[', 2))), " ", ":")  
  tidalgross$SUNSET <- tidalgross$SUNSET %>%
    substr(., 1, nchar(.)-2) %>%
    str_trim() %>%
    str_replace(" ", ":")
  
  #### TIME & HEIGHT OF TIDES #####
  # wanted to loop over the different tides, but not sure how to use for loop with column names. 
  # or how to write a function doing this, so for now do each tide separately. 
  ## TIDE 1
  tidalgross$TIDE_1_HEIGHT <- sapply(str_split(tidalgross$TIDE_1, "\\n"), '[', 2)
  tidalgross$TIDE_1_HEIGHT <- substr(tidalgross$TIDE_1_HEIGHT, 1, nchar(tidalgross$TIDE_1_HEIGHT)-4) 
  # check if for all heights it's the last 4 characters (appears correct)
  tidalgross$TIDE_1_TIME <- sapply(str_split(tidalgross$TIDE_1, "\\n"), '[', 1)
  tidalgross$TIDE_1_TIME <- tidalgross$TIDE_1_TIME %>%
    substr(., 1, nchar(.)-2)
  
  # use if statement to only append 0 if first 2 character is under 10
  x <- as.numeric(substr(tidalgross$TIDE_1_TIME, 1, nchar(tidalgross$TIDE_1_TIME)-3)) 
  for (j in 1:length(x)) {
    print(j)
    tidalgross$TIDE_1_TIME[j] <- ifelse((x[j] < 10), (paste("0",tidalgross$TIDE_1_TIME[j], sep = "")), tidalgross$TIDE_1_TIME[j]) 
  } 
  
  ## TIDE 2
  tidalgross$TIDE_2_HEIGHT <- sapply(str_split(tidalgross$TIDE_2, "\\n"), '[', 2)
  tidalgross$TIDE_2_HEIGHT <- substr(tidalgross$TIDE_2_HEIGHT, 1, nchar(tidalgross$TIDE_2_HEIGHT)-4) 
  # check if for all heights it's the last 4 characters
  tidalgross$TIDE_2_TIME <- sapply(str_split(tidalgross$TIDE_2, "\\n"), '[', 1)
  tidalgross$TIDE_2_TIME <- tidalgross$TIDE_2_TIME %>%
    substr(., 1, nchar(.)-2)
  
  # use if statement to only append 0 if first 2 character is under 10
  x <- as.numeric(substr(tidalgross$TIDE_2_TIME, 1, nchar(tidalgross$TIDE_2_TIME)-3)) 
  for (j in 1:length(x)) {
    print(j)
    tidalgross$TIDE_2_TIME[j] <- ifelse((x[j] < 10), (paste("0",tidalgross$TIDE_2_TIME[j], sep = "")), tidalgross$TIDE_2_TIME[j]) 
  } 
  
  ## TIDE 3
  tidalgross$TIDE_3_HEIGHT <- sapply(str_split(tidalgross$TIDE_3, "\\n"), '[', 2)
  tidalgross$TIDE_3_HEIGHT <- substr(tidalgross$TIDE_3_HEIGHT, 1, nchar(tidalgross$TIDE_3_HEIGHT)-4) 
  # check if for all heights it's the last 4 characters
  tidalgross$TIDE_3_TIME <- sapply(str_split(tidalgross$TIDE_3, "\\n"), '[', 1)
  tidalgross$TIDE_3_TIME <- tidalgross$TIDE_3_TIME %>%
    substr(., 1, nchar(.)-2)
  # are no times below 10 in the third tide so removed for loop
  
  ## TIDE 4
  tidalgross$TIDE_4_HEIGHT <- sapply(str_split(tidalgross$TIDE_4, "\\n"), '[', 2)
  tidalgross$TIDE_4_HEIGHT <- substr(tidalgross$TIDE_4_HEIGHT, 1, nchar(tidalgross$TIDE_4_HEIGHT)-4) 
  # check if for all heights it's the last 4 characters
  tidalgross$TIDE_4_TIME <- sapply(str_split(tidalgross$TIDE_4, "\\n"), '[', 1)
  tidalgross$TIDE_4_TIME <- tidalgross$TIDE_4_TIME %>%
    substr(., 1, nchar(.)-2)
  # are no times below 10 in the fourth tide so removed for loop

  ##### COEFFICIENT AND AVERAGE #####
  # simply split up numeric and alphabetical characters
  tidalgross$COEF <- as.numeric(str_extract(tidalgross$COEFFICIENT, "[0-9]+"))
  # want to take the "very high" in its entirety so take out the spaces
  tidalgross$COEFFICIENT <- str_replace_all(tidalgross$COEFFICIENT, " ", "")
  tidalgross$AVERAGE <- (str_extract(tidalgross$COEFFICIENT, "[aA-zZ]+"))
  
  # make everything nice and tidy
  tidalclean <- tidalgross[,c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_1_TIME", "TIDE_1_HEIGHT",
                              "TIDE_2_TIME", "TIDE_2_HEIGHT", "TIDE_3_TIME", "TIDE_3_HEIGHT", "TIDE_4_TIME",
                              "TIDE_4_HEIGHT", "COEF", "AVERAGE")]
  
    return(tidalclean) # return the beautiful cleaned dataframe
}

# clean all of the data frames inside our list
tidal_data_clean <- lapply(tidal_data, TideClean)

# turn this list into one master dataframe
Tides <- list.rbind(tidal_data_clean)

# check if you got all the dates per year
table(Tides$YEAR)

#write.csv(Tides, "cebaco_island_tide_charts/TidesClean.csv")

##### ADVANCED CLEANUP #####
# High or low tide?
Tides$TIDE_1_HEIGHT <- as.numeric(Tides$TIDE_1_HEIGHT)
Tides$TIDE_2_HEIGHT <- as.numeric(Tides$TIDE_2_HEIGHT)
Tides$TIDE_3_HEIGHT <- as.numeric(Tides$TIDE_3_HEIGHT)
Tides$TIDE_4_HEIGHT <- as.numeric(Tides$TIDE_4_HEIGHT)

hist(Tides$TIDE_1_HEIGHT)
# low tide is always below 2, high tide above 2 meters

# Go from wide to long format
# drop coef & average
TidesW <- Tides[, -c(14,15)]

# split into 4 datasets
Tides1 <- TidesW[, c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_1_TIME", "TIDE_1_HEIGHT")]
Tides2 <- TidesW[, c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_2_TIME", "TIDE_2_HEIGHT")]
Tides3 <- TidesW[, c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_3_TIME", "TIDE_3_HEIGHT")]
Tides4 <- TidesW[, c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_4_TIME", "TIDE_4_HEIGHT")]

Tides1$TIDE_NR <- 1
Tides2$TIDE_NR <- 2
Tides3$TIDE_NR <- 3
Tides4$TIDE_NR <- 4

colnames(Tides1) <- c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_TIME", "TIDE_HEIGHT", "TIDE_NR")
colnames(Tides2) <- c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_TIME", "TIDE_HEIGHT", "TIDE_NR")
colnames(Tides3) <- c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_TIME", "TIDE_HEIGHT", "TIDE_NR")
colnames(Tides4) <- c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_TIME", "TIDE_HEIGHT", "TIDE_NR")

TidesF <- rbind(Tides1, Tides2, Tides3, Tides4)

# turn tide time into date/time object
TidesF$DATE <- paste(TidesF$YEAR, TidesF$MONTH, TidesF$DAY, sep = "-")
TidesF$TIDE_TIME <- as.POSIXct(paste(TidesF$DATE, TidesF$TIDE_TIME), tz = "America/Panama", format = "%Y-%B-%d %H:%M")
TidesF <- TidesF[order(TidesF$TIDE_TIME),]
TidesF$HIGH <- ifelse(TidesF$TIDE_HEIGHT > 2, "TRUE", "FALSE")

# make sunrise and sunset date/time classes too
TidesF$SUNRISE <- as.POSIXct(paste(TidesF$DATE, TidesF$SUNRISE), tz = "America/Panama", format = "%Y-%B-%d %H:%M")
TidesF$SUNSET <- as.POSIXct(paste(TidesF$DATE, TidesF$SUNSET),tz = "America/Panama", format = "%Y-%B-%d %H:%M")

TidesLow <- subset(TidesF, TidesF$HIGH == FALSE)
TidesHigh <- subset(TidesF, TidesF$HIGH == TRUE)

h.lub <- hour(TidesLow$TIDE_TIME)
hist(h.lub)

###### WORKING CODE TO CLEAN ONE CSV ######
# I used this a lot for troubleshooting
## still have problem with some that apparently have less white lines in the top? So can't do skip = 9 (which works for some)
# instead do blank.lines.skip = TRUE, this seems to get them all at the same amount of unnecessary lines at the top that we can deal with later
tidalgross <- read.csv("cebaco_island_tide_charts/csvs/2017_01_cebaco.csv", header = FALSE, blank.lines.skip = TRUE)

##### GENERAL CLEAN-UP #####
# some files seem to have a 10th column with only NAs. Remove that one if it's there 
tidalgross <- tidalgross[, colSums(is.na(tidalgross)) != nrow(tidalgross)]
# remove second and ninth column filled with weird symbols
tidalgross <- tidalgross[, -c(2,9)]
# renames columns to things we can actually use
colnames(tidalgross) <- c("DAY", "SUN", "TIDE_1", "TIDE_2", "TIDE_3", "TIDE_4", "COEFFICIENT")

#### DAY, MONTH, YEAR ####
# now we need to do some witchery to find the month/year, which can be in two places
# first want to get rid of blank rows
# set blank values to NA
tidalgross[tidalgross == ""] <- NA
# remove any rows that are NA
tidalgross <- tidalgross[rowSums(is.na(tidalgross[ , 0:ncol(tidalgross)])) < ncol(tidalgross), ]

# Option 1: month year is at the top of the csv, in row 3
# make this value a new column (if there's no month year there, this will be the "DAY" title)
tidalgross$MONTHYEAR <- tidalgross$DAY[3]
# remove unnecessary rows at top (this shouldn't vary but might vary with new csvs?)
tidalgross <- tidalgross[-c(1:4),]

# Option 2: month year is at the bottom of the excel, either after "CEBACO ISLAND" or on its own
# so if the MONTHYEAR column just says "DAY", use this bottom row
# Otherwise use the data from the MONTHYEAR column
if(sum(tidalgross$MONTHYEAR == "DAY") == nrow(tidalgross)) { 
  # remove the CEBACO ISLAND part (as this is not in all of the csvs)
  tidalgross$DAY[nrow(tidalgross)] <- str_replace(tidalgross$DAY[nrow(tidalgross)], "CEBACO ISLAND\n", "")
  # day and year are separated by comma
  tidalgross$YEAR <- sapply(str_split(tidalgross$DAY[nrow(tidalgross)], ","), '[', 2)
  tidalgross$MONTH <- sapply(str_split(tidalgross$DAY[nrow(tidalgross)], ","), '[', 1) 
} else { 
  tidalgross$YEAR <- sapply(str_split(tidalgross$MONTHYEAR, ","), '[', 2) 
  tidalgross$MONTH <- sapply(str_split(tidalgross$MONTHYEAR, ","), '[', 1)
} 

# cut unnecessary bottom part of and remove MONTHYEAR (column 8)
tidalgross <- tidalgross[-c(nrow(tidalgross), (nrow(tidalgross)-1)), -8]
# keep only day number (day of the week is always 3 characters)
tidalgross$DAY <- substr(tidalgross$DAY, 1, nchar(tidalgross$DAY)-3)
# remove rows that have a blank value for DAY to get all dataframes equally trimmed
tidalgross <- tidalgross[!is.na(tidalgross$DAY), ]

#### SUNRISE AND SUNSET #####
# remove all non-alphanumeric characters and extract one time
tidalgross$SUN <- str_replace_all(tidalgross$SUN, "[^[:alnum:]]", " ")

# Sunrise
# could not incorporate line below in pipe, so do that first
tidalgross$SUNRISE <- sapply(str_split(tidalgross$SUN, "h "), '[', 1)
tidalgross$SUNRISE <- tidalgross$SUNRISE %>%
  str_trim() %>%
  str_replace(" ", ":") %>%
  paste("0",., sep="") # don't have it as date or time object currently, add 0 for 24h clock consistency

# Sunset
tidalgross$SUNSET <- str_replace((str_trim(sapply(str_split(tidalgross$SUN, "h "), '[', 2))), " ", ":")  
tidalgross$SUNSET <- tidalgross$SUNSET %>%
  substr(., 1, nchar(.)-2) %>%
  str_trim() %>%
  str_replace(" ", ":")

#### TIME & HEIGHT OF TIDES #####
# wanted to loop over the different tides, but not sure how to use for loop with column names. 
# or how to write a function doing this, so for now do each tide separately. 
## TIDE 1
tidalgross$TIDE_1_HEIGHT <- sapply(str_split(tidalgross$TIDE_1, "\\n"), '[', 2)
tidalgross$TIDE_1_HEIGHT <- substr(tidalgross$TIDE_1_HEIGHT, 1, nchar(tidalgross$TIDE_1_HEIGHT)-4) 
# check if for all heights it's the last 4 characters (appears correct)
tidalgross$TIDE_1_TIME <- sapply(str_split(tidalgross$TIDE_1, "\\n"), '[', 1)
tidalgross$TIDE_1_TIME <- tidalgross$TIDE_1_TIME %>%
  substr(., 1, nchar(.)-2)

# use if statement to only append 0 if first 2 character is under 10
x <- as.numeric(substr(tidalgross$TIDE_1_TIME, 1, nchar(tidalgross$TIDE_1_TIME)-3)) 
for (j in 1:length(x)) {
  print(j)
  tidalgross$TIDE_1_TIME[j] <- ifelse((x[j] < 10), (paste("0",tidalgross$TIDE_1_TIME[j], sep = "")), tidalgross$TIDE_1_TIME[j]) 
} 

## TIDE 2
tidalgross$TIDE_2_HEIGHT <- sapply(str_split(tidalgross$TIDE_2, "\\n"), '[', 2)
tidalgross$TIDE_2_HEIGHT <- substr(tidalgross$TIDE_2_HEIGHT, 1, nchar(tidalgross$TIDE_2_HEIGHT)-4) 
# check if for all heights it's the last 4 characters
tidalgross$TIDE_2_TIME <- sapply(str_split(tidalgross$TIDE_2, "\\n"), '[', 1)
tidalgross$TIDE_2_TIME <- tidalgross$TIDE_2_TIME %>%
  substr(., 1, nchar(.)-2)

# use if statement to only append 0 if first 2 character is under 10
x <- as.numeric(substr(tidalgross$TIDE_2_TIME, 1, nchar(tidalgross$TIDE_2_TIME)-3)) 
for (j in 1:length(x)) {
  print(j)
  tidalgross$TIDE_2_TIME[j] <- ifelse((x[j] < 10), (paste("0",tidalgross$TIDE_2_TIME[j], sep = "")), tidalgross$TIDE_2_TIME[j]) 
} 

## TIDE 3
tidalgross$TIDE_3_HEIGHT <- sapply(str_split(tidalgross$TIDE_3, "\\n"), '[', 2)
tidalgross$TIDE_3_HEIGHT <- substr(tidalgross$TIDE_3_HEIGHT, 1, nchar(tidalgross$TIDE_3_HEIGHT)-4) 
# check if for all heights it's the last 4 characters
tidalgross$TIDE_3_TIME <- sapply(str_split(tidalgross$TIDE_3, "\\n"), '[', 1)
tidalgross$TIDE_3_TIME <- tidalgross$TIDE_3_TIME %>%
  substr(., 1, nchar(.)-2)
# are no times below 10 in the third tide so removed for loop

## TIDE 4
tidalgross$TIDE_4_HEIGHT <- sapply(str_split(tidalgross$TIDE_4, "\\n"), '[', 2)
tidalgross$TIDE_4_HEIGHT <- substr(tidalgross$TIDE_4_HEIGHT, 1, nchar(tidalgross$TIDE_4_HEIGHT)-4) 
# check if for all heights it's the last 4 characters
tidalgross$TIDE_4_TIME <- sapply(str_split(tidalgross$TIDE_4, "\\n"), '[', 1)
tidalgross$TIDE_4_TIME <- tidalgross$TIDE_4_TIME %>%
  substr(., 1, nchar(.)-2)
# are no times below 10 in the fourth tide so removed for loop

##### COEFFICIENT AND AVERAGE #####
# simply split up numeric and alphabetical characters
tidalgross$COEF <- as.numeric(str_extract(tidalgross$COEFFICIENT, "[0-9]+"))
# want to take the "very high" in its entirety so take out the spaces
tidalgross$COEFFICIENT <- str_replace_all(tidalgross$COEFFICIENT, " ", "")
tidalgross$AVERAGE <- (str_extract(tidalgross$COEFFICIENT, "[aA-zZ]+"))

# make everything nice and tidy
tidalclean <- tidalgross[,c("YEAR", "MONTH", "DAY", "SUNRISE", "SUNSET", "TIDE_1_TIME", "TIDE_1_HEIGHT",
                            "TIDE_2_TIME", "TIDE_2_HEIGHT", "TIDE_3_TIME", "TIDE_3_HEIGHT", "TIDE_4_TIME",
                            "TIDE_4_HEIGHT", "COEF", "AVERAGE")]

### MOST ANNOYING PROBLEM THAT IS SOLVED FOR NOW BUT MAY RETURN:
# Not all files have "month year" in the last row of the dataset, some have it somewhere at the top randomly
# for now I have tried to solve this with ifelse statements
# but there are still a few operations that specify a rownumber or column number 
# for all of the current csvs it seems to work, so as long as new ones stick to any of the known patterns the function can clean them
# alternative would be to get the month-year from the title of the csv, but I didn't figure that out so took the long way around

#### DATES #####
# seems like you can't have a time column without the date. So consider making the sunset and sunrise columns contain the date too?
# then could combine month year day column with sunrise or sunset column to create this
# maybe something to keep for the end? Can also easily change "January" to 1 etc
# or keep it as character as it is (but now you can't really calculate with it)

# some resources that were useful/might still be useful
# https://stats.idre.ucla.edu/r/codefragments/read_multiple/
# https://community.rstudio.com/t/multiple-datasets-with-for-loop/20582/4
# https://stackoverflow.com/questions/17197062/creating-an-array-of-dataframes-in-r
# https://gis.stackexchange.com/questions/231601/looping-over-several-input-files-in-r
# https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files
# https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames

