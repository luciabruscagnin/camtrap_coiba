

#shell scrpt from brett to rename files by metadata

#in the terminal set the working directory to the location of the Jfile and run


##makes file  YYYYMMDD_HHMMSS_FILENAME_ #double extensions though
exiftool -r '-filename<${CreateDate}_${FileName;}%-c.%e' -d "%Y%m%d_%H%M%S" .
exiftool -r '-filename<${CreateDate}_${FileName;}%-c.' -d "%Y%m%d_%H%M%S" .

####adds copyright and artist to all files in wd
exiftool -r '-filename<${CreateDate}_${FileName;}%-c.' -d "%Y%m%d_%H%M%S" .

exiftool -r -overwrite_original -artist="Brendan J Barrett" -copyright="Max Planck Institue of Animal Behavior" -comment="CEBUS-01-R1" .
#below is very close, we need to put dashes in between files names or not

exiftool -r '-filename<${comment}_${CreateDate}%-c.%e' -d "%Y%m%d_%H%M%S" .


exiftool -r '-filename<${CreateDate}_${FileName;}%-c.%e.' -d "%Y%m%d_%H%M%S" .


exiftool -r '-filename<${CreateDate}_${FileName;}%-c.%e' -d "%Y%m%d_%H%M%S" .


exiftool -r '-filename<${CreateDate}_${FileName;}%-c.%e' -d "%Y%m%d_%H%M%S" .

exiftool -r "-filename<CreateDate" -d "%Y-%m-%d_%H-%M-%S - %%f.%%e" RCNX0012.MP4

exiftool -r '-filename<${CreateDate}_${%dogs%}%-c.%e' -d "%Y%m%d_%H%M%S" .

#########READING INFO ON EXIfTOOL
#userlabel
 exiftool -g1 -a -s -UserLabel /Volumes/Coiba\ Image\ Data/renamed_data/R1/CEBUS-01-R1/video/CEBUS-01-R1_20170402_102908.JPG 
#user label and filename and Create Date
exiftool -g1 -a -s -UserLabel -FileName -CreateDate /Volumes/Coiba\ Image\ Data/renamed_data/R1/CEBUS-01-R1/video/CEBUS-01-R1_20170402_102908.JPG 

###WRITING METALDATA TO CSV
#first batch of just videos from Cebus-01-R1, both mp4 and jpg
exiftool -csv -FileName -CreateDate /Volumes/Coiba\ Image\ Data/renamed_data/R1/CEBUS-01-R1/video/ > /Users/BJB/Dropbox/camtrap_coiba/Cebus-01-R1_exif_metadata.csv
##just mp4 files name and timestamp
exiftool -csv -FileName -CreateDate -ext mp4  /Volumes/Coiba\ Image\ Data/renamed_data/R1/CEBUS-01-R1/video/ > /Users/BJB/Dropbox/camtrap_coiba/Cebus-01-R1_exif_metadata_cleam_mp4.csv
##just mp4 files name and timestamp which is dd/mm/yyyy hh:mm:ss for Agouti
exiftool -csv -FileName -CreateDate -ext mp4 -d %d/%m/%Y_%H:%M:%S /Volumes/Coiba\ Image\ Data/renamed_data/R1/CEBUS-01-R1/video/ > /Users/BJB/Dropbox/camtrap_coiba/Cebus-01-R1_exif_metadata_clean_mp4.csv


####rename all images and videos in paricular folders

####R1
#CEBUS-01-R1
cd /Volumes/Coiba\ Image\ Data/renamed_data/R1/CEBUS-01-R1/video 
exiftool -r -overwrite_original -artist="Brendan J Barrett" -copyright="Max Planck Institue of Animal Behavior" -comment="CEBUS-01-R1" .
exiftool -r '-filename<${Comment}__${CreateDate}%-c.%e' -d "%Y-%m-%d__%H-%M-%S" . #does not have the double rename thing if same timestep, but that is OK for video
exiftool -csv -FileName -CreateDate -ext mp4 -d %d/%m/%Y_%H:%M:%S /Volumes/Coiba\ Image\ Data/renamed_data/R1/CEBUS-01-R1/video/ > /Users/BJB/Dropbox/camtrap_coiba/Cebus-01-R1_exif_metadata_clean_mp4.csv
#SURVEY-CEBUS-02-01-R1
cd /Volumes/Coiba\ Image\ Data/renamed_data/R1/SURVEY-CEBUS-02-01-R1 
exiftool -r -overwrite_original -artist="Brendan J Barrett" -copyright="Max Planck Institue of Animal Behavior" -comment="SURVEY-CEBUS-02-01-R1" .
exiftool -r '-filename<${Comment}__${CreateDate}%-c.%e' -d "%Y-%m-%d__%H-%M-%S" . #does not have the double rename thing if same timestep, but that is OK for video
exiftool -csv -FileName -CreateDate -ext mp4 -d %d/%m/%Y_%H:%M:%S /Volumes/Coiba\ Image\ Data/renamed_data/R1/SURVEY-CEBUS-02-01-R1 > /Users/BJB/Dropbox/camtrap_coiba/Survey-Cebus-02-01-R1_exif_metadata_clean_mp4.csv
#SURVEY-CEBUS-03-01-R1
cd /Volumes/Coiba\ Image\ Data/renamed_data/R1/SURVEY-CEBUS-03-01-R1 
exiftool -r -overwrite_original -artist="Brendan J Barrett" -copyright="Max Planck Institue of Animal Behavior" -comment="SURVEY-CEBUS-03-01-R1" .
exiftool -r '-filename<${Comment}__${CreateDate}%-c.%e' -d "%Y-%m-%d__%H-%M-%S" . #does not have the double rename thing if same timestep, but that is OK for video
exiftool -csv -FileName -CreateDate -ext mp4 -d %d/%m/%Y_%H:%M:%S /Volumes/Coiba\ Image\ Data/renamed_data/R1/SURVEY-CEBUS-03-01-R1 > /Users/BJB/Dropbox/camtrap_coiba/Survey-Cebus-03-01-R1_exif_metadata_clean_mp4.csv

#SURVEY-CEBUS-06-01-R1
cd /Volumes/Coiba\ Image\ Data/renamed_data/R1/SURVEY-CEBUS-06-01-R1 
exiftool -r -overwrite_original -artist="Brendan J Barrett" -copyright="Max Planck Institue of Animal Behavior" -comment="SURVEY-CEBUS-06-01-R1" .
exiftool -r '-filename<${Comment}__${CreateDate}%-c.%e' -d "%Y-%m-%d__%H-%M-%S" . #does not have the double rename thing if same timestep, but that is OK for video
exiftool -csv -FileName -CreateDate -ext mp4 -d %d/%m/%Y_%H:%M:%S /Volumes/Coiba\ Image\ Data/renamed_data/R1/SURVEY-CEBUS-06-01-R1 > /Users/BJB/Dropbox/camtrap_coiba/Survey-Cebus-06-01-R1_exif_metadata_clean_mp4.csv

########R2
#now will copy from raw folder into renamed folder




