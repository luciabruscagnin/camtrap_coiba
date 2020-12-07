

#shell scrpt from brett to rename files by metadata

#in the terminal set the working directory to the location of the JPG and run

cd "/Users/brendanbarrett/Dropbox/Jicaron Photos/Phone"
cd "/Dropbox/Jicaron Photos/"

cd "/Users/BJB/Desktop/exiftool_test/"
#below is brett code
exiftool -r '-filename<${datetimeoriginal}_${model;}%-c.%e' -d "%Y%m%d_%H%M%S" .
#my attempt
exiftool -r '-filename<${CreateDate}_${UserLabel;}%-c.%e' -d "%Y%m%d_%H%M%S" .
#read 
exiftool RCNX0012.MP4
exiftool -s -G RCNX0012.MP4


exiftool -g1 -a -s -CreateDate -TrackCreateDate RCNX0012.MP4
exiftool -g1 -a -s -CreateDate RCNX0012.MP4
exiftool -g1 -a -s -UserLabel RCNX0012.MP4

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


