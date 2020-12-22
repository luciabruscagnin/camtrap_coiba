#accident 279.94 per annum
173.16 

303 570 0370

1) Set system of equations and convert to frequencies.
2) Compute selection gradients of each strategy with reference to l and L
	b) solve for q hat at steady state
	a) solve stationary demography, which is stable age distribution of the frequency of s,h,N,
	A. This is the dominant eigenvector, if using a matric aka Caswel approach.
    c) calculate lambda, the growth rate of each strategy take partial derivatives with respect to L and l
3) Plot evolutionary dynamics of s and h over time


#shell scrpt from brett to rename files by metadata

#in the terminal set the working directory to the location of the JPG and run


cd "/Users/BJB/Desktop/exiftool_test/"
#below renames images with datetimeoriginal tag and camera model
exiftool -r '-filename<${datetimeoriginal}_${model;}%-c.%e' -d "%Y%m%d_%H%M%S" .
exiftool -r '-filename<${ile Modification Date/Time}_${model;}%-c.%e' -d "%Y%m%d_%H%M%S" .

File Modification Date/Time

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
###for videos
# rename file with created date, name, and YYMMDDHHMMSS
exiftool -r '-filename<${CreateDate}_${FileName;}%-c.' -d "%Y%m%d_%H%M%S" .
####adds copyright and artist to all files in wd

exiftool -r -overwrite_original -artist="Brendan J Barrett" -copyright="Max Planck Institue of Animal Behavior" -comment="CEBUS-01-R1" .
#below is very close, we need to put dashes in between files names,, or not
exiftool -r '-filename<${Comment}_${CreateDate}%-c.%e' -d "%Y%m%d_%H%M%S" .


exiftool -r '-filename<${CreateDate}_${FileName;}%-c.%e.' -d "%Y%m%d_%H%M%S" .


exiftool -r '-filename<${CreateDate}_${FileName;}%-c.%e' -d "%Y%m%d_%H%M%S" .


exiftool -r '-filename<${CreateDate}_${FileName;}%-c.%e' -d "%Y%m%d_%H%M%S" .

exiftool -r "-filename<CreateDate" -d "%Y-%m-%d_%H-%M-%S - %%f.%%e" RCNX0012.MP4

exiftool -r '-filename<${CreateDate}_${%dogs%}%-c.%e' -d "%Y%m%d_%H%M%S" .

#read user label and filename
 exiftool -g1 -a -s -Date/Time Original -Filename path/file.jpg 
#read user label and filename
 exiftool -g1 -a -s -UserLabel -Filename path/file.jpg 

exiftool -csv -datetimeoriginal /path/to/dir > /Users/BJB/Dropbox/camtrap_coiba/Cebus-01-R1_exif_metadata.csv

