# exiftool -r -csv -FileName -CreateDate -AmbientTemperature -ext jpg -d %d/%m/%Y_%H:%M:%S /Volumes/Coiba\ Image\ Data/renamed_data/R1    > /Users/sifaka/Dropbox/camtrap_coiba/exiftool_metadata/temperature_seqlength_exif/r1_temperature.csv 
#r1
exiftool  -r -csv -FileName -CreateDate -MediaDuration -AmbientTemperature -d %d/%m/%Y_%H:%M:%S /Volumes/Coiba\ Image\ Data/renamed_data/R1    > /Users/sifaka/Dropbox/camtrap_coiba/exiftool_metadata/temperature_seqlength_exif/r1_temperature_sequencelength.csv 
#r2
exiftool  -r -csv -FileName -CreateDate -MediaDuration -AmbientTemperature -d %d/%m/%Y_%H:%M:%S /Volumes/Coiba\ Image\ Data/renamed_data/R2    > /Users/sifaka/Dropbox/camtrap_coiba/exiftool_metadata/temperature_seqlength_exif/r2_temperature_sequencelength.csv 
