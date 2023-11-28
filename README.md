# camtrap_coiba
Project repository of the Coiba Tool Use project (MPI-AB)

Subfolders include:
- *agouti_output*: to be put on everyone's local computer (not pushed to GitHub because of size of files). Should contain the data exported from Agouti.eu, in its subfolder (e.g. named "coiba-national-park-tool-use-20230629133316"). The folder "agouti_output" should also contain the csv file "individual_age_sex_species_202306131450" which has the keys for all individuals. Agouti output can be cleaned using the "agouti_cleaning.R" script which is in the main camtrap_coiba repo folder   
- *cebaco_island_tide_charts*: folder containing scraped csv files (from tides4fishing.com) of all low and high tide times. These can be cleaned using the script "tide_cleaning.R" which is in the "tide_analysis" folder.   
- *covid_project*: from Brendan, for inclusion of Coiba data in covid project by Roland Kays  
- *detailedtools*: for BORIS coding of detailed tool use sequences. Contains the coded data from Meredith, Zoë, and Leonie, as well as a script to clean this data "detailedtools.R" and Zoë's analyses for her thesis "detailedtools_analyses.R"  
- *exiftool_metadata*: containing metadata pulled from deployed cameras (temperature for all, and sequence length for videos). In the subfolder temperature_seqlength_exif there is the script for cleaning this data, which is "exiftempseq_cleaning.R".  
- *exiftool_renaming*: for renaming raw camera trap data. "camtrapR_coiba.R" is Brendan's original script, "camtrapR_coiba_ZG.R" is Zoë's script which works on Windows. These two R scripts are both for renaming still images, for renaming videos we need to use the shell script "exiftool_coiba". This folder also contains the shellscript for extracting temperature and sequence length ("temperature_sequencelength_exif.R")   
- *grid_sampling*: folder containing script and information to generate the gridpoints that we placed at the tool site and non tool use site on Coiba in 2022-2023   
- *gridanalyses*: for Zoë's dissertation chapter on group cohesion. This uses the grid data and for now is some initial analyses and Gaussian processes. 
- *map*: from Brendan, for creating a map of the camera points, tracks, and streams
- *sexbias*: from Zoë, for her dissertation chapter on male-biased tool use. These are older scripts, the final versions can be found in the DOI'd repository [here](https://zenodo.org/records/8316263)
- *stone_scoring*: for Meredith's dissertation on hammerstone selectivity. 
- *tide_analysis*: from Zoë, for her dissertation chapter on tool use and tidal cycles. These are older scripts, the final versions can be found in the DOI'd repository [here](https://zenodo.org/records/8129505). This also contains some other assorted scripts of analyses for Zoë's dissertation, namely "activity_other.R" contains some initial analyses on camera inspection and comparing activity of tool users and non tool users, "seasonality_analysis.R" contains preliminary analyses of seasonality of tool use and foraging. This is also where the script is to clean the tidal csvs ("tide_cleaning.R")

## Cleaning Agouti output
Agouti output can be cleaned using the "agouti_cleaning.R" script. Currently this leads to two outputs: "agouticlean" where one row is one observation (e.g., one capuchin) and "agoutisequence" where one row is one sequence. 
Dependencies of this script that need to be run first are "exiftempseq_cleaning.R" amd "tide_cleaning.R" (or the TidesClean csv from cebaco_island_tide_charts needs to be loaded in).


