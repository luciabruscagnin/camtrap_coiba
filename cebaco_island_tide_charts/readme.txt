Below are tide charts at Coiba beginning in March 2017 to July 2021. The were downloaded from this website on April 14-16, 2021 (2017 until May 2021) and on April 23-25, 2022 (2015 and from June 2021 onwards) by Sylvia Garza:

https://tides4fishing.com/pa/oceano-pacifico/isla-cebaco

They were scraped and converted to PDFs using the application Tabuala version 1.2.1, with the autodetect grid option:
https://tabula.technology/

The tide charts can be cleaned using the "tide_cleaning.R" script.

Data has DM, some weird unicode seperators that need to be removes, and each of the 3-4 high and low tide times and their corresponding height. 

There is also some coefficient that explains the magnitude of the height.

Some dates were missing from the CSVs downloaded on April 23-25 2022, even though they were present on the website, so these were added to the CSVs manually.
Dates missing were 2015-June-30, 2015-November-30, 2015-September-30, 2021-November-30, 2021-September-30