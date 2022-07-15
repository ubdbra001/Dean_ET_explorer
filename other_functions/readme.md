# Other functions

This directory contains other functions that can be used to download and prepare
the original data from the osf repo

## R - get_data:

Contains functions that:
- Download a zipped version of the files in the repo
- Unzip the data into a temp dir
- Move the data into the raw data directory

## R - extract_data:

Contains functions that:
- Start a Matlab session
- Runs a Matlab script
- Ends the Matlab session

## Matlab - processScript:

Contains a script that takes the raw data (in .mat files), splits the data into the different trials, and extracts the columns of interest for analysis in R