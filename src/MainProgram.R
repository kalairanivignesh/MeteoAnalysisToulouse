source("Meteo_Analysis.R") # Include the Source from another R
Input_files <- read.csv("../Input/Input_file.csv",header=F) # List of station to fetch
# Step 1 - Download Data
Download_Dataset(Input_files)
