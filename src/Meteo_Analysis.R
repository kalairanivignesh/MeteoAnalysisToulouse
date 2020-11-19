#Download a file from an URL

library(RCurl)
library(dplyr)
##download.file("https://toulouse-metropole.opendatasoft.com/explore/dataset/00-station-meteo-toulouse-valade/download/?format=csv",
#              destfile = "../Data/00-station-meteo-toulouse-valade.csv",method="libcurl")

#Reading an Input file which contains stations list
##Input_files <- read.csv("../Input/Input_file_test.csv",header=F)




#Download multiple files

Download_Dataset <- function(x,base_url="https://toulouse-metropole.opendatasoft.com/explore/dataset/", 
                             download_parameter="/download/?format=csv",dest_location="../Data/",file_extension=".csv")
{
  sapply(x$V1,function(i)
    {
    download.file(paste0(base_url,i,download_parameter),destfile = paste0(dest_location,i,file_extension),method="libcurl") # paste0 uses NULL seperation factor
  })

}



MergeAndCleanDataSets <- function(sourcePath = "../Data/Stations", extension = "*.csv")
{
  list_files <- paste0(sourcePath,"/",list.files(path=sourcePath,pattern = extension))
  all_station_data <- lapply(list_files,read.csv,sep=";",header=T ) # Read all files, and store in list of dataframes
  # Do a col rename if it has wrong col name which does not match with other station dataset
  all_station_data <- lapply(all_station_data,function(x){
    if(!("heure_utc" %in% colnames(x)))
    {
      x <- rename(x,heure_utc=mm_dd_yy_hh_mm)
    }
    x
  })
  # Extract the common columns from all the datasets
  common_cols <- Reduce(intersect, lapply(all_station_data, names)) # To Find common columns in all the files
  #Merge All Dataframe and their common columns
  Consolidated_station<- do.call(rbind,lapply(all_station_data, "[", common_cols))
  Consolidated_station
}



