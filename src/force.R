
source("Meteo_Analysis.R")
Data <- MergeAndCleanDataSets()

#Univariate Analysis of Wind force
univariate_without_log(Data$force_moyenne_du_vecteur_vent,"Wind_Force","Count","Average force of wind vector") + xlim(0,20)

#Univariate Analysis of Burst Force
univariate_without_log(Data$force_rafale_max,"Burst_Force","Count","Max Burst Force") + xlim(0,40)

Data <- na.omit(Data)

Data <-Data[!Data$id==47,]

#Bivariate Analysis of Wind Force2
bivariate_bar(Data$id,Data$force_moyenne_du_vecteur_vent,"Stations","Wind_Force","Wind Force in different Stations")


bivariate_bar(Data$id,Data$force_rafale_max,"Stations","Burst_Force","Max Burst Force in different Stations")






