source("Meteo_Analysis.R")
ConsolidatedData <-MergeAndCleanDataSets()
#Step 4 - Univariable analysis

#Level of direction_du_vecteur_de_vent_max.Values are between 1 to 15 and distribution of values are drawn by histogram
levels(as.factor(ConsolidatedData$direction_du_vecteur_de_vent_max))
createHistogramWithoutNA(ConsolidatedData$direction_du_vecteur_de_vent_max,breaks=seq(-.5,15.5,1),name="direction_du_vecteur_de_vent_max")

#Level of direction_du_vecteur_vent_moyen.Values are between 0 to 510(Values are even numbers) and distribution of values are drawn by histogram
levels(as.factor(ConsolidatedData$direction_du_vecteur_vent_moyen))
createHistogramWithoutNA(ConsolidatedData$direction_du_vecteur_vent_moyen,breaks=seq(-0.5,511.5,2),name="direction_du_vecteur_vent_moyen")

#Occurence of Zero is around .9 million so creating histogram without Zero 
createHistogramWithoutNAandZero(ConsolidatedData$direction_du_vecteur_vent_moyen,breaks=seq(0.5,510.5,2),name="direction_du_vecteur_vent_moyen")

#Level of direction_du_vecteur_de_vent_max_en_degres.Values are between 0 to 337.5 and distribution of values are drawn by histogram
summary(ConsolidatedData$direction_du_vecteur_de_vent_max_en_degres)
levels(as.factor(ConsolidatedData$direction_du_vecteur_de_vent_max_en_degres))
createHistogramWithoutNA(ConsolidatedData$direction_du_vecteur_de_vent_max_en_degres,breaks=seq(-0.5,360,22.5),name="direction_du_vecteur_de_vent_max_en_degres")
#Output of histogram direction_du_vecteur_de_vent_max_en_degres is same as direction_du_vecteur_de_vent_max.
#So it is degree representation of direction_du_vecteur_de_vent_max.Going forward only one of these can consider for analysis not both.


#Step 5 -Bivariate analysis


Bivariate_whiskerplot_Month(ConsolidatedData,colName="direction_du_vecteur_de_vent_max",
                            str="Distribution of direction_du_vecteur_de_vent_max by Month")
