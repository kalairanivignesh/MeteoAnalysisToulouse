source("Meteo_Analysis.R")


# Univariate Analysis of pluie

univariate_without_log(consolidatedData$pluie, "Values of Pluie", "Count", "Distribution of Pluie")

univariate_with_log(consolidatedData$pluie, "Values of Pluie", "Count", "Distribution of Pluie with Log10 Transformation")


# Univariate Analysis of Temperature

univariate_without_log(consolidatedData$temperature_en_degre_c, "Values of Temperature", "Count", "Distribution of Temperature")


# Bivariate Analysis of pluie

bivariate_line(consolidatedData$date, consolidatedData$pluie, "Date", "Pluie", "Distribution of Pluie across dates")


# Bivaraiate Analysis of temperature

bivariate_line(consolidatedData$date, consolidatedData$temperature_en_degre_c, "Date", "Temperature", "Distribution of Temperature across dates")

correlation_plot(consolidatedData)


# Bivariate Analysis with moving average

bivariate_line_mean(consolidatedData$date, consolidatedData$pluie, "Date", "Pluie", "Mean value of Pluie across dates")

bivariate_line_mean(consolidatedData$date, consolidatedData$temperature_en_degre_c, "Date", "Temperature", "Mean value of Temperature across dates")



# Bivariate Barplot


ConsolidatedData <- na.omit(consolidatedData)

Data <- ConsolidatedData[!ConsolidatedData$id==47,]



bivariate_bar(Data$id, Data$pluie, "Station ID", "Pluie", "Distribution of Pluie across stations")

bivariate_bar(Data$id, Data$temperature_en_degre_c, "Station ID", "Temperature", "Distribution of Temperature across stations")


bivariate_bar(Data$type_de_station, Data$pluie, "Station ID", "Pluie", "Distribution of Pluie across stations")


# Idendifying top significant parameters
coordinates <- linear_model(consolidatedData)


# Getting Correlation Matrix

corr_matrix <- correlation(consolidatedData)

