source("Meteo_Analysis.R")


# Level of humidite is from 0 to 100
levels(as.factor(consolidatedData$humidite))

# Remove NA values.
HumiditeWithoutNA <- (consolidatedData$humidite[!is.na(consolidatedData$humidite)])
HumiditeWithoutNA

## Univariate analysis Option 2: Use programmed function "univariate_without_log" to plot a histogram of the dataset Humidite (without NA values).
univariate(HumiditeWithoutNA, xlab = "% of Humidity", ylab = "Frequency", title = "Frequency of Humidity")
  
# Plot a line graph with variables of "humidite and "CoercedUTC" to draw the distribution of how humidity changes along time.
bivariate_line(date(consolidatedData$CoercedUTC), consolidatedData$humidite, "Date","% of Humidity", "Distribution of Humidity Across Dates") 

# Plot a line graph with the variables of "mean of humidite" and "CoercedUTC" to draw the distribution of how average humidity changes along time.
bivariate_line_mean(date(consolidatedData$CoercedUTC), consolidatedData$humidite, "Date","% of Humidity", "Distribution of Daily Average Humidity Across Dates")


