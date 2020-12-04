source("Meteo_Analysis.R")

# Level of pression is from 0 to 115500
levels(as.factor(consolidatedData$pression))

# Remove Outliers.
PressionWithout_outliers<-as.numeric(consolidatedData$pression[consolidatedData$pression>121],consolidatedData$pression[!is.na(consolidatedData$pression)])
PressionWithout_outliers

## Univariate analysis Option 2: Use programmed function “univariate_without_log” to plot a histogram of the dataset Humidite (without NA values).
univariate(PressionWithout_outliers, xlab = "Pression(Pa)", ylab = "Frequency", title = "Frequency of Pression")


# Plot a line graph with variables of "pression" and "CoercedUTC" to draw the distribution of how pressure changes along time.
bivariate_line(date(consolidatedData$CoercedUTC), consolidatedData$pression, "Date", "Pression", "Distribution of Pression Across Dates")

# Plot a line graph with the variables of "mean of pression" and "CoercedUTC" to draw the distribution of how average pressure changes along time.
bivariate_line_mean(date(consolidatedData$CoercedUTC), consolidatedData$pression, "Date", "Pression", "Distribution of Daily Average Pression Across Dates")
