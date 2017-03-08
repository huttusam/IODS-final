# Sampsa Huttunen, 5.3.2017, Wrangling Data & Analyzing for IODS course's Final Assignment

# Access libraries
library(dplyr)

#Set working directory
setwd("I:/Google Drive/Helsingin yliopisto/Intro to Open Data Science/IODS-final")

###############################
# WRANGLING dataset 'affairs' #
###############################

# reading data from 'Fair.csv' to table 'affairs'
affairs <- read.csv("data/Fair.csv", header = TRUE)


# looking at dimensions and structure of affairs
dim(affairs)
str(affairs)
#601 observations, 10 variables, first variable/column has index numbers

#Excluding first column
affairs <- affairs[-1]

# looking at structure of affairs
str(affairs)
#OK. Now: 601 observations, 9 variables. First column excluded.

# Save modified data 'affairs' to a file that looks good in a European version of Excel, but also has a decimal point instead of a comma
write.table(affairs, file = "data/affairs.csv", sep = ";", qmethod="double", col.names=TRUE, row.names=FALSE)

