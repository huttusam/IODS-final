# Sampsa Huttunen, 5.3.2017, Wrangling Data for IODS course's Final Assignment

# Access the dplyr library
library(dplyr)

# Access the gglot2 library
library(ggplot2)

#Set working directory
setwd("I:/Google Drive/Helsingin yliopisto/Intro to Open Data Science/IODS-final")

# reading data from a Suomen Elokuvasäätiö CSV files to tables 'elokuvat', 'sestuet' and 'teatterit'
elokuvat <- read.csv2("data/Ensi-illat_ja_katsojat_2004-2014.csv", header = TRUE)
sestuet <- read.csv2("data/SES_Tukipaeaetoekset_2008-2015.csv", header = TRUE)
teatterit <- read.csv2("data/Elokuvateatterit_2015.csv", header = TRUE)

# viewing sestuet: a dataset containing information of the Finnish Film Foundation funds between 2008-2015
View(sestuet)

# looking at dimensions of sestuet
dim(sestuet)

# looking at structure of sestuet
str(sestuet)

# printing a summary of sestuet
summary(sestuet)

# Changing name of Tukityypin nimi to tukityyppi
colnames(sestuet)[1] <- "tukityyppi"

# Changing name of Elokuvan lajityyppi to lajityyppi
colnames(sestuet)[2] <- "lajityyppi"

# Changing name of Tukihak päätöspvm to pvm
colnames(sestuet)[3] <- "pvm"

# Changing name of Esittelijän nimi to esittelija
colnames(sestuet)[4] <- "esittelija"

# Changing name of tuenhakijan nimi to hakija
colnames(sestuet)[5] <- "hakija"

# Changing name of Tukihakemuksen kohde to elokuva
colnames(sestuet)[6] <- "elokuva"

# Changing name of Tukihakemumuksen päätöseur to summa
colnames(sestuet)[7] <- "summa"

# print out the column names in data
colnames(sestuet)

# select rows where tukityyppi is 'Käsikirjoitusapuraha'
kasistuet <- filter(sestuet, tukityyppi == 'Käsikirjoitusapuraha')

# viewing the new dataset kasistuet containing only grants for scriptwriting
View(kasistuet)

# looking at dimensions of kasistuet
dim(kasistuet)

# looking at structure of kasistuet
str(kasistuet)

# printing a summary of kasistuet
summary(kasistuet)

# choosing only relevant columns
keep_columns <- c("lajityyppi", "esittelija", "hakija", "elokuva", "summa")

# select the 'keep_columns' to create a new dataset without tukityyppi
kasistuet <- select(kasistuet, one_of(keep_columns))

# print out the column names in kasistuet
colnames(kasistuet)


# Compute the average sum of summa
mean(kasistuet$summa)
