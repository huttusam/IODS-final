################################
# WRANGLING dataset 'elokuvat' #
################################

# Sampsa Huttunen, 5.3.2017, Wrangling Data & Analyzing for IODS course's Final Assignment

# Access the dplyr library
library(dplyr)

# Access the gglot2 library
library(ggplot2)

# access the GGally
library(GGally)

# access the stringr package
library(stringr)

#Set working directory
setwd("I:/Google Drive/Helsingin yliopisto/Intro to Open Data Science/IODS-final")

# reading data from a Suomen Elokuvasäätiö CSV file to table 'elokuvat'
elokuvat <- read.csv2("data/Ensi-illat_ja_katsojat_2004-2014.csv", header = TRUE)

# checking structure of 'elokuvat': a dataset containing information of the Finnish movie premieres and audiences between 2004-2014
str(elokuvat)

# looking at dimensions of 'elokuvat'
dim(elokuvat)

# printing a summary of 'elokuvat'
summary(elokuvat)

# Changing variable name 'NIMI' to 'elokuva'
colnames(elokuvat)[1] <- "elokuva"

# Changing variable name 'ALKPERÄISNIMI' to 'orignimi'
colnames(elokuvat)[2] <- "orignimi"

# Changing variable name 'ENSI.ILTA' to 'enskari'
colnames(elokuvat)[3] <- "enskari"

# Changing variable name 'KUMULATIIVISET.KATSOJAT' to 'katsojat'
colnames(elokuvat)[4] <- "katsojat"

# Changing variable name of 'LEVITTÄJÄ' to 'levittaja'
colnames(elokuvat)[5] <- "levittaja"

# Changing variable name 'MAANIMI' to 'maa'
colnames(elokuvat)[6] <- "maa"

# print out the column names in data
colnames(elokuvat)

# Variable 'katsojat' is not numeric, removing the space from the string and changing type to numeric
str_replace(elokuvat$katsojat, pattern=" ", replace ="") %>% as.numeric() -> elokuvat$katsojat

# Variable 'enskari' is not a date: changing type to date
elokuvat$enskari <- as.Date(elokuvat$enskari, "%d.%m.%Y")

# Extracting year from date to a new variable 'vuosi'
elokuvat$vuosi <- lubridate::year(elokuvat$enskari)

# Changing 'vuosi' from numeral to factor
elokuvat$vuosi <- factor(elokuvat$vuosi)

# Reordering by column index to move variable 'vuosi' before 'enskari'
elokuvat <- elokuvat[c(1,4,7,5,3,2,6)]

# looking at structure of elokuvat
str(elokuvat)
# looking good, type of 'katsojat' is now numeric, type of 'enskari' is now date and premiere year is in 'vuosi'

# Save 'elokuvat' to a csv file
write.csv2(elokuvat, file = "data/elokuvat.csv")

# We are only interested in films produced in Finland: excluding rows where value of 'maa' does not contain the string 'SUOMI'
suomielokuvat <- elokuvat %>% filter(str_detect(maa, "SUOMI"))

# Checking the structure of the new dataset 'suomielokuvat'
View(suomielokuvat)
# OK. Only Finnish films left

# Save 'suomielokuvat' to a csv file
write.csv2(suomielokuvat, file = "data/suomielokuvat.csv")
