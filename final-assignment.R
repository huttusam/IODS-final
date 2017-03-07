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

# reading data from a Suomen Elokuvasäätiö CSV files to tables 'elokuvat', 'sestuet' and 'teatterit'
elokuvat <- read.csv2("data/Ensi-illat_ja_katsojat_2004-2014.csv", header = TRUE)
sestuet <- read.csv2("data/SES_Tukipaeaetoekset_2008-2015.csv", header = TRUE)
teatterit <- read.csv2("data/Elokuvateatterit_2015.csv", header = TRUE)

################################
# WRANGLING datasedt 'sestuet' #
################################

# viewing sestuet: a dataset containing information of the Finnish Film Foundation funds/grants between 2008-2015
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

# Variable 'summa' is not numeric, removing the space from the string and changing type to numeric
str_replace(sestuet$summa, pattern=" ", replace ="") %>% as.numeric() -> sestuet$summa

# Variable 'pvm' is not a date: changing character string to date
sestuet$pvm <- as.Date(sestuet$pvm, "%d.%m.%Y")

# Extracting year from date to a new variable 'vuosi'
sestuet$vuosi <- lubridate::year(sestuet$pvm)

# Reordering by column index to move variable vuosi before date
sestuet <- sestuet[c(1,2,8,3,4,5,6,7)]

# Variable 'lajityyppi' has both values 'TV-sarja' and 'Tv-sarja': Renaming
sestuet$lajityyppi <- replace(sestuet$lajityyppi, sestuet$lajityyppi=="Tv-sarja", "TV-sarja")

# looking at structure of sestuet
str(sestuet)
View(sestuet)
# looking good, type of summa is now numeric, type of pvm is now date, we have the year, and no more value 'Tv-sarja' in lajityyppi

# We are only interested in funding issued for films: excluding rows where value of lajityyppi is empty
elokuvatuet <- filter(sestuet, lajityyppi != '')

# Checking the new dataset elokuvatuet
View(elokuvatuet)
# OK. Only funding for films left

# selecting only issued script grants ('Käsikirjoitusapuraha')
kasistuet <- filter(sestuet, tukityyppi == 'Käsikirjoitusapuraha')

# viewing the new dataset kasistuet
View(kasistuet)
# Lookin good: containing only script grants

# excluding variable 'tukityyppi' since it's the same in all observations
keep_columns <- c("lajityyppi", "pvm", "esittelija", "hakija", "elokuva", "summa")
kasistuet <- select(kasistuet, one_of(keep_columns))

# print out the column names in kasistuet
View(kasistuet)
# OK. No column 'tukityyppi' left

#################################
# WRANGLING datasedt 'elokuvat' #
#################################

# viewing elokuvat: a dataset containing information of the Finnish movie premieres and audiences between 2004-2014
View(elokuvat)

# looking at dimensions of elokuvat
dim(elokuvat)

# looking at structure of elokuvat
str(elokuvat)

# printing a summary of elokuvat
summary(elokuvat)

# Changing name of Tukityypin nimi to tukityyppi
colnames(elokuvat)[1] <- "nimi"

# Changing name of Elokuvan lajityyppi to lajityyppi
colnames(elokuvat)[2] <- "orignimi"

# Changing name of Tukihak päätöspvm to pvm
colnames(elokuvat)[3] <- "enskari"

# Changing name of Esittelijän nimi to esittelija
colnames(elokuvat)[4] <- "katsojat"

# Changing name of tuenhakijan nimi to hakija
colnames(elokuvat)[5] <- "levittaja"

# Changing name of Tukihakemuksen kohde to elokuva
colnames(elokuvat)[6] <- "maa"

# print out the column names in data
colnames(elokuvat)

# Variable 'katsojat' is not numeric, removing the space from the string and changing type to numeric
str_replace(elokuvat$katsojat, pattern=" ", replace ="") %>% as.numeric() -> elokuvat$katsojat

# Variable 'enskari' is not a date: changing character string to date
elokuvat$enskari <- as.Date(elokuvat$enskari, "%d.%m.%Y")

# Extracting year from date to a new variable 'vuosi'
elokuvat$vuosi <- lubridate::year(elokuvat$enskari)

# Reordering by column index to move variable vuosi before date
elokuvat <- elokuvat[c(1,2,7,3,4,5,6)]

# looking at structure of elokuvat
str(elokuvat)
View(elokuvat)
# looking good, type of katsojat is now numeric, type of enskari is now date and we have the year

# We are only interested in films produced in Finland: excluding rows where value of maa does not contain the string 'SUOMI'
suomielokuvat <- elokuvat %>% filter(str_detect(maa, "SUOMI"))

# Checking the new dataset elokuvatuet
View(suomielokuvat)
# OK. Only Finnish films left




