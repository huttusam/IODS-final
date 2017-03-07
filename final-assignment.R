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

# reading data from a Suomen Elokuvasäätiö CSV file to table 'sestuet'
sestuet <- read.csv2("data/SES_Tukipaeaetoekset_2008-2015.csv", header = TRUE)

# Added variable 'sukup' (gender) to data by hand! Possible values: M, N or O (company)

###############################
# WRANGLING dataset 'sestuet' #
###############################

# looking at structure of sestuet
str(sestuet)
# A dataset containing information of the Finnish Film Foundation funds/grants between 2008-2015

# looking at dimensions of sestuet
dim(sestuet)

# printing a summary of sestuet
summary(sestuet)

# Changing variable name 'Tukityypin nimi' to 'tukityyppi'
colnames(sestuet)[1] <- "tukityyppi"

# Changing variable name 'Elokuvan lajityyppi' to 'lajityyppi'
colnames(sestuet)[2] <- "lajityyppi"

# Changing variable name 'Tukihak päätöspvm' to 'tukipvm'
colnames(sestuet)[3] <- "tukipvm"

# Changing variable name 'Esittelijän nimi' to 'esittelija'
colnames(sestuet)[4] <- "esittelija"

# Changing variable name 'tuenhakijan nimi' to 'hakija'
colnames(sestuet)[5] <- "hakija"

# Changing variable name 'Tukihakemuksen kohde' to 'elokuva'
colnames(sestuet)[6] <- "elokuva"

# Changing variable name 'Tukihakemumuksen päätöseur' to 'tukisumma'
colnames(sestuet)[7] <- "tukisumma"

# print out the column names in data
colnames(sestuet)
# Looking ok

# Variable 'summa' is not numeric, removing the space from the string and changing type to numeric
str_replace(sestuet$tukisumma, pattern=" ", replace ="") %>% as.numeric() -> sestuet$tukisumma

# Variable 'tukipvm' is not a date: changing character string to date
sestuet$tukipvm <- as.Date(sestuet$tukipvm, "%d.%m.%Y")

# Extracting year from 'pvm' to a new variable 'tukivuosi'
sestuet$tukivuosi <- lubridate::year(sestuet$tukipvm)

# Changing 'vuosi' from numeral to factor
sestuet$tukivuosi <- factor(sestuet$tukivuosi)

# Reordering by column index to move variable 'tukivuosi' before 'tukipvm'
sestuet <- sestuet[c(6,7,1,2,5,4,8,3)]

# Variable 'lajityyppi' has some incorrect values: Renaming
sestuet$lajityyppi <- replace(sestuet$lajityyppi, sestuet$lajityyppi=="Tv-sarja", "TV-sarja")
sestuet$lajityyppi <- replace(sestuet$lajityyppi, sestuet$lajityyppi=="Lyhyt elokuva", "Lyhytelokuva")
sestuet$lajityyppi <- replace(sestuet$lajityyppi, sestuet$lajityyppi=="Animaatioelokuva", "Pitkä animaatioelokuva")
sestuet$lajityyppi <- replace(sestuet$lajityyppi, sestuet$lajityyppi=="Lasten ja nuorten elokuva", "Pitkä lasten ja nuorten elokuva")

# We are only interested in films produced in Finland: excluding rows where value of 'maa' does not contain the string 'SUOMI'
# suomielokuvat <- elokuvat %>% filter(str_detect(maa, "SUOMI"))

# looking at structure of 'sestuet'
str(sestuet)
# looking good, type of 'tukisumma' is now numeric, type of 'pvm' is now date, we have the funding year in 'tukivuosi', and no more value 'Tv-sarja' in 'lajityyppi'

# Save 'sestuet' to a csv file
write.csv2(sestuet, file = "data/sestuet.csv")

# We are only interested in funding issued for films: excluding rows where value of 'lajityyppi' is empty, but not NA
elokuvatuet <- filter(sestuet, (lajityyppi != '') | (is.na(lajityyppi) == TRUE))

# Checking the new dataset 'elokuvatuet'
View(elokuvatuet)
# OK. Only funding for films left.

# Save 'elokuvatuet' to a csv file
write.csv2(elokuvatuet, file = "data/elokuvatuet.csv")

# selecting only issued script grants ('Käsikirjoitusapuraha')
kasistuet <- filter(sestuet, tukityyppi == 'Käsikirjoitusapuraha')

# viewing the new dataset 'kasistuet'
View(kasistuet)
# Lookin good: containing only script grants

# excluding variable 'tukityyppi' since it's the same in all observations
keep_columns <- c("hakija", "elokuva", "lajityyppi", "tukisumma", "tukivuosi", "esittelija", "tukipvm")
kasistuet <- select(kasistuet, one_of(keep_columns))

# print out the column names in 'kasistuet'
colnames(kasistuet)
# OK. No column 'tukityyppi' left
summary(kasistuet)
# Write 'kasistuet' to a csv file
write.csv2(kasistuet, file = "data/kasistuet.csv")

# Added variable 'sukup' (gender) to 'kasistuet-sukup.csv' by hand! Possible values: M, N or O (company)

# Read 'kasistuet-sukup.csv' from a csv file
kasistuet <- read.csv2(file = "data/kasistuet-sukup.csv", header = TRUE)

# Vuosi has changed from factro to numeral!
str(kasistuet)

# Changing 'vuosi' from numeral to factor (again)
kasistuet$tukivuosi <- factor(kasistuet$tukivuosi)

#################################
# ANALYSING dataset 'kasistuet' #
#################################

# Excluding observations where applicant was a company (sukup=O)
kasistuet_human <- filter(kasistuet, sukup != "O")

View(kasistuet_human)
kasistuet_human$lajityyppi

# building a datase where only women's script grants
naiset <- filter(kasistuet_human, sukup == "N")
keep_columns <- c("lajityyppi","tukisumma","tukivuosi", "esittelija")
naiset <- select(naiset, one_of(keep_columns))

# building a datase where only men's script grants
miehet <- filter(kasistuet, sukup == "M")
keep_columns <- c("lajityyppi","tukisumma","tukivuosi", "esittelija")
miehet <- select(miehet, one_of(keep_columns))

# Printing summary of naiset and miehet
summary(miehet)
summary(naiset)

keep_columns <- c("sukup", "tukivuosi","tukisumma")
analysis_dataset <- select(kasistuet_human, one_of(keep_columns))
View(analysis_dataset)

# create a more advanced plot matrix with ggpairs()
p <- ggpairs(analysis_dataset, mapping = aes(col = sukup, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
# draw the plot
p


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




