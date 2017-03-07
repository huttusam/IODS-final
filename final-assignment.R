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
keep_columns <- c("hakija", "elokuva", "lajityyppi", "tukisumma", "tukivuosi", "esittelija", "esit-sukup", "tukipvm")
kasistuet <- select(kasistuet, one_of(keep_columns))

# print out the column names in 'kasistuet'
colnames(kasistuet)
# OK. No column 'tukityyppi' left
summary(kasistuet)
# Write 'kasistuet' to a csv file
write.csv2(kasistuet, file = "data/kasistuet.csv")

# Added variable 'sukup' (gender) and 'esit-sukup' (gender) to 'kasistuet-sukup.csv' by hand! Possible values: M, N or O (company)

# Read 'kasistuet-sukup.csv' from a csv file
kasistuet <- read.csv2(file = "data/kasistuet-sukup.csv", header = TRUE)

# Vuosi has changed from factro to numeral!
View(kasistuet)

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
keep_columns <- c("lajityyppi","tukisumma","tukivuosi", "esit.sukup")
naiset <- select(naiset, one_of(keep_columns))

# building a datase where only men's script grants
miehet <- filter(kasistuet, sukup == "M")
keep_columns <- c("lajityyppi","tukisumma","tukivuosi", "esit.sukup")
miehet <- select(miehet, one_of(keep_columns))

# Printing summary of naiset and miehet
summary(miehet)
summary(naiset)

keep_columns <- c("sukup", "tukivuosi","lajityyppi", "tukisumma", "esit.sukup")
analysis_dataset <- select(kasistuet_human, one_of(keep_columns))
View(analysis_dataset)

# create a more advanced plot matrix with ggpairs()
p <- ggpairs(analysis_dataset, mapping = aes(col = esit.sukup, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
# draw the plot
p

