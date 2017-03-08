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

# reading data from a CSV file to table 'affairs'
affairs <- read.csv("data/Fair.csv", header = TRUE)

###############################
# WRANGLING dataset 'affairs' #
###############################

# looking at structure of affairs
str(affairs)
# A dataset containing answers to an extramarital affairs study

# looking at dimensions of affairs
dim(affairs)
# 601 observations and 10 variables

# printing a summary of affairs
summary(affairs)
# n= 601; 315 females, 286 males; mean age: 32.48 years

# Changing variable name 'Tukityypin nimi' to 'tukityyppi'
colnames(affairs)[1] <- "tukityyppi"

# Changing variable name 'Elokuvan lajityyppi' to 'lajityyppi'
colnames(affairs)[2] <- "lajityyppi"

# Changing variable name 'Tukihak päätöspvm' to 'tukipvm'
colnames(affairs)[3] <- "tukipvm"

# Changing variable name 'Esittelijän nimi' to 'esittelija'
colnames(affairs)[4] <- "esittelija"

# Changing variable name 'tuenhakijan nimi' to 'hakija'
colnames(affairs)[5] <- "hakija"

# Changing variable name 'Tukihakemuksen kohde' to 'elokuva'
colnames(affairs)[6] <- "elokuva"

# Changing variable name 'Tukihakemumuksen päätöseur' to 'tukisumma'
colnames(affairs)[7] <- "tukisumma"

# print out the column names in data
colnames(affairs)
# Looking ok

# Variable 'summa' is not numeric, removing the space from the string and changing type to numeric
str_replace(affairs$tukisumma, pattern=" ", replace ="") %>% as.numeric() -> affairs$tukisumma

# Variable 'tukipvm' is not a date: changing character string to date
affairs$tukipvm <- as.Date(affairs$tukipvm, "%d.%m.%Y")

# Extracting year from 'pvm' to a new variable 'tukivuosi'
affairs$tukivuosi <- lubridate::year(affairs$tukipvm)

# Changing 'vuosi' from numeral to factor
affairs$tukivuosi <- factor(affairs$tukivuosi)

# Reordering by column index to move variable 'tukivuosi' before 'tukipvm'
affairs <- affairs[c(6,7,1,2,5,4,8,3)]

# Variable 'lajityyppi' has some incorrect values: Renaming
affairs$lajityyppi <- replace(affairs$lajityyppi, affairs$lajityyppi=="Tv-sarja", "TV-sarja")
affairs$lajityyppi <- replace(affairs$lajityyppi, affairs$lajityyppi=="Lyhyt elokuva", "Lyhytelokuva")
affairs$lajityyppi <- replace(affairs$lajityyppi, affairs$lajityyppi=="Animaatioelokuva", "Pitkä animaatioelokuva")
affairs$lajityyppi <- replace(affairs$lajityyppi, affairs$lajityyppi=="Lasten ja nuorten elokuva", "Pitkä lasten ja nuorten elokuva")

# looking at structure of 'affairs'
str(affairs)
# looking good, type of 'tukisumma' is now numeric, type of 'pvm' is now date, we have the funding year in 'tukivuosi', and no more value 'Tv-sarja' in 'lajityyppi'

# Save 'affairs' to a csv file
write.csv2(affairs, file = "data/affairs.csv")

# We are only interested in funding issued for films: excluding rows where value of 'lajityyppi' is empty, but not NA
elokuvatuet <- filter(affairs, (lajityyppi != '') | (is.na(lajityyppi) == TRUE))

# Checking the new dataset 'elokuvatuet'
View(elokuvatuet)
# OK. Only funding for films left.

# Save 'elokuvatuet' to a csv file
write.csv2(elokuvatuet, file = "data/elokuvatuet.csv")

# selecting only issued script grants ('Käsikirjoitusapuraha')
kasistuet <- filter(affairs, tukityyppi == 'Käsikirjoitusapuraha')

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

sum(miehet$tukisumma)
sum(naiset$tukisumma)

mean(miehet$tukisumma)
mean(naiset$tukisumma)

median(naiset$tukisumma)
median(miehet$tukisumma)

keep_columns <- c("sukup", "tukisumma")
analysis_dataset <- select(kasistuet_human, one_of(keep_columns))
View(analysis_dataset)

# create a more advanced plot matrix with ggpairs()
p <- ggpairs(analysis_dataset, mapping = aes(col = sukup, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
# draw the plot
p

