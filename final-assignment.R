
#Set working directory
setwd("I:/Google Drive/Helsingin yliopisto/Intro to Open Data Science/IODS-project/data")

#Read the "Human development" and "Gender inequality" datas
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#Load dplyr library
library(dplyr)

#Explore the datasets
dim(hd)
colnames(hd)
str(hd)
summary(hd)
dim(gii)
colnames(gii)
str(gii)
summary(gii)

#Rename the variables
colnames(hd)[1] <- "HDIrank"
colnames(hd)[2] <- "Country"
colnames(hd)[3] <- "HDI"
colnames(hd)[4] <- "LifeExp"
colnames(hd)[5] <- "ExpEduYrs"
colnames(hd)[6] <- "MeanEduYrs"
colnames(hd)[7] <- "GNI"
colnames(hd)[8] <- "GNIminusHDIrank"

colnames(gii)[1] <- "GIIrank"
colnames(gii)[2] <- "Country"
colnames(gii)[3] <- "GII"
colnames(gii)[4] <- "MMR"
colnames(gii)[5] <- "ABR"
colnames(gii)[6] <- "FemaleParl"
colnames(gii)[7] <- "EduFemale"
colnames(gii)[8] <- "EduMale"
colnames(gii)[9] <- "LabourFemale"
colnames(gii)[10] <- "LabourMale"

#Check the new variable names
colnames(hd)
colnames(gii)

#Mutate the Gender inequality data and create two new variables
gii$EduRatio <- gii$EduFemale / gii$EduMale
gii$LabourRatio <- gii$LabourFemale / gii$LabourMale

#Checking variable names and structure of Gender inequality data
colnames(gii)
str(gii)

# Joining the datasets using Country as identifier
human <- inner_join(hd, gii, by = "Country")

#Checking variable names and structure of the new combined dataset
colnames(human)
str(human)

#Checking dimensions of the new dataset
dim(human)
# 195 observations and 19 variables

# Save joined and modified data 'human' to a file that looks good in a European version of Excel, but also has a decimal point instead of a comma
write.table(human, file = "human.csv", sep = ";", qmethod="double", row.names=FALSE)

# access the stringr package
library(stringr)

# remove the commas from GNI and create a numeric version of it
str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric() -> human$GNI

# Checking if human$GNI is now numeric: looks ok.
str(human$GNI)

# Accessing dplyr library
library(dplyr)

# Keeping only desired columns
keep <- c("Country", "EduFemale", "LabourFemale", "ExpEduYrs", "LifeExp", "GNI", "MMR", "ABR", "FemaleParl")
human <- select(human, one_of(keep))

# Checking structure of human: looks ok.
str(human)

# filter out all rows with NA values
human <- filter(human, complete.cases(human))

#Checking human: looks ok, no NA values
human

# look for regions in human$countries
human$Country
# Looks like regions are the last variables

# look at the last 10 observations of human
tail(human, n=10)
# Seems that last 7 should go

# define the last indice we want to keep
last <- nrow(human) - 7

# choose everything until the last 7 observations
human <- human[1:last, ]

#Checking human$Country: looks ok, only countries
human$Country

# Define the row names of the data by the country names
rownames(human) <- human$Country

#Checking row names: ok.
rownames(human)

# remove the Country variable
human <- select(human, -Country)

# Save joined and modified data 'human' over 'human.csv' with row names
write.table(human, file = "human.csv", sep = ";", qmethod="double", row.names=TRUE, col.names = NA)

