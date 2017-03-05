# Sampsa Huttunen, 5.3.2017, Wrangling Data for IODS course's Final Assignment

# Access the dplyr library
library(dplyr)

# Access the gglot2 library
library(ggplot2)

# reading data from a Suomen Elokuvasäätiö CSV files to tables 'elokuvat', 'sestuet' and 'teatterit'
elokuvat <- read.csv2("data/Ensi-illat_ja_katsojat_2004-2014.csv", header = TRUE)
sestuet <- read.csv2("data/SES_Tukipaeaetoekset_2008-2015.csv", header = TRUE)
teatterit <- read.csv2("data/Elokuvateatterit_2015.csv", header = TRUE)
