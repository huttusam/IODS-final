# Sampsa Huttunen, 5.3.2017, Wrangling Data & Analyzing for IODS course's Final Assignment

# Access libraries
library(dplyr)
library(ggplot2)
library(GGally)
library(stringr)
library(FactoMineR)
library(tidyr)

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

###############################
# ANALYSING dataset 'affairs' #
###############################

# Printing summary of 'affairs'
summary(affairs)
# n= 601; 315 females, 286 males; mean age: 32.48 years

# creating a geom_bar summary with gather()
gather(affairs) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()

# creating a plot matrix with ggpairs()
ggpairs(affairs, mapping = aes(alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))

# create a regression model with multiple explanatory variables
my_model2 <- lm(nbaffairs ~ ym + rate +religious, data = affairs)

# print out a summary of the model
summary(my_model2)

# Plotting nbaffairs vs rate
rap1 <- ggplot(affairs, aes(x = rate, y = nbaffairs, col = sex))
# define the visualization type (points)
rap2 <- rap1 + geom_point()
# draw the plot
rap2
# add a regression line
rap3 <- rap2 + geom_smooth(method = "lm")
# add a main title and draw the plot
rap4 <- rap3 + ggtitle("Experienced Happiness in Marriage versus Number of Extra-marital Affairs")
rap4

# Plotting nbaffairs vs ym
yp1 <- ggplot(affairs, aes(x = ym, y = nbaffairs, col = sex))
# define the visualization type (points)
yp2 <- yp1 + geom_point()
# draw the plot
yp2
# add a regression line
yp3 <- yp2 + geom_smooth(method = "lm")
# add a main title and draw the plot
yp4 <- yp3 + ggtitle("Number of Years Married versus Number of Extra-marital Affairs")
yp4

# Plotting nbaffairs vs religious
rlp1 <- ggplot(affairs, aes(x = religious, y = nbaffairs, col = sex))
# define the visualization type (points)
rlp2 <- rlp1 + geom_point()
# draw the plot
rlp2
# add a regression line
rlp3 <- rlp2 + geom_smooth(method = "lm")
# add a main title and draw the plot
rlp4 <- rlp3 + ggtitle("Religiousness versus Number of Extra-marital Affairs")
rlp4

# Plotting nbaffairs vs age
agp1 <- ggplot(affairs, aes(x = age, y = nbaffairs, col = sex))
# define the visualization type (points)
agp2 <- agp1 + geom_point()
# draw the plot
agp2
# add a regression line
agp3 <- agp2 + geom_smooth(method = "lm")
# add a main title and draw the plot
agp4 <- agp3 + ggtitle("Religiousness versus Number of Extra-marital Affairs")
agp4



# create a regression model with multiple explanatory variables
my_model2 <- lm(nbaffairs ~ ym + rate + religious + age, data = affairs)

# draw diagnostic plots using the plot() function. Choose the plots 1, 2 and 5
par(mfrow = c(2,2))
plot(my_model2, which = c(1:2, 5))
