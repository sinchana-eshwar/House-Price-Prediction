#Import Libraries

library(ggthemes)
library(dplyr)
library(gridExtra)
library(corrplot)
library(ggplot2)
library(GGally)
library(data.table)
library(scales)
library(MVA)

#Read Training and Testing dataset
training <- read.csv("D:/MultiAnalysis/Project/house-prices-advanced-regression-techniques/Data.csv.csv", stringsAsFactors=FALSE)
View(training)
testing <- read.csv("D:/MultiAnalysis/Project/house-prices-advanced-regression-techniques/test.csv")
View(testing)

?legend()

#Get the dimension and summary of training and testing data set
dim(training)
str(training)
summary(training)

dim(testing)
str(testing)
summary(testing)

#Since the train and test sets are divided into half we are combing them into one single dataset for further modeling purposes
housedata<-bind_rows(training,testing)
View(housedata)
head(housedata)
dim(housedata)
str(housedata)
summary(housedata)

#Find the total percentage of missing data
sum(is.na(housedata)/(nrow(housedata)*nrow(housedata)))
#Check for duplicate rows
unique(nrow(housedata))

#Check for N/A values all the columns
colSums(sapply(housedata,is.na))
library(Amelia)
missmap(training, main ="Missing values vs observed")


#Choosing the categorical and numerical variables for our analysis
catvar <- c('MSZoning','Street', 'Neighborhood', 'LandContour','BldgType', 'LandSlope', 'RoofStyle',
             'HouseStyle','CentralAir','PavedDrive','SaleCondition','OverallCond' )
numvar<-c('SalePrice','LotArea','TotalBsmtSF','GrLivArea','BedroomAbvGr','TotRmsAbvGrd','GarageCars','GarageArea'
          ,'OpenPorchSF','EnclosedPorch','WoodDeckSF','PoolArea')

attach(housedata)
catdf<-housedata[,catvar]
numdf<-housedata[,numvar]

#Check for distribution of variables that could attribute for higher Sale Price
par(mfrow=c(2,4))
hist(training$LotArea,xlab="Lot Area", main="Lot Area")
hist(training$TotalBsmtSF, xlab="Basement Area", main="Basement Area")
hist(training$BedroomAbvGr, xlab="No of Bedrooms", main="No of Bedrooms")
hist(training$GrLivArea, xlab="Living Area",main="Living Area")
hist(training$GarageCars, xlab="No. of Cars",main="No. of Cars")
hist(training$GarageArea, xlab="Garage Area",main="Garage Area")
hist(training$PoolArea, xlab="Pool Area",main="Pool Area")
hist(training$OpenPorchSF, xlab="Open Porch Area",main="Open Porch Area")

#Check how Sale price varies as per different Sale Conditions
ggplot(training, aes(x = SaleCondition, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

ggplot(training, aes(x = MSZoning, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


ggplot(training, aes(x = Street, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

ggplot(training, aes(x = LandContour, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

ggplot(training, aes(x = BldgType, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

ggplot(training, aes(x = LandSlope, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

ggplot(training, aes(x = RoofStyle, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

ggplot(training, aes(x = HouseStyle, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

ggplot(training, aes(x = CentralAir, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


ggplot(training, aes(x = PavedDrive, y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


ggplot(training, aes(x = as.factor(OverallCond), y = SalePrice)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


#Check relationship between Sale price and Lot Area
plot(SalePrice ~ LotArea, data = training, xlab ="Sale Price ", ylab = "Lot Area")


# Car Library has a scatterplot matrix
library(car)
#Check how Sale Price varies with plot area, basement area and ground living area
pairs(~SalePrice+LotArea+TotalBsmtSF+GrLivArea, data=training,col=c('red','blue','green'))

#Check for correlation between Sale Price and other variables
library(PerformanceAnalytics)
my_data <- training[, c('SalePrice','LotArea','TotalBsmtSF','GrLivArea','GarageArea')]
chart.Correlation(my_data, histogram=TRUE, pch=19)

my_data <- training[, c('SalePrice','BedroomAbvGr','TotRmsAbvGrd','GarageCars','OpenPorchSF','EnclosedPorch','WoodDeckSF')]
chart.Correlation(my_data, histogram=TRUE, pch=19)
