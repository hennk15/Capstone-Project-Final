#Loading packages
library(tidyverse)
library(readr)
library(ggplot2)
library(caret)
#library(RCurl) #To read the csv file from GitHub directly
library(rmarkdown)

#Opening file directory
setwd("C:/Users/Henry.Nketeh/Desktop")

#Opening team attendance file and assign it to a variable 
cleannew <- read.csv("teamAttendance5.csv")
glimpse(cleannew)

#Removing columns from the dataframe 
cleannew$X <- NULL 
cleannew$X.1 <- NULL
cleannew$Teams.Operating.Income <- NULL
cleannew$metro.area.population <- NULL
cleannew$X2018.2019.season.Road.Game.Attendance <- NULL
cleannew$Teams.Roster.Salary <- NULL
cleannew$star.expenses <- NULL

#creating column to show teams that met/did not meet the guarentee increase rate. 
cleannew$Increase<- ifelse(cleannew$Teams.Revenue >= 169.99,1,0)

#Renaming data frame variables
colnames(cleannew)[colnames(cleannew)=="Teams.Revenue"] <- "Difference"
colnames(cleannew)[colnames(cleannew)=="win.to.player.cost.ratio.increase"] <- "Percentage increase"
colnames(cleannew)[colnames(cleannew)=="Teams.location.median.income"] <- "1 percent increase of attendance"
glimpse(cleannew)
str(cleannew)
   
#Validation results
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(attendance$Team.Rating, p=0.80, list=FALSE)
# select 20% of the data for testing 
validation <- attendance[-validation_index,]
# use the remaining 80% of data to training and testing the models
Adata <- attendance[validation_index,]
#summary of 80% of data 
summary(Adata)

#Statistical Analysis 
#Creating Scatter plot and assigning x and y axis and title
ggplot(Adata, aes(y = NBA.team, x = Difference , color=Increase)) +
  geom_point() + ggtitle("2019 NBA Team Attendance Increase")

#Creating Scatter plot and assigning x and y axis and title 
ggplot(Adata, aes(y = NBA.team, x =Team.Rating , color=NRTG)) +
  geom_point() + ggtitle("2019 NBA Team Ratings")

#Machine Learning
#Linear regression model for dependant variable
model = lm( Team.Rating ~ NRTG, data= Adata)
summary(model)

#Creating a second model for training data 
model2 =lm(Team.Rating ~ Difference, data=Adata)
summary(model2)

#Creating a third model for the training data 
model3=lm(Team.Rating ~ NRTG + Difference, data=Adata)
summary(model3)

#Randomly going through the training data set and an it 5 times to get the best model out of the 5
control <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(Team.Rating ~ NRTG,data=Adata, method="lm", metric="RMSE", trControl=control)

# display results
print(fit)

#Saving new file for overall project code
write.csv(cleannew, "CapstoneCode.csv")

