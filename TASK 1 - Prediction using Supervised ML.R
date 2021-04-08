#clear the console
cat("\f")
#The Sparks Foundation || GRIP_APRIL-21
# Data Science and Business Analytics internship with spark foundation
# TASK 1 - Prediction using Supervised ML
# date:8/4/21
# Author : EBENEZER M
# 
# To Predict the percentage of marks of the students based on 
#the number of hours they studied & find the predicted score if a student 
#studies for 9.25 hrs/day.

# Steps involved

# Reading the Dataset
# Visualizing the given data
# Splitting the data into train and test splits
# create the supervised ML model using "lm" function 
# Finding coefficient, intercept values to get the line equation
# Drawing a Regression line for the data
# Analysing how better the data is predicted
# Evaluate the model

# set the working directory
setwd("E:/sparkfoundation")

#import the dataset 
md=read.csv("study.csv")

# convert the csv file format into data_frame format
md=as.data.frame(md)

# view the data set
head(md)
# structure of the data set
str(md)
# summary of the data set
summary(md)
#checking for Missing Values
print(any(is.na(md)))



#correlation among the data
cor(md)

#Visualizing the given data using ggplot2 package
library(ggplot2)
ggplot(md, aes(x= Hours, y = Scores,col = 'red'))+ geom_point()
#Drawing a Regression line for the data
ggplot(md, aes(x= Hours, y = Scores))+ geom_point(aes(col = "#C42126")) + geom_smooth(method='lm',formula = y~x)

#Splitting the data into train and test splits
pd=sample(2,nrow(md),replace = TRUE,prob = c(0.7,0.3))
pd=as.data.frame(pd)
train=md[pd==1,]
test=md[pd==2,]

#Splitting the data into dependent and independent variables
y=c(train$Scores)
x=c(train$Hours)
#create the supervised ML model called mod using lm function
mod=lm(y~x)

#print the model
print(mod)

#Analysing how better the data is predicted
res=predict(mod,test)
print(res)

# Task given : What will be 
# predicted score if a student studies for 9.25 hrs/day?
g=data.frame(x=9.2)
r=predict(mod,g)
print(r)
#Evaluate the model using root mean square error method
sqrt(mean(mod$residuals^2))

