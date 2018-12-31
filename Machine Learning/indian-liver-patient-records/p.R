library(randomForest)
library(ggplot2)
library(caret)
library(C50)
library(gmodels)
library(gbm)
library(e1071)

data <- read.csv("indian_liver_patient.csv",header = T)
summary(data)

data$Label <- as.factor(data$Label)

#Removing NA's in Albumin_and_Globulin_Ratio
data$Albumin_and_Globulin_Ratio <- ifelse(is.na(data$Albumin_and_Globulin_Ratio),
                                          mean(data$Albumin_and_Globulin_Ratio,na.rm = T),
                                          data$Albumin_and_Globulin_Ratio)

#Plotting data to see outliers 
boxplot(data$Total_Bilirubin)
boxplot(data$Direct_Bilirubin)
boxplot(data$Alkaline_Phosphotase)
boxplot(data$Alamine_Aminotransferase)
boxplot(data$Aspartate_Aminotransferase)
boxplot(data$Total_Protiens)
boxplot(data$Albumin_and_Globulin_Ratio)

#Removing some outlier data
data <- data[-which.max(data$Total_Bilirubin),]
data <- data[-which.max(data$Aspartate_Aminotransferase)]

#Checking the distribution of data
set.seed(3000)
table(trainData$Label)/nrow(trainData)
table(testData$Label)/nrow(testData)

#Creating training and testing data
trainIndex <- createDataPartition(data$Label,p = .9,list = F,times = 1)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

#Finding the most important independent variables
randomForestModel <- randomForest(data$Label ~ .,data,importance = T)
plot(randomForestModel)

varImpPlot(randomForestModel,sort = T,n.var = 3)

importantVar <- data.frame(importance(randomForestModel,type = 2))
importantVar$Name <- row.names(importantVar)
importantVar <- importantVar[order(importantVar$MeanDecreaseGini,decreasing = T),]
# 3 Most important variables are 
imp <- head(importantVar,n=3)
write.csv(imp,"impvariables.csv")


#Random Forrest
set.seed(100)
randomForestModelMain <- randomForest(trainData$Label ~ .,trainData,importance = T)
randomForestModelMain

testData$Predict <- predict(randomForestModelMain,testData)
testData
plot(randomForestModelMain)
confusionMatrix(data = testData$Predict,reference = testData$Label)

write.csv(testData,"rf.csv")
# Improving model performance

inrPer <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
randomForestModelMainImproved <- train(trainData$Label ~ .,data = trainData,method = "rf",
                                       trControl = inrPer) #randomForest(trainData$Label ~ .,trainData,trControl = inrPer)

randomForestModelMainImproved

varImpPlot(randomForestModelMain,sort = T,n.var = 7)
#newRFTrainData <- data.frame(trainData$Aspartate_Aminotransferase)
#newRFTrainData$Direct_Bilirubin <- trainData$Direct_Bilirubin
#newRFTrainData$Total_Bilirubin <- trainData$Total_Bilirubin
#newRFTrainData$Alamine_Aminotransferase <- trainData$Alamine_Aminotransferase
#newRFTrainData$Albumin <- trainData$Albumin
#newRFTrainData$Age <- trainData$Age
#newRFTrainData$Gender <- trainData$Gender
#newRFTrainData$Label <- trainData$Label

#randomForestModelMain1 <- randomForest(newRFTrainData$Label~.,newRFTrainData,importance = T)
#randomForestModelMain1


#The accuracy of random forrest is 71

#Decision Tree

dim(trainData)
decisionTreeModel <- C5.0(trainData[-11],trainData$Label,trials = 10)
decisionTreeModel
summary(decisionTreeModel)

dtPrediction <- predict(decisionTreeModel,testData)
testData$Prediction <- dtPrediction

write.csv(testData,"dt.csv")
confusionMatrix(dtPrediction,testData$Label)
CrossTable(testData$Label,dtPrediction)

#The accuracy of Decision trees is 77 after boosting(73)

#Gradient Boost

#Getting data ready
trainData$Label <- ifelse(trainData$Label == 1,0,1)
trainData$Gender <- ifelse(trainData$Gender == "Male",0,1)
str(trainData)
gbModel <- gbm(formula = trainData$Label ~ .,data = trainData,distribution = "bernoulli"
              ,shrinkage = 0.3)
str(testData)
dbPrediction <- predict(gbModel,testData[-11],n.trees = 50)
dbPrediction
prediction <- ifelse(dbPrediction>0.5,1,0)
prediction
print(gbModel)
plot(gbModel)

#Naive Baise
summary(trainData)
dim(trainData)
dim(testData)
str(trainData)
summary(trainData[,-11])

trainData$Label <- as.factor(trainData$Label)


nbModel <- naiveBayes(trainData[,-11],trainData[,11],laplace = 2)


summary(testData)

testDataNB <- testData[-11]

testData$Prediction <- predict(nbModel,testDataNB)  
write.csv(testData,file = "nb.csv")
snbModelPrediction  
testData  
  
confusionMatrix(testData$Prediction,testData$Label)
  
#Accuracy is 61
  
  
  





