library(caret)
library(ROCR)
library(pROC)
library(DMwR)
library(glmnet)
library(randomForest)

source("ReadData.R")
source("SMOTE_Sampling.R")
source("Measures.R")
source("Logistic_Sampling.R")

inTrain <-createDataPartition(train$Donation, p=0.01, list = FALSE) 
training <- train[inTrain, ]
validation <- train[-inTrain, ]

set.seed(1357)
k = 5 #k folds
percentage <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
percentage <- c(0.2, 0.3)

TimeBegin <- Sys.time()
Log <- Logistic_Sampling(train, 'Donation', k, percentage)
TimeEnd <- Sys.time()

# store all the output of RF into a text file.
sink("logistic.txt")
print(Log)
sink()
closeAllConnections()
