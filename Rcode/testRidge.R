library(caret)
library(ROCR)
library(pROC)
library(DMwR)
library(glmnet)
library(randomForest)

source("ReadData.R")
source("SMOTE_Sampling.R")
source("Measures.R")
source("RF_Sampling.R")
source("Ridge_Sampling.R")

inTrain <-createDataPartition(train$Donation, p=0.01, list = FALSE) 
training <- train[inTrain, ]
validation <- train[-inTrain, ]

set.seed(1357)
k = 5 #k folds
percentage <- c(0.1, 0.2, 0.3)

TimeBegin <- Sys.time()
Ridge <- Ridge_Sampling(train, 'Donation', k, percentage)
TimeEnd <- Sys.time()

# store all the output of RF into a text file.
sink("Ridge.txt")
print(Ridge)
sink()
closeAllConnections()
