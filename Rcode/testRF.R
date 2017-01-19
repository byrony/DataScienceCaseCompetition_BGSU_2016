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

inTrain <-createDataPartition(train$Donation, p=0.01, list = FALSE) 
training <- train[inTrain, ]
validation <- train[-inTrain, ]

set.seed(1357)
k = 3 #k folds
percentage <- c(0.2, 0.3)

TimeBegin <- Sys.time()
RF <- RF_Sampling(training, 'Donation', k, percentage, ntree=300)
TimeEnd <- Sys.time()

# store all the output of RF into a text file.
sink("sink.txt")
print(RF)
sink()
closeAllConnections()
