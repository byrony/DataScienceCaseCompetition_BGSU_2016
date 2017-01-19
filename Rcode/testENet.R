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
source("ElasticNet_tuning_alpha.R")

inTrain <-createDataPartition(train$Donation, p=0.01, list = FALSE) 
training <- train[inTrain, ]
validation <- train[-inTrain, ]

set.seed(1357)
k = 10 #k folds
#alpha = seq(0, 0.2, by=0.1)
alpha = c(0.5, 0.8, 0.9)

TimeBegin <- Sys.time()
ENet <- rep(list(NA), length(alpha))
for(i in 1:length(alpha)){
    ENet[[i]] <- ElasticNet_tuning_alpha(train, 'Donation', k, alpha[i])
}

TimeEnd <- Sys.time()

# store all the output of RF into a text file.
sink("ENet.txt")
print(ENet)
sink()
closeAllConnections()
