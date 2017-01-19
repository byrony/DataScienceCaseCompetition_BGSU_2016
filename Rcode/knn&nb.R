# select the training data from train
inTrain <-createDataPartition(train$Donation, p=0.01, list = FALSE) 
training <- train[inTrain, ]
validation <- train[-inTrain, ]

# change the level name of Donation in order to compute Prob. using Caret
# it seems the factor name can't begin with numeric

t <- sapply(training$Donation, function(x){ifelse(x == 0, 'NO', 'YES')} )
training$Donation <- as.factor(t)

eGrid <- expand.grid(alpha=(0:10)*0.1, 
                     lambda = seq(0,0.03,by=0.01))

Control <- trainControl(method = 'cv', 
                        number = 5, 
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary
)
netFit <- train(Donation~., data=training, 
                method="glmnet", 
                tuneGrid=eGrid, 
                trControl=Control,
                metric = "ROC",
                preProcess=c('center', 'scale'))


#-------------
#-----------------------------------knn----------------------------------------
train_knn <- train

set.seed(1357)
t <- rep(NA, nrow(train_knn))
t <- sapply(train_knn$Donation, function(x){ifelse(x == 0, 'NO', 'YES')} )
train_knn$Donation <- as.factor(t)

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
                           )
knnfit <- train(Donation ~., data=train_knn[, 134:136], method="knn",
                trControl = fitControl,
                metric = 'ROC',
                preProc = c("center", "scale"))

knn.pred <- predict(knnfit, train_knn)

KNN <- Measures(knn.pred[,2], training$Donation)




#-----------------------------------naive bayes-------------------------------
library(e1071)
TimeBegin <- Sys.time()
nb <- naiveBayes(Donation ~., data = train)
TimeEnd <- Sys.time()
pred <- predict(nb, train, type = "raw")
#table(pred, training$Donation)
nb_pred <- Measures(pred[,2], train$Donation)


# use caret
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 1,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
                           )
nbfit <- train(Donation ~., data=training, method = 'nb',
               trControl = fitControl,
               metric = 'ROC',
               na.action = na.omit
               )
nbfit_pred <- predict(nbfit, data=training, type = "prob")
Measures(nbfit_pred[,2], training$Donation)


# naive bayes k-folds
nb_k_folds <- function(data, preVariable, k){
    folds <- createFolds(data[ ,which(names(data) == preVariable)], k=k, list = TRUE)
    allAuc <- rep(NA, k); allF <- rep(NA, k); allLift <- rep(NA, k)
    raw <- rep(list(NA), k)
    for(i in 1:k){
        kfolds.train <- data[-folds[[i]], ]
        kfolds.test <- data[folds[[i]], ]
        
        f <- paste(preVariable, '~', '.')
        nb <- naiveBayes(as.formula(f), data = kfolds.train)
        
        nb.pred <- predict(nb, kfolds.test, type='raw')

        measures <- Measures(nb.pred[,2], kfolds.test[, which(names(kfolds.test) == preVariable)])
        allAuc[i] <- measures[[1]]
        allF[i] <- as.numeric(measures[[2]][2])
        allLift[i] <- as.numeric(measures[[2]][3])
        raw[[i]] <- measures
    }
    return(list(Auc=allAuc, F1=allF, Lift=allLift, Raw=raw))
}

TimeBegin <- Sys.time()
NB <- nb_k_folds(train, 'Donation', 5)
TimeEnd <- Sys.time()
