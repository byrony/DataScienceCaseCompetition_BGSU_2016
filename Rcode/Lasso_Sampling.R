## Function Lasso_Sampling fit Lasso logisic regression
## Do k-folds cross validation for each sampling percentage
## Four parameters, data, response variable, k-folds, and sampling percentage

Lasso_Sampling <- function(data, preVariable, k, percentage){
    folds <- createFolds(data[ ,which(names(data) == preVariable)], k=k, list = TRUE)
    m <- length(percentage)
    # Set array and list to store result of each percentage in each fold. Each has k rows and m columns
    allAuc <- array(NA, dim=c(k, m)); allF <- array(NA, dim=c(k, m)); allLift <- array(NA, dim=c(k, m))
    raw <- rep(list(NA), m*k) # no way to construct a two-dimensional list?
    for(i in 1:k){
        # For each fold, get the train and test data.
        kfolds.train_beforeResample <- data[-folds[[i]], ]
        kfolds.test <- data[folds[[i]], ]
        for(j in 1:m){
            # For each fold, resample the training data.
            kfolds.train <- SMOTE_Sampling(kfolds.train_beforeResample, 'Donation', percentage[j])
            x <- model.matrix(~., data=kfolds.train[, -which(names(kfolds.train) == preVariable)])
            y <- kfolds.train[ ,which(names(kfolds.train) == preVariable)]
            cv.lasso.fit <- cv.glmnet(x, y, alpha = 1, family = 'binomial')
            
            newx <- model.matrix(~., data=kfolds.test[, -which(names(kfolds.test) == preVariable)])
            cv.lasso.pred <- predict(cv.lasso.fit, newx=newx, s='lambda.min', type='response')
            
            measures <- Measures(cv.lasso.pred, kfolds.test[, which(names(kfolds.test) == preVariable)])
            allAuc[i,j] <- measures[[1]]
            allF[i, j] <- as.numeric(measures[[2]][2])
            allLift[i, j] <- as.numeric(measures[[2]][3])
            raw[[ (i-1)*m +j ]] <- measures
        }
    }
    return(list(Auc=allAuc, F1=allF, Lift=allLift, Raw=raw))
}