## ElasticNet tunes alpha, only handle the original data, as we found the original ratio performs best, 
## so no extra sampling is added here

ElasticNet_tuning_alpha <- function(data, preVariable, k, alpha){
    folds <- createFolds(data[ ,which(names(data) == preVariable)], k=k, list = TRUE)
    allAuc <- rep(NA, k); allF <- rep(NA, k); allLift <- rep(NA, k)
    raw <- rep(list(NA), k)
    for(i in 1:k){
        kfolds.train <- data[-folds[[i]], ]
        kfolds.test <- data[folds[[i]], ]
        x <- model.matrix(~., data=kfolds.train[, -which(names(kfolds.train) == preVariable)])
        y <- kfolds.train[ ,which(names(kfolds.train) == preVariable)]
        cv.lasso.fit <- cv.glmnet(x, y, alpha = alpha, family = 'binomial')
        
        newx <- model.matrix(~., data=kfolds.test[, -which(names(kfolds.test) == preVariable)])
        cv.lasso.pred <- predict(cv.lasso.fit, newx=newx, s='lambda.min', type='response')
        
        measures <- Measures(cv.lasso.pred, kfolds.test[, which(names(kfolds.test) == preVariable)])
        allAuc[i] <- measures[[1]]
        allF[i] <- as.numeric(measures[[2]][2])
        allLift[i] <- as.numeric(measures[[2]][3])
        raw[[i]] <- measures
    }
    return(list(Auc=allAuc, F1=allF, Lift=allLift, Raw=raw))
}
