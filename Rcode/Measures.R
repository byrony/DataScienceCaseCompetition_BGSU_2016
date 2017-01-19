# measure the performance of the predicted values.

Measures <- function(fitted, ref){
    # fitted is the probability
    pred <- prediction(fitted, ref)
    tpr_fpr <- performance(pred, "tpr", "fpr")
    f <- performance(pred, "f")
    auc.temp <- performance(pred, "auc"); auc <- as.numeric(auc.temp@y.values)
    f_lift <- performance(pred, 'f','lift')
    #plot(tpr_fpr);plot(f)
    cutoffs_tpr_fpr <- data.frame(cut=tpr_fpr@alpha.values[[1]], 
                                  tpr=tpr_fpr@y.values[[1]], fpr=tpr_fpr@x.values[[1]])
    cutoffs_tpr_fpr <- cutoffs_tpr_fpr[-1, ]
    cutoffs_f_lift <- data.frame(cut=f_lift@alpha.values[[1]], 
                                 f=f_lift@y.values[[1]], lift=f_lift@x.values[[1]])
    cutoffs_f_lift <- cutoffs_f_lift[-1, ]
    
    best <- cutoffs_f_lift[which(cutoffs_f_lift$f==max(cutoffs_f_lift$f)), ]
    cutoff <- best$cut; f <- best$f; Lift <- best$lift
    predicted <- rep(NA, length(fitted))
    for(i in 1:length(fitted)){
        ifelse(fitted[i] >= cutoff, predicted[i] <- 1, predicted[i] <- 0)
    }
    return(list(auc=auc, f_lift=best, table=table(predicted, ref)))
}