# SMOTE_Sampling

SMOTE_Sampling <- function(data, preVariable,PercOfRare, ApproxTotalNumber = 60000){
    # notice after SMOTE: new rare = rare(perc.over/100 +1); new common= prec.under/100 * (rare*prec.over/100)
    # the parameter perc.over should only be multiple of 100, e.g.200, 300, 1200...
    rareInitial = table(data[, which(names(data) %in% preVariable)])[2]
    rareTotalAmount = ApproxTotalNumber * PercOfRare
    if(rareTotalAmount < rareInitial){
        stop("Initial rare amount is more than target amount")
    }
    else{
        over = (rareTotalAmount - rareInitial)/rareInitial
        perc.over = round(over, 0)
        perc.under = (perc.over+1)/perc.over * (1-PercOfRare)/PercOfRare
        #newTotalAmount = rareInitial*perc.over + rareInitial*perc.over*perc.under + rareInitial
        #return(c(perc.over, perc.under, newTotalAmount))
        f <- paste(preVariable, '~', '.')
        sampling <- SMOTE(as.formula(f), data=data, k=5, perc.over = perc.over*100, perc.under = perc.under*100)
        return(sampling)
    }
}