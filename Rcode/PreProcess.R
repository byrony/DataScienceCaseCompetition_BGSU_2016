library(rpart)
library(rattle)

#------------------------------read the data---------------------------------------

readData <- function(path.name, file.name, column.types, missing.types){
    read.csv(paste(path.name, file.name, sep = ''), colClasses = column.types, na.strings = missing.types)
}
case.path <- "D:/ASOR Courses/CaseCompData/data/"
train.file.name <- "CaseCompData_Train2.csv"
test.file.name <- "CaseCompData_test.csv"
missing.types <- c('NA', '', ' ') # in Gender, some missing values are Space
train.column.types <- c('State' = 'factor',
                        'Home' = 'factor',
                        'NeighborhoodCode' = 'character',
                        'Gender' = 'factor',
                        'Donation' = 'factor'
)
train <- readData(case.path, train.file.name, train.column.types, missing.types)
test <- readData(case.path, test.file.name, train.column.types, missing.types)
test$Donation <- NA
test$AmDonated <- NA # add two columns of response variables 

combi <- rbind(train, test)

#-------------------------------clean the data--------------------------------------

# zip code missed the first few digits if they are zero
combi$Zip <- abs(combi$Zip)
combi$NumberOfChild[is.na(combi$NumberOfChild)] <- 0

# convert DateofFirstGift into months to today
convertDate <- function(x, endYear = 97, endMonth = 12){
    x <- as.character(x)
    sst <- strsplit(x, '')[[1]]
    out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])
    result <- (endYear - as.numeric(out[1]))*12 + endMonth-as.numeric(out[2])
    return(result)
}
combi$MonthOfFirstGift <- sapply(combi$DateOfFirstGift, FUN = convertDate)

# temporarily delete unnecessary columns
# here delete State since some states has only a few samples
combi <- combi[ , -which(names(combi) %in% 
                             c('DateOfFirstGift', 'State', 'Zip', 'DOB', 'CustID'))]
# convert NA in Gender to Unknow; simply convert A and C to Female
combi$Gender[is.na(combi$Gender)] <- factor('U')
combi$Gender[combi$Gender == 'A'] <- factor('F')
combi$Gender[combi$Gender == 'C'] <- factor('F')

# convert NeighborhoodCode NA's to a new factor level "N"
combi$NeighborhoodCode[is.na(combi$NeighborhoodCode)] <- 'N'
combi$NeighborhoodCode <- factor(combi$NeighborhoodCode)

# fit the NAs in Age using regression tree
Agefit_data <- combi[, -which(names(combi) %in% c('Donation', 'AmDonated'))]
Agefit <- rpart(Age ~., data = Agefit_data[!is.na(Agefit_data$Age) , ], method = 'anova')
fancyRpartPlot(Agefit)
combi$Age[is.na(combi$Age)] <- predict(Agefit, Agefit_data[is.na(Agefit_data$Age) , ])

# fit the NAs in Income using regression tree
Incomefit_data <- Agefit_data
Incomefit <- rpart(Income ~., data = Incomefit_data[!is.na(Incomefit_data$Income), ], method = 'anova')
fancyRpartPlot(Incomefit)
combi$Income[is.na(combi$Income)] <- predict(Incomefit, Incomefit_data[is.na(Incomefit_data$Income) , ])

train <- combi[1:nrow(train), ]
test <- combi[(nrow(train)+1):nrow(combi), ]

# generate the file. You can change the variables and regerate the file.
write.csv(train, file = "train.csv", row.names=FALSE)
write.csv(test, file = "test.csv", row.names=FALSE)

# data preprocessing, center and scale
dataProcess <- function(x){
    for(i in 1:ncol(x)){
        if(!is.factor(x[,i])){
            x[,i] <- (x[,i] - mean(x[,i])) / sd(x[,i])
        }
    }
    return(x)
}
train <- dataProcess(train)
test <- dataProcess(train)
