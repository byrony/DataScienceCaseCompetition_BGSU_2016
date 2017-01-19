# read data. Response variable is 'Donation'

readData <- function(path.name, file.name, column.types, missing.types){
    read.csv(paste(path.name, file.name, sep = ''), colClasses = column.types, na.strings = missing.types)
}
case.path <- ""
train.file.name <- "train.csv"
test.file.name <- "test.csv"
missing.types <- c('NA', '', ' ') # in Gender, some missing values are Space
train.column.types <- c('Home' = 'factor',
                        'NeighborhoodCode' = 'factor',
                        'Gender' = 'factor',
                        'Donation' = 'factor'
)
train <- readData(case.path, 'train.csv', train.column.types, missing.types)
train <- train[, -which(names(train) %in% c('AmDonated'))]