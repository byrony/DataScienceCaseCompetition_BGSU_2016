# some important features
# variables with coefficients greater than 0.05

library(corrplot)
features <- c('Income', 'PercElder', 'AvgHomeVal', 'LifeGiftPromNum', 'LastDol', 'MonthOfFirstGift'
              ,'AmDonated')
d_explore <- data.frame(train_2[, features])
M <- cor(d_explore)
corrplot(M, method='number')
cor(data.frame(train$MonthOfFirstGift, train$AmDonated))

# 
train_2 <- train
#prop.table(table(train$NeighborhoodCode, train$Donation),1)

t <- c('R2', 'R3', 'U4', 'N')
sapply(t, function(x){strsplit(x, '')})
NeighborhoodNew <- function(x){
    x <- as.character(x)
    NeighborPlace <- sapply(x, function(x){strsplit(x, '')[[1]][1]})
    NeighborStatus <- sapply(x, function(x){strsplit(x, '')[[1]][2]})
    mean <- mean(na.omit(as.numeric(NeighborStatus)))
    NeighborStatus <- sapply(NeighborStatus, function(x){ifelse(is.na(x), mean, x)})
    NeighborStatus <- as.numeric(NeighborStatus)
    return(data.frame(NeighborPlace, NeighborStatus))
}

train_2 <- cbind(train_2, NeighborhoodNew(train_2$NeighborhoodCode))
#write.csv(train_2, file='train_2.csv', row.names = FALSE)

library(ggplot2)
p1 <- ggplot(train_2, aes(Age, colour = Donation, fill=Donation)) + geom_density(alpha=0.1)
p2 <- ggplot(train_2, aes(LastDol, colour = Donation, fill=Donation)) + 
    geom_density(alpha=0.1) + 
    xlim(0, 150)

# State vs. Donation
prop.table(table(train$State, train$Donation), 1)
ggplot(train, aes(State, colour = Donation, fill=Donation)) + 
    geom_histogram()
