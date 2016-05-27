Sys.setenv(LANG = "en")
setwd("D:\\Work\\data\\stockprices")

library(foreach)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(plyr)
library(corrgram)
library(corrplot)
library(kernlab)
library(deepnet)
library(lattice)
library(caret)
library(caTools)
library(e1071)
library(forecast)
library(MASS)

#parameters
train_file = ".\\eth_train_data.csv"
train_ratio = 0.75
train_random = T

predict_file = ".\\eth_pred_data.csv"

#logic

ethdata <- read.csv(train_file)

NROW(ethdata)

ethdata$isBullish <- as.factor(ethdata$isBullish)

summary(ethdata)

if (train_random) {
    train_idx = sample.split(ethdata$isBullish, SplitRatio = train_ratio)
} else {
    train_idx = c(rep(T, NROW(ethdata)*train_ratio), rep(F, NROW(ethdata)*(1-train_ratio)))
}


train.set = ethdata[ train_idx,]
test.set  = ethdata[!train_idx,]
nrow(train.set)/nrow(ethdata)
nrow(test.set)/nrow(ethdata)

#learn by random forest
for (seed in rep(1:2, 1)) {
    set.seed(seed)
    modelRF1 <- randomForest(isBullish ~ day1 + day2 + day3 + day4 + day5 + day6 + day7 + day8 + day9 + day10 
                             + day11 + day12 + day13 + day14 + day15 
                           #  + vol1 + vol2 + vol3 + vol4 + vol5 + vol6 + vol7 + vol8 + vol9 + vol10 
                            # + vol11 + vol12 + vol13 + vol14 + vol15 
                           #  + day11 + day12 + day13 + day14 + day15 + day20 + day17 + day18 + day19 + day20
                              , data=train.set, importance=TRUE, ntree=250)
    predRF1 = predict(modelRF1, test.set)
    test.set$predicted <- predRF1
    
    #subset(test.set, select=c(isBullish, predicted))
    
    tab <- table(test.set$predicted, test.set$isBullish)
    print(seed)
    mat <- confusionMatrix(test.set$predicted, test.set$isBullish)
    print(mat)
    write.matrix(mat, file="confusion_matrix.txt")
}

#prediction
eth_pred_data <- read.csv(predict_file);
eth_pred_data$predicted <- predict(modelRF1, eth_pred_data)
eth_pred_data
subset(test.set, select=c(date, isBullish, predicted))




#learn by svm
modelSVM1 <- ksvm(isBullish ~ day1 + day2 + day3 + day4 + day5 + day6 + day7 + day8 + day9 + day10
                  + day11 + day12 + day13 + day14 + day15
                  #+ day11 + day12 + day13 + day14 + day15 + day16 + day17 + day18 + day19 + day20
                  , data=train.set, kernel="rbf", C=10,prob.model=TRUE)
predSVM1 = predict(modelSVM1, test.set)
test.set$predicted <- predSVM1

subset(test.set, select=c(isBullish, predicted))

table(test.set$predicted, test.set$isBullish)
confusionMatrix(test.set$predicted, test.set$isBullish)

