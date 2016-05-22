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

#parameters
train_file = ".\\btc_train_data.csv"
train_ratio = 0.75
train_random = T


#logic

btcdata <- read.csv(train_file)

NROW(btcdata)

btcdata$isBullish <- as.factor(btcdata$isBullish)

summary(btcdata)

if (train_random) {
    train_idx = sample.split(btcdata$isBullish, SplitRatio = train_ratio)
} else {
    train_idx = c(rep(T, NROW(btcdata)*train_ratio), rep(F, NROW(btcdata)*(1-train_ratio)))
}


train.set = btcdata[ train_idx,]
test.set  = btcdata[!train_idx,]
nrow(train.set)/nrow(btcdata)
nrow(test.set)/nrow(btcdata)

#learn by random forest
set.seed(222)
modelRF1 <- randomForest(isBullish ~ day1 + day2 + day3 + day4 + day5 + day6 + day7 + day8 + day9 + day10 
                         + day11 + day12 + day13 + day14 + day15 
                       #  + day11 + day12 + day13 + day14 + day15 + day20 + day17 + day18 + day19 + day20
                          , data=btcdata, importance=TRUE, ntree=50)
predRF1 = predict(modelRF1, test.set)
test.set$predicted <- predRF1

subset(test.set, select=c(isBullish, predicted))

table(test.set$predicted, test.set$isBullish)
confusionMatrix(test.set$predicted, test.set$isBullish)


