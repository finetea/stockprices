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


tesladata <- read.csv(".\\tesla_data.csv")

NROW(tesladata)

tesladata$isBullish <- as.factor(tesladata$isBullish)

summary(tesladata)


train_idx = sample.split(tesladata$isBullish, SplitRatio = .75)
train.set = tesladata[ train_idx,]
test.set  = tesladata[!train_idx,]
nrow(train.set)/nrow(tesladata)
nrow(test.set)/nrow(tesladata)

#learn by random forest
set.seed(222)
modelRF1 <- randomForest(isBullish ~ day1 + day2 + day3 + day4 + day5 + day6 + day7 + day8 + day9 + day10 
                         + day11 + day12 + day13 + day14 + day15 
                       #  + day11 + day12 + day13 + day14 + day15 + day16 + day17 + day18 + day19 + day20
                          , data=train.set, importance=TRUE, ntree=250)
predRF1 = predict(modelRF1, test.set)
test.set$predicted <- predRF1

subset(test.set, select=c(isBullish, predicted))

table(test.set$predicted, test.set$isBullish)
confusionMatrix(test.set$predicted, test.set$isBullish)





#learn by svm
modelSVM1 <- ksvm(isBullish ~ day1 + day2 + day3 + day4 + day5 + day6 + day7 + day8 + day9 + day10
                   + day11 + day12 + day13 + day14 + day15 
                   #+ day11 + day12 + day13 + day14 + day15 + day16 + day17 + day18 + day19 + day20
                  , data=train.set, kernel="rbf", C=10,prob.model=TRUE)
predSVM1 = predict(modelSVM1, test.set)
test.set$predicted <- predSVM1

subset(test.set, select=c(isBullish, predicted))
confusionMatrix(test.set$predicted, test.set$isBullish)

