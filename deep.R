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


Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
x <- matrix(c(Var1, Var2), nrow = 50, ncol = 2)
y <-c(rep(1,50),rep(0,50))
exDnn <- dbn.dnn.train(x, y, hidden = c(50, 50,50,50))


testVar1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
testVar2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
testX <- matrix(c(testVar1, testVar2), nrow = 50, ncol = 2)


# 실험데이터를 이용해 결과물 출력
predicted<-nn.predict(exDnn,testX)
predicted

nn.test(exDnn, x, y, t=0.5)
y
