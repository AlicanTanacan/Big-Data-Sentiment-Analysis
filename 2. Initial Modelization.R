### ------- Ubiqum Module 4: Big Data ------- ###
### ---------- Sentiment Analysis ----------- ###
### ------------ Alican Tanaçan ------------- ###
### ---- Version 2: Initial Modelization ---- ###

## Description: Modelization with 6 factor levels dependent variable and
## and including independent variables that is defined with recursive
## feature elimination.

### ---- Libraries ----
if(require("pacman") == "FALSE"){
  install.packages("pacman")
}
p_load(dplyr, ggplot2, plotly, caret, corrplot, GGally,
       doParallel, tidyverse, e1071, randomForest, caTools,
       plyr)

### ---- Working Directory ----
setwd("D:/RStudio/R Studio Working Directory")

### ---- Import Ready Data ----
iphonedata <- readRDS("iphone_dataframe.rds")

galaxydata <- readRDS("galaxy_dataframe.rds")

### ---- Preprocessing ----
## Inspect data types
iphonedata$iphonesentiment <- as.factor(iphonedata$iphonesentiment)
str(iphonedata)

galaxydata$galaxysentiment <- as.factor(galaxydata$galaxysentiment)
str(galaxydata)

### ---- Data Partition ----
## iphone
set.seed(1234)
iphonesample <- iphonedata[sample(1:nrow(iphonedata),
                                  1000, 
                                  replace = FALSE), ]
intrain1 <- createDataPartition(y = iphonesample$iphonesentiment, 
                               p = 0.7, 
                               list = FALSE)
iphonetrain <- iphonesample[intrain1,]
iphonetest <- iphonesample[-intrain1,]

## galaxy
set.seed(2345)
intrain2 <- createDataPartition(y = galaxydata$galaxysentiment, 
                                p = 0.7, 
                                list = FALSE)
galaxytrain <- galaxydata[intrain2,]
galaxytest <- galaxydata[-intrain2,]

### ---- Core Selection ----
## Find how many cores are on your machine
detectCores() # Result = 8

## Create cluster with desired number of cores.
cl <- makeCluster(4)

## Register cluster
registerDoParallel(cl)

## Confirm how many cores are now assigned to R & RStudio
getDoParWorkers() # Result = 4

### ---- C5.0 Modelization ----
set.seed(3456)
## Set Train Control and Grid
C50trctrl <- trainControl(method = "repeatedcv", 
                          number = 10, 
                          repeats = 2,
                          classProbs = TRUE)

C50grid <- expand.grid(.model="tree",.trials = c(1:10),.winnow = FALSE)

## iphone
C50model1 <- train(iphonesentiment~.,
                   data = iphonetrain,
                   method = "C5.0",
                   metric = "Accuracy",
                   tuneGrid = C50grid,
                   trControl = C50trctrl)

C50model1

plot(C50model1)

varImp(C50model1)

predC50model1 <- predict(C50model1, iphonetest)

postResample(predC50model1, iphonetest$iphonesentiment) -> C50model1metrics

C50model1metrics

cmC50iphone <- confusionMatrix(predC50model1, iphonetest$iphonesentiment) 
cmC50iphone

## galaxy
C50model2 <- train(galaxysentiment~.,
                   data = galaxytrain,
                   method = "C5.0",
                   metric = "Accuracy",
                   tuneGrid = C50grid,
                   trControl = C50trctrl)

C50model2

plot(C50model2)

varImp(C50model2)

predC50model2 <- predict(C50model2, galaxytest)

postResample(predC50model2, galaxytest$galaxysentiment) -> C50model2metrics

C50model2metrics

cmC50galaxy <- confusionMatrix(predC50model2, galaxytest$galaxysentiment) 
cmC50galaxy

### ---- Random Forest Modelization ----
set.seed(4567)
## Set Train Control and Grid
RFtrctrl <- trainControl(method = "repeatedcv",
                         number = 10,
                         repeats = 2)

RFgrid <- expand.grid(mtry=c(1:5))

## iphone
RFmodel1 <- train(iphonesentiment ~ ., 
                  iphonetrain,
                  method = "rf",
                  trControl = RFtrctrl,
                  tuneGrid = RFgrid,
                  tuneLenght = 2)

RFmodel1

plot(RFmodel1)

varImp(RFmodel1)

predRFmodel1 <- predict(RFmodel1, iphonetest)

postResample(predRFmodel1, iphonetest$iphonesentiment) -> RFmodel1metrics

RFmodel1metrics

cmRFiphone <- confusionMatrix(predRFmodel1, iphonetest$iphonesentiment) 
cmRFiphone

## galaxy
RFmodel2 <- train(galaxysentiment ~ ., 
                  galaxytrain,
                  method = "rf",
                  trControl = RFtrctrl,
                  tuneGrid = RFgrid,
                  tuneLenght = 2)

RFmodel2

plot(RFmodel2)

varImp(RFmodel2)

predRFmodel2 <- predict(RFmodel2, galaxytest)

postResample(predRFmodel2, galaxytest$galaxysentiment) -> RFmodel2metrics

RFmodel2metrics

cmRFgalaxy <- confusionMatrix(predRFmodel2, galaxytest$galaxysentiment) 
cmRFgalaxy

### ---- SVM Modelization ----
set.seed(5678)
## SVM Train Control
SVMcontrol <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 2,
                           preProc = "center")

## iphone
SVMmodel1 <- train(iphonesentiment ~ ., 
                   iphonetrain,
                   method = "svmLinear",
                   trControl = SVMcontrol,
                   tuneLenght = 2)

SVMmodel1

plot(SVMmodel1)

predSVMmodel1 <- predict(SVMmodel1, iphonetest)

postResample(predSVMmodel1, iphonetest$iphonesentiment) -> SVMmodel1metrics

SVMmodel1metrics

cmSVMiphone <- confusionMatrix(predSVMmodel1, iphonetest$iphonesentiment) 
cmSVMiphone

## galaxy
SVMmodel2 <- train(galaxysentiment ~ ., 
                   galaxytrain,
                   method = "svmLinear",
                   trControl = SVMcontrol,
                   tuneLenght = 2)

SVMmodel2

plot(SVMmodel2)

predSVMmodel2 <- predict(SVMmodel2, galaxytest)

postResample(predSVMmodel2, galaxytest$galaxysentiment) -> SVMmodel2metrics

SVMmodel2metrics

cmSVMgalaxy <- confusionMatrix(predSVMmodel2, galaxytest$galaxysentiment) 
cmSVMgalaxy

### ---- kNN Modelization ----
set.seed(6789)
## kNN Train Control
kNNcontrol <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 2,
                           preProc = c("center", "range"))

## iphone
kNNmodel1 <- train(iphonesentiment ~ ., 
                   iphonetrain,
                   method = "knn",
                   trControl = kNNcontrol)

kNNmodel1

plot(kNNmodel1)

predkNNmodel1 <- predict(kNNmodel1, iphonetest)

postResample(predkNNmodel1, iphonetest$iphonesentiment) -> kNNmodel1metrics

kNNmodel1metrics

cmkNNiphone <- confusionMatrix(predkNNmodel1, iphonetest$iphonesentiment) 
cmkNNiphone

## galaxy
kNNmodel2 <- train(galaxysentiment ~ ., 
                   galaxytrain,
                   method = "knn",
                   trControl = kNNcontrol)

kNNmodel2

plot(kNNmodel2)

varImp(kNNmodel2)

predkNNmodel2 <- predict(kNNmodel2, galaxytest)

postResample(predkNNmodel2, galaxytest$galaxysentiment) -> kNNmodel2metrics

kNNmodel2metrics

cmkNNgalaxy <- confusionMatrix(predkNNmodel2, galaxytest$galaxysentiment) 
cmkNNgalaxy

## Stop Cluster
stopCluster(cl)

### ---- Model Comparison ----
## Creating data frames for performance and accuracy metrics
AccuracyMetrics <- data.frame(C50model1metrics, 
                              C50model2metrics,
                              RFmodel1metrics,
                              RFmodel2metrics,
                              SVMmodel1metrics,
                              SVMmodel2metrics,
                              kNNmodel1metrics,
                              kNNmodel2metrics)

## Transposing the data frame
AccuracyMetrics <- data.frame(t(AccuracyMetrics))

## Naming the rows
AccuracyMetrics$Algorithms <- rownames(AccuracyMetrics)

## Reordering the columns
AccuracyMetrics <- AccuracyMetrics[, c(3,1,2)]

## Arranging by Rsquared, MAE and Accuracy to see the best models
AccuracyMetrics %>% 
  arrange(desc(Accuracy))

# Random forest models perform better than other algorithms.