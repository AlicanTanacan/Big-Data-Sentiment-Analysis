### ------- Ubiqum Module 4: Big Data ------- ###
### ---------- Sentiment Analysis ----------- ###
### ------------ Alican Tanaçan ------------- ###
### ---- Version 3: Feature Engineering ----- ###

## Description: Using principal component analysis and 
## recursive feature elimination to get better weighted
## variables. Also converted sentiment into binary.

## Result: The modelization with engineered features and
## previous preprocesses did NOT improve the performance.

### ---- Libraries ----
if(require("pacman") == "FALSE"){
  install.packages("pacman")
}
p_load(dplyr, ggplot2, plotly, caret, corrplot, GGally,
       doParallel, tidyverse, e1071, randomForest, caTools,
       plyr, ROSE)

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

## Create a new dataset that will be used for recoding sentiment
iphoneRC <- iphonedata

galaxyRC <- galaxydata

## Recode sentiment to combine factor levels
iphoneRC$iphonesentiment <- recode(iphoneRC$iphonesentiment, 
                                   "VN" = "N", 
                                   "N" = "N", 
                                   "SN" = "N", 
                                   "VP" = "P", 
                                   "P" = "P", 
                                   "SP" = "P") 

galaxyRC$galaxysentiment <- recode(galaxyRC$galaxysentiment, 
                                   "VN" = "N", 
                                   "N" = "N", 
                                   "SN" = "N", 
                                   "VP" = "P", 
                                   "P" = "P", 
                                   "SP" = "P") 

## Change dependent variable data type
iphoneRC$iphonesentiment <- as.factor(iphoneRC$iphonesentiment)

galaxyRC$galaxysentiment <- as.factor(galaxyRC$galaxysentiment)

str(iphoneRC)
str(galaxyRC)

### ---- Sampling  ----
## iphone undersampling
set.seed(1234)
iphonedata.under <- ovun.sample(iphonesentiment~., 
                               data = iphoneRC, 
                               p = 0.5, 
                               seed = 1, 
                               method = "under")$data

iphonedata.under %>% 
  group_by(iphonesentiment) %>% 
  summarise(count(iphonesentiment))

## galaxy oversampling
set.seed(2345)
galaxydata.over <- ovun.sample(galaxysentiment~., 
                               data = galaxyRC, 
                               p = 0.5, 
                               seed = 1, 
                               method = "over")$data

galaxydata.over %>% 
  group_by(galaxysentiment) %>% 
  summarise(count(galaxysentiment))

### ---- Core Selection ----
## Find how many cores are on your machine
detectCores() # Result = 8

## Create cluster with desired number of cores.
cl <- makeCluster(4)

## Register cluster
registerDoParallel(cl)

## Confirm how many cores are now assigned to R & RStudio
getDoParWorkers() # Result = 4

### ---- Principal Component Analysis ----
## iphone
preprocessParamsiphone <- preProcess(iphonedata.under[,-14], 
                                     method=c("center", "scale", "pca"), 
                                     thresh = 0.95)
print(preprocessParamsiphone)

# use predict to apply pca parameters, create training, exclude dependant
iphone.pca <- predict(preprocessParamsiphone, iphonedata.under[,-14])

# add the dependent to training
iphone.pca$iphonesentiment <- iphonedata.under$iphonesentiment

# inspect results
str(iphone.pca)

## galaxy
preprocessParamsgalaxy <- preProcess(galaxydata.over[,-14], 
                                     method=c("center", "scale", "pca"), 
                                     thresh = 0.95)
print(preprocessParamsgalaxy)

# use predict to apply pca parameters, create training, exclude dependant
galaxy.pca <- predict(preprocessParamsgalaxy, galaxydata.over[,-14])

# add the dependent to training
galaxy.pca$galaxysentiment <- galaxydata.over$galaxysentiment

# inspect results
str(galaxy.pca)

### ---- Recursive Feature Elimination ----
## Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

## Use rfe and omit the response variable (attribute 8 iphonesentiment & 5 galaxysentiment) 
rfeResults1 <- rfe(iphone.pca[,1:7], 
                   iphone.pca$iphonesentiment, 
                   sizes = (1:7), 
                   rfeControl = ctrl)

rfeResults2 <- rfe(galaxy.pca[,1:4], 
                   galaxy.pca$galaxysentiment, 
                   sizes = (1:4), 
                   rfeControl = ctrl)

## Get results
rfeResults1
predictors(rfeResults1)

rfeResults2
predictors(rfeResults2)

## Plot results
plot(rfeResults1train, type=c("g", "o"))

plot(rfeResults2train, type=c("g", "o"))

## Create new data set with rfe recommended features
iphoneRFE <- iphone.pca[,predictors(rfeResults1)]

galaxyRFE <- galaxy.pca[,predictors(rfeResults2)]

## Add the dependent variable to iphoneRFE and galaxyRFE
iphoneRFE$iphonesentiment <- iphone.pca$iphonesentiment

galaxyRFE$galaxysentiment <- galaxy.pca$galaxysentiment

## Review outcome
str(iphoneRFE)

str(galaxyRFE)

### ---- Data Partition ----
## iphone data partition
intrain1 <- createDataPartition(y = iphoneRFE$iphonesentiment, 
                                p = 0.7, 
                                list = FALSE)
iphonetrain <- iphoneRFE[intrain1,]
iphonetest <- iphoneRFE[-intrain1,]

## galaxy data partition
intrain2 <- createDataPartition(y = galaxyRFE$galaxysentiment, 
                                p = 0.7, 
                                list = FALSE)
galaxytrain <- galaxyRFE[intrain2,]
galaxytest <- galaxyRFE[-intrain2,]

### ---- Random Forest Modelization ----
set.seed(4567)
## Set Train Control and Grid
RFtrctrl <- trainControl(method = "repeatedcv",
                         number = 10,
                         repeats = 2)

## iphone
RFmodel1 <- train(iphonesentiment ~ ., 
                  iphonetrain,
                  method = "rf",
                  trControl = RFtrctrl,
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
                  tuneLenght = 2)

RFmodel2

plot(RFmodel2)

varImp(RFmodel2)

predRFmodel2 <- predict(RFmodel2, galaxytest)

postResample(predRFmodel2, galaxytest$galaxysentiment) -> RFmodel2metrics

RFmodel2metrics

cmRFgalaxy <- confusionMatrix(predRFmodel2, galaxytest$galaxysentiment) 
cmRFgalaxy

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

### ---- kkNN Modelization ----
set.seed(6789)
## kkNN Train Control
kkNNcontrol <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 1,
                            preProc = c("center", "scale"))

## iphone
kkNNmodel1 <- train(iphonesentiment ~ ., 
                    iphonetrain,
                    method = "kknn",
                    trControl = kkNNcontrol)

kkNNmodel1

plot(kkNNmodel1)

predkkNNmodel1 <- predict(kkNNmodel1, iphonetest)

postResample(predkkNNmodel1, iphonetest$iphonesentiment) -> kkNNmodel1metrics

kkNNmodel1metrics

cmkkNNiphone <- confusionMatrix(predkkNNmodel1, iphonetest$iphonesentiment) 
cmkkNNiphone

## galaxy
kkNNmodel2 <- train(galaxysentiment ~ ., 
                    galaxytrain,
                    method = "kknn",
                    trControl = kkNNcontrol)

kkNNmodel2

plot(kkNNmodel2)

predkkNNmodel2 <- predict(kkNNmodel2, galaxytest)

postResample(predkkNNmodel2, galaxytest$galaxysentiment) -> kkNNmodel2metrics

kkNNmodel2metrics

cmkkNNgalaxy <- confusionMatrix(predkkNNmodel2, galaxytest$galaxysentiment) 
cmkkNNgalaxy

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
                              kkNNmodel1metrics,
                              kkNNmodel2metrics)

## Transposing the data frame
AccuracyMetrics <- data.frame(t(AccuracyMetrics))

## Naming the rows
AccuracyMetrics$Algorithms <- rownames(AccuracyMetrics)

## Reordering the columns
AccuracyMetrics <- AccuracyMetrics[, c(3,1,2)]

## Arranging by Rsquared, MAE and Accuracy to see the best models
AccuracyMetrics %>% 
  arrange(desc(Accuracy))
