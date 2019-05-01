### ------- Ubiqum Module 4: Big Data ------- ###
### ---------- Sentiment Analysis ----------- ###
### ------------ Alican Tanaçan ------------- ###
### ---- Version 3: Feature Engineering ----- ###

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

### ---- Sampling & Data Partition ----
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

## iphone data partition
intrain1 <- createDataPartition(y = iphonedata.under$iphonesentiment, 
                                p = 0.7, 
                                list = FALSE)
iphonetrain <- iphonedata.under[intrain1,]
iphonetest <- iphonedata.under[-intrain1,]

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

## galaxy data partition
intrain2 <- createDataPartition(y = galaxydata.over$galaxysentiment, 
                                p = 0.7, 
                                list = FALSE)
galaxytrain <- galaxydata.over[intrain2,]
galaxytest <- galaxydata.over[-intrain2,]

### ---- Principal Component Analysis ----
## iphone
preprocessParamstrain1 <- preProcess(iphonetrain[,-14], 
                                     method=c("center", "scale", "pca"), 
                                     thresh = 0.95)
print(preprocessParamstrain1)

# use predict to apply pca parameters, create training, exclude dependant
iphonetrain.pca <- predict(preprocessParamstrain1, iphonetrain[,-14])

# add the dependent to training
iphonetrain.pca$iphonesentiment <- iphonetrain$iphonesentiment

# use predict to apply pca parameters, create testing, exclude dependant
iphonetest.pca <- predict(preprocessParamstrain1, iphonetest[,-14])

# add the dependent to training
iphonetest.pca$iphonesentiment <- iphonetest$iphonesentiment

# inspect results
str(iphonetrain.pca)
str(iphonetest.pca)

## galaxy
preprocessParamstrain2 <- preProcess(galaxytrain[,-14], 
                                     method=c("center", "scale", "pca"), 
                                     thresh = 0.95)
print(preprocessParamstrain2)

# use predict to apply pca parameters, create training, exclude dependant
galaxytrain.pca <- predict(preprocessParamstrain2, galaxytrain[,-14])

# add the dependent to training
galaxytrain.pca$galaxysentiment <- galaxytrain$galaxysentiment

# use predict to apply pca parameters, create testing, exclude dependant
galaxytest.pca <- predict(preprocessParamstrain2, galaxytest[,-14])

# add the dependent to training
galaxytest.pca$galaxysentiment <- galaxytest$galaxysentiment

# inspect results
str(galaxytrain.pca)
str(galaxytest.pca)

### ---- Core Selection ----
## Find how many cores are on your machine
detectCores() # Result = 8

## Create cluster with desired number of cores.
cl <- makeCluster(4)

## Register cluster
registerDoParallel(cl)

## Confirm how many cores are now assigned to R & RStudio
getDoParWorkers() # Result = 4

### ---- Recursive Feature Elimination ----
## Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

## Use rfe and omit the response variable (attribute 15 iphonesentiment & galaxysentiment) 
rfeResults1 <- rfe(iphonetrain.pca[,1:7], 
                   iphonetrain.pca$iphonesentiment, 
                   sizes = (1:7), 
                   rfeControl = ctrl)

rfeResults2 <- rfe(galaxytrain.pca[,1:4], 
                   galaxytrain.pca$galaxysentiment, 
                   sizes = (1:4), 
                   rfeControl = ctrl)

## Get results
rfeResults1

predictors(rfeResults1)

rfeResults2

predictors(rfeResults2)

## Plot results
plot(rfeResults1, type=c("g", "o"))

plot(rfeResults2, type=c("g", "o"))

## Create new data set with rfe recommended features
iphoneRFE <- iphone.df[,predictors(rfeResults1)]

galaxyRFE <- galaxy.df[,predictors(rfeResults2)]

## Add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphone.df$iphonesentiment

galaxyRFE$galaxysentiment <- galaxy.df$galaxysentiment

## Review outcome
str(iphoneRFE)

str(galaxyRFE)

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
                   data = iphonetrain.pca,
                   method = "C5.0",
                   metric = "Accuracy",
                   tuneGrid = C50grid,
                   trControl = C50trctrl)

C50model1

plot(C50model1)

varImp(C50model1)

predC50model1 <- predict(C50model1, iphonetest.pca)

postResample(predC50model1, iphonetest.pca$iphonesentiment) -> C50model1metrics

C50model1metrics

cmC50iphone <- confusionMatrix(predC50model1, iphonetest.pca$iphonesentiment) 
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