### ------- Ubiqum Module 4: Big Data ------- ###
### ---------- Sentiment Analysis ----------- ###
### ------------ Alican Tanaçan ------------- ###
### ----- Version 4: Model Development ------ ###

## Description: Creating new variables and useing new subsetting 
## methods to achieve better machine learning models. We go back
## and start over with the original data.

## Result: It is realized that googleandroid, ios and
## total reviews are not important for our model. It seems 
## modelization with raw data with better feature 
## selection and noise cleaning (zero variance rows) 
## might improve our models.

### ---- Libraries ----
if(require("pacman") == "FALSE"){
  install.packages("pacman")
}
p_load(dplyr, ggplot2, plotly, caret, corrplot, GGally,
       doParallel, tidyverse, e1071, randomForest, caTools,
       plyr, ROSE, kknn)

### ---- Working Directory ----
setwd("D:/RStudio/R Studio Working Directory")

### ---- Import Original Data ----
iphone.small.matrix <- read.csv("iphone_smallmatrix_labeled_8d.csv")

galaxy.small.matrix <- read.csv("galaxy_smallmatrix_labeled_9d.csv")

### ---- Feature Engineering & Selection ----
iphone.small.matrix %>%
  filter(iphone >= 1) %>% 
  select(starts_with("iphone")) -> iphonedata

galaxy.small.matrix %>% 
  filter(samsunggalaxy >= 1) %>% 
  select(starts_with("samsung"), starts_with("galaxy")) -> galaxydata

## Create new variables for the sum of the positive, negative and unclear word count
# iphone data
iphonedata$PositiveWords <- iphonedata$iphonecampos + 
  iphonedata$iphonedispos + 
  iphonedata$iphoneperpos

iphonedata$NegativeWords <- iphonedata$iphonecamneg + 
  iphonedata$iphonedisneg + 
  iphonedata$iphoneperneg

iphonedata$UnclearWords <- iphonedata$iphonecamunc + 
  iphonedata$iphonedisunc + 
  iphonedata$iphoneperunc

iphonedata %>% 
  group_by(iphonesentiment) %>% 
  summarise(count(iphonesentiment))

# galaxy data
galaxydata %>% 
  group_by(galaxysentiment) %>% 
  summarise(count(galaxysentiment))

## Recode sentiment to combine factor levels
iphonedata$iphonesentiment <- recode(iphonedata$iphonesentiment, 
                                     "0" = "N", 
                                     "1" = "N", 
                                     "2" = "N", 
                                     "3" = "P", 
                                     "4" = "P", 
                                     "5" = "P") 

galaxydata$galaxysentiment <- recode(galaxydata$galaxysentiment, 
                                     "0" = "N", 
                                     "1" = "N", 
                                     "2" = "N", 
                                     "3" = "P", 
                                     "4" = "P", 
                                     "5" = "P") 

## Change dependent variables' data type
str(iphonedata)
str(galaxydata)
iphonedata$iphonesentiment <- as.factor(iphonedata$iphonesentiment)
galaxydata$galaxysentiment <- as.factor(galaxydata$galaxysentiment)

## Select features
iphoneready <- select(iphonedata, 
                      PositiveWords, 
                      NegativeWords,
                      UnclearWords,
                      iphonesentiment)

galaxyready <- select(galaxydata,
                      -samsunggalaxy)

### ---- Sampling & Data Partition ----
## iphone sampling
set.seed(1234)
iphonedata.both <- ovun.sample(iphonesentiment~., 
                                data = iphoneready, 
                                N = nrow(iphoneready),
                                p = 0.5, 
                                seed = 1, 
                                method = "both")$data

iphonedata.both %>% 
  group_by(iphonesentiment) %>% 
  summarise(count(iphonesentiment))

## iphone data partition
intrain1 <- createDataPartition(y = iphonedata.both$iphonesentiment, 
                                p = 0.7, 
                                list = FALSE)
iphonetrain <- iphonedata.both[intrain1,]
iphonetest <- iphonedata.both[-intrain1,]

## galaxy sampling
set.seed(2345)
galaxydata.both <- ovun.sample(galaxysentiment~., 
                               data = galaxyready,
                               N = nrow(galaxyready), 
                               p = 0.5, 
                               seed = 1, 
                               method = "both")$data

galaxydata.both %>% 
  group_by(galaxysentiment) %>% 
  summarise(count(galaxysentiment))

## galaxy data partition
intrain2 <- createDataPartition(y = galaxydata.both$galaxysentiment, 
                                p = 0.7, 
                                list = FALSE)
galaxytrain <- galaxydata.both[intrain2,]
galaxytest <- galaxydata.both[-intrain2,]

### ---- Core Selection ----
## Find how many cores are on your machine
detectCores() # Result = 8

## Create cluster with desired number of cores.
cl <- makeCluster(4)

## Register cluster
registerDoParallel(cl)

## Confirm how many cores are now assigned to R & RStudio
getDoParWorkers() # Result = 4

### ---- Random Forest Modelization ----
set.seed(4567)
## Set Train Control and Grid
RFtrctrl <- trainControl(method = "repeatedcv",
                         number = 10,
                         preProc = c("center", "scale"),
                         repeats = 2,
                         verboseIter = TRUE)

## iphone
RFmodel1 <- train(iphonesentiment ~ ., 
                  iphonetrain,
                  method = "rf",
                  trControl = RFtrctrl,
                  tuneLenght = 2)

RFmodel1

plot(RFmodel1)

plot(varImp(RFmodel1))

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
                          preProc = c("center", "scale"),
                          verboseIter = TRUE)

## iphone
C50model1 <- train(iphonesentiment~.,
                   data = iphonetrain,
                   method = "C5.0",
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
                   trControl = C50trctrl)

C50model2

plot(C50model2)

varImp(C50model2)

predC50model2 <- predict(C50model2, galaxytest)

postResample(predC50model2, galaxytest$galaxysentiment) -> C50model2metrics

C50model2metrics

cmC50galaxy <- confusionMatrix(predC50model2, galaxytest$galaxysentiment) 
cmC50galaxy

### ---- kkNN Modelization ----
set.seed(6789)
## kkNN Train Control
kkNNcontrol <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 2,
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