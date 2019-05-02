### ------- Ubiqum Module 4: Big Data ------- ###
### ---------- Sentiment Analysis ----------- ###
### ------------ Alican Tanaçan ------------- ###
### --- Version 5: Final Model Development -- ###

### ---- Libraries ----
if(require("pacman") == "FALSE"){
  install.packages("pacman")
}
p_load(dplyr, ggplot2, plotly, caret, corrplot, GGally,
       doParallel, tidyverse, e1071, randomForest, caTools,
       plyr, ROSE, kknn)

### ---- Working Directory ----
setwd("D:/RStudio/R Studio Working Directory")

### ---- Import Ready Data ----
iphone.small.matrix <- read.csv("iphone_smallmatrix_labeled_8d.csv")

galaxy.small.matrix <- read.csv("galaxy_smallmatrix_labeled_9d.csv")

# protect raw data
iphonedata <- iphone.small.matrix

galaxydata <- galaxy.small.matrix

### ---- Feature Selection & Preprocessing ----
## Removing rows with no iphone and galaxy observations
# Column 1-5 represents the number of instances that type of phone mentioned in a webpage
# and take only iphone reviews because ios reviews are creating noise.
iphonedata %>%
  filter(iphone > 0) %>% 
  select(starts_with("iphone"),
         -iphone) -> iphonedata

# Take the sentiment column out of the data
iphone.sent <- iphonedata[ncol(iphonedata)]
iphone.vars <- iphonedata[-ncol(iphonedata)]

# Sum the total reviews
iphone.vars$rowsums <- rowSums((iphone.vars))
summary(iphone.vars$rowsums)

# Bind the sentiment column
iphone.df <- bind_cols(iphone.vars, iphone.sent)

# Remove rows with zero variance, change this number to alter number of rows
iphone.df <- iphone.df %>% filter(rowsums > 5)

# Remove the unnecessary column
iphone.df$rowsums <- NULL

# Take only samsung reviews because google android reviews are irrelevant.
galaxydata %>%
  filter(samsunggalaxy > 0) %>% 
  select(starts_with("samsung"), 
         starts_with("galaxy"),
         -samsunggalaxy) -> galaxydata

# Take the sentiment column out of the data
galaxy.sent <- galaxydata[ncol(galaxydata)]
galaxy.vars <- galaxydata[-ncol(galaxydata)]

# Sum the total reviews
galaxy.vars$rowsums <- rowSums((galaxy.vars))
summary(galaxy.vars$rowsums)

# Bind the sentiment column
galaxy.df <- bind_cols(galaxy.vars, galaxy.sent)

# Remove rows with zero variance, change this number to alter number of rows
galaxy.df %>% filter(rowsums > 2) -> galaxy.df

# Remove the unnecessary column
galaxy.df$rowsums <- NULL

## Check balance of data
# iphone data
iphone.df %>% 
  group_by(iphonesentiment) %>% 
  summarise(count(iphonesentiment))

# galaxy data
galaxy.df %>% 
  group_by(galaxysentiment) %>% 
  summarise(count(galaxysentiment))

## Recode sentiment to combine factor levels
iphone.df$iphonesentiment <- recode(iphone.df$iphonesentiment, 
                                     "0" = "N", 
                                     "1" = "N", 
                                     "2" = "N", 
                                     "3" = "P", 
                                     "4" = "P", 
                                     "5" = "P") 

galaxy.df$galaxysentiment <- recode(galaxy.df$galaxysentiment, 
                                     "0" = "N", 
                                     "1" = "N", 
                                     "2" = "N", 
                                     "3" = "P", 
                                     "4" = "P", 
                                     "5" = "P") 

## Change dependent variables' data type
iphone.df$iphonesentiment <- as.factor(iphone.df$iphonesentiment)
galaxy.df$galaxysentiment <- as.factor(galaxy.df$galaxysentiment)
str(iphone.df)
str(galaxy.df)

### ---- Sampling & Data Partition ----
## iphone sampling
set.seed(1234)
iphonedata.both <- ovun.sample(iphonesentiment~., 
                               data = iphone.df, 
                               N = nrow(iphone.df),
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
                               data = galaxy.df,
                               N = nrow(galaxy.df), 
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
                         repeats = 1,
                         verboseIter = TRUE)

## iphone
RFmodel1 <- train(iphonesentiment ~ ., 
                  iphonetrain,
                  method = "rf",
                  trControl = RFtrctrl)

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
                  trControl = RFtrctrl)

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
                          repeats = 1,
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

#          Algorithms  Accuracy     Kappa
# 1   RFmodel1metrics 0.8750000 0.7494127
# 2 kkNNmodel1metrics 0.8703125 0.7398629
# 3   RFmodel2metrics 0.8648649 0.7297297
# 4  C50model1metrics 0.8390625 0.6755778
# 5 kkNNmodel2metrics 0.8378378 0.6756757
# 6  C50model2metrics 0.7837838 0.5675676