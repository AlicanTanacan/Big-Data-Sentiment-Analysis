### ------- Ubiqum Module 4: Big Data ------- ###
### ---------- Sentiment Analysis ----------- ###
### ------------ Alican Tanaçan ------------- ###
### ------ Version 1: Data Exploration ------ ###

### ---- Libraries ----
if(require("pacman") == "FALSE"){
  install.packages("pacman")
}
p_load(dplyr, ggplot2, plotly, caret, corrplot, GGally,
       doParallel, tidyverse, e1071, randomForest, caTools)

### ---- Working Directory ----
setwd("D:/RStudio/R Studio Working Directory")

### ---- Import Small Matrices ----
iphone.small.matrix <- read.csv("iphone_smallmatrix_labeled_8d.csv")
# 12973 obs. 59 variables

galaxy.small.matrix <- read.csv("galaxy_smallmatrix_labeled_9d.csv")
# 12911 obs. 59 variables

### ---- Data Exploration ----
## Distribution among the sentiment ratings
iphone.small.matrix %>% 
  filter(iphone != 0) %>% 
  group_by(iphonesentiment) %>% 
  summarise(count(iphonesentiment))

galaxy.small.matrix %>% 
  filter(samsunggalaxy != 0) %>% 
  group_by(galaxysentiment) %>% 
  summarise(count(galaxysentiment))

## Detect NA's
any(is.na(iphone.small.matrix)) # Result = 0
any(is.na(galaxy.small.matrix)) # Result = 0

## Inspect the data
summary(iphone.small.matrix)
str(iphone.small.matrix)

summary(galaxy.small.matrix)
str(galaxy.small.matrix)

### ---- Feature Selection ----
## Removing rows with no iphone and galaxy observations
# Column 1-5 represents the number of instances that type of phone mentioned in a webpage
iphone.small.matrix %>%
  filter(iphone != 0) %>% 
  select(starts_with("ios"), starts_with("iphone")) -> iphone.df

galaxy.small.matrix %>%
  filter(samsunggalaxy != 0) %>% 
  select(starts_with("google"), starts_with("samsung"), starts_with("galaxy")) -> galaxy.df

## Dependent variable visualizations
plot_ly(iphone.df, x= ~iphone.df$iphonesentiment, type='histogram')

plot_ly(galaxy.df, x= ~galaxy.df$galaxysentiment, type='histogram')

## Increase max print 
options(max.print = 1000000)

## Check correlations
cor(iphone.df)
ggcorr(iphone.df)

cor(galaxy.df)
ggcorr(galaxy.df)

### ---- Preprocessing ----
## Inspect the data
summary(iphone.df)
str(iphone.df)

summary(galaxy.df)
str(galaxy.df)

iphone.df$iphonesentiment <- as.factor(iphone.df$iphonesentiment)

galaxy.df$galaxysentiment <- as.factor(galaxy.df$galaxysentiment)

### ---- doParallel Package ----
## Find how many cores are on your machine
detectCores() # Result = 8

## Create cluster with desired number of cores.
cl <- makeCluster(4)

## Register cluster
registerDoParallel(cl)

## Confirm how many cores are now assigned to R & RStudio
getDoParWorkers() # Result = 4

### ---- Recursive Feature Elimination ----
## Sample the data before using RFE
set.seed(1234)
iphone.sample <- iphone.df[sample(1:nrow(iphone.df),
                                  1000, 
                                  replace = FALSE), ]

## Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

## Use rfe and omit the response variable (attribute 15 iphonesentiment & galaxysentiment) 
rfeResults1 <- rfe(iphone.sample[,1:14], 
                   iphone.sample$iphonesentiment, 
                   sizes = (1:14), 
                   rfeControl = ctrl)

rfeResults2 <- rfe(galaxy.df[,1:14], 
                   galaxy.df$galaxysentiment, 
                   sizes = (1:14), 
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

## Add the dependent variable to iphoneRFE & galaxyRFE
iphoneRFE$iphonesentiment <- iphone.df$iphonesentiment

galaxyRFE$galaxysentiment <- galaxy.df$galaxysentiment

## Review outcome
str(iphoneRFE)

str(galaxyRFE)

### ---- Rename Levels of Factor ----
## 6 level factor
iphoneRFE %>%
  mutate(
    iphone.sentiment = 
      case_when(iphonesentiment %in% "0" ~ "VN",
                iphonesentiment %in% "1" ~ "N",
                iphonesentiment %in% "2" ~ "SN",
                iphonesentiment %in% "3" ~ "SP",
                iphonesentiment %in% "4" ~ "P",
                iphonesentiment %in% "5" ~ "VP")) -> iphoneRFE2

iphoneRFE2$iphonesentiment <- NULL

names(iphoneRFE2)[names(iphoneRFE2) == "iphone.sentiment"] <- "iphonesentiment"

galaxyRFE %>%
  mutate(
    galaxy.sentiment = 
      case_when(galaxysentiment %in% "0" ~ "VN",
                galaxysentiment %in% "1" ~ "N",
                galaxysentiment %in% "2" ~ "SN",
                galaxysentiment %in% "3" ~ "SP",
                galaxysentiment %in% "4" ~ "P",
                galaxysentiment %in% "5" ~ "VP")) -> galaxyRFE2

galaxyRFE2$galaxysentiment <- NULL

names(galaxyRFE2)[names(galaxyRFE2) == "galaxy.sentiment"] <- "galaxysentiment"

### ---- Save Datasets for Modelization ----
saveRDS(iphoneRFE2, file = "iphone_dataframe.rds")

saveRDS(galaxyRFE2, file = "galaxy_dataframe.rds")

## Stop Cluster
stopCluster(cl)