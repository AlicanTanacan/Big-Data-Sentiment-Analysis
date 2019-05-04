### ------- Ubiqum Module 4: Big Data ------- ###
### ---------- Sentiment Analysis ----------- ###
### ------------ Alican Tanaçan ------------- ###
### - Version 6: Prediction on Large Matrix - ###

### ---- Libraries & Source ----
if(require("pacman") == "FALSE"){
  install.packages("pacman")
}
p_load(dplyr, ggplot2, plotly, caret, corrplot, GGally,
       doParallel, tidyverse, e1071, randomForest, caTools,
       plyr, ROSE, kknn)

## Take models from previous versions with source
source(file = "D:/RStudio/R Studio Working Directory/M4-BigData/Sentiment Analysis v5.R")

### ---- Import Large Matrix ----
largematrix <- read.csv("LargeMatrix.csv")

iphone.small.matrix <- read.csv("iphone_smallmatrix_labeled_8d.csv")

galaxy.small.matrix <- read.csv("galaxy_smallmatrix_labeled_9d.csv")

### ---- Preprocessing ----
## Large Matrix
largematrix %>%
  filter(iphone > 0) %>% 
  select(starts_with("iphone"),
         -iphone) -> iphone.large.matrix

largematrix %>%
  filter(samsunggalaxy > 0) %>% 
  select(starts_with("samsung"), 
         starts_with("galaxy"),
         -samsunggalaxy) -> galaxy.large.matrix

## Recode sentiment to combine factor levels
iphone.small.matrix$iphonesentiment <- recode(iphone.small.matrix$iphonesentiment, 
                                              "0" = "N", 
                                              "1" = "N", 
                                              "2" = "N", 
                                              "3" = "P", 
                                              "4" = "P", 
                                              "5" = "P") 

galaxy.small.matrix$galaxysentiment <- recode(galaxy.small.matrix$galaxysentiment,
                                              "0" = "N", 
                                              "1" = "N", 
                                              "2" = "N", 
                                              "3" = "P", 
                                              "4" = "P", 
                                              "5" = "P") 

iphone.small.matrix %>% 
  filter(iphone > 0) %>% 
  select(starts_with("iphone"),
         -iphone) -> iphone.small.matrix

galaxy.small.matrix %>% 
  filter(samsunggalaxy > 0) %>% 
  select(starts_with("samsung"), 
         starts_with("galaxy"),
         -samsunggalaxy) -> galaxy.small.matrix

### ---- Predictions ----
## Best performing models for both data is random forest
# iphone predictions
iphone.predictions <- predict(RFmodel1, newdata = iphone.large.matrix)
summary(iphone.predictions)

# probability comparison
prop.table(table(iphone.predictions))
prop.table(table(iphone.small.matrix$iphonesentiment))

# galaxy predictions
galaxy.predictions <- predict(RFmodel2, newdata = galaxy.large.matrix)
summary(galaxy.predictions)

# probability comparison
prop.table(table(galaxy.predictions))
prop.table(table(galaxy.small.matrix$galaxysentiment))
