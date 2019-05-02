### ------- Ubiqum Module 4: Big Data ------- ###
### ---------- Sentiment Analysis ----------- ###
### ------------ Alican Tanaçan ------------- ###
### - Version 6: Prediction on Large Matrix - ###

### ---- Libraries ----
if(require("pacman") == "FALSE"){
  install.packages("pacman")
}
p_load(dplyr, ggplot2, plotly, caret, corrplot, GGally,
       doParallel, tidyverse, e1071, randomForest, caTools,
       plyr, ROSE, kknn)

source(file = "D:/RStudio/R Studio Working Directory/M4-BigData/Sentiment Analysis v5.R")

### ---- Import Large Matrix ----
largematrix <- read.csv("LargeMatrix.csv")

### ---- Preprocessing ----
largematrix %>%
  filter(iphone > 0) %>% 
  select(starts_with("iphone"),
         -iphone) -> iphone.large.matrix

largematrix %>%
  filter(samsunggalaxy > 0) %>% 
  select(starts_with("samsung"), 
         starts_with("galaxy"),
         -samsunggalaxy) -> galaxy.large.matrix

### ---- Predictions ----
## Best performing models for both data is random forest
# iphone predictions
iphone.predictions <- predict(RFmodel1, newdata = iphone.large.matrix)
summary(iphone.predictions)

# probability comparison
prop.table(table(iphone.predictions))
prop.table(table(iphone.df$iphonesentiment))

# galaxy predictions
galaxy.predictions <- predict(RFmodel2, newdata = galaxy.large.matrix)
summary(galaxy.predictions)

# probability comparison
prop.table(table(galaxy.predictions))
prop.table(table(galaxy.df$galaxysentiment))
