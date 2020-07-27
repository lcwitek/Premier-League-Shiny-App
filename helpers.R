library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(randomForest)
library(knitr)
library(caret)
library(tree)
library(janitor)
library(glmnet)
library(splines)
library(pls)
library(boot)
library(plotly)
library(ggplot2)
library(cluster)
library(factoextra)
library(grid)
library(class)
library(cowplot)
library(e1071)


# Data Exploration
releg <- read_csv("Relegations.csv")
releg2 <- releg %>% gather(Games, Amount, c("Wins", "Draws", "Losses"))
releg2$Games <- factor(releg2$Games, c("Wins", "Losses", "Draws"))

# Modeling - kNN 
releg3 <- releg %>% select(Wins, Draws, Losses, `Goal Difference`, `Relegated - Post`)
releg3 <- clean_names(releg3)

# Train/Test data - 2013-2016 Seasons Train, 2016-2019 Season Test
trainKnn <- releg3[1:60,]
testKnn <- releg3[61:120,]

trainMat <- trainKnn %>% select(-relegated_post) %>% as.matrix()
testMat <- testKnn %>% select(-relegated_post) %>% as.matrix()
trainY <- as.matrix(trainKnn$relegated_post)

# Removing clearly correlated data and/or unwanted predictors
data <- read_csv("Soccer Data.csv")
data2 <- data %>% select(-HTG, -ATG, -Date, -Day, -Month, -Year, -AY, -HY)

# K Means Clustering
releg4 <- releg %>% rename(Club = Team)

# Scroll Data
data3 <- data %>% select(-Day, -Month, -Year, -GD)

# Transfers
income <- read_csv("Income_PL.csv")
releg5 <- releg %>% rename(Club = Team) %>% mutate(RD = `Final Ranking` - EndJan) %>% select(Season, Club, RD, `Final Ranking`, EndJan)
join <- full_join(income, releg5, by = c("Season", "Club"))