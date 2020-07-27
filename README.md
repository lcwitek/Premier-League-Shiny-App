Lauren Witek Final Project
================

## Premier League Shiny App

This Shiny App looks at Premier League data from the 2013-2014 season up
to the 2018-2019 season. It analyses the data through Data Exploration,
k Means Clusters, k-Nearest Neighbors, and Random Forests.

There are three data sets utilized in this analysis.

1.  Relegations

2.  Soccer Data

3.  Income PL

Please note that some data sets use club and some use team. Club and
team are interchangeable and will be used as such throughout the
content.

#### Available through GitHub:

``` r
shiny::runGitHub("ST558-Final-Project", "lcwitek")
```

#### Also available through shinyapps.io:

[ST558 Final Project](https://lcwitek.shinyapps.io/ST558-Final-Project/)

## Packages Required for app.R

``` r
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
```
