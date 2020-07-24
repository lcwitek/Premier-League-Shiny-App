library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(knitr)
library(shinydashboard)
library(plotly)

releg <- read_csv("..//Relegations.csv")
income <- read_csv("..//Income_PL.csv")
data <- read_csv("..//Soccer Data.csv")
releg2 <- releg %>% gather(Games, Amount, c("Wins", "Draws", "Loses"))
releg2$Games <- factor(releg2$Games, c("Wins", "Loses", "Draws"))

# Define UI for application that draws a histogram
dashboardPage(skin = "green",

    # Application title
    dashboardHeader(title = "Premier League Data"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info", icon = icon("info")),
            menuItem("Data Exploration", tabName = "dataexpl", icon = icon("chart-bar")),
            menuItem("PCA", tabName = "pca", icon = icon("chart-area")),
            menuItem("Modeling", tabName = "classtree", icon = icon("chart-line")),
            menuItem("Modeling", tabName = "randomforest", icon = icon("chart-line")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        )
    ),

    # Sidebar with a slider input for number of bins
    dashboardBody(
        tabItems(
            tabItem(tabName = "info", 
                    h4("Information")),
            
            tabItem(tabName = "dataexpl",
                fluidPage(
                    box(selectizeInput("Season", "Season", selected = "13-14", 
                                       choices = levels(as.factor(releg2$Season))),
                # Providing the Champion
                checkboxInput("champion", h4("Champion", style = "color:green;")),
        
                # Providing the Teams that are Relegated
                checkboxInput("relegated", h4("Relegated", style = "color:red;")),

                # Show a plot of the generated distribution
                strong(textOutput("info")),
                br(),
                strong(textOutput("rele")),
                br(),
                plotlyOutput("posChart")),
                br(),
                box(tableOutput("table")))),
            
            tabItem(tabName = "pca", 
                    h4("Information")),
            
            tabItem(tabName = "classtree", 
                    h4("Information")),
            
            tabItem(tabName = "randomforest", 
                    h4("Information")),
            
            tabItem(tabName = "data", 
                    h4("Information"))
                
                )
             )
        )
    
