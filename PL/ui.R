#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

releg <- read_csv("..//Relegations.csv")
income <- read_csv("..//Income_PL.csv")
data <- read_csv("..//Soccer Data.csv")
releg2 <- releg %>% gather(Games, Amount, c("Wins", "Draws", "Loses"))
releg2$Games <- factor(releg2$Games, c("Wins", "Loses", "Draws"))

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Premier League Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectizeInput("Season", "Season", selected = "13-14", choices = levels(as.factor(releg2$Season))),
            # Providing the Champion
            checkboxInput("champion", h4("Champion", style = "color:green;")),
        
        # Providing the Teams that are Relegated
        checkboxInput("relegated", h4("Relegated", style = "color:red;"))
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
            strong(textOutput("info")),
            br(),
            strong(textOutput("rele")),
            br(),
            plotOutput("distPlot"),
            br(),
            tableOutput("table")
        )
    )
))
