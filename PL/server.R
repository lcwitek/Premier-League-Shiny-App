library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(knitr)

releg <- read_csv("..//Relegations.csv")
income <- read_csv("..//Income_PL.csv")
data <- read_csv("..//Soccer Data.csv")
releg2 <- releg %>% gather(Games, Amount, c("Wins", "Draws", "Loses"))
releg2$Games <- factor(releg2$Games, c("Wins", "Loses", "Draws"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    getData <- reactive({
        releg2 <- releg2 %>% filter(Season == input$Season)
    })
    
    getData2 <- reactive({
        releg3 <- releg2 %>% filter(Season == input$Season, `Final Ranking` == 1)
    })
    
    getData3 <- reactive({
        releg4 <- releg2 %>% filter(Season == input$Season, `Relegated - Post` == "Yes")
    })
    getData4 <- reactive({
        releg5 <- releg %>% filter(Season == input$Season) %>% select(`Final Ranking`, Team, Wins, Loses, Draws)
    })

    output$distPlot <- renderPlot({

        #get filtered data
        newData <- getData()
        
        #create plot
        ggplot(newData, aes(x = Team)) +
            geom_col(aes(y = Amount, fill = Games),position = "dodge") +
            theme(axis.text.x = element_text(angle = 45)) +
            scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
            scale_fill_brewer(palette="Dark2") +
            labs(title = paste0("Wins, Loses, and Draws for the ", input$Season, " Season"), x = "Teams", y = "Number of Wins, Loses, and Draws")
    })
    
    # Output from Champion Check box
    output$info <- renderText({
        # Get filtered Data
        newData2 <- getData2()
        if(input$champion)
        paste("The Champion of the", input$Season, "Premier League Season is ", newData2$Team[1], delim = " ")
    })
    
    #Output from Relegation Check box
    output$rele <- renderText({
        # Get filtered Data
        newData3 <- getData3()
        if(input$relegated)
            paste("The teams that will be relegated are", newData3$Team[1], ",", newData3$Team[2], ", and", newData3$Team[3], delim = " ")
    })

    # Output for Table with Final Rankings
    output$table <- renderTable({
        # Get filtered Data
        newData4 <- getData4()
        newData4$`Final Ranking` <- as.integer(newData4$`Final Ranking`)
        newData4$Wins <- as.integer(newData4$Wins)
        newData4$Loses <- as.integer(newData4$Loses)
        newData4$Draws <- as.integer(newData4$Draws)
        newData4 <- unique(newData4)
        newData4
    })
    
})
