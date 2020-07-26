source("helpers.R")

# Define UI for application that draws a histogram
dashboardPage(skin = "green",

    # Application title
    dashboardHeader(title = "Premier League Data"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info", icon = icon("info")),
            menuItem("Data Exploration", tabName = "dataexpl", icon = icon("chart-bar")),
            menuItem("PCA", tabName = "pca", icon = icon("chart-area")),
            menuItem("k Nearest Neighbors", tabName = "knn", icon = icon("chart-line")),
            menuItem("Random Forest", tabName = "randomforest", icon = icon("chart-line")),
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
                    # Select Season to display
                    box(selectizeInput("Season", "Season", selected = "13-14", 
                                       choices = levels(as.factor(releg2$Season))),
                # Champion Checkbox
                checkboxInput("champion", h4("Champion", style = "color:green;")),
        
                # Champion Checkbox
                checkboxInput("relegated", h4("Relegated", style = "color:red;")),

                # Champion Name
                strong(textOutput("info")),
                br(),
                # Relegated Teams
                strong(textOutput("rele")),
                br(),
                # W/L/D Plot
                plotlyOutput("posChart")),
                br(),
                # List of W/L/D
                box(tableOutput("table")))),
            
            tabItem(tabName = "pca", 
                    h4("Information")),
            
            tabItem(tabName = "knn",
                    h1(strong("k Nearest Neighbors"), align = "center"),
                fluidPage(
                    #Slider input
                    box(sliderInput("knn", "k Observations", 
                                    min = 1, max = 18, value = 9),
                        br(),
                        # Graph of kNN
                        plotlyOutput("knnPlot"),
                        h5("To save plot as .jpg: Hover over graph, 
                           then click camera picture at top of graph")),
                    # Prediction Table
                    box(tableOutput("knnTable"),
                        br(),
                        # Accuracy and Misclass Table
                        tableOutput("knnAcc")),
                    box(h4("Wins, Loses, and Draw should add up to 38 to total the number of games in one season. 
                           This prediction uses k = 9."),
                        h5(textOutput("relegated")),
                        numericInput("wins", "Wins",
                                     value = 15, min = 0, max = 38),
                        numericInput("loses", "Loses",
                                     value = 10, min = 0, max = 38),
                        numericInput("draws", "Draws",
                                     value = 5, min = 0, max = 38),
                        numericInput("gd", "Goal Difference",
                                     value = 15, min = -60, max = 80)))),
            
            tabItem(tabName = "randomforest", 
                    h1(strong("Random Forest"), align = "center"),
                fluidRow(
                    box(width = 6, selectizeInput("preds", "Number of Predictors", 
                                                  selected = "13", choices = c(3:13)),
                        br(),
                        plotOutput("preds"))),
                fluidRow(
                    box(plotOutput("rForest")),
                    box(width = 6, tableOutput("rfAcc")))
                    ),
            
            tabItem(tabName = "data", 
                    h4("Information"))
                
                )
             )
        )
    
