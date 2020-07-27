source("helpers.R")

# Define UI for application that draws a histogram
dashboardPage(skin = "green",

    # Application title
    dashboardHeader(title = "Premier League Data"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info", icon = icon("info")),
            menuItem("Data Exploration", tabName = "dataexpl", icon = icon("chart-bar")),
            menuItem("k Means Cluster", tabName = "kmeans", icon = icon("chart-area")),
            menuItem("k Nearest Neighbors", tabName = "knn", icon = icon("chart-line")),
            menuItem("Random Forest", tabName = "randomforest", icon = icon("chart-line")),
            menuItem("Season Data", tabName = "data", icon = icon("database")),
            menuItem("Transfer Expenditure", tabName = "money", icon = icon("euro sign"))
        )
    ),

    # Body of each Tab Item
    dashboardBody(
        tabItems(
            
            # Information Page
            tabItem(tabName = "info", 
                    h1(strong("Information"), align = "center"),
                    fluidPage(
                        fluidRow(column(width = 6, 
                                   includeMarkdown("..//PL-Logo.md")),
                                 column(width = 6, 
                                        includeMarkdown("..//pllogo2.md"))),
                        fluidRow(
                            box(width = 6,
                                column(width = 12,
                                       includeMarkdown("..//DataSets.md"))),
                            box(width = 6,
                                column(width = 12, includeMarkdown("..//abilities.md"))))
                        )
                    ),
            
            # Data Exploration
            tabItem(tabName = "dataexpl",
                    h1(strong("Data Exploration"), align = "center"),
                fluidPage(
                    # Select Season
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
                    plotlyOutput("posChart"),
                    h5("To save plot as .jpg: Hover over graph, 
                           then click camera picture at top of graph")),
                    br(),
                    #List of W/L/D
                    box(tableOutput("table")))),
            
            # k Means Cluster
            tabItem(tabName = "kmeans", 
                    h1(strong("k Means Clustering"), align = "center"),
                fluidPage(
                    # Select Season
                    box(selectizeInput("year", "Season", selected = "13-14", 
                                       choices = levels(as.factor(releg4$Season))),
                        sliderInput("cluster", "k Clusters", 
                                    min = 2, max = 10, value = 4),
                        plotOutput("means")),
                    box(h4(strong("Cluster Means")),
                        tableOutput("center"),
                        h4(strong("Distance Matrix")),
                        h5("Clubs with large dissimilarities (red) versus
                           those that appear to be fairly similar (teal)"),
                        plotOutput("dist"))
                    )),
            
            # KNN
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
                    # Prediction Outcome
                    box(tableOutput("knnTable"),
                        br(),
                        # Accuracy and Misclass Table
                        tableOutput("knnAcc")),
                    box(h4("Wins, Loses, and Draw should add up to 
                           38 to total the number of games in one season. 
                           This prediction uses k = 9."),
                        # Make Own Prediction
                        h5(textOutput("relegated")),
                        numericInput("wins", "Wins",
                                     value = 15, min = 0, max = 38),
                        numericInput("loses", "Loses",
                                     value = 10, min = 0, max = 38),
                        numericInput("draws", "Draws",
                                     value = 5, min = 0, max = 38),
                        numericInput("gd", "Goal Difference",
                                     value = 15, min = -60, max = 80)))),
            
            # Random Forests
            tabItem(tabName = "randomforest", 
                    h1(strong("Random Forest"), align = "center"),
                    h5("Please note this page takes a minute to load"),
                fluidRow(
                    box(width = 6, selectizeInput("preds", "Number of Predictors", 
                                                  selected = "13", 
                                                  choices = c(3:13)),
                        br(),
                        plotOutput("mda"),
                        uiOutput("gini")),
                    box(
                        plotOutput("finModel"))),
                fluidRow(
                    box(width = 6, 
                        column(width = 4, align="center", 
                               tableOutput("accMatrix"))),
                    box(width = 6, dataTableOutput("overall")))),
            
            # Scroll Data
            tabItem(tabName = "data",
                    h1(strong("Season Data"), align = "center"),
                fluidPage(box(selectizeInput("match", "Season", selected = NULL, 
                                         choices = c("All Seasons", levels(as.factor(data3$Season)))),
                              selectizeInput("team", "Club Name", selected = NULL, 
                                         choices = c("All Teams", levels(as.factor(data3$HomeTeam))))),
                              tableOutput("schedule"))),
            
            # Transfers
            tabItem(tabName = "money",
                    h1(strong("Transfer Expenditure"), align = "center"),
                fluidRow(
                    box(selectizeInput("seas", "Season", selected = "13-14", 
                                       choices = levels(as.factor(join$Season))),
                        # Download Graph
                        downloadButton("downloadGraph", "Download"),
                        plotOutput("transfer")),
                    box(tableOutput("money"))))
                )
             )
         )

