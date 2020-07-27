source("helpers.R")

# Define UI for application that draws a histogram
ui <-  dashboardPage(skin = "green",
              
              # Application title
              dashboardHeader(title = "Premier League Data"),
              
              # Tabs
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
                            # Logos
                            fluidRow(column(width = 6, 
                                            includeMarkdown("PL-Logo.md")),
                                     column(width = 6, 
                                            includeMarkdown("pllogo2.md"))),
                            fluidRow(
                                     box(width = 12, 
                                         h3(strong("About the Premier League")),
                                         h5("The Premier League, also known as the English Premier League (EPL) is the top level of the English Footballl League System. Each season consists of 38 games with each club playing each other twice, once at their home stadium and once at the opponent's stadium. Teams are ranked by total points, goal difference, and goals scored. The Premier League has a system of promotion and relegation each season. The three lowest placed teams in the Premier League are relegated to the English Football League Championship (also known as just Championship). The Championship is one level below the premier league. The top two teams from the Championship are then promoted to to the Premier League with an additional team being promoted after a play-off between the third, fourth, fifth, and sixth placed clubs in the Championship. In total, the Premier League maintains 20 clubs every season."),
                                     h5("More information:", a("Premier League", href = "https://www.premierleague.com"))
                                     )),
                            # Data set Info
                            fluidRow(
                              box(width = 6,
                                  column(width = 12,
                                         includeMarkdown("DataSets.md"))),
                              # App Abilities
                              box(width = 6,
                                  column(width = 12, includeMarkdown("abilities.md")))))),
                  
                  # Data Exploration
                  tabItem(tabName = "dataexpl",
                          h1(strong("Data Exploration"), align = "center"),
                          h5("Choose a season to view the wins, losses, and draws for each team. 
                             Click on the Champion check box to see the Premier League Champion for that season. 
                             The Relegated check box will show the three lowest placed teams that will be relegated to the Championship."),
                          fluidPage(
                            # Select Season
                            box(selectizeInput("Season", "Season", selected = "13-14", 
                                               choices = levels(as.factor(releg2$Season))),
                                # Champion Checkbox
                                checkboxInput("champion", h4("Champion", style = "color:green;")),
                                # Champion Name
                                strong(textOutput("info")),
                                # Relegated Checkbox
                                checkboxInput("relegated", h4("Relegated", style = "color:red;")),
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
                          h5("k Means Clustering looks to find relationships between the n observations by partitioning them into k clusters. These clusters are produced by computing the distance (or disimilarity) between each pair of observations and clustering them based on their similarity. Each cluster is represented by its center (or centroid)."),
                          h5("Choose a season and change the k Clusters to see how teams could potentially be grouped or clustered together. The cluster means are also given for each cluster. The distance matrix shows clubs with large dissimilarities (red) versus those that appear to be fairly similar (teal)."),
                          fluidPage(
                            # Select Season
                            box(selectizeInput("year", "Season", selected = "13-14", 
                                               choices = levels(as.factor(releg4$Season))),
                                # Select k
                                sliderInput("cluster", "k Clusters", 
                                            min = 2, max = 10, value = 4),
                                # Graph for k Means
                                plotOutput("means")),
                            # Cluster Means
                            box(h4(strong("Cluster Means")),
                                tableOutput("center"),
                                # Distance Matrix
                                h4(strong("Distance Matrix")),
                                h5("Clubs with large dissimilarities (red) versus 
                                   those that appear to be fairly similar (teal)"),
                                plotOutput("dist")))),
                  
                  # KNN
                  tabItem(tabName = "knn",
                          h1(strong("k Nearest Neighbors"), align = "center"),
                          h5("The k value, in k Nearest Neighbors refers to the k closest observations to a new example to classify. 
                             These closest observations are found by using distance measures such as Euclidean Distance.
                             Then the new example is then voted into a class by majority vote of its k Neighbors."),
                          h5("Move the slider to change the k Nearest Neighbors(kNN). 
                             Here the prediction is looking at whether a team will be relegated or not based on their wins, losses, draws, and goal difference. 
                             The predicted vs actual values change along with the slider along with the accuracy and the misclassification rate."), 
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
                            box(h3(strong("Prediction, k = 9")),
                                h5("Input the amount of wins, losses, draws and goal difference and see if that team would be relegated. "),
                                h6("Wins, losses, and draws should add up to 38. Goal difference has a range of (-60, 80)"),
                                # Make Own Prediction
                                numericInput("wins", "Wins",
                                             value = 15, min = 0, max = 38),
                                numericInput("losses", "Losses",
                                             value = 10, min = 0, max = 38),
                                numericInput("draws", "Draws",
                                             value = 5, min = 0, max = 38),
                                numericInput("gd", "Goal Difference",
                                             value = 15, min = -60, max = 80),
                                h5(strong(textOutput("relegated"))),))),
                  
                  # Random Forests
                  tabItem(tabName = "randomforest", 
                          h1(strong("Random Forest"), align = "center"),
                          h5("Decision trees use a value of a predictor to ask a question about the data. This question is then answered true or false and splits the data (known as a node). Random forest is a bunch of decision trees bundled together. Instead a simple true or false, random forest looks at the greatest reduction in Gini Impurity to split a node. The Gini Impurity is the probability that a random chosen sample would be incorrectly labeled. It repeats this splitting process until it reaches a maximum depth, or each node contains only one sample from one class."), 
                          h5("This random forest is trying to predict what team will win the game (i.e, Home, Away, or Draw). Select the number of predictors and see how their importance, the final model, and the accuracy and misclassifiction change. The table provided shows the actual result versus the predicted result and whether the classification was correct."),
                          h5(strong("Please note this page takes a minute to load")),
                          fluidRow(
                            # Select Number of Predictors
                            box(width = 6, selectizeInput("preds", "Number of Predictors", 
                                                          selected = "13", 
                                                          choices = c(3:13)),
                                br(),
                                # Rank of Predictors by Gini
                                plotOutput("mda"),
                                # MathJax
                                uiOutput("gini")),
                            box(
                              # Final Model
                              plotOutput("finModel"))),
                          fluidRow(
                            # Accuracy and Misclass Table
                            box(width = 6, 
                                column(width = 4, align="center", 
                                       tableOutput("accMatrix"))),
                            # Actual vs Predicted
                            box(width = 6, dataTableOutput("overall")))),
                  
                  # Scroll Data
                  tabItem(tabName = "data",
                          h1(strong("Season Data"), align = "center"),
                          h5("This data shows the scores from all seasons and all teams. Click on a season, a team, or both to view the data. "),
                          # Select Season
                          fluidPage(
                            fluidRow(
                              box(width = 4, selectizeInput("match", "Season", selected = NULL, 
                                                       choices = c("All Seasons", levels(as.factor(data3$Season)))),
                                        # Select Club 
                                        selectizeInput("team", "Club Name", selected = NULL, 
                                                       choices = c("All Teams", levels(as.factor(data3$HomeTeam))))),
                              box(width = 8, column(width = 6, h6("Date = Match Date (dd/mm/yy)"), h6("HomeTeam = Home Team"),
                                  h6("AwayTeam = Away Team"), h6("RES = Result (H=Home Win, D=Draw, A=Away Win)"), h6("HS = Home Team Shots"),h6("AS = Away Team Shots"),h6("HST = Home Team Shots on Target"), h6("AST = Away Team Shots on Target"), h6("HSM = Home Team Shots not on Target")),
                                  column(width = 6, h6("ASM = Away Team Shots not on Target" ),h6("HF = Home Team Fouls Committed"), h6("AF = Away Team Fouls Committed"),h6("HC = Home Team Corners"),h6("AC = Away Team Corners"), h6("HY = Home Team Yellow Cards"), h6("AY = Away Team Yellow Cards"), h6("HR = Home Team Red Cards"), h6("AR = Away Team Red Cards")))),
                                    # Schedule Output
                                    box(width = 12, column(width = 12, tableOutput("schedule"), style  = "overflow-x: scroll")))),
                  
                  # Transfers
                  tabItem(tabName = "money",
                          h1(strong("Transfer Expenditure"), align = "center"),
                          h5("Premier League clubs are allowed to purchase and sell players' contracts during a summer and winter transfer window. The summer window occurs before the season begins and the winter transfer window happens in January which is roughly mid-season. "),
                          h5("Select a year to look at the difference in league position between a clubs' midseason ranking and final ranking against the amount of money the club spent during the winter transfer window. "),
                          fluidRow(
                            # Select Season
                            box(selectizeInput("seas", "Season", selected = "13-14", 
                                               choices = levels(as.factor(join$Season))),
                                # Expenditure by Team Graph
                                plotOutput("transfer")),
                            # Money Spent Table
                            box(tableOutput("money")))))))


server <- # Define server logic required to draw a histogram
            shinyServer(function(input, output, session) {
              
        # Data Exploration
              
              # Create W/L/D Plot
              output$posChart <- renderPlotly({
                
                # Get Filtered Data
                releg2 <- releg2 %>% filter(Season == input$Season)
                
                #Create Plot
                finalChart <- ggplot(releg2, aes(x = Team, label = `Final Ranking`)) + 
                              geom_col(aes(y = Amount, fill = Games),position = "dodge") +
                              theme(axis.text.x = element_text(angle = 45)) +
                              scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
                              scale_fill_brewer(palette="Dark2") +
                              labs(title = paste0("Wins, Losses, and Draws for the ", input$Season, " Season"), 
                               x = "Teams", y = "Number of Wins, Losses, and Draws")
                ggplotly(finalChart)
              })
              
              # Output from Champion Check box
              output$info <- renderText({
                
                # Get filtered Data
                releg3 <- releg2 %>% filter(Season == input$Season, `Final Ranking` == 1)
                
                #Champion Text
                if(input$champion)
                paste("The Champion of the", input$Season, "Premier League Season is ",
                      releg3$Team[1], delim = " ")
              })
              
              # Output from Relegation Check box
              output$rele <- renderText({
                
                # Get filtered Data
                releg4 <- releg2 %>% filter(Season == input$Season, `Relegated - Post` == "Yes")
                
                # Relegation Text
                if(input$relegated)
                paste("The teams that will be relegated are", releg4$Team[1], ",", releg4$Team[2], 
                        ", and", releg4$Team[3], delim = " ")
              })
              
              # Output for Table with Final Rankings
              output$table <- renderTable({
                
                # Get filtered Data
                releg5 <- releg %>% filter(Season == input$Season) %>% 
                  select(`Final Ranking`, Team, Wins, Losses, Draws)
                
                # Table Output for Each Season
                releg5$`Final Ranking` <- as.integer(releg5$`Final Ranking`)
                releg5$Wins <- as.integer(releg5$Wins)
                releg5$Losses <- as.integer(releg5$Losses)
                releg5$Draws <- as.integer(releg5$Draws)
                releg5 <- unique(releg5)
                releg5
              })
              
        # k Means Clustering
              
              # Create Distance Graph
              output$dist <- renderPlot({
                
                # Get Filtered Data
                releg4 <- releg4 %>% filter(Season == input$year) %>% 
                          select(Club, `Final Ranking`, Wins, Draws, Losses, `Goals For`, `Goals Against`)
                
                # Compute Distance Matrix
                releg5 <- releg4 %>% remove_rownames() %>% column_to_rownames(var = unique("Club"))
                releg5 <- scale(releg5)
                distance <- get_dist(releg5)
                
                # Visualize Distance Matrix
                dist <- fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
                        guides(fill = "none")
                dist
              })
              
              # Create k Clusters
              output$means <- renderPlot({
                
                # Get Filtered Data
                releg4 <- releg4 %>% filter(Season == input$year) %>% 
                          select(Club, `Final Ranking`, Wins, Draws, Losses, `Goals For`, `Goals Against`)
                releg5 <- releg4 %>% remove_rownames() %>% column_to_rownames(var = unique("Club"))
                releg5 <- scale(releg5)
                
                # Set Clusters
                k2 <- kmeans(releg5, centers = input$cluster, nstart = 25)
                
                # Graph of Clusters
                means <- fviz_cluster(k2, data = releg5) +
                         scale_x_continuous(limits = c(-5, 5)) +
                         scale_y_continuous(limits = c(-3,3)) + 
                         ggtitle(paste("k = ", input$cluster)) +
                         theme(legend.position="none")
                means
              })
              
              # k Centers for each cluster
              output$center <- renderTable({
                
                # Get Filtered Data
                releg4 <- releg4 %>% filter(Season == input$year) %>% 
                         select(Club, `Final Ranking`, Wins, Draws, Losses, `Goals For`, `Goals Against`)
                releg5 <- releg4 %>% remove_rownames() %>% column_to_rownames(var = unique("Club"))
                releg5 <- scale(releg5)
                
                # Set Clusters
                k2 <- kmeans(releg5, centers = input$cluster, nstart = 25)
                
                # Output
                cent <- k2$centers
                cent
              }, rownames = TRUE)
              
        # k Nearest Neighbors
              
              # Make tables for Prediction
              output$knnTable <- renderTable({
                
                ## Get Data
                knnPred <- knn(trainMat, testMat, trainY, k = input$knn)
                
                ## Prediction Table
                tbl <- table(knnPred, testKnn$relegated_post)
                tbl <- data.frame(tbl[,1:2])
                tbl <- tbl %>% rename(Predicted = knnPred, Actual = Var2, Frequency = Freq)
                tbl
              })
              
              # Accuracy and Misclassification Table
              output$knnAcc <- renderTable({
                
                ## Get Data
                knnPred <- knn(trainMat, testMat, trainY, k = input$knn)
                
                # Accuracy and Misclass
                modelMatrix <- matrix(c(paste0(round(mean(knnPred == testKnn$relegated_post)*100, 2), "%"), 
                                        paste0(round(mean(knnPred != testKnn$relegated_post)*100, 2), "%")),
                                      ncol = 2, nrow = 1,
                                      dimnames = list(c("k-Nearest Neighbors"), 
                                                      c("Accuracy", "Misclassification Rate")))
                modelMatrix
              },  rownames = TRUE)
              
              # Predict Relegation
              output$relegated <- renderText({
                
                # Set up Predictions for K = 9
                set.seed(1234)
                
                trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
                
                # Train Model
                knnFit <- train(relegated_post ~ ., data = trainKnn, 
                                method = "knn",
                                trControl = trctrl, 
                                tuneLength = 3)
                
                # Test out predictions
                predKnn <- predict(knnFit, newdata = data.frame(wins = input$wins, losses = input$losses,
                                                                draws = input$draws, goal_difference = input$gd))
                relegated <- paste("Team Relegated:",ifelse(predKnn == "No", "No", "Yes"), sep = " ")
                relegated
              })
              
              # Create kNN Plot
              output$knnPlot <- renderPlotly({
                
                #Predict number
                knnPred <- knn(trainMat, testMat, trainY, k = input$knn)
                
                # Create a dataframe to simplify charting
                plot.df <-  data.frame(testKnn, predicted = knnPred)
                
                # First use Convex hull to determine boundary points of each cluster
                plot.df1 <-  data.frame(x = plot.df$wins, 
                                        y = plot.df$losses, 
                                        predicted = plot.df$predicted)
                
                find_hull <-  function(df) df[chull(df$x, df$y), ]
                boundary <-  plyr::ddply(plot.df1, .variables = "predicted", .fun = find_hull)
                
                # Actual Plot
                knnPlot <- ggplot(plot.df, aes(wins, losses, color = predicted, fill = predicted)) + 
                  geom_polygon(data = boundary, aes(x,y), alpha = 0.5) +
                  geom_point(size = 3) +
                  labs(x = "Wins", y = "Losses")
                
                ggplotly(knnPlot)
              })
              
        # Random Forest
              
              # Gini index using MathJax
              output$gini <- renderUI({
                withMathJax(helpText("The Gini Index is defined by $$G = \\sum_{k=1}^{K} \\hat p_{mk} (1 - \\hat p_{mk})$$"))
              })
              
              # Mean Decrease Gini Plot
              output$mda <- renderPlot({
                
                # Set up Predictors
                if(input$preds == 13){
                  colPreds <- c("Matchday", "HomeTeam", "AwayTeam", "HS", 
                                "AS", "HST", "AST", "HF", "AF", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 12){
                  colPreds <- c("HomeTeam", "AwayTeam", "HS", "AS", "HST", "AST",
                                "HF", "AF", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 11){
                  colPreds <- c("HomeTeam", "AwayTeam", "HS", "AS", "HST", 
                                "AST", "HF", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 10){
                  colPreds <- c("HomeTeam", "AwayTeam", "HS", "AS", 
                                "HST", "AST", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 9){
                  colPreds <- c("AwayTeam", "HS", "AS", "HST", "AST", "HC", 
                                "AC", "HR", "AR")
                }
                else if(input$preds == 8){
                  colPreds <- c("AwayTeam", "HS", "AS", "HST", "AST", "AC", "HR", "AR")
                }
                else if(input$preds == 7){
                  colPreds <- c("HS", "AS", "HST", "AST", "AC", "HR", "AR")
                }
                else if(input$preds == 6){
                  colPreds <- c("HS", "AS", "HST", "AST", "HR", "AR")
                }
                else if(input$preds == 5){
                  colPreds <- c("HS", "AS", "HST", "AST", "HR")
                }
                else if(input$preds == 4){
                  colPreds <- c("HS", "AS", "HST", "AST")
                }
                else if(input$preds == 3){
                  colPreds <- c("HS", "HST", "AST")
                }
                
                #Train/Test data - 2013-2018 Seasons Train, 2018-2019 Season Test
                xTrain <- data2[1:1900, colPreds]
                xTest <- data2[1901:2280, colPreds]
                
                yTrain <- as.factor(data2[1:1900,]$RES)
                yTest <- as.factor(data2[1901:2280,]$RES)
                
                set.seed(1234)
                
                # Checking Mean Decrease Gini
                model <- randomForest(xTrain, yTrain,
                                      mtry = 2,
                                      ntree = 500,
                                      strata = yTrain, 
                                      importance = TRUE)
                
                # Importance of Predictors
                imp <- as_tibble(importance(model, type = 2))
                imp <- cbind(colPreds, imp)
                imp <- imp %>% rename(Predictors = colPreds) %>% arrange(desc(MeanDecreaseGini))
                
                # Graph Predictors by Importance
                mda <- ggplot(imp, aes(x = reorder(Predictors, MeanDecreaseGini), y = MeanDecreaseGini)) +
                  geom_bar(fill = "darkgreen", stat = "identity") +
                  coord_flip() +
                  labs(title = "Random Forests - Rank by Importance", 
                       x = "Predictors", y = "Mean Decrease in Gini")
                mda
              })
              
              # Random Forest Graph
              output$finModel <- renderPlot({
                
                withProgress(message = 'Making plot',{
                  
                  #Train/Test data - 2013-2018 Seasons Train, 2018-2019 Season Test
                  if(input$preds == 13){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, Matchday, HomeTeam, AwayTeam, HS, AS, HST, AST, HF, AF, HC, AC, HR, AR)
                  }
                  else if(input$preds == 12){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, HomeTeam, AwayTeam, HS, AS, HST, AST, HF, AF, HC, AC, HR, AR)
                  }
                  else if(input$preds == 11){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, HomeTeam, AwayTeam, HS, AS, HST, AST, HF, HC, AC, HR, AR)
                  }
                  else if(input$preds == 10){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, HomeTeam, AwayTeam, HS, AS, HST, AST, HC, AC, HR, AR)
                  }
                  else if(input$preds == 9){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, AwayTeam, HS, AS, HST, AST, HC, AC, HR, AR)
                  }
                  else if(input$preds == 8){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, AwayTeam, HS, AS, HST, AST, AC, HR, AR)
                  }
                  else if(input$preds == 7){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, HS, AS, HST, AST, AC, HR, AR)
                  }
                  else if(input$preds == 6){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, HS, AS, HST, AST, HR, AR)
                  }
                  else if(input$preds == 5){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, HS, AS, HST, AST, AR)
                  }
                  else if(input$preds == 4){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, HS, AS, HST, AST)
                  }
                  else if(input$preds == 3){
                    xTrain <- data2 %>% filter(Season != "18-19") %>% 
                      select(RES, HS, HST, AST)
                  }
                  
                  set.seed(1234)
                  
                  trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
                  
                  # Train Model
                  model <- train(RES ~ ., data = xTrain,
                                 method = "rf",
                                 tuneLength = 3,
                                 trControl = trctrl)
                  
                  # Final Model Graph
                  finModel <- plot(model$finalModel, main = "Final Random Forest Model")
                  finModel
                })
              })
              
              # Table listing Predicted vs Actual Predict
              output$overall <- renderDataTable({
                
                # Set up Predictors
                if(input$preds == 13){
                  colPreds <- c("Matchday", "HomeTeam", "AwayTeam", "HS", 
                                "AS", "HST", "AST", "HF", "AF", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 12){
                  colPreds <- c("HomeTeam", "AwayTeam", "HS", "AS", "HST", "AST",
                                "HF", "AF", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 11){
                  colPreds <- c("HomeTeam", "AwayTeam", "HS", "AS", "HST", 
                                "AST", "HF", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 10){
                  colPreds <- c("HomeTeam", "AwayTeam", "HS", "AS", 
                                "HST", "AST", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 9){
                  colPreds <- c("AwayTeam", "HS", "AS", "HST", "AST", "HC", 
                                "AC", "HR", "AR")
                }
                else if(input$preds == 8){
                  colPreds <- c("AwayTeam", "HS", "AS", "HST", "AST", "AC", "HR", "AR")
                }
                else if(input$preds == 7){
                  colPreds <- c("HS", "AS", "HST", "AST", "AC", "HR", "AR")
                }
                else if(input$preds == 6){
                  colPreds <- c("HS", "AS", "HST", "AST", "HR", "AR")
                }
                else if(input$preds == 5){
                  colPreds <- c("HS", "AS", "HST", "AST", "HR")
                }
                else if(input$preds == 4){
                  colPreds <- c("HS", "AS", "HST", "AST")
                }
                else if(input$preds == 3){
                  colPreds <- c("HS", "HST", "AST")
                }
                
                # Train/Test data - 2013-2018 Seasons Train, 2018-2019 Season Test
                xTrain <- data2[1:1900, colPreds]
                xTest <- data2[1901:2280, colPreds]
                
                yTrain <- as.factor(data2[1:1900,]$RES)
                yTest <- as.factor(data2[1901:2280,]$RES)
                
                set.seed(1234)
                
                trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
                
                # Train Model
                suppressWarnings( 
                  model <- train(xTrain, yTrain,
                                 method = "rf",
                                 strata = yTrain,
                                 tuneLength = 3,
                                 trControl = trctrl))
                
                # Prediction
                rfPred <- predict(model, xTest)
                
                # Creating Table showing Comparison between Predicted vs Actual
                teamNames <- data2 %>% filter(Season == "18-19")
                
                overall <- as_tibble(cbind(teamNames$HomeTeam, teamNames$AwayTeam, yTest, rfPred))
                
                overall$yTest <- ifelse(overall$yTest == "1", "A", 
                                        ifelse(overall$yTest == "2", "D", "H"))
                
                overall$rfPred <- ifelse(overall$rfPred == "1", "A", 
                                         ifelse(overall$rfPred == "2", "D", "H"))
                
                overall <- overall %>% mutate(Correct = ifelse(yTest == rfPred, "Yes", "No")) %>% 
                  rename("Home Team" = V1, "Away Team" = V2,
                         "Actual Result" = yTest, "Predicted Result" = rfPred)
                overall
              })
              
              # RF Accuracy and Misclassification
              output$accMatrix <- renderTable({
                
                # Set up Predictors
                if(input$preds == 13){
                  colPreds <- c("Matchday", "HomeTeam", "AwayTeam", "HS", 
                                "AS", "HST", "AST", "HF", "AF", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 12){
                  colPreds <- c("HomeTeam", "AwayTeam", "HS", "AS", "HST", "AST",
                                "HF", "AF", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 11){
                  colPreds <- c("HomeTeam", "AwayTeam", "HS", "AS", "HST", 
                                "AST", "HF", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 10){
                  colPreds <- c("HomeTeam", "AwayTeam", "HS", "AS", 
                                "HST", "AST", "HC", "AC", "HR", "AR")
                }
                else if(input$preds == 9){
                  colPreds <- c("AwayTeam", "HS", "AS", "HST", "AST", "HC", 
                                "AC", "HR", "AR")
                }
                else if(input$preds == 8){
                  colPreds <- c("AwayTeam", "HS", "AS", "HST", "AST", "AC", "HR", "AR")
                }
                else if(input$preds == 7){
                  colPreds <- c("HS", "AS", "HST", "AST", "AC", "HR", "AR")
                }
                else if(input$preds == 6){
                  colPreds <- c("HS", "AS", "HST", "AST", "HR", "AR")
                }
                else if(input$preds == 5){
                  colPreds <- c("HS", "AS", "HST", "AST", "HR")
                }
                else if(input$preds == 4){
                  colPreds <- c("HS", "AS", "HST", "AST")
                }
                else if(input$preds == 3){
                  colPreds <- c("HS", "HST", "AST")
                }
                
                # Train/Test data - 2013-2018 Seasons Train, 2018-2019 Season Test
                xTrain <- data2[1:1900, colPreds]
                xTest <- data2[1901:2280, colPreds]
                
                yTrain <- as.factor(data2[1:1900,]$RES)
                yTest <- as.factor(data2[1901:2280,]$RES)
                
                set.seed(1234)
                
                trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
                
                # Train Model
                suppressWarnings(
                  model <- train(xTrain, yTrain,
                                 method = "rf",
                                 strata = yTrain,
                                 tuneLength = 3,
                                 trControl = trctrl))
                
                # Prediction
                rfPred <- predict(model, xTest)
                
                # Create Table
                tbl <- table(rfPred, yTest)
                
                # Create Table with Accuracies and Misclassifications
                accMatrix <- matrix(c(paste0(round((mean(rfPred == yTest))*100, 2), "%"), 
                                      paste0(round((tbl[3,3]/(tbl[3,3] + tbl[3,1] + tbl[3,2]))*100, 2), "%"),
                                      paste0(round((tbl[1,1]/(tbl[1,1] + tbl[1,2] + tbl[1,3]))*100, 2), "%"),
                                      paste0(round((tbl[2,2]/(tbl[2,2] + tbl[2,1] + tbl[2,3]))*100, 2), "%"),
                                      paste0(round((mean(rfPred != yTest))*100, 2), "%"),
                                      paste0(round(((tbl[3,1] + tbl[3,2])/(tbl[3,3] + tbl[3,1] + tbl[3,2]))*100, 2), "%"),
                                      paste0(round(((tbl[1,2] + tbl[1,3])/(tbl[1,1] + tbl[1,2] + tbl[1,3]))*100, 2), "%"),
                                      paste0(round(((tbl[2,1] + tbl[2,3])/(tbl[2,2] + tbl[2,1] + tbl[2,3]))*100, 2), "%")),
                                    ncol = 2, nrow = 4,
                                    dimnames = list(c("Overall", "Home", "Away", "Draw"),
                                                    c("Accuracy", "Misclassification")))
                accMatrix
              }, rownames = TRUE)
              
              # Scroll Data
              
              # View Teams Schedules by season
              output$schedule <- renderTable({
                
                # Filter season and Team
                if(input$match == "All Seasons" & input$team != "All Teams"){
                  data3 <- data3 %>% filter(HomeTeam == input$team | AwayTeam == input$team) 
                  data3 <- data3 %>% unite("FinalScore", HTG, ATG, sep = " - ")
                  data3[,8:21] <- lapply(data3[,8:21], as.integer)
                  data3$Matchday <- as.integer(data3$Matchday)
                  data3
                }
                else if(input$match != "All Seasons" & input$team == "All Teams"){
                  data3 <- data3 %>% filter(Season == input$match)
                  data3 <- data3 %>% unite("FinalScore", HTG, ATG, sep = " - ")
                  data3[,8:21] <- lapply(data3[,8:21], as.integer)
                  data3$Matchday <- as.integer(data3$Matchday)
                  data3
                }
                else if(input$match != "All Seasons" & input$team != "All Teams"){
                  data3 <- data3 %>% filter(Season == input$match)
                  data3 <- data3 %>% filter(HomeTeam == input$team | AwayTeam == input$team) 
                  data3 <- data3 %>% unite("FinalScore", HTG, ATG, sep = " - ")
                  data3[,8:21] <- lapply(data3[,8:21], as.integer)
                  data3$Matchday <- as.integer(data3$Matchday)
                  data3
                }
                else{
                  data3 <- data3 %>% unite("FinalScore", HTG, ATG, sep = " - ")
                  data3[,8:21] <- lapply(data3[,8:21], as.integer)
                  data3$Matchday <- as.integer(data3$Matchday)
                  data3
                }
              })
              
              # Money Table
              
              # Transfer Expenditure
              output$transfer <- renderPlot({
                
                # Get Filtered Data
                join2 <- join %>% filter(Season == input$seas, Transfer == "Winter") %>% 
                  select(Season, Club, `Final Ranking`, RD, Expenditure, Transfer) %>% 
                  arrange(`Final Ranking`)
                
                # Put Plot into a function for output
                plots <- function(x){
                  par(mar=c(5.9, 4.1, 2.3, 4))
                  
                  barplot(join2$Expenditure ~ join2$Club, xlab = "", ylab = "", 
                          las = 2, ylim = c(0,100), xaxt = "n", col = "mediumseagreen")
                  
                  par(new = TRUE) 
                  plot(join2$`Final Ranking`,join2$RD, type = "o", xaxt = "n", yaxt = "n", 
                       xlab = "", ylab = "", col = "royalblue3", lwd = 2, pch = 15) 
                  
                  abline(h = 0, lty = 2, col = "royalblue3") 
                  
                  text(x = 1:20,
                       y = par("usr")[3] - 0.10,
                       labels = join2$Club,
                       xpd = NA, 
                       srt = 50, 
                       cex = 0.75, 
                       adj = .95) 
                  
                  axis(side = 4, at = pretty(range(join2$RD))) 
                  mtext("League Position +/-", side = 4, line = 2, cex = .90) 
                  mtext("Transfer Spending (in Millions \u20AC)", side = 2, line = 2.5, cex = .90) 
                  mtext("Winter Transfer Expenditure", side = 3, line = 1)
                }
                transfer <- plots(join)
                transfer
              })
              
              #Table for Transfer Money
              output$money <- renderTable({
                
                # Get filtered data
                join3 <- join %>% filter(Season == input$seas, Transfer == "Winter") %>% 
                         select(Season, Club, EndJan, `Final Ranking`, Expenditure, Transfer) %>% 
                         arrange(`Final Ranking`)
                join3 <- join3 %>% select(EndJan, `Final Ranking`, Club, Expenditure) %>% 
                  rename("Exenditure (in Millions \u20AC)" = Expenditure, "Midseason Rank" = EndJan)
                
                # Change variables into integers
                join3$`Final Ranking` <- as.integer(join3$`Final Ranking`)
                join3$`Midseason Rank` <- as.integer(join3$`Midseason Rank`)
                
                # Add Euro symbol to data set
                money <- lapply(join3[,4], function(x){paste(x, "\u20AC", sep = " ")})
                join3 <- join3 %>% select(- `Exenditure (in Millions €)`)
                monies <- cbind(join3, money)
                monies
              })
            })


shinyApp(ui = ui, server = server)