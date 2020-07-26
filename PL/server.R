source("helpers.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

# Data Exploration
    
    #Create W/L/D Plot
    output$posChart <- renderPlotly({

        #Get Filtered Data
        releg2 <- releg2 %>% filter(Season == input$Season)
        
        #Create Plot
        finalChart <- ggplot(releg2, aes(x = Team, label = `Final Ranking`)) +
            geom_col(aes(y = Amount, fill = Games),position = "dodge") +
            theme(axis.text.x = element_text(angle = 45)) +
            scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
            scale_fill_brewer(palette="Dark2") +
            labs(title = paste0("Wins, Loses, and Draws for the ", input$Season, " Season"), 
                 x = "Teams", y = "Number of Wins, Loses, and Draws")
        
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
                select(`Final Ranking`, Team, Wins, Loses, Draws)
            
            # Table Output for Each Season
            releg5$`Final Ranking` <- as.integer(releg5$`Final Ranking`)
            releg5$Wins <- as.integer(releg5$Wins)
            releg5$Loses <- as.integer(releg5$Loses)
            releg5$Draws <- as.integer(releg5$Draws)
            releg5 <- unique(releg5)
            releg5
        })
        
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
        
        #Accuracy and Misclassification Table
        output$knnAcc <- renderTable({
            
            ## Get Data
            knnPred <- knn(trainMat, testMat, trainY, k = input$knn)
            
            ## Accuracy
            mean(knnPred == testKnn$relegated_post)
            
            ## Misclassification
            mean(knnPred != testKnn$relegated_post)
            
            modelMatrix <- matrix(c(paste0(round(mean(knnPred == testKnn$relegated_post)*100, 2), "%"), 
                                    paste0(round(mean(knnPred != testKnn$relegated_post)*100, 2), "%")),
                                  ncol = 2, nrow = 1,
                                  dimnames = list(c("k-Nearest Neighbors"), 
                                                  c("Accuracy", "Misclassification Rate")))
            modelMatrix
        })
        
        #Predict Relegation
        output$relegated <- renderText({
           
             ## Set up Predictions for K = 9
            set.seed(1234)
        
            trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
        
            knnFit <- train(relegated_post ~ ., data = trainKnn, 
                            method = "knn",
                            trControl = trctrl, 
                            tuneLength = 3)
        
            ## Test out predictions
            predKnn <- predict(knnFit, newdata = data.frame(wins = input$wins, loses = input$loses,
                                                            draws = input$draws, goal_difference = input$gd))
            relegated <- paste("Team Relegated:",ifelse(predKnn == "No", "No", "Yes"), sep = " ")
            relegated
        })
            
        #Create kNN Plot
        output$knnPlot <- renderPlotly({
            
            #Predict number
            knnPred <- knn(trainMat, testMat, trainY, k = input$knn)
            
            # Create a dataframe to simplify charting
            plot.df <-  data.frame(testKnn, predicted = knnPred)
            
            # First use Convex hull to determine boundary points of each cluster
            
            plot.df1 <-  data.frame(x = plot.df$wins, 
                                    y = plot.df$loses, 
                                    predicted = plot.df$predicted)
            
            find_hull <-  function(df) df[chull(df$x, df$y), ]
            boundary <-  plyr::ddply(plot.df1, .variables = "predicted", .fun = find_hull)
            
            # Actual Plot
            knnPlot <- ggplot(plot.df, aes(wins, loses, color = predicted, fill = predicted)) + 
                geom_polygon(data = boundary, aes(x,y), alpha = 0.5) +
                geom_point(size = 3) +
                labs(x = "Wins", y = "Loses")
            
            ggplotly(knnPlot)
        })
        
# Random Forest
        # Mean Decrease Gini Plot
        output$preds <- renderPlot({
            
            #Setting up Predictors
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
        output$rForest <- renderPlot({
            
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
                
                    ## Random Forest
                    trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
                    
                    model <- train(RES ~ ., data = xTrain,
                                   method = "rf",
                                   tuneLength = 3,
                                   trControl = trctrl)
                    
                    finModel <- plot(model$finalModel, main = "Final Random Forest Model")
                    finModel
                })
        })
        
        # Accuracy and Misclassification
        output$rfAcc <- renderTable({
            
            #Setting up Predictors
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
            
            ## Random Forest
            trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
                    
            model <- train(xTrain, yTrain,
                           method = "rf",
                           strata = yTrain,
                           tuneLength = 3,
                           trControl = trctrl)
            
            #Prediction
            rfPred <- predict(model, xTest)
            
            #Creating Table showing Comparison between Predicted vs Actual
            teamNames <- data2 %>% filter(Season == "18-19")
            
            overall <- as_tibble(cbind(teamNames$HomeTeam, teamNames$AwayTeam, yTest, rfPred))
            
            overall$xTest <- ifelse(overall$yTest == "1", "A", 
                                    ifelse(overall$yTest == "2", "D", "H"))
            
            overall$rfPred <- ifelse(overall$rfPred == "1", "A", 
                                     ifelse(overall$rfPred == "2", "D", "H"))
            
            overall <- overall %>% mutate(Correct = ifelse(yTest == rfPred, "Yes", "No")) %>% 
                                   rename("Home Team" = V1, "Away Team" = V2, "Actual Result" = xTest, 
                                          "Predicted Result" = rfPred)
            overall
        })
        
})

