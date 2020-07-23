#Last Updated on July 23 2020

#Loading Libraries
library(shiny)
library(shinythemes)
library(broom)
library(dplyr)
library(gdata)
library(ggplot2)
library(stringr)
library(readr)
library(curl)
library(tidyr)

#Importing Data
n <- sample(c(0,1), size = 1)

if(n == 0){
  data.all <- readr::read_csv("RaceKartData.csv")
  #data.all <-readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/racekart/getdata.php") 
  
} else {
  data.all <- readr::read_csv("RaceKartData.csv")
  #data.all <-readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/racekart/getdata.php") 
}

#Creating Level Column (To be removed later)
data.all <- mutate(data.all, Level = ifelse(Sandbox == "1", "Sandbox", "Other"))

#Renaming Columns (To be removed later)
data.all <- data.all %>% 
  rename(GameDate = Date, FinishTime = FinishedTime, TopSpeedReached = TopSpeed)

#Creating a Date column
data.all <- data.all %>% mutate(Date = str_sub(GameDate, 1, 10))
data.all$Date <- as.Date(data.all$Date, format = "%m/%d/%Y")

#To Lower
data.all$PlayerID <- tolower(data.all$PlayerID)
data.all$GroupID <- tolower(data.all$GroupID)

#Changing to Factor/Character
data.all$Level <- as.factor(data.all$Level)
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)
data.all$Track <- as.factor(data.all$Track)
data.all$Finished <- as.factor(data.all$Finished)
data.all$Surface <- as.factor(data.all$Surface)
data.all$Tire <- as.factor(data.all$Tire)
data.all$Engine <- as.factor(data.all$Engine)
data.all$Body <- as.factor(data.all$Body)


#To use for Inputs
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))
all_tracks <- sort(unique(data.all$Track))
all_levels <- sort(unique(data.all$Level))



#UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),

  titlePanel("Race Kart Hypothesis Tests"),
  
  fluidRow(
    column(2,
           
           selectInput(inputId = "groupID",
                       label = "Group ID:", 
                       choices =  all_groups,
                       multiple = TRUE,
                       selectize = TRUE,
                       selected = "test"),
           
           selectInput(inputId = "playerID",
                       label = "Remove Player ID:",
                       choices =  all_players,
                       multiple = TRUE,
                       selectize = TRUE),
           
           selectInput("levels", "Level",
                       choices = all_levels,
                       multiple = FALSE,
                       selectize = TRUE,
                       selected = all_levels[1]),
           
           selectInput(inputId = "tracks",
                       label = "Track:",
                       choices = all_tracks,
                       multiple = FALSE,
                       selectize = TRUE,
                       selected = all_tracks[1]),
           
           selectInput(inputId = "xvar",
                       label = "X Variable:",
                       #columns of the dataset
                       choices = c("Body", "Engine", "Tire", "Track", "Surface", "PlayerID"),
                       selected = "Body",
                       multiple = FALSE),
           
           selectInput(inputId = "yvar",
                       label = "Y Variable:",
                       #columns of the dataset
                       choices = c("FinishTime", "TopSpeedReached", "TimeToTopSpeed"),
                       selected = "FinishTime",
                       multiple = FALSE)),
    
    column(2, 
           
           selectInput(inputId = "color",
                       label = "Color by:",
                       choices = c("Body", "Engine", "Tire", "Track", "Surface", "PlayerID"),
                       selected = "Body",
                       multiple = FALSE),
           
           selectInput(inputId = "tests",
                       label = HTML("Statistical Tests <br/> (for X Variable)"),
                       choices = c("None", "two-sample t-test", "ANOVA", "Block Design", 
                                   "Two Sample Randomization Test"),
                       selected = "None",
                       multiple = FALSE),
           
           checkboxInput('bplot',"Add boxplot", FALSE),
           checkboxInput("summary", "Show Summary Statistics", FALSE),
           
           downloadButton('downloadData', label = "Race Kart Data"),
           
           a(h5("Instructor Details"),
             href="https://stat2labs.sites.grinnell.edu/racer.html", 
             align="left", target = "_blank")),
    
    
    column(8,
           
           #Outputs
           tabsetPanel(
             tabPanel("General",  plotOutput(outputId = "Plot"),
                      verbatimTextOutput("twosamp"), 
                      verbatimTextOutput("anova"),
                      verbatimTextOutput("blocked"),
                      tableOutput("summarytable"),
                      verbatimTextOutput("twor"),
                      uiOutput("summarytext")),
             
             tabPanel("Residuals", uiOutput("residualtext"),
                      fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"), 
                                    plotOutput("rplot1"), plotOutput("rplot2"))))
             
           ))
        ))


#Server
server <- function(input, output,session) {
  
  #Reactive Data
  plotDataR <- reactive({
    
    data <- filter(data.all, GroupID %in% input$groupID, 
                   Level %in% input$levels, Track %in% input$tracks, 
                   !(PlayerID %in% input$playerID))
    
    return(data)
  })
    
  
  #Dynamic Remove PlayerID Input 
  observe({
    
    gamedata <- filter(data.all, GroupID %in% input$groupID, 
                       Level %in% input$levels, Track %in% input$tracks)
    
    updateSelectInput(session, 
                      "playerID",
                      choices = c(sort(unique(gamedata$PlayerID))))
  })
  
    
  # Creating Vizualizations
  output$Plot <- renderPlot({
    
    #Requiring inputs
    req(input$groupID)
    
    #Using Reactive Data
    plotData <- plotDataR()
    
    
    #If boxplot option is selected
    if(input$bplot == "TRUE"){
      
      #Creating Visual
      myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
        geom_boxplot() +
        geom_point(position=position_dodge(width = 0.75), size = 3) +
        labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) 
      
    #If boxplot option is not selected  
    } else {
      
      #Creating Visual
      myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color), plot.title = element_text(size = 18)) +
        geom_point(position =position_dodge(width = 0.1), size = 3) +
        labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14))
    }
    

    #ANOVA Output
    output$anova = renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()
      
      #We need data to run ANOVA test
      if(nrow(plotData) > 0){
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      XVariable = drop.levels(as.factor(XVariable))
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(ColorVariable)
    
      #If test option is ANOVA
      if(input$tests == "ANOVA") {
        
        #At least 2 levels for X Variable
        if(nlevels(XVariable) > 1){
      
          #Two way ANOVA
          if(nlevels(ColorVariable) > 1){
            anovatest = anova(aov(YVariable ~ XVariable + ColorVariable + XVariable*ColorVariable))
        
          #One way ANOVA
          } else{
            anovatest = aov(YVariable ~ XVariable)
          }
          
          #Making Tidy table and adding columns/rows
          check2 = tidy(anovatest)
          sum_df = sum(check2$df)
          sum_ss = sum(check2$'sumsq')
          sum_df
          sum_ss
          check2 = add_row(check2,term = "Total", df = sum_df, sumsq = sum_ss)
          check2$sumsq = round(check2$sumsq, digits = 2)
          check2$meansq = round(check2$meansq, digits = 2)
          check2$statistic = round(check2$statistic, digits = 2)
          
          return(check2)
          
        #If there is one or less levels for X Variable
        } else{
         "At least two levels are needed for the X Variable to run the ANOVA test."
        }
        
      }
     }
  })
        
    #Blocked Design
    output$blocked = renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()
      
      #We need data to run the block design ANOVA
      if(nrow(plotData) > 0){
        
        #Setting Up
        YVariable = plotData %>% pull(input$yvar)
        XVariable = plotData %>% pull(input$xvar)
        XVariable = drop.levels(as.factor(XVariable))
        ColorVariable = plotData %>% pull(input$color)
        ColorVariable = drop.levels(ColorVariable)
        PlayerID = plotData$PlayerID
        
        #Block design option is selected
        if(input$tests == "Block Design"){
          
          #Error Message if PlayerID is selected as X Variable or Color
          if(input$xvar == "PlayerID" | input$color == "PlayerID"){
            "When using the Block Design, the X-Variabe/Color Variable cannot be PlayerID"
           
          #We can run the block design test 
          } else {
            
            #At least 2 levels for X Variable
            if(nlevels(XVariable) > 1){
            
              #Two Way Blocked ANOVA
              if(nlevels(ColorVariable) > 1){
                anovatest = aov(YVariable ~ PlayerID + XVariable + ColorVariable + XVariable*ColorVariable)
              
              #One Way Blocked
              } else{
                anovatest = aov(YVariable ~ PlayerID + XVariable)
              }
              
              #Making Tidy table and adding columns/rows
              check2 = tidy(anovatest)
              options(digits = 3)
              sum_df = sum(check2$df)
              sum_ss = sum(check2$'sumsq')
              sum_df
              sum_ss
              check2$sumsq = round(check2$sumsq, digits = 2)
              check2$meansq = round(check2$meansq, digits = 2)
              check2$statistic = round(check2$statistic, digits = 2)
              check2 = add_row(check2,term = "Total", df = sum_df, sumsq = sum_ss)
              
              return(check2)
            
            #If there is one or less levels for X Variable
            } else{
              "At least two levels are needed for the X Variable to run the Block design test."
            }
          }
        }
      }
    })
    
    
    #Two Sample T-Test
    output$twosamp = renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()  
      
      #We need data to run the two sample t-test
      if(nrow(plotData) > 0){
        
        #Setting up
        YVariable = plotData %>% pull(input$yvar)
        XVariable = plotData %>% pull(input$xvar)
        ColorVariable = plotData %>% pull(input$color)
        ColorVariable = drop.levels(as.factor(ColorVariable))
        
        #If two sample t-test option is selected
        if(input$tests == "two-sample t-test"){
        
          #X-variable and Color option must be the same
          if(input$xvar == input$color) {
            dropped = drop.levels(as.factor(XVariable))
            
            #If there are two levels for the X-variable option, run the test
            if(nlevels(dropped) == 2) {
              
              #Each group needs more than 1 observation
              check <- plotData %>% group_by_at(input$xvar) %>%
                summarize(Count = n())
              
              if(!(1 %in% check$Count)){
                t.test(YVariable ~ XVariable)
                
              } else {
                "Not enough observations to run the t-test."
              }
            
            } else{
              "t-tests are only valid when there are exactly two groups."
            }
            
          } else{
            "The X variable and the Color variable should be the same for a t-test."
          }
        }
      }
    })
    
    
    #Two Sample Randomization Test
    output$twor <- renderPrint({
      
      #Reactive Data
      plotData <- plotDataR()
      
      #We need data to run the two sample randomization test
      if(nrow(plotData) > 0){
        
      #Setting up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      
      
      if(input$tests == "Two Sample Randomization Test"){
        
        #X variable and Color option must be the same
        if(input$xvar == input$color) {
          dropped = drop.levels(as.factor(XVariable))
          
          #If there are two levels for the X variable option, run the test
          if(nlevels(dropped) == 2) {
            
            #Each group needs more than 1 observation
            check <- plotData %>% group_by_at(input$xvar) %>%
              summarize(Count = n())
            
            if(!(1 %in% check$Count)){
              
              #Small Data Frame
              data <- data.frame(XVariable, YVariable)
              
              #Identifying the Two Groups and creating necessary vectors
              groups <- sort(unique(data$XVariable))
              group1 <- groups[1]
              group2 <- groups[2]
              
              data1 <- data %>% filter(XVariable == group1)
              data2 <- data %>% filter(XVariable == group2)
              
              group1vec <- data1$YVariable
              group2vec <- data2$YVariable
              
              #Running the Two Sample Randomization Test
              
              #Setting up
              meandiff <- mean(group1vec) - mean(group2vec)
              R <- 100000
              results <- numeric()
              
              for(i in 1:R){
                samp <- sample(data$YVariable, size = length(data$YVariable), replace = FALSE)
                
                samp1 <- samp[1:length(group1vec)]
                samp2 <- samp[(length(group1vec) + 1):length(samp)]
                
                results[i] <- mean(samp1) - mean(samp2)
              }
              
              pvalue <- (1 + sum(results >= abs(meandiff)) + sum(results <= -abs(meandiff))) / (R+1)
              
              return(paste("P Value:", round(pvalue,5)))
              
            } else{
              "Not enough observations to run the two sample randomization test."
            }
            
          } else {
            "Two sample randomization tests are only valid when there are exactly two groups."
          }
          
        } else{
          "The X variable and the Color variable should be the same for a t-test."
        }
        
      }
     }
   })
    
    #Summary Table Output
    output$summarytable <- renderTable({
      
      #Using reactive data
      plotData <- plotDataR()
      
      #If summary checkbox is selected
      if(input$summary == "TRUE"){
        
        #If there is data
        if(nrow(plotData) != 0){
          
          #Creating summary table
          stable <- plotData %>% select(input$xvar, input$yvar) %>% 
            rename(`X Variable` = input$xvar, Yvar = input$yvar) %>%
            group_by(`X Variable`) %>%
            summarize(N = n(), Mean = mean(Yvar), SD = sd(Yvar))
          
          #Removing dynamic help text
          output$summarytext <- renderUI({""})
          
          #If there is no data
        } else{
          
          #Empty data frame to return  
          stable <- data.frame()
          
          #Help Text
          output$summarytext <- renderUI(HTML(paste(
            em("There is no data"))))
        }
        
        return(stable)
      }
    })
    
    #Making sure help text goes away if checkbox is unchecked
    observeEvent(input$summary, {
      
      if(input$summary == "FALSE"){
        output$summarytext <- renderUI({""})
      }
    })
    
    
    #Residual Histogram (RPLOT 1)
    output$rplot1 <- renderPlot({
      
      #Using reactive data
      plotData <- plotDataR()
      
      #We need data for tests to run
      if(nrow(plotData) > 0){
      
      #Test is Not None
      if(input$tests != "None"){
        
        #Setting up
        YVariable = plotData %>% pull(input$yvar)
        XVariable = plotData %>% pull(input$xvar)
        XVariable = drop.levels(as.factor(XVariable))
        ColorVariable = plotData %>% pull(input$color)
        ColorVariable = drop.levels(as.factor(ColorVariable))
        PlayerID = plotData$PlayerID
        
        
        ##Two sample t-test/Two Sample Randomization test
        if(input$tests %in% c("two-sample t-test", "Two Sample Randomization Test")){
          
          #X-variable and Color option must be the same
          if(input$xvar == input$color) {
            dropped = drop.levels(as.factor(XVariable))
            
            #If there are two levels for the X-variable option, run the test
            if(nlevels(dropped) == 2) {
              
              #Each group needs more than 1 observation
              check <- plotData %>% group_by_at(input$xvar) %>%
                summarize(Count = n())
              
              if(!(1 %in% check$Count)){
                
                #Small Data Frame
                data <- data.frame(XVariable, YVariable)
                
                #Identifying the Two Groups and creating necessary vectors
                groups <- sort(unique(data$XVariable))
                group1 <- groups[1]
                group2 <- groups[2]
                
                data1 <- data %>% filter(XVariable == group1)
                data2 <- data %>% filter(XVariable == group2)
                
                group1vec <- data1$YVariable
                group2vec <- data2$YVariable
                
                #Calculate Residuals
                group1res <- group1vec - mean(group1vec)
                group2res <- group2vec - mean(group2vec)
                
                #Combining residuals into one vector
                residuals <- c(group1res, group2res)
                
                #Remove Message
                output$residualtext <- renderUI({""})
                
                #Creating plot
                plot <- hist(residuals, main = "Histogram of Residuals",
                             xlab = "Residuals",
                             ylab = "Count")
                
                return(plot)
                
                
              } else {
                output$residualtext <- renderUI(HTML(paste(
                  em("A valid statistical test must be in place for the residual plots to be generated."))))
              }
                
              } else {
                output$residualtext <- renderUI(HTML(paste(
                  em("A valid statistical test must be in place for the residual plots to be generated."))))
              }
              
            } else{
              output$residualtext <- renderUI(HTML(paste(
                em("A valid statistical test must be in place for the residual plots to be generated."))))
            }
          
          ##ANOVA
        } else if(input$tests == "ANOVA"){
          
          #At least 2 levels for X Variable
          if(nlevels(XVariable) > 1){
            
            #Two way ANOVA
            if(nlevels(ColorVariable) > 1){
              model <- aov(YVariable ~ XVariable + ColorVariable + XVariable*ColorVariable)
              
              #One way ANOVA
            } else{
              model <- aov(YVariable ~ XVariable)
            }
            
            #Remove Message
            output$residualtext <- renderUI({""})
            
            #Creating plot
            plot <- hist(model$residuals, main = "Histogram of Residuals",
                         xlab = "Residuals",
                         ylab = "Count")
            
            return(plot)
           
          #Less than 2 levels for X Variable 
          } else{
            output$residualtext <- renderUI(HTML(paste(
              em("A valid statistical test must be in place for the residual plots to be generated."))))
          }
       
        ##Block Design 
        } else if (input$tests == "Block Design") {
          
          #Error Message if PlayerID is selected as X-variable or Color
          if(input$xvar == "PlayerID" | input$color == "PlayerID"){
            
            output$residualtext <- renderUI(HTML(paste(
              em("A valid statistical test must be in place for the residual plots to be generated."))))
            
            
          #We can run the block design test 
          } else {
            
            #At least 2 levels for X Variable
            if(nlevels(XVariable) > 1){
              
              #Two Way Blocked ANOVA
              if(nlevels(ColorVariable) > 1){
                model <- aov(YVariable ~ PlayerID + XVariable + ColorVariable + XVariable*ColorVariable)
                
                #One Way Blocked
              } else{
                model <- aov(YVariable ~ PlayerID + XVariable)
              }
          
              #Remove Message
              output$residualtext <- renderUI({""})
              
              #Creating plot
              plot <- hist(model$residuals, main = "Histogram of Residuals",
                           xlab = "Residuals",
                           ylab = "Count")
              
              return(plot)
              
            } else{
              output$residualtext <- renderUI(HTML(paste(
                em("A valid statistical test must be in place for the residual plots to be generated."))))
            }
          }
        }
        
      #Test option is none 
      } else{
        output$residualtext <- renderUI(HTML(paste(
          em("A valid statistical test must be in place for the residual plots to be generated."))))
      }
    }
  })
        
        
        #Normal QQ Plot (RPLOT 2)
        output$rplot2 <- renderPlot({
          
          #Using reactive data
          plotData <- plotDataR()
          
          #We need data for tests to run
          if(nrow(plotData) > 0){
          
          #Test is Not None
          if(input$tests != "None"){
            
            #Setting up
            YVariable = plotData %>% pull(input$yvar)
            XVariable = plotData %>% pull(input$xvar)
            XVariable = drop.levels(as.factor(XVariable))
            ColorVariable = plotData %>% pull(input$color)
            ColorVariable = drop.levels(as.factor(ColorVariable))
            PlayerID = plotData$PlayerID
            
            
            ##Two sample t-test/Two Sample Randomization Test
            if(input$tests %in% c("two-sample t-test","Two Sample Randomization Test")){
              
              #X-variable and Color option must be the same
              if(input$xvar == input$color) {
                dropped = drop.levels(as.factor(XVariable))
                
                #If there are two levels for the X-variable option, run the test
                if(nlevels(dropped) == 2) {
                  
                  #Each group needs more than 1 observation
                  check <- plotData %>% group_by_at(input$xvar) %>%
                    summarize(Count = n())
                  
                  if(!(1 %in% check$Count)){
                    
                    #Small Data Frame
                    data <- data.frame(XVariable, YVariable)
                    
                    #Identifying the Two Groups and creating necessary vectors
                    groups <- sort(unique(data$XVariable))
                    group1 <- groups[1]
                    group2 <- groups[2]
                    
                    data1 <- data %>% filter(XVariable == group1)
                    data2 <- data %>% filter(XVariable == group2)
                    
                    group1vec <- data1$YVariable
                    group2vec <- data2$YVariable
                    
                    #Calculate Residuals
                    group1res <- group1vec - mean(group1vec)
                    group2res <- group2vec - mean(group2vec)
                    
                    #Combining residuals into one vector
                    residuals <- c(group1res, group2res)
                    
                    #Creating plot
                    plot <- qqnorm(residuals) 
                    plot <- qqline(residuals)
                    
                    return(plot)
                    
                  }
                }
              }
              
            ##ANOVA 
            } else if(input$tests == "ANOVA") {
              
              #At least 2 levels for X Variable
              if(nlevels(XVariable) > 1){
                
                #Two way ANOVA
                if(nlevels(ColorVariable) > 1){
                  model <- aov(YVariable ~ XVariable + ColorVariable + XVariable*ColorVariable)
                }
                
                #One way ANOVA
                else{
                  model <-  aov(YVariable ~ XVariable)
                }
                
                #Creating plot
                plot <- qqnorm(model$residuals) 
                plot <- qqline(model$residuals)
                
                return(plot)
                
              }
              
            ##Block Design
            } else if (input$tests == "Block Design") {
              
              #Error Message if PlayerID is selected as X-variable or Color
              if(input$xvar == "PlayerID" | input$color == "PlayerID"){
                
              } else {
                
                #At least 2 levels for X Variable
                if(nlevels(XVariable) > 1){
                  
                  #Two Way Blocked ANOVA
                  if(nlevels(ColorVariable) > 1){
                    model <- aov(YVariable ~ PlayerID + XVariable + ColorVariable + XVariable*ColorVariable)
                    
                    #One Way Blocked
                  } else{
                    model <- aov(YVariable ~ PlayerID + XVariable)
                  }
                  
                  #Creating plot
                  plot <- qqnorm(model$residuals) 
                  plot <- qqline(model$residuals)
                  
                  return(plot)
                }
              }
            }
          }
         }
       })
            
        
    return(myplot)
  
  })
  

  #Download Data
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(plotDataR(), con)
    })
  
  
#Closes Server 
}

#Creating Shiny App
shinyApp(ui = ui, server = server)