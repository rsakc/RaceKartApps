#Last Updated on July 23 2020

#Loading Libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)
library(curl)

#Importing Data
n <- sample(c(0,1), size = 1)

if(n == 0){
  #Sample data for now
  data.all <- readr::read_csv("RaceKartData.csv")
  
  #data.all <-readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/racekart/getdata.php") 
  
} else{
  #Sample data for now
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
all_levels <- sort(unique(data.all$Level))
all_body <- sort(unique(data.all$Body))
all_engine <- sort(unique(data.all$Engine))
all_tire <- sort(unique(data.all$Tire))
all_track <- sort(unique(data.all$Track))
all_surface <- sort(unique(data.all$Surface))



#UI
ui <- fluidPage(
  theme = shinytheme("sandstone"),

  titlePanel("Race Kart Visualizations"),
  fluidRow(
    column(3, tabsetPanel(
      
      #Visual Tab
      tabPanel("Visual",
               selectInput(inputId = "groupID",
                           label = "Group ID:", 
                           choices =  c(all_groups),
                           multiple = TRUE,
                           selectize = TRUE,
                           selected = "test"),
               
               selectInput(inputId = "playerID",
                           label = "Player ID:",
                           choices =  c("all", all_players),
                           multiple = TRUE,
                           selectize = TRUE,
                           selected = "all"),
               
               selectInput(inputId = "xvar",
                           label = "X Variable:",
                           choices = c("Body", "Engine", "Tire", "Track", "Surface", "PlayerID"),
                           selected = "Body",
                           multiple = FALSE),
               
               selectInput(inputId = "yvar",
                           label = "Y Variable:",
                           choices = c("FinishTime", "TopSpeedReached", "TimeToTopSpeed"),
                           selected = "FinishTime",
                           multiple = FALSE),
               
               checkboxInput('bplot',"Add boxplot",FALSE),
               checkboxInput("summary", "Show Summary Statistics (For X Variable)", FALSE),
               
               selectInput(inputId = "color",
                           label = "Color by:",
                           choices = c("Body", "Engine", "Tire", "Track", "Surface", "PlayerID"),
                           selected = "Body",
                           multiple = FALSE),
               
               selectInput(inputId = "facets",
                           label = "Facet by:",
                           choices = c("None", "Body", "Engine", "Tire", "Track", "Surface"),
                           selected = "None",
                           multiple = FALSE),
               
               downloadButton('downloadData', label = "Race Kart Data"),
               
               a(h5("Instructor Resources"),
                 href="https://stat2labs.sites.grinnell.edu/racer.html", 
                 align="left", target = "_blank")),
      
      #Filters Tab 
      tabPanel("Filters",
               selectInput(inputId = "body",
                           label = "Filter by Body", 
                           choices = all_body,
                           multiple = TRUE,
                           selectize = TRUE,
                           selected = all_body),
               
               selectInput(inputId = "engine",
                           label = "Filter by Engine", 
                           choices = all_engine,
                           multiple = TRUE,
                           selectize = TRUE,
                           selected = all_engine),
               
               selectInput(inputId = "tire",
                           label = "Filter by Tire", 
                           choices = all_body,
                           multiple = TRUE,
                           selectize = TRUE,
                           selected = all_body),
               
               selectInput(inputId = "levels", 
                           label = "Filter by Level",
                           choices = all_levels,
                           multiple = TRUE,
                           selectize = TRUE,
                           selected = all_levels),
               
               selectInput(inputId = "track",
                           label = "Filter by Track", 
                           choices = all_track,
                           multiple = TRUE,
                           selectize = TRUE,
                           selected = all_track),
               
               selectInput(inputId = "surface",
                           label = "Filter by Surface", 
                           choices = all_surface,
                           multiple = TRUE,
                           selectize = TRUE,
                           selected = all_surface),
               
               numericInput(inputId = "outlier",
                            label = "Remove Most Extreme N Values",
                            value = 0,
                            min = 0,
                            width = "69%"),
               
               uiOutput("help"),
               helpText("No points will be removed if the inputted number is larger than the data")))),
    
    
    column(9, 
           mainPanel(
             #Outputs
             plotOutput(outputId = "Plot", height = "500px", width = "800px"),
             tableOutput("summarytable"),
             uiOutput("summarytext")
             
           ))))
               
#Server
server <- function(input, output,session) {
  
  #Reactive Data
  plotDataR <- reactive({
    
    if("all" %in% input$playerID){
      gamedata <- filter(data.all, GroupID %in% input$groupID)
      gamedata <- filter(gamedata, Level %in% input$levels, Body %in% input$body, Engine %in% input$engine, 
                         Tire %in% input$tire, Track %in% input$track, Surface %in% input$surface)
      
      #Pulling Y Variable  
      y <- gamedata %>% pull(input$yvar)
  
      #Requiring numeric input
      req(input$outlier)
  
      if(input$outlier < nrow(gamedata)){
        #Removing Outliers
        gamedata <- gamedata %>% arrange(y)
        gamedata <- gamedata[1:(nrow(gamedata) - floor(input$outlier)),]
      }
      
      
    } else{
      gamedata <- filter(data.all, GroupID %in% input$groupID, PlayerID %in% input$playerID)
      gamedata <- filter(gamedata, Level %in% input$levels, Body %in% input$body, Engine %in% input$engine, 
                         Tire %in% input$tire, Track %in% input$track, Surface %in% input$surface)
      
      
      #Pulling Y Variable  
      y <- gamedata %>% pull(input$yvar)
      
      #Requiring numeric input
      req(input$outlier)
      
      if(input$outlier < nrow(gamedata)){
        #Removing Outliers
        gamedata <- gamedata %>% arrange(y)
        gamedata <- gamedata[1:(nrow(gamedata) - floor(input$outlier)),]
      }
    }
    
    return(gamedata)
  
  })
      
  #Dynamic PlayerID Input
  observe({
    
    gamedata <- filter(data.all, GroupID %in% input$groupID)
    
    updateSelectInput(session, 
                      "playerID",
                      choices = c("all", sort(unique(gamedata$PlayerID))),
                      selected = "all")
  }) 
  
  #Dynamic Help Text
  output$help <- renderUI({
    plotData <- plotDataR()
    helpText(paste("Number of data points left: ", nrow(plotData)))
  })
  
  
  #Creating Vizualizations
  output$Plot <- renderPlot({
    
    #Require
    req(input$groupID)
    
    #Using Reactive Data
    plotData <- plotDataR()
    
    #Boxplot option is checked
    if(input$bplot == "TRUE"){
  
      #Creating Visual
      myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
        geom_boxplot() +
        geom_point(position=position_dodge(width = 0.75), size = 3) +
        labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) 
  
    #If boxplot option is not selected  
    } else{
      
      #Creating Visual
      myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color), plot.title = element_text(size = 18)) +
        geom_point(position =position_dodge(width = 0.5), size = 3) +
        labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14))
    }
    
    #If facet option is selected
    if(input$facets != "None"){
      myplot <- myplot + facet_wrap(as.formula(paste("~", input$facets))) +
        labs(title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color, "and Faceted by", input$facets)) +
        theme(strip.text = element_text(size = 16))
    }
    

    
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
    
    #Returning Visual
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
  

#Close Server
}     
      
#Creating Shiny App
shinyApp(ui = ui, server = server)