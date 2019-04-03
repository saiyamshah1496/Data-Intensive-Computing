#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
header <- dashboardHeader(title = "Flu Data Analysis")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard - CDC vs FLU", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Dashboard - Flu Terms", tabName = "dashboard2", icon = icon("dashboard"))
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "CDC Map"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("cdcHeatMap", height = "300px")
  )
  ,box(
    title = "Map - Tweets(FLU)"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("fluHeatMap", height = "300px")
  ) 
)

brow1 <- fluidRow(
  valueBoxOutput("value11")
  ,valueBoxOutput("value21")
  ,valueBoxOutput("value31")
  ,valueBoxOutput("value41")
  ,valueBoxOutput("value51")
  ,valueBoxOutput("value61")
  ,valueBoxOutput("value71")
  ,valueBoxOutput("value81")
  ,valueBoxOutput("value91")
  
)
brow2 <- fluidRow( 
  box(
    title = "CDC Map"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("cdcHeatMap1", height = "300px")
  )
  ,box(
    title = "Map - Tweets(FLU)"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("fluHeatMap1", height = "300px")
  ) 
  ,box(
    title = "Map - Tweets(Influenza)"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("influenzaHeatMap1", height = "300px")
  ) 
  ,box(
    title = "Map - Tweets(Fever)"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("feverHeatMap1", height = "300px")
  ) 
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            frow1,
            frow2
    ),
    tabItem(tabName = "dashboard2",
            brow1,
            brow2)
  ) # /tabItems
) # /dashboardBody


uiFinal <- dashboardPage(title = 'Flu analysis', header, sidebar, body, skin='red')

library(ggplot2)
library(reshape2)
library(grid)
library(magrittr) 
library(fiftystater)
library(mapproj)
X7_Flu_Heat_Map <- read.csv("HeatMap.csv")
copySeries <- X7_Flu_Heat_Map$ACTIVITY.LEVEL

newSeries <- seq(1,54,1)
for (i in seq(1,54)) {
  process <-as.character(copySeries[i]) %>% strsplit(" ") %>% extract2(1) %>% extract(2) %>% as.integer()
  
  newSeries[i] <- process
}

X7_Flu_Heat_Map$ACTIVITY.LEVEL <- newSeries %>% factor(levels = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1", "0"))
X7_Flu_Heat_Map$ACTIVITY.LEVEL
X7_Flu_Heat_Map$STATENAME <- X7_Flu_Heat_Map$STATENAME %>% tolower()

library(ggplot2)
library(reshape2)
library(grid)
library(magrittr) 
library(fiftystater)
library(mapproj)
tweetStateChartFinal <- read.csv("tweetStateChartFinal.csv")
copySeries2 <- tweetStateChartFinal$ACTIVITY.LEVEL

newSeries2 <- seq(1,49,1)
for (i in seq(1,49)) {
  process <-as.character(copySeries2[i]) %>% as.integer()
  
  newSeries2[i] <- process
}

tweetStateChartFinal$ACTIVITY.LEVEL <- newSeries2 %>% factor(levels = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1", "0"))

tweetStateChartFinal$STATENAME <- tweetStateChartFinal$STATENAME %>% tolower()

library(ggplot2)
library(reshape2)
library(grid)
library(magrittr) 
library(fiftystater)
library(mapproj)
tweetStateChartInfluenzaFinal <- read.csv("tweetStateChartInfluenzaFinal.csv")
copySeries3 <- tweetStateChartInfluenzaFinal$ACTIVITY.LEVEL

newSeries3 <- seq(1,49,1)
for (i in seq(1,49)) {
  process <-as.character(copySeries3[i]) %>% as.integer()
  
  newSeries3[i] <- process
}

tweetStateChartInfluenzaFinal$ACTIVITY.LEVEL <- newSeries3 %>% factor(levels = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1", "0"))

tweetStateChartInfluenzaFinal$STATENAME <- tweetStateChartInfluenzaFinal$STATENAME %>% tolower()


library(ggplot2)
library(reshape2)
library(grid)
library(magrittr) 
library(fiftystater)
library(mapproj)
tweetStateChartFeverFinal <- read.csv("tweetStateChartFeverFinal.csv")
copySeries4 <- tweetStateChartFeverFinal$ACTIVITY.LEVEL

newSeries4 <- seq(1,49,1)
for (i in seq(1,49)) {
  process <-as.character(copySeries4[i]) %>% as.integer()
  
  newSeries4[i] <- process
}

tweetStateChartFeverFinal$ACTIVITY.LEVEL <- newSeries4 %>% factor(levels = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1", "0"))

tweetStateChartFeverFinal$STATENAME <- tweetStateChartFeverFinal$STATENAME %>% tolower()


freq <- read.csv("Frequency.csv")

freqInfluenza <- read.csv("FrequencyInfluenza.csv")

freqFever <- read.csv("FrequencyFever.csv")

# Define server logic required to draw a histogram
# create the server functions for the dashboard  
serverFinal <- function(input, output,session) { 
  session$onSessionEnded(function() {
    stopApp()
  })
  #some data manipulation to derive the values of KPI boxes
  maxFreq <- max(freq$Freq)
  maxState <- freq[ which(freq$Freq == maxFreq), "Var1"]
  minFreq <- min(freq$Freq)
  minState <- freq[ which(freq$Freq == minFreq), "Var1"]
  maxFreqInfluenza <- max(freqInfluenza$Freq)
  maxStateInfluenza <- freqInfluenza[ which(freqInfluenza$Freq == maxFreqInfluenza), "Var1"]
  minFreqInfluenza <- min(freqInfluenza$Freq)
  minStateInfluenza <- freqInfluenza[ which(freqInfluenza$Freq == minFreqInfluenza), "Var1"]
  maxFreqFever <- max(freqFever$Freq)
  maxStateFever <- freqFever[ which(freqFever$Freq == maxFreqFever), "Var1"]
  minFreqFever <- min(freqFever$Freq)
  minStateFever <- freqFever[ which(freqFever$Freq == minFreqFever), "Var1"]
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(maxFreq, format="d", big.mark=',')
      ,paste('Highest Tweets :',maxState)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(minFreq, format="d", big.mark=',')
      ,paste('Lowest Tweets :',minState)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  
  output$value3 <- renderValueBox({ 
    valueBox(
      formatC(sum(freq$Freq), format="d", big.mark=',')
      ,paste('Total Tweets Flu :')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "orange")  
  })
  
  output$value11 <- renderValueBox({ 
    valueBox(
      formatC(maxFreq, format="d", big.mark=',')
      ,paste('Maximum Tweets - FLU :',maxState)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value21 <- renderValueBox({ 
    valueBox(
      formatC(minFreq, format="d", big.mark=',')
      ,paste('Lowest Tweets - FLU :',minState)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  output$value31 <- renderValueBox({ 
    valueBox(
      formatC(maxFreqInfluenza, format="d", big.mark=',')
      ,paste('Maximum Tweets - Influenza :',maxStateInfluenza)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "orange")  
  })
  output$value41 <- renderValueBox({ 
    valueBox(
      formatC(minFreqInfluenza, format="d", big.mark=',')
      ,paste('Lowest Tweets - Influenza :',minStateInfluenza)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$value51 <- renderValueBox({ 
    valueBox(
      formatC(sum(freq$Freq), format="d", big.mark=',')
      ,paste('Total Tweets - Flu :')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "orange")  
  })
  
  output$value61 <- renderValueBox({ 
    valueBox(
      formatC(sum(freqInfluenza$Freq), format="d", big.mark=',')
      ,paste('Total Tweets - Influenza :')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  
  output$value71 <- renderValueBox({ 
    valueBox(
      formatC(sum(freqFever$Freq), format="d", big.mark=',')
      ,paste('Total Tweets - Fever :')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$value81 <- renderValueBox({ 
    valueBox(
      formatC(minFreqFever, format="d", big.mark=',')
      ,paste('Minimum Tweets - Fever :' ,minStateFever)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  
  output$value91 <- renderValueBox({ 
    valueBox(
      formatC(maxFreqFever, format="d", big.mark=',')
      ,paste('Maximum Tweets - Fever :',maxStateFever)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "orange")  
  })
  #creating the plotOutput content
  output$cdcHeatMap <- renderPlot({
    ggplot(X7_Flu_Heat_Map, aes(fill=ACTIVITY.LEVEL, map_id=STATENAME)) +
      geom_map(map=fifty_states, color = 'black', size = 0.1) +
      expand_limits(x=fifty_states$long, y=fifty_states$lat) +
      coord_map() +
      theme_classic() +
      ggtitle("2017-18 Influenza Season Week 5 ending Feb 09, 2019") +
      scale_fill_manual(drop=FALSE, name = "ILI Activity Level", values = c("#cc0000", "#fa4f00", "#fc8200", "#fcb100", "#f7df00", "#e0f500", "#baf700", "#8cf700", "#5bf700", "#00c200", "white")) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), axis.title.y = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            line = element_blank(),plot.title = element_text(hjust = 0.5, face = 'bold', size = 10),
            #legend.text = element_blank(),
            legend.title = element_text(face = 'bold', size = 8))
  })
  output$fluHeatMap <- renderPlot({
    ggplot(tweetStateChartFinal, aes(fill=ACTIVITY.LEVEL, map_id=STATENAME)) +
      geom_map(map=fifty_states, color = 'black', size = 0.1) +
      expand_limits(x=fifty_states$long, y=fifty_states$lat) +
      coord_map() +
      theme_classic() +
      ggtitle("2018-19 Influenza Season Week 5 ending Feb 09, 2019") +
      scale_fill_manual(drop=FALSE, name = "ILI Activity Level", values = c("#cc0000", "#fa4f00", "#fc8200", "#fcb100", "#f7df00", "#e0f500", "#baf700", "#8cf700", "#5bf700", "#00c200", "white")) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), axis.title.y = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            line = element_blank(),plot.title = element_text(hjust = 0.5, face = 'bold', size = 10),
            #legend.text = element_blank(),
            legend.title = element_text(face = 'bold', size = 8))
  })
  output$cdcHeatMap1 <- renderPlot({
    ggplot(X7_Flu_Heat_Map, aes(fill=ACTIVITY.LEVEL, map_id=STATENAME)) +
      geom_map(map=fifty_states, color = 'black', size = 0.1) +
      expand_limits(x=fifty_states$long, y=fifty_states$lat) +
      coord_map() +
      theme_classic() +
      ggtitle("2018-19 Influenza Season Week 5 ending Feb 09, 2019") +
      scale_fill_manual(drop=FALSE, name = "ILI Activity Level", values = c("#cc0000", "#fa4f00", "#fc8200", "#fcb100", "#f7df00", "#e0f500", "#baf700", "#8cf700", "#5bf700", "#00c200", "white")) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), axis.title.y = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            line = element_blank(),plot.title = element_text(hjust = 0.5, face = 'bold', size = 10),
            #legend.text = element_blank(),
            legend.title = element_text(face = 'bold', size = 8))
  })
  
  output$fluHeatMap1 <- renderPlot({
    ggplot(tweetStateChartFinal, aes(fill=ACTIVITY.LEVEL, map_id=STATENAME)) +
      geom_map(map=fifty_states, color = 'black', size = 0.1) +
      expand_limits(x=fifty_states$long, y=fifty_states$lat) +
      coord_map() +
      theme_classic() +
      ggtitle("2018-19 Influenza Season Week 5 ending Feb 09, 2019") +
      scale_fill_manual(drop=FALSE, name = "ILI Activity Level", values = c("#cc0000", "#fa4f00", "#fc8200", "#fcb100", "#f7df00", "#e0f500", "#baf700", "#8cf700", "#5bf700", "#00c200", "white")) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), axis.title.y = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            line = element_blank(),plot.title = element_text(hjust = 0.5, face = 'bold', size = 10),
            #legend.text = element_blank(),
            legend.title = element_text(face = 'bold', size = 8))
  })
  
  output$influenzaHeatMap1 <- renderPlot({
    ggplot(tweetStateChartInfluenzaFinal, aes(fill=ACTIVITY.LEVEL, map_id=STATENAME)) +
      geom_map(map=fifty_states, color = 'black', size = 0.1) +
      expand_limits(x=fifty_states$long, y=fifty_states$lat) +
      coord_map() +
      theme_classic() +
      ggtitle("2018-19 Influenza Season Week 5 ending Feb 09, 2019") +
      scale_fill_manual(drop=FALSE, name = "ILI Activity Level", values = c("#cc0000", "#fa4f00", "#fc8200", "#fcb100", "#f7df00", "#e0f500", "#baf700", "#8cf700", "#5bf700", "#00c200", "white")) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), axis.title.y = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            line = element_blank(),plot.title = element_text(hjust = 0.5, face = 'bold', size = 10),
            #legend.text = element_blank(),
            legend.title = element_text(face = 'bold', size = 8))
  })
  
  output$feverHeatMap1 <- renderPlot({
    ggplot(tweetStateChartFeverFinal, aes(fill=ACTIVITY.LEVEL, map_id=STATENAME)) +
      geom_map(map=fifty_states, color = 'black', size = 0.1) +
      expand_limits(x=fifty_states$long, y=fifty_states$lat) +
      coord_map() +
      theme_classic() +
      ggtitle("2018-19 Influenza Season Week 5 ending Feb 09, 2019") +
      scale_fill_manual(drop=FALSE, name = "ILI Activity Level", values = c("#cc0000", "#fa4f00", "#fc8200", "#fcb100", "#f7df00", "#e0f500", "#baf700", "#8cf700", "#5bf700", "#00c200", "white")) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), axis.title.y = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            line = element_blank(),plot.title = element_text(hjust = 0.5, face = 'bold', size = 10),
            #legend.text = element_blank(),
            legend.title = element_text(face = 'bold', size = 8))
  })
}



# Run the application 
shinyApp(ui = uiFinal, server = serverFinal)

