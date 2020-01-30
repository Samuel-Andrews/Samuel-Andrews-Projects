#Name: Samuel Andrews
#
#Class: DSCI 325
#
#Purpose: This R Shiny App will allow one look at the distribution of hp across various car models
#         for a given number of gears, and determine the total mean of various columns in the dataset
#       
#Output: The Shiny app that makes use of the built in R data set mtcars. 
#        it includes a plot output and a verbatim text output







library(shiny)
library(ggplot2)
library(dplyr)

#This is app uses the built in r dataset mtcars
car <- mtcars

ui <- fluidPage(
  titlePanel("Car Statistics across models"),
  sidebarLayout(
    sidebarPanel(
      
      
      uiOutput("cly"),
      uiOutput("gear")
      
    ),
    
    mainPanel(plotOutput("Cplot"), br(), verbatimTextOutput("AHP"))
  )
)

server <- function (input, output){
 
  output$cly <- renderUI({
         selectInput("c", "No. of Cylinders", sort(unique(car$cyl)), selected = 4)
       })
  
  output$gear <- renderUI({
    radioButtons("AV", "Average of:", choices = c("HP","MPG","WT"), selected = 'HP')
    
  })
  
  
  xdata <- reactive(
  car %>%
    filter(cyl == input$c)
    
  )
    
  xdata2 <- reactive(
    if (input$AV == "HP")
    {
      mean(car[,4])
    }else if (input$AV == "MPG")
    {
      mean(car[,1])
    }else if (input$AV == "WT")
    {
      mean(car[,6])
    }else
    {
      "Error"
    }
    
      
  )
    
  
    

 output$Cplot <- renderPlot({
   
   ggplot(data = xdata()) + geom_histogram(mapping = aes(hp), binwidth = 10) + ggtitle("Distribution of Horsepower based on car models with selected cylinder count")
 })
 

 output$AHP <- renderText({
    paste("The Average value of the selected column is",xdata2())
   
   
   
 })
  
}
 
shinyApp(ui = ui, server = server)






















