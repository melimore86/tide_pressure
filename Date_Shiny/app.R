library("shiny")
library("shinythemes")
library("tidyverse")
library("dplyr")
library("ggplot2")


setwd("C:/Users/melimore86/Desktop/tide_pressure")

data<- read.csv("data.csv", header= T)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("yeti"),
   
   # Application title
   titlePanel("Tide Inundation"),
   
  h3("SELECT DATE"),
  
  sliderInput("date", 
              "Choose Date Range:", 
              min = as.POSIXct("2017-09-01"), max = as.POSIXct(Sys.Date()), 
              value = c(as.POSIXct("2017-09-02"), Sys.Date())),
  
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("inunplot")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$inunplot <- renderPlot({
    
     ggplot() +
       
       geom_line(data = data, aes(x = input$date, y = pred_normalization, color= "Predicted Tidal Height (m)"), size =1.2)  +
       
       geom_line(data = data, aes(x = input$date, y = normalization, color= "Tidal Height (m)"), size =1.2)  +
       
       geom_line(data = data, aes(x = input$date, y = site_normalization, color= "Atmospheric pressure at elevation height at Height"), size =1.2)  +

       scale_colour_manual(values=c("#0072B2","#D55E00","#CC79A7")) +
       
       theme(legend.position=("bottom"),
             panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
             axis.text.x = element_text(angle = 90, hjust = 1)) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

