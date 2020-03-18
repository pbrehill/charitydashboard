library(tidyverse)
library(shiny)
library(shinydashboard)

# source("data_processing.R")
source("helpers.R")
source("ui.R")
# source("server.R")

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  # Define filter function
  filterDF <- reactive({
    # Hardcoded df17 - maybe merge all into one DF?
    df17 %>%
      filter(`main activity` == input$category)
  })
  
  # Number of charities box
  output$charity_num <- renderValueBox({
    valueBox(
      nrow(filterDF()),
      subtitle = "Number of charities"
    )
  })
  
  # Total revenue box
  output$total_revenue <- renderValueBox({
    valueBox(
      round_dollars(sum(filterDF()$`total revenue`)),
      subtitle = "Total revenue"
    )
  })
  
  # Total giving revenue
  output$total_giving <- renderValueBox({
    valueBox(
      round_dollars(sum(filterDF()$`donations and bequests`)),
      subtitle = "Total giving revenue"
    )
  })
  
}

shinyApp(ui, server)
