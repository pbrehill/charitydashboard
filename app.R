library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)

# source("data_processing.R")
source("helpers.R")
source("ui.R")
# source("server.R")

ui <- dashboardPage(header, sidebar, body, skin = "purple")

server <- function(input, output) {
  
  # Define filter function
  filterDF <- reactive({
    # Hardcoded df17 - maybe merge all into one DF?
    if (input$category != "All activities") {
      df17 %>%
        filter(`main activity` == input$category)
    } else {
      df17
    }
  })
  
  # Number of charities box
  output$charity_num <- renderValueBox({
    valueBox(
      nrow(filterDF()),
      subtitle = "Number of charities",
      icon = icon('ribbon')
    )
  })
  
  # Total revenue box
  output$total_revenue <- renderValueBox({
    valueBox(
      round_dollars(sum(filterDF()$`total revenue`)),
      subtitle = "Total revenue",
      icon = icon('dollar-sign')
    )
  })
  
  # Total giving revenue
  output$total_giving <- renderValueBox({
    valueBox(
      round_dollars(sum(filterDF()$`donations and bequests`)),
      subtitle = "Total giving revenue",
      icon = icon('hand-holding-usd')
    )
  })
  
  # Revenue sources graph
  output$revenue_graph <- renderPlotly({
    
    # Calculate revenue totals
    df <- filterDF() %>%
      select(`donations and bequests`, 
             `revenue from government`, 
             `revenue from goods and services`,
             `revenue from investments`,
             `other income`) %>%
      rename(giving = `donations and bequests`, 
             government = `revenue from government`, 
             `goods and services` = `revenue from goods and services`,
             investments = `revenue from investments`,
             other = `other income`) %>%
      summarise_all(sum) %>%
      t() %>%
      as.data.frame() %>%
      mutate(category = capFirst(rownames(.)),
             string_num = round_dollars(V1))

    fig <- df %>% plot_ly(labels = ~category, 
                          values = ~V1,
                          text = ~string_num,
                          hovertemplate = "%{label}: <br> %{text} </br> %{percent} <extra></extra>"
    )
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = "Revenue by source",  showlegend = T,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          legend = list(orientation = 'h')
                          )
    
    fig
  })
  
  # # Ranking tabs
  # output$tabset1Selected <- renderText({
  #   input$tabset1
  # })
  
}

shinyApp(ui, server)
