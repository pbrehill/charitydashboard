server <- function(input, output) {
  
  # Combine collapse checkboxes
  
  combine_lists <- reactive({
    return(c(input$vs_main,
             input$vs_revenue,
             input$vs_expenditure,
             input$vs_staffing,
             input$`vs_specific activities`,
             input$vs_beneficiaries,
             input$vs_purpose,
             input$`vs_international activity`,
             input$vs_location,
             input$vs_financials,
             input$vs_fundraising,
             input$vs_reporting
    ))
  })
  
  output$test <- renderText({combine_lists()})
  
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
    fig <- fig %>% layout(showlegend = T,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          legend = list(orientation = 'h')
    )
    
    fig
  })
  
  # Ranking tabs
  output$all.revenue.ranking <- renderPlotly({
    df1 <- filterDF() %>%
      select(`donations and bequests`, 
             `revenue from government`, 
             `revenue from goods and services`,
             `revenue from investments`,
             `other income`,
             `total revenue`,
             `main activity`) %>%
      group_by(`main activity`) %>%
      summarise_all(sum) %>%
      as.data.frame() %>%
      arrange(desc(`total revenue`)) %>%
      mutate(
        selected = `main activity` == input$category,
        selected_colour = ifelse(`selected`, 'purple', 'rgba(204,204,204,1)')
             ) %>%
      drop_na()
      
      fig <- df1 %>% plot_ly(y = ~reorder(`main activity`, `total revenue`), 
                          x = ~`total revenue`,
                          text = ~round_dollars(`total revenue`),
                          type = 'bar',
                          orientation = 'h',
                          marker = list(color = df1$selected_colour),
                          hovertemplate = "%{y}: <br> %{text} </br> <extra></extra>"
                          
    )
      
      fig <- fig %>%
        layout(xaxis = list(title = "Total revenue"), 
               yaxis = list(title = '', tickfont = list(size = 10)),
               margin = list(l = 330, r = 10, b = 10, t = 20, pad = 4)
               )
    
    fig
  })
  
  # Text ranking
  
  output$rank.text <- renderText({
    cat_rank <- rank3[grepl(input$category, rank3$`main activity`), "total.revenue.rank"]
    
    if (length(cat_rank) != 0) {
      paste0(input$category, " ranks ", toOrdinal(cat_rank), " of ", nrow(rank3), " categories\n")
    } else{
      NULL
    }
  })
  
  # Dashboard header
  output$dashboard_header <- renderText({
    paste(input$category, 'overview')
  })
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })
  
  # Select variables in data view
  output$all_data <- renderDataTable({
    
    selectedDT <- df17[,combine_lists()]
    
    datatable(selectedDT, options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  # Initialise reactive values
  rv <- reactiveValues()
  rv$main <- c('charity name', 'abn')
  rv$revenue <- c('donations and bequests', 'total income')
  rv$expenditure <- NULL
  rv$staffing <- NULL
  rv$`specific activities` <- NULL
  rv$beneficiaries <- NULL
  rv$purpose <- NULL
  rv$`international activity` <- NULL
  rv$location <- NULL
  rv$financials <- NULL
  rv$fundraising <- NULL
  rv$reporting <- NULL
  
  # Change collapsable box colours
  output$collapse_boxes <- renderUI({
    
    # Look at variables in big list, check if they're selected, include category, 
    # unique entries, set on a list whether true or false
    combined <- combine_lists()
    
    # for (i in nrow(variable_groupings)) {
    #   variable_groupings[i, 'selected'] <- variable_groupings[i, 'Var'] %in% combined
    # }
    
    selected_cats <- active_categories(combined)
    
    bsCollapse(
      collapseVarBox('main', rv, c('charity name', 'abn')),
      collapseVarBox('revenue', rv, c('donations and bequests', 'total income')),
      collapseVarBox('expenditure', rv),
      collapseVarBox('staffing', rv),
      collapseVarBox('specific activities', rv),
      collapseVarBox('beneficiaries', rv),
      collapseVarBox('purpose', rv),
      collapseVarBox('international activity', rv),
      collapseVarBox('location', rv),
      collapseVarBox('financials', rv),
      collapseVarBox('fundraising', rv),
      collapseVarBox('reporting', rv),
      multiple = TRUE,
      open = "vs_main")
  })
  
  # Observe box ticks
  observeEvent(combine_lists(), {
    # changes in this variable will make the UI "controls" to be rendered again
    newly_selected <- combine_lists()

    new_selected_cats <- variable_groupings[newly_selected == variable_groupings$Var,]

    for (i in nrow(new_selected_cats)) {
      rv[[new_selected_cats[i, 'Categorisation']]] <- new_selected_cats[i, 'Var']
    }

  })
}