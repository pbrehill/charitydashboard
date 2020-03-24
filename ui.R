header <- dashboardHeader(title = "Charities Data Dashboard")
sidebar <- dashboardSidebar(
  selectInput(
    "category",
    "Select a charity activity",
    # Hardcoded to DF17
    c("All activities", sort(unique(df17$`main activity`))),
    "All activities"
  ),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Rankings", tabName = "rankings", icon = icon("list"))
    # menuItem("Data", tabName = "widgets", icon = icon("th"))
  )
)


body <- dashboardBody(
  # Topline figures
  tabItems(
    tabItem(
      tabName = "dashboard",
      fluidRow(
        valueBoxOutput("charity_num"),
        valueBoxOutput("total_revenue"),
        valueBoxOutput("total_giving")
      ),
      # Plots1
      fluidRow(
        # Revenue by source
        box(plotlyOutput("revenue_graph")),
        box(
          title = "Controls",
          sliderInput("slider", "Number of observations:", 1, 100, 50)
        )
      )
    )#,
    # tabItem(
    #   tabName = "rankings",
    #   fluidRow(
    #     tabBox(
    #       # Title can include an icon
    #       id = 'tabset1',
    #       title = "Rankings",
    #       width = 12,
    #       tabPanel(
    #         "Allrevenue",
    #        verbatimTextOutput("tabset1Selected")
    #       ),
    #       tabPanel(
    #         "Tab2", 
    #         verbatimTextOutput("tabset1Selected")
    #       )
    #     )
    #   ),
    #   fluidRow(
    #     textOutput("tabset1Selected")
    #   )
    )
  )
