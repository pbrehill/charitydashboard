header <- dashboardHeader(title = "Charities Data Dashboard")
sidebar <- dashboardSidebar(
  selectInput(
    "category",
    "Select a charity activity",
    # Hardcoded to DF17
    sort(unique(df17$`main activity`)),
    "Social services"
  ),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
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
      # Plots
      fluidRow(
        box(plotOutput("plot1", height = 250)),
        box(
          title = "Controls",
          sliderInput("slider", "Number of observations:", 1, 100, 50)
        )
      )
    )
  )
)