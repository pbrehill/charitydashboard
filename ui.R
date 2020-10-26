header <- dashboardHeader(title = "Charities Data Dashboard")
sidebar <- dashboardSidebar(
  selectInput(
    # TODO: Finish this input after fixing on the server side
    "year_selected",
    "Year",
    # Hardcoded to DF17
    c(sort(unique(all_years_data$year), decreasing = TRUE)),
    "2018"
  ),
  selectInput(
    "category",
    "Charity activity",
    # Hardcoded to DF17
    c("All activities", sort(unique(df17$main.activity))),
    "All activities"
  ),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Rankings", tabName = "rankings", icon = icon("list")),
    menuItem("Data", tabName = "data", icon = icon("th"))
  )
)


body <- dashboardBody(
  # Insert CSS link
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  # Topline figures
  tabItems(
    tabItem(
      tabName = "dashboard",
      h2(textOutput("dashboard_header")),
      fluidRow(
        valueBoxOutput("charity_num"),
        valueBoxOutput("total_revenue"),
        valueBoxOutput("total_giving"),
        valueBoxOutput("total_staff"),
        valueBoxOutput("total_volunteers")
      ),
      # Plots1
      fluidRow(
        # Revenue by source
        box(
          title = "Revenue by source",
          plotlyOutput("revenue_graph")
          ),
        box(
          title = "Concentration of giving",
          plotlyOutput("giving_percentiles")
        )
      )
    ),
    tabItem(
      tabName = "rankings",
      # Add all rankings on left, split on right selected from menu
      fluidRow(
        box(
          title = "Rankings summary",
          width = 3,
          actionButton("total_revenue_button", "Total revenue"),
          actionButton("total_giving_button", "Giving revenue"),
          actionButton("total_govt_button", "Government grants"),
          actionButton("total_investment_button", "Investment revenue"),
          textOutput('test')
        ),
        box(
          title = "Rankings",
          width = 9,
          height = 500,
          textOutput("rank.text"),
          plotlyOutput("all.revenue.ranking")
          )
        )
      ),
    tabItem(
      tabName = "data",
      # Add all rankings on left, split on right selected from menu
      fluidRow(
        box(
          width = 3,
          uiOutput('collapse_boxes')
        ),
        box(
          width = 9,
          dataTableOutput("all_data")
          )
        )
      )
    )
  )