# --------------------------------------------------------------
# Note: for some reason you can't just run the app, use runApp('app.R')
# --------------------------------------------------------------


# install.packages(c("tidyverse", "shiny", "shinydashboard", "toOrdinal",
#                    "plotly", "shinyBS", "DT"))


library(tidyverse)
library(shiny)
library(shinydashboard)
library(toOrdinal)
library(plotly)
library(shinyBS)
library(DT)

# TODO: Add list of dependencies

source("process_data.R")
source("helpers.R")
source("ui.R")
source("server.R")


ui <- dashboardPage(header, sidebar, body, skin = "purple")

shinyApp(ui, server)
