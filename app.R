library(tidyverse)
library(shiny)
library(shinydashboard)
library(toOrdinal)

source("data_processing.R")
source("helpers.R")
source("ui.R")
source("server.R")

library(plotly)


ui <- dashboardPage(header, sidebar, body, skin = "purple")

shinyApp(ui, server)
