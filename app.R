library(tidyverse)
library(shiny)
library(shinydashboard)
library(toOrdinal)
library(plotly)
library(shinyBS)
library(DT)

source("data_processing.R")
source("helpers.R")
source("ui.R")
source("server.R")


ui <- dashboardPage(header, sidebar, body, skin = "purple")

shinyApp(ui, server)
