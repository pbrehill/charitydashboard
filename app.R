# install.packages(c("tidyverse", "shiny", "shinydashboard", "toOrdinal",
#                    "plotly", "shinyBS", "DT"))

# TODO: Figure out why we can't source script but can run in console.

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
