library(shiny)

source("R/app_ui.R")
source("R/app_server.R")

shiny::shinyApp(ui = app_ui, server = app_server)
