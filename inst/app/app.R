# StreamFind Shiny App
# This file allows the app to be run with shiny::runApp()
# Usage: shiny::runApp("inst/app") or navigate to inst/app/ and run shiny::runApp()

# Load required packages
library(shiny)
library(StreamFind)

# Source the main app components from the package
ui <- StreamFind:::app_ui
server <- StreamFind:::app_server

# Create the Shiny app
shinyApp(ui = ui, server = server)
