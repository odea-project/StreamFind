# StreamFind Shiny App
# This file allows the app to be run with shiny::runApp()
# Usage: shiny::runApp("inst/app") or navigate to inst/app/ and run shiny::runApp()

# Load required packages
library(shiny)
library(StreamFind)

# Source the main app components from the package
ui <- StreamFind:::app_ui

# Modified server function to handle RInno session management
server <- function(input, output, session) {
  # Call the main server function
  StreamFind:::app_server(input, output, session)

  # Add session end handling for RInno (closes app properly when user exits)
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}

# Create the Shiny app
shinyApp(ui = ui, server = server)
