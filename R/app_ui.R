
#' .make_app_ui
#' 
#' @description Creates the user interface for the Shiny app.
#' 
#' @param self A **CoreEngine** or child self object.
#' 
#' @noRd
#'
.make_app_ui <- function(self) {
  
  # library(shinyjs)

  ui <- dashboardPage(skin = "black",
    
    dashboardHeader(title = is(self), dropdownMenuOutput("warningMenu")),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = NULL),
        menuItem("Explorer", tabName = "explorer", icon = NULL)
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "overview",
                
          fluidRow(
            
            box(width = 12, solidHeader = TRUE, uiOutput("wdir")),
            
            box(title = "Headers", width = 12, solidHeader = TRUE, uiOutput("headers")),
            
            column(12, uiOutput("save_engine"), uiOutput("reset_engine")),
            
            column(12,
              fluidRow(
                column(12,
                  div(style = "display: flex; align-items: center; justify-content: space-between;",
                    tags$b(style = "width: 110px; margin-bottom: 15px;", "Header name: "),
                    textInput("new_header_name", label = NULL, width = '100%')
                  )
                ),
                column(12,
                  div(style = "display: flex; align-items: center; justify-content: space-between;",
                    tags$b(style = "width: 110px; margin-bottom: 15px;", "Header value: "),
                    textInput("new_header_value", label = NULL, width = '100%')
                  )
                )
              ),
              
              actionButton("add_header_button", label = "Add Entry", width = 200),
              div(style = "margin-bottom: 20px;")
            ),
            
            box(title = "Analyses", width = 12, solidHeader = TRUE, uiOutput("overview_analyses")),
            
            column(width = 12,
              actionButton("add_analyses_button", label = "Add Analysis Files", width = 200),
              actionButton("update_analyses_button", label =  "Update Analyses", width = 200),
              actionButton("reset_analyses_button", label =  "Discard Changes", width = 200)
            ),
          )
        ),
        
        tabItem(tabName = "explorer",
          fluidRow(
            box(title = "Box 3", status = "warning", solidHeader = TRUE, "Content for box 3"),
            box(title = "Box 4", status = "warning", solidHeader = TRUE, "Content for box 4")
          )
        )
      )
    )
  )

  return(ui)
}