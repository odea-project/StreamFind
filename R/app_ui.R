#' .make_app_ui
#' 
#' @description Creates the user interface for the Shiny app.
#' 
#' @param self A **CoreEngine** or child self object.
#' 
#' @noRd
#'
.make_app_ui <- function(self) {

  ui <- shinydashboard::dashboardPage(skin = "black",
    
    shinydashboard::dashboardHeader(title = is(self), shinydashboard::dropdownMenuOutput("warningMenu")),
    
    shinydashboard::dashboardSidebar(
      
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Overview", tabName = "overview", icon = NULL),
        shinydashboard::menuItem("Explorer", tabName = "explorer", icon = NULL),
        shinydashboard::menuItem("Workflow", tabName = "workflow", icon = NULL),
        shinydashboard::menuItem("Results", tabName = "results", icon = NULL),
        shinydashboard::menuItem("Audit Trail", tabName = "history", icon = NULL)
      )
    ),
    
    shinydashboard::dashboardBody(
      
      shinydashboard::tabItems(
        
        shinydashboard::tabItem(tabName = "overview",
                
            shiny::fluidRow(
            
            shinydashboard::box(width = 12, solidHeader = TRUE, shiny::uiOutput("wdir")),
            
            shiny::column(12, shiny::uiOutput("save_engine"), shiny::uiOutput("reset_engine")),
            
            shiny::fluidRow(shiny::uiOutput("headers_ui")),
            
            shiny::fluidRow(shiny::uiOutput("analyses_ui"))
          )
        ),
        
        shinydashboard::tabItem(tabName = "explorer", shiny::fluidRow(shiny::uiOutput("explorer_ui"))),
        
        shinydashboard::tabItem(tabName = "workflow", shiny::fluidRow(shiny::uiOutput("workflow_ui"))),
        
        shinydashboard::tabItem(tabName = "results", shiny::fluidRow()),
        
        shinydashboard::tabItem(tabName = "history", shiny::fluidRow(shinydashboard::box(width = 12, solidHeader = TRUE, shiny::dataTableOutput("history_table"))))
      )
    )
  )

  ui
}