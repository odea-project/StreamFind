#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#' 
#' @noRd
app_ui <- function(request) {
  
  tagList(
    
    golem_add_external_resources(),
    
    shinydashboard::dashboardPage(skin = "black",
                                  
      shinydashboard::dashboardHeader(title = shiny::uiOutput("engine_type_ui"),
        shinydashboard::dropdownMenuOutput("warningMenu")
      ),
      
      shinydashboard::dashboardSidebar(
        
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Project", tabName = "project", icon = NULL),
          shinydashboard::menuItem("Analyses", tabName = "analyses", icon = NULL),
          shinydashboard::menuItem("Explorer", tabName = "explorer", icon = NULL),
          shinydashboard::menuItem("Workflow", tabName = "workflow", icon = NULL),
          shinydashboard::menuItem("Results", tabName = "results", icon = NULL),
          shinydashboard::menuItem("Audit Trail", tabName = "history", icon = NULL)
        )
      ),
      
      shinydashboard::dashboardBody(
        
        shinydashboard::tabItems(
          
          shinydashboard::tabItem(tabName = "project",
            shiny::fluidRow(
              shiny::uiOutput("wdir"),
              shiny::column(12, shiny::uiOutput("restart_app"), shiny::uiOutput("save_engine"), shiny::uiOutput("reset_engine")),
              shiny::fluidRow(shiny::uiOutput("headers_ui"))
            )
          ),
          
          shinydashboard::tabItem(tabName = "analyses", shiny::fluidRow(shiny::uiOutput("analyses_ui"))),
          
          shinydashboard::tabItem(tabName = "explorer", shiny::fluidRow(shiny::uiOutput("explorer_ui"))),
          
          shinydashboard::tabItem(tabName = "workflow", shiny::fluidRow(shiny::uiOutput("workflow_ui"))),
          
          shinydashboard::tabItem(tabName = "results", shiny::fluidRow()),
          
          shinydashboard::tabItem(tabName = "history", shiny::fluidRow(
            shinydashboard::box(width = 12, solidHeader = TRUE, DT::dataTableOutput("historyTable")))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#' 
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  htmltools::tags$head(
    golem::favicon(ext = "png"),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "StreamFind"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
