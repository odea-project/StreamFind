#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#' 
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shinydashboard::dashboardPage(skin = "black",
      shinydashboard::dashboardHeader(title = shiny::uiOutput("app_mode_ui"),
        shinydashboard::dropdownMenuOutput("warningMenu")
      ),
      shinydashboard::dashboardSidebar(shinydashboard::sidebarMenuOutput("sidebar_ui")),
      shinydashboard::dashboardBody(shiny::uiOutput("body_ui"))
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
