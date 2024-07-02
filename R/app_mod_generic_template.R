#' @title .mod_generic_template_UI
#' 
#' @description Template for Shiny module UI.
#' 
#' @noRd
#'
.mod_generic_template_UI <- function(id) {
  ns <- shiny::NS(id)
}

#' @title .mod_generic_template_Server
#' 
#' @description Template for Shiny module server.
#' 
#' @noRd
#'
.mod_generic_template_Server <- function(id, engine, reactive_headers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
