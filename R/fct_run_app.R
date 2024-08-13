
#' run_app
#'
#' @param file An `sqlite` file. 
#'
#' @export
#'
run_app <- function(file = NA_character_) {
  
  engine_save_file <- NULL
  
  engine_type <- NULL
  
  if (!is.na(file)) {
    
    if (!grepl(".sqlite$", file)) {
      msg <- paste("The file", file, "is not an sqlite file!")
      stop(msg)
      
    } else {
      
      if (!file.exists(file)) {
        msg <- paste("The file", file, "does not exist!")
        stop(msg)
        
      } else {
        
        engine_save_file <- file
        
      }
    }
  }
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    warning("Shiny package not installed!")
    return(invisible(self))
  }
  
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    warning("htmltools package not installed!")
    return(invisible(self))
  }
  
  if (!requireNamespace("shinydashboard", quietly = TRUE)) {
    warning("shinydashboard package not installed!")
    return(invisible(self))
  }
  
  if (!requireNamespace("shinycssloaders", quietly = TRUE)) {
    warning("shinycssloaders package not installed!")
    return(invisible(self))
  }
  
  if (!requireNamespace("shinyFiles", quietly = TRUE)) {
    warning("shinyFiles package not installed!")
    return(invisible(self))
  }
  
  if (!requireNamespace("sortable", quietly = TRUE)) {
    warning("sortable package not installed!")
    return(invisible(self))
  }
  
  shiny::shinyApp(ui = .make_app_ui(), server = .make_app_server(engine_type, engine_save_file))
}
