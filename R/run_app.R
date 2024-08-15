#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(onStart = NULL, options = list(), enableBookmarking = NULL, uiPattern = "/", ...) {
  
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
  
  dots <- list(...)
  
  if ("file" %in% names(dots)) {
    file <- dots$file
    if (!is.na(file)) {
      if (!grepl(".sqlite$", file)) {
        msg <- paste("The file", file, "is not an sqlite file!")
        stop(msg)
      } else {
        if (!file.exists(file)) {
          msg <- paste("The file", file, "does not exist!")
          stop(msg)
        }
      }
    }
  }
  
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
