#' **UvEngine** R6 class and methods
#'
#' @description
#' The UvEngine R6 class handles management, processing, visualization and 
#' reporting of any two-dimensional data.
#' 
#' @template arg-headers
#' @template arg-ms-analyses
#' @template arg-ms-settings
#'
UvEngine <- R6::R6Class("UvEngine",

  inherit = CoreEngine,

  # _ private fields -----
  private = list(),
  
  # _ public fields/methods -----
  public = list(
    ## ___ create -----
    
    #' @description
    #' Creates an R6 class *UvEngine*.
    #'
    initialize = function(headers = NULL,
                          settings = NULL,
                          analyses = NULL) {

      if (is.null(headers)) headers <- ProjectHeaders()

      if (!is.null(headers)) suppressMessages(self$add_headers(headers))

      if (!is.null(settings)) suppressMessages(self$add_settings(settings))

      if (!is.null(analyses)) suppressMessages(self$add_analyses(analyses))

      private$.register(
        "created",
        "Core",
        headers$name,
        "StreamFind",
        as.character(packageVersion("StreamFind")),
        paste(c(headers$author, headers$path), collapse = ", ")
      )

      message("\U2713 Engine created!")
    }
  )
)
