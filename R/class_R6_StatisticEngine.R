#' **StatisticEngine** R6 class and methods
#' 
#' @description
#' The *StatisticEngine* R6 class is a framework for performing statistical analysis on data.
#' 
#' @template arg-headers
#' @template arg-settings-and-list
#' @template arg-results
#' @template arg-analyses
#'
#' @export
#'
StatisticEngine <- R6::R6Class("StatisticEngine",

  inherit = CoreEngine,

  # _ private fields -----
  private = list( ),

  # _ active bindings -----
  active = list(
    
    #' @field data Matrix of the data, where rows represent analyses and columns variables.
    #'
    data = function() {
     
      res <- lapply(self$get_analyses(), function(x) x$data)
     
      names(res) <- self$get_analysis_names()
     
      res <- Map(function(x, y) {
        x$analysis <- y
        x
      }, res, names(res))
     
      res
    }
  ),

  # _ public fields -----
  public = list(
    
    #' @description Creates an R6 class *StatisticEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param data Data.frame, data-table or matrix with data.
    #'
    initialize = function(data = NULL, headers = NULL, settings = NULL, analyses = NULL, results = NULL) {
     
     if (!is.null(analyses)) {
       
       if (is(analyses, "RamanAnalysis")) analyses <- list(analyses)
       
       if (!all(vapply(analyses, function(x) is(x, "RamanAnalysis"), FALSE))) {
         warning("The argument analyses must be a RamanAnalysis object or a list of RamanAnalysis objects! Not done.")
         analyses <- NULL
       }
     }
     
     super$initialize(headers, settings, analyses, results)
     
     if (!is.null(data)) self$add_data(data)
     
     private$.register(
      "created",
      "StatisticEngine",
      headers$name,
      "StreamFind",
      as.character(packageVersion("StreamFind")),
      paste(c(headers$author, headers$path), collapse = ", ")
     )
    },
    
    ## ___ get -----
    
    #' @description Gets an overview data.frame of all the analyses.
    #'
    get_overview = function() {
     
      if (length(private$.analyses) > 0) {
       
        ov <- super$get_overview()
        
        row.names(ov) <- seq_len(nrow(ov))
        
        ov
       
      } else {
       data.frame()
      }
    },
    
    ## ___ add -----
    
    #' @description Adds data to the *StatisticEngine* object.
    #' 
    #' @param data Data.frame, data-table or matrix with data.
    #'  
    add_data = function(data) {
      
      
      
      
    },
    
    ## ___ info -----
    
    ### ___ processing_function_calls -----
    
    #' @description A data.table with available data processing methods.
    #'
    processing_methods = function() {
      data.table(
        name = c(
          
        ),
        max = c(
          
        ),
      )
    }
  )
)