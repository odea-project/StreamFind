## MARK: DB_MassSpecEngine
#' @title DB_Engine dedicated to Mass Spectrometry (MS) data
#' @description R6 child of DB_Engine for MassSpec data that uses DB_MassSpecAnalyses for on-disk storage.
#' @template arg-core-projectPath
#' @template arg-core-metadata
#' @template arg-core-workflow
#' @template arg-ms-files
#' @template arg-ms-centroid
#' @template arg-ms-levels
#' @template arg-core-configuration
#' @template arg-analyses
#' @template arg-sql-tableName
#' @template arg-sql-sql
#' @template arg-sql-params
#' @export
#' 
DB_MassSpecEngine <- R6::R6Class(
  "DB_MassSpecEngine",
  inherit = DB_Engine,

  private = list(
    .dataType = "DB_MassSpec",
    .Analyses = NULL,
    .NonTargetAnalysis = NULL
  ),

  active = list(
    #' @field Analyses A DB_MassSpecAnalyses object backed by DuckDB.
    Analyses = function(value) {
      if (missing(value)) {
        if (is.null(private$.Analyses)) {
          private$.Analyses <- DB_MassSpecAnalyses(
            projectPath = private$.projectPath
          )
        }
        return(private$.Analyses)
      }
      if (is(value, "DB_MassSpecAnalyses")) {
        if (!is.null(validate_object(value))) {
          warning("Invalid DB_MassSpecAnalyses object! Not added.")
          return(invisible(self))
        }
        private$.Analyses <- value
      } else {
        warning("Analyses must be a DB_MassSpecAnalyses object! Not added.")
      }
      invisible(self)
    },
    #' @field NonTargetAnalysis A DB_MassSpecResults_NonTargetAnalysis object backed by DuckDB.
    NonTargetAnalysis = function(value) {
      if (missing(value)) {
        if (is.null(private$.NonTargetAnalysis)) {
          private$.NonTargetAnalysis <- DB_MassSpecResults_NonTargetAnalysis(
            projectPath = private$.projectPath
          )
        }
        return(private$.NonTargetAnalysis)
      }
      if (is(value, "DB_MassSpecResults_NonTargetAnalysis")) {
        if (!is.null(validate_object(value))) {
          warning("Invalid DB_MassSpecResults_NonTargetAnalysis object! Not added.")
          return(invisible(self))
        }
        private$.NonTargetAnalysis <- value
      } else {
        warning("NonTargetAnalysis must be a DB_MassSpecResults_NonTargetAnalysis object! Not added.")
      }
    }
  ),

  public = list(
    #' @description Initialize DB_MassSpecEngine
    initialize = function(projectPath = "data",
                          metadata = NULL,
                          workflow = NULL,
                          files = NULL,
                          centroid = FALSE,
                          levels = c(1, 2)) {
      super$initialize(
        projectPath = projectPath,
        metadata = metadata,
        workflow = workflow,
        dataType = "DB_MassSpec"
      )
      private$.Analyses <- DB_MassSpecAnalyses(
        projectPath = private$.projectPath,
        files = files,
        centroid = centroid,
        levels = levels
      )
    }
  )
)
