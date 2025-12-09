## MARK: DB_MassSpecEngine
#' @title DB_Engine dedicated to Mass Spectrometry (MS) data
#' @description R6 child of DB_Engine for MassSpec data that uses DB_MassSpecAnalyses for on-disk storage.
#' @template arg-core-project-dir
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
    .data_type = "DB_MassSpec",
    .Analyses = NULL,
    .NonTargetAnalysis = NULL
  ),

  active = list(
    #' @field Analyses A DB_MassSpecAnalyses object backed by DuckDB.
    Analyses = function(value) {
      if (missing(value)) {
        if (is.null(private$.Analyses)) {
          private$.Analyses <- DB_MassSpecAnalyses(
            db = file.path(private$.project_path, "DB_MassSpecAnalyses.duckdb")
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
          nts_db_path <- file.path(private$.project_path, "DB_MassSpecResults_NonTargetAnalysis.duckdb")
          if (!file.exists(nts_db_path)) {
            private$.NonTargetAnalysis <- DB_MassSpecResults_NonTargetAnalysis(
              db = nts_db_path,
              analyses = query_db(self$Analyses, "SELECT * FROM Analyses")
            )
          } else {
            private$.NonTargetAnalysis <- DB_MassSpecResults_NonTargetAnalysis(db = nts_db_path)
          }
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
    initialize = function(project_dir = "data.sf",
                          metadata = NULL,
                          workflow = NULL,
                          files = NULL,
                          centroid = FALSE,
                          levels = c(1, 2),
                          configuration = NULL) {
      super$initialize(
        project_dir = project_dir,
        metadata = metadata,
        workflow = workflow,
        configuration = configuration,
        data_type = "DB_MassSpec"
      )
      private$.Analyses <- DB_MassSpecAnalyses(
        db = file.path(private$.project_path, "DB_MassSpecAnalyses.duckdb"),
        files = files,
        centroid = centroid,
        levels = levels
      )
    }
  )
)
