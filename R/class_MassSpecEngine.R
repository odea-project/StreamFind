## MARK: MassSpecEngine
#' @title Engine dedicated to Mass Spectrometry (MS) data
#' @description R6 child of Engine for MassSpec data that uses MassSpecAnalyses for on-disk storage.
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
MassSpecEngine <- R6::R6Class(
  "MassSpecEngine",
  inherit = Engine,

  private = list(
    .dataType = "MassSpec",
    .Analyses = NULL,
    .NonTargetAnalysis = NULL,
    .Chromatograms = NULL,
    .Spectra = NULL
  ),

  active = list(
    #' @field Analyses A MassSpecAnalyses object backed by DuckDB.
    Analyses = function(value) {
      if (missing(value)) {
        if (is.null(private$.Analyses)) {
          private$.Analyses <- MassSpecAnalyses(
            projectPath = private$.projectPath
          )
        }
        return(private$.Analyses)
      }
      if (is(value, "MassSpecAnalyses")) {
        if (!is.null(validate_object(value))) {
          warning("Invalid MassSpecAnalyses object! Not added.")
          return(invisible(self))
        }
        private$.Analyses <- value
      } else {
        warning("Analyses must be a MassSpecAnalyses object! Not added.")
      }
      invisible(self)
    },
    #' @field NonTargetAnalysis A MassSpecResults_NonTargetAnalysis object backed by DuckDB.
    NonTargetAnalysis = function(value) {
      if (missing(value)) {
        if (is.null(private$.NonTargetAnalysis)) {
          private$.NonTargetAnalysis <- MassSpecResults_NonTargetAnalysis(
            projectPath = private$.projectPath
          )
        }
        return(private$.NonTargetAnalysis)
      }
      if (is(value, "MassSpecResults_NonTargetAnalysis")) {
        if (!is.null(validate_object(value))) {
          warning("Invalid MassSpecResults_NonTargetAnalysis object! Not added.")
          return(invisible(self))
        }
        private$.NonTargetAnalysis <- value
      } else {
        warning("NonTargetAnalysis must be a MassSpecResults_NonTargetAnalysis object! Not added.")
      }
    },
    #' @field Chromatograms A MassSpecResults_Chromatograms object backed by DuckDB.
    Chromatograms = function(value) {
      if (missing(value)) {
        if (is.null(private$.Chromatograms)) {
          private$.Chromatograms <- MassSpecResults_Chromatograms(
            projectPath = private$.projectPath
          )
        }
        return(private$.Chromatograms)
      }
      if (is(value, "MassSpecResults_Chromatograms")) {
        if (!is.null(validate_object(value))) {
          warning("Invalid MassSpecResults_Chromatograms object! Not added.")
          return(invisible(self))
        }
        private$.Chromatograms <- value
      } else {
        warning("Chromatograms must be a MassSpecResults_Chromatograms object! Not added.")
      }
    },
    #' @field Spectra A MassSpecResults_Spectra object backed by DuckDB.
    Spectra = function(value) {
      if (missing(value)) {
        if (is.null(private$.Spectra)) {
          private$.Spectra <- MassSpecResults_Spectra(
            projectPath = private$.projectPath
          )
        }
        return(private$.Spectra)
      }
      if (is(value, "MassSpecResults_Spectra")) {
        if (!is.null(validate_object(value))) {
          warning("Invalid MassSpecResults_Spectra object! Not added.")
          return(invisible(self))
        }
        private$.Spectra <- value
      } else {
        warning("Spectra must be a MassSpecResults_Spectra object! Not added.")
      }
    }
  ),

  public = list(
    #' @description Initialize MassSpecEngine
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
        dataType = "MassSpec"
      )
      private$.Analyses <- MassSpecAnalyses(
        projectPath = private$.projectPath,
        files = files,
        centroid = centroid,
        levels = levels
      )
    }
  )
)
