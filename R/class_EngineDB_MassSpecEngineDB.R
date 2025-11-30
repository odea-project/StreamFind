## MARK: MassSpecEngineDB
#' @title EngineDB dedicated to Mass Spectrometry (MS) data
#' @description R6 child of EngineDB for MassSpec data that uses MassSpecAnalysesDB for on-disk storage.
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
MassSpecEngineDB <- R6::R6Class(
  "MassSpecEngineDB",
  inherit = EngineDB,

  private = list(
    .data_type = "MassSpec",
    .Analyses = NULL,
    .Results = list()
  ),

  active = list(
    #' @field Analyses A MassSpecAnalysesDB object backed by DuckDB.
    Analyses = function(value) {
      if (missing(value)) {
        if (is.null(private$.Analyses)) {
          private$.Analyses <- MassSpecAnalysesDB(db = file.path(private$.sf_root, "MassSpecAnalyses.duckdb"))
        }
        return(private$.Analyses)
      }
      if (is(value, "MassSpecAnalysesDB")) {
        if (!is.null(validate_object(value))) {
          warning("Invalid MassSpecAnalysesDB object! Not added.")
          return(invisible(self))
        }
        private$.Analyses <- value
      } else {
        warning("Analyses must be a MassSpecAnalysesDB object! Not added.")
      }
      invisible(self)
    },
    #' @field NonTargetAnalysis A MassSpecResults_NonTargetAnalysisDB object backed by DuckDB.
    NonTargetAnalysis = function() {
      nts_db_path <- file.path(private$.sf_root, "MassSpecResults_NonTargetAnalysis.duckdb")
      if (!file.exists(nts_db_path)) {
        NULL
      } else {
        MassSpecResults_NonTargetAnalysisDB(db = nts_db_path)
      }
    }
  ),

  public = list(
    #' @description Initialize MassSpecEngineDB
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
        data_type = "MassSpec"
      )
      private$.Analyses <- MassSpecAnalysesDB(
        db = file.path(private$.sf_root, "MassSpecAnalyses.duckdb"),
        files = files,
        centroid = centroid,
        levels = levels
      )
    },

    #' @description Add analyses (append; overwrites duplicates)
    add_analyses = function(files, centroid = FALSE, levels = c(1, 2)) {
      self$Analyses <- add_analyses(self$Analyses, files = files, centroid = centroid, levels = levels)
      invisible(self)
    },
    #' @description Get analysis names
    get_analysis_names = function() get_analysis_names(self$Analyses),
    #' @description Get replicate names
    get_replicate_names = function() get_replicate_names(self$Analyses),
    #' @description Get blank names
    get_blank_names = function() get_blank_names(self$Analyses),
    #' @description Get concentrations
    get_concentrations = function() get_concentrations(self$Analyses),
    #' @description Set replicate names
    #' @param value Character vector of replicate names matching analyses.
    set_replicate_names = function(value) { set_replicate_names(self$Analyses, value); invisible(self) },
    #' @description Set blank names
    #' @param value Character vector of blank names matching analyses.
    set_blank_names = function(value) { set_blank_names(self$Analyses, value); invisible(self) },
    #' @description Set concentrations
    #' @param value Numeric vector of concentrations matching analyses.
    set_concentrations = function(value) { set_concentrations(self$Analyses, value); invisible(self) },
    #' @description Get info summary
    info_analyses = function() info(self$Analyses),
    #' @description Get spectra headers for an analysis
    get_spectra_headers = function(analyses = NULL) get_spectra_headers(self$Analyses, analyses),
    #' @description Get chromatograms headers for an analysis
    get_chromatograms_headers = function(analyses = NULL) get_chromatograms_headers(self$Analyses, analyses)
  )
)
