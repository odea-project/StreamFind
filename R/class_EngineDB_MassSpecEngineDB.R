# MARK: MassSpecEngineDB
#' @title EngineDB dedicated to Mass Spectrometry (MS) data
#' @description R6 child of EngineDB for MassSpec data that uses MassSpecAnalysesDB for on-disk storage.
#' @export
MassSpecEngineDB <- R6::R6Class(
  "MassSpecEngineDB",
  inherit = EngineDB,

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
    }
  ),

  public = list(
    #' @description Initialize MassSpecEngineDB
    #' @param project_dir Path to StreamFind project folder (.sf). EngineDB will place main.duckdb inside it.
    #' @param metadata Optional metadata to persist.
    #' @param workflow Optional workflow to persist.
    #' @param analyses Optional MS files/data.frame to load into analyses DB.
    #' @param configuration Optional configuration list to persist.
    initialize = function(project_dir = "data.sf",
                          metadata = NULL,
                          workflow = NULL,
                          analyses = NULL,
                          configuration = NULL) {
      super$initialize(
        project_dir = project_dir,
        metadata = metadata,
        workflow = workflow,
        analyses = NULL,
        configuration = configuration,
        data_type = "MassSpec"
      )
      private$.Analyses <- MassSpecAnalysesDB(
        db = file.path(private$.sf_root, "MassSpecAnalyses.duckdb"),
        files = analyses
      )
    },

    #' @description Add analyses (append; overwrites duplicates)
    #' @param files Files/data.frame accepted by `.get_MassSpecAnalysis_from_files`.
    #' @param centroid Logical, passed to parser.
    #' @param levels Integer vector, MS levels to keep.
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
    #' @param analysis Analysis name.
    get_spectra_headers = function(analysis = NULL) get_spectra_headers(self$Analyses, analysis),
    #' @description Get chromatograms headers for an analysis
    #' @param analysis Analysis name.
    get_chromatograms_headers = function(analysis = NULL) get_chromatograms_headers(self$Analyses, analysis),
    #' @description List tables in analyses DB
    list_db_tables = function() list_db_tables(self$Analyses),
    #' @description Get table info in analyses DB
    #' @param table_name Table name.
    get_db_table_info = function(table_name) get_db_table_info(self$Analyses, table_name),
    #' @description Run query on analyses DB
    #' @param sql SQL string.
    #' @param params Optional params.
    query_db = function(sql, params = NULL) query_db(self$Analyses, sql, params)
  ),

  private = list(
    .Analyses = NULL
  )
)
