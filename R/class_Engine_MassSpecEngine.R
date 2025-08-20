# MARK: MassSpecEngine
# MassSpecEngine -----
#' @title Engine dedicated to Mass Spectrometry (MS) data processing
#' @description The `MassSpecEngine` R6 class is a framework for parsing, processing and inspecting mass spectrometry (MS) data. The engine has *MassSpec* as data type. The *Analyses* active field has class [StreamFind::MassSpecAnalyses], where MS data (i.e., spectra and chromatograms, including chromatograms produced by UV detection) can be loaded from *mzML* and *mzXML*. See methods [StreamFind::MassSpecAnalyses] for more details. If `msconvert` from \href{https://proteowizard.sourceforge.io/}{ProteoWizard} is installed and found via CLI (i.e., must be added to the environmental variables), the engine can also load vendor formats (i.e., Agilent MassHunter .d, Thermo Scientific RAW, Shimadzu LCD (except ITOF), Sciex WIFF /WIFF2) by direct conversion to mzML. Note that conversion of vendor formats is only possible under Windows OS. The *results* active field is a list that can hold the classes: [StreamFind::MassSpecResults_NonTargetAnalysis], [StreamFind::MassSpecSpectra] and [StreamFind::Chromatograms]. Note that the *MassSpecEngine* is a subclass of [StreamFind::Engine] and inherits all methods from the parent class.
#' 
#' @note The `MassSpecEngine` is using several \href{https://github.com/rickhelmus/patRoon}{patRoon} assets for assembly of Non-Target Analysis (NTA) data processing workflows.
#' 
#' @template arg-value
#' @template arg-analyses
#' 
#' @seealso [StreamFind::Engine] for the parent class.
#' 
#' @references
#' \insertRef{patroon01}{StreamFind}
#' 
#' \insertRef{patroon02}{StreamFind}
#' 
#' \insertRef{pugixml01}{StreamFind}
#' 
#' \insertRef{proteo01}{StreamFind}
#' 
#' \insertRef{proteo02}{StreamFind}
#' 
#' @export
#' 
MassSpecEngine <- R6::R6Class("MassSpecEngine",
  inherit = Engine,

  # MARK: active_bindings
  # Active Bindings -----
  active = list(

    # MARK: MassSpecResults_NonTargetAnalysis
    ## MassSpecResults_NonTargetAnalysis -----
    #' @field MassSpecResults_NonTargetAnalysis Get/Set for the `MassSpecResults_NonTargetAnalysis` results class.
    MassSpecResults_NonTargetAnalysis = function(value) {
      if (missing(value)) {
        return(self$Analyses$results[["MassSpecResults_NonTargetAnalysis"]])
      }
      if (is(value, "MassSpecResults_NonTargetAnalysis")) {
        if (is.null(validate_object(value))) {
          self$Analyses$results$MassSpecResults_NonTargetAnalysis <- value
        }
      } else {
        warning("Value must be a MassSpecResults_NonTargetAnalysis results object! Not done.")
      }
      invisible(self)
    },

    # MARK: MassSpecResults_Spectra
    #' @field MassSpecResults_Spectra Get/set for the `MassSpecResults_Spectra` results class.
    MassSpecResults_Spectra = function(value) {
      if (missing(value)) {
        return(self$Analyses$MassSpecResults_Spectra)
      }
      if (is(value, "MassSpecResults_Spectra")) {
        if (is.null(validate_object(value))) {
          self$Analyses$results$MassSpecResults_Spectra <- value
        }
      } else {
        warning("Value must be a MassSpecResults_Spectra object! Not done.")
      }
      invisible(self)
    },

    # MARK: MassSpecResults_Chromatograms
    #' @field MassSpecResults_Chromatograms Get/set for the MassSpecResults_Chromatograms results class.
    MassSpecResults_Chromatograms = function(value) {
      if (missing(value)) {
        return(self$Analyses@Chromatograms)
      }
      if (is(value, "MassSpecResults_Chromatograms")) {
        if (is.null(validate_object(value))) {
          self$Analyses$results$MassSpecResults_Chromatograms <- value
        }
      } else {
        warning("Value must be a MassSpecResults_Chromatograms object! Not done.")
      }
      invisible(self)
    }
  ),

  # MARK: public fields
  # Public Fields -----
  public = list(

    # MARK: initialize
    ## initialize -----
    #' @description Creates an R6 `MassSpecEngine` class object.
    #' @template arg-core-metadata
    #' @template arg-core-workflow
    #' @param analyses A [StreamFind::MassSpecAnalyses] class object or a `character vector` with full file paths to mzML files or a `data.frame` with `colnames`: "file", "replicate" and "blank". The "replicate" column is used to group the analyses and the "blank" column is used to identify the blank samples. The "file" column is the full path to the mzML files. If `msconvert` from \href{https://proteowizard.sourceforge.io/}{ProteoWizard} is installed and found via CLI (i.e., must be added to the environmental variables), the engine can also load vendor formats (i.e., Agilent MassHunter .d, Thermo Scientific RAW, Shimadzu LCD (except ITOF), Sciex WIFF / WIFF2) by direct conversion to mzML. Note that conversion of vendor formats is only possible under Windows OS.
    #' @param centroid Logical (length 1). Set to `TRUE` to centroid data when converting from vendor formats to mzML.
    #' @param levels Numeric vector with the MS levels to consider when centroiding data. Default is `c(1, 2)`.
    #' @seealso [StreamFind::Engine]
    #' @return A new `MassSpecEngine` class object.
    #'
    initialize = function(metadata = NULL,
                          workflow = NULL,
                          analyses = NULL,
                          centroid = FALSE,
                          levels = c(1, 2)) {
      super$initialize(metadata, workflow, analyses, centroid, levels, type = "MassSpec")
      invisible(self)
    },

    # MARK: add_analyses
    #' @description Adds analyses. Note that when adding new analyses, any existing results are removed.
    #' @param analyses A character vector with full file paths to mzML/mzXML files. f `msconvert` from \href{https://proteowizard.sourceforge.io/}{ProteoWizard} is installed and found via CLI (i.e., must be added to the environmental variables), the engine can also load vendor formats (i.e., Agilent MassHunter .d, Thermo Scientific RAW, Shimadzu LCD (except ITOF), Sciex WIFF/WIFF2) by direct conversion to mzML. Note that conversion of vendor formats is only possible under Windows OS.
    add_analyses = function(analyses = NULL) {
      self$Analyses <- add(self$Analyses, analyses)
      invisible(self)
    },
    
    # MARK: remove_analyses
    #' @description Removes analyses.
    remove_analyses = function(analyses = NULL) {
      self$Analyses <- remove(self$Analyses, analyses)
      invisible(self)
    },
    
    # MARK: add_replicate_names
    #' @description Set replicate names to the analysis, using a character vector with the same length as the number of analyses.
    set_replicate_names = function(value) {
      self$Analyses <- set_replicate_names(self$Analyses, value)
      invisible(self)
    },
    
    # MARK: set_blank_names
    #' @description Set blank names to the analysis, using a character vector with the same length as the number of analyses. Note that the names must be one of the replicate names.
    set_blank_names = function(value) {
      self$Analyses <- set_blank_names(self$Analyses, value)
      invisible(self)
    },

    # MARK: get_analysis_names
    #' @description Gets a character vector with the analysis replicate names.
    get_analysis_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses$analyses, analyses)
      get_analysis_names(self$Analysess)[analyses]
    },

    # MARK: get_replicate_names
    #' @description Gets a character vector with the analysis replicate names.
    get_replicate_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses$analyses, analyses)
      get_replicate_names(self$Analyses)[analyses]
    },

    # MARK: get_blank_names
    #' @description Gets a character vector with the analysis blank replicate names.
    get_blank_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses$analyses, analyses)
      get_blank_names(self$Analyses)[analyses]
    },

    # MARK: get_files
    #' @description Gets a character vector with the full file paths of each analysis.
    get_files = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses$analyses, analyses)
      vapply(
        self$Analyses$analyses[analyses],
        function(x) x$file,
        NA_character_
      )
    }
  )
)
