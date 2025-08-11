# MARK: MassSpecEngine
# MassSpecEngine -----
#' Mass Spectrometry Engine
#' 
#' @description The `MassSpecEngine` R6 class is a framework for parsing, processing and inspecting mass spectrometry (MS) data. The engine has *MassSpec* as data type. The *Analyses* active field has class [StreamFind::MassSpecAnalyses], where MS data (i.e., spectra and chromatograms, including chromatograms produced by UV detection) can be loaded from *mzML* and *mzXML*. See methods [StreamFind::MassSpecAnalyses] for more details. If `msconvert` from \href{https://proteowizard.sourceforge.io/}{ProteoWizard} is installed and found via CLI (i.e., must be added to the environmental variables), the engine can also load vendor formats (i.e., Agilent MassHunter .d, Thermo Scientific RAW, Shimadzu LCD (except ITOF), Sciex WIFF /WIFF2) by direct conversion to mzML. Note that conversion of vendor formats is only possible under Windows OS. The *results* active field is a list with that can hold the classes: [StreamFind::NonTargetAnalysisResults], [StreamFind::MassSpecSpectra] and [StreamFind::Chromatograms]. Note that the *MassSpecEngine* is a subclass of [StreamFind::Engine] and inherits all methods from the parent class.
#' 
#' @note The `MassSpecEngine` is using several \href{https://github.com/rickhelmus/patRoon}{patRoon} assets for assembly of Non-Target Analysis (NTA) data processing workflows.
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

    # MARK: NonTargetAnalysisResults
    ## NonTargetAnalysisResults -----
    #' @field NonTargetAnalysisResults Get/Set for the `NonTargetAnalysisResults` results class.
    NonTargetAnalysisResults = function(value) {
      if (missing(value)) {
        return(self$Analyses$results[["NonTargetAnalysisResults"]])
      }
      if (is(value, "StreamFind::NonTargetAnalysisResults")) {
        self$Analyses$results$NonTargetAnalysisResults <- value
      } else {
        warning("Value must be an NonTargetAnalysisResults results object! Not done.")
      }
      invisible(self)
    },

    # MARK: Spectra
    ## Spectra -----
    #' @field Spectra Get/set for the `MassSpecSpectra` results class.
    Spectra = function(value) {
      if (missing(value)) {
        return(self$Analyses$Spectra)
      }
      if (is(value, "StreamFind::MassSpecSpectra")) {
        self$Analyses$MassSpecSpectra <- value
      } else {
        warning("Value must be a Spectra object! Not done.")
      }
      invisible(self)
    },

    # MARK: Chromatograms
    ## Chromatograms -----
    #' @field Chromatograms Get/set for the chromatograms results class.
    Chromatograms = function(value) {
      if (missing(value)) {
        return(self$Analyses@Chromatograms)
      }
      if (is(value, "StreamFind::Chromatograms")) {
        self$Analyses@Chromatograms <- value
      } else {
        warning("Value must be a Chromatograms object! Not done.")
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
    #' @param analyses A `MassSpecAnalyses` S7 class object or a `character vector` with full file
    #' paths to mzML files or a `data.frame` with `colnames`: "file", "replicate" and "blank". The
    #' "replicate" column is used to group the analyses and the "blank" column is used to identify
    #' the blank samples. The "file" column is the full path to the mzML files. If `msconvert` from
    #' \href{https://proteowizard.sourceforge.io/}{ProteoWizard} is installed and found via CLI 
    #' (i.e., must be added to the environmental variables), the engine can also load vendor formats
    #' (i.e., Agilent MassHunter .d, Thermo Scientific RAW, Shimadzu LCD (except ITOF), Sciex WIFF
    #' / WIFF2) by direct conversion to mzML. Note that conversion of vendor formats is only
    #' possible under Windows OS.
    #' @param centroid Logical (length 1). Set to `TRUE` to centroid data when converting from
    #' vendor formats to mzML.
    #' @param levels Numeric vector with the MS levels to consider when centroiding data. Default
    #' is `c(1, 2)`.
    #' 
    #' @seealso [StreamFind::Engine]
    #'
    #' @return A new `MassSpecEngine` class object.
    #'
    initialize = function(metadata = NULL,
                          workflow = NULL,
                          analyses = NULL,
                          centroid = FALSE,
                          levels = c(1, 2)) {
      super$initialize(metadata, workflow, analyses, centroid, levels, type = "MassSpec")
      invisible(self)
    }
  )
)
