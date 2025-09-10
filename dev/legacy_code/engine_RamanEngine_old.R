# MARK: RamanEngine
#' Raman Spectroscopy Engine
#'
#' @description The *RamanEngine* R6 class is a framework for parsing, processing, inspecting and 
#' storing Raman spectroscopic data. Raman data can be loaded from "asc", "sif", "json", "wdf", 
#' "sdf", "csv" and "txt" files.
#'
#' @template arg-core-metadata
#' @template arg-core-workflow
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-title
#' @template arg-legendNames
#' @template arg-colorBy
#' @template arg-labs
#' @template arg-interactive
#' @template arg-renderEngine
#' @template arg-useRawData
#' @template arg-raman-target
#' @template arg-raman-minIntensity
#' @template arg-raman-targets
#'
#' @references
#' \insertRef{orpl01}{StreamFind}
#'
#' @export
#'
RamanEngine <- R6::R6Class("RamanEngine",
  inherit = CoreEngine,

  # MARK: active bindings
  # active bindings -----
  active = list(

    # MARK: spectra
    ### Spectra -----
    #' @field Spectra `RamanSpectra` results object for each analysis or replicate.
    Spectra = function(value) {
      if (missing(value)) {
        return(self$Analyses$Spectra)
      }
      self$Analyses$Spectra <- value
      invisible(self)
    }
  ),

  # MARK: public fields
  # public fields -----
  public = list(

    # MARK: initialize
    ## initialize -----
    #' @description Creates an R6 class *RamanEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param analyses A `RamanAnalyses` S7 class object or a `character vector` with full file 
    #' paths to "asc", "sif", "json", "wdf", "sdf", "csv" and/or "txt" raman files or a `data.frame`
    #' with colnames `file`, `replicate` and `blank`. The "replicate" column is used to group the 
    #' analyses and the "blank" column is used to identify the blank samples. The "file" column is 
    #' the full to the raman files.
    #'
    initialize = function(metadata = NULL,
                          workflow = NULL,
                          analyses = NULL) {
      super$initialize(metadata, workflow, analyses)
      invisible(self)
    },

    # MARK: get_analysis_names
    ## get_analysis_names -----
    #' @description Gets the analysis replicate names.
    get_analysis_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      names(self$Analyses)[analyses]
    },

    # MARK: get_replicate_names
    ## get_replicate_names -----
    #' @description Gets the analysis replicate names.
    get_replicate_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses$replicates[analyses]
    },

    # MARK: get_blank_names
    ## get_blank_names -----
    #' @description Gets the analysis blank replicate names.
    get_blank_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses$blanks[analyses]
    },

    # MARK: get_files
    ## get_files -----
    #' @description Gets the full file paths of each analysis.
    get_files = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses$files[analyses]
    },

    # MARK: get_overview
    ## get_overview -----
    #' @description Gets an overview data.frame of all the analyses.
    get_overview = function() {
      self$Analyses$info
    },
    
    # MARK: get_spectra
    ## get_spectra -----
    #' @description Gets a list of spectra `data.table` objects for each analysis/replicate.
    get_spectra = function(analyses = NULL,
                           targets = NULL,
                           rt = NULL,
                           shift = NULL,
                           minIntensity = NULL,
                           useRawData = FALSE) {
      get_spectra(
        self$Analyses,
        analyses,
        targets,
        rt,
        shift,
        minIntensity,
        useRawData
      )
    },
    
    # MARK: get_spectra_matrix
    ## get_spectra_matrix -----
    #' @description Gets a matrix with spectra.
    get_spectra_matrix = function(analyses = NULL,
                                  targets = NULL,
                                  rt = NULL,
                                  shift = NULL,
                                  minIntensity = NULL,
                                  useRawData = FALSE) {
      get_spectra_matrix(
        self$Analyses,
        analyses,
        targets,
        rt,
        shift,
        minIntensity,
        useRawData
      )
    },
    
    # MARK: get_chromatograms_peaks
    ## get_chromatograms_peaks -----
    #' @description Gets a `data.table` with chromatographic peaks.
    get_chromatograms_peaks = function(analyses = NULL, targets = NULL, rt = NULL) {
      get_chromatograms_peaks(self$Analyses, analyses, targets, rt)
    },
    
    # MARK: add_analyses
    ## add_analyses -----
    #' @description Adds analyses to the engine from Raman files. Note that when adding new files,
    #' any existing results are removed.
    #'
    #' @param analyses A `character vector` with full file paths to "asc", "sif", "json", "wdf",
    #' "sdf", "csv" and/or "txt" raman files.
    #'
    add_analyses = function(analyses = NULL) {
      self$Analyses <- add(self$Analyses, analyses)
      invisible(self)
    },
    
    # MARK: remove_analyses
    ## remove_analyses -----
    #' @description Removes analyses.
    remove_analyses = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses <- remove(self$Analyses, analyses)
      invisible(self)
    },
    
    # MARK: add_replicate_names
    ## add_replicate_names -----
    #' @description Adds replicate names to the analysis.
    #' 
    #' @param value Character vector with the replicate names. Must have the same length as the
    #' number of analyses.
    #' 
    add_replicate_names = function(value) {
      self$Analyses$replicates <- value
      invisible(self)
    },
    
    # MARK: add_blank_names
    ## add_blank_names -----
    #' @description Adds blank names to the analysis.
    #' 
    #' @param value Character vector with the replicate names. Must have the same length as the
    #' number of analyses and must be one of replicate names.
    #' 
    add_blank_names = function(value) {
      self$Analyses$blanks <- value
      invisible(self)
    },
    
    # MARK: has_spectra
    ## has_spectra -----
    #' @description Checks if there are spectra, returning `TRUE` or `FALSE`.
    has_spectra = function() {
      self$Analyses$has_spectra
    },
    
    # MARK: plot_spectra
    ## plot_spectra -----
    #' @description Plots spectra.
    plot_spectra = function(analyses = NULL,
                            targets = NULL,
                            rt = NULL,
                            shift = NULL,
                            minIntensity = NULL,
                            useRawData = FALSE,
                            xLab = NULL,
                            yLab = NULL,
                            title = NULL,
                            colorBy = "analyses",
                            interactive = TRUE,
                            renderEngine = "webgl") {
      plot_spectra(
        self$Analyses,
        analyses,
        targets,
        rt,
        shift,
        minIntensity,
        useRawData,
        xLab,
        yLab,
        title,
        colorBy,
        interactive,
        renderEngine
      )
    },
    
    # MARK: plot_spectra_3d
    ## plot_spectra_3d -----
    #' @description Plots spectra in 3D, when a time dimension is available.
    #' 
    #' @param zLab Character (length 1). The label for the z-axis.
    #'
    plot_spectra_3d = function(analyses = NULL,
                               targets = NULL,
                               rt = NULL,
                               shift = NULL,
                               minIntensity = NULL,
                               useRawData = FALSE,
                               legendNames = TRUE,
                               colorBy = "analyses",
                               xLab = NULL,
                               yLab = NULL,
                               zLab = NULL,
                               renderEngine = "webgl") {
      plot_spectra_3d(
        self$Analyses,
        analyses,
        targets,
        rt,
        shift,
        minIntensity,
        useRawData,
        legendNames,
        colorBy,
        xLab,
        yLab,
        zLab,
        renderEngine
      )
    },
    
    # MARK: plot_spectra_baseline
    ## plot_spectra_baseline -----
    #' @description Plots the spectra baseline correction.
    plot_spectra_baseline = function(analyses = NULL,
                                     targets = NULL,
                                     rt = NULL,
                                     shift = NULL,
                                     minIntensity = NULL,
                                     xLab = NULL,
                                     yLab = NULL,
                                     title = NULL,
                                     colorBy = "analyses",
                                     interactive = TRUE,
                                     renderEngine = "webgl") {
      plot_spectra_baseline(
        self$Analyses,
        analyses,
        targets,
        rt,
        shift,
        minIntensity,
        xLab,
        yLab,
        title,
        colorBy,
        interactive,
        renderEngine
      )
    },
    
    
    
    # MARK: plot_chromatograms
    ## plot_chromatograms -----
    #' @description Plots chromatograms from each analysis/replicates by applying a cumulative sum
    #' of the spectrum signals for each time unit.
    plot_chromatograms = function(analyses = NULL,
                                  targets = NULL,
                                  rt = NULL,
                                  shift = NULL,
                                  minIntensity = NULL,
                                  useRawData = FALSE,
                                  xLab = NULL,
                                  yLab = NULL,
                                  title = NULL,
                                  colorBy = "analyses",
                                  interactive = TRUE,
                                  renderEngine = "webgl") {
      plot_chromatograms(
        self$Analyses,
        analyses,
        targets,
        rt,
        shift,
        minIntensity,
        useRawData,
        xLab,
        yLab,
        title,
        colorBy,
        interactive,
        renderEngine
      )
    },

    # MARK: plot_chromatograms_peaks
    ## plot_chromatograms_peaks -----
    #' @description Plots peaks from chromatograms.
    plot_chromatograms_peaks = function(analyses = NULL,
                                        targets = NULL,
                                        rt = NULL,
                                        title = NULL,
                                        legendNames = TRUE,
                                        colorBy = "targets",
                                        xLab = NULL,
                                        yLab = NULL,
                                        interactive = TRUE,
                                        renderEngine = "webgl") {
      plot_chromatograms_peaks(
        self$Analyses,
        analyses,
        targets,
        rt,
        title,
        legendNames,
        colorBy,
        xlim = NULL,
        ylim = NULL,
        xLab,
        yLab,
        interactive,
        renderEngine
      )
    }
  )
)
