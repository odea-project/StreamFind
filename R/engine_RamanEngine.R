# MARK: RamanEngine
#' **RamanEngine** R6 class and methods
#'
#' @description The *RamanEngine* R6 class is a framework for parsing, processing, inspecting and 
#' storing Raman spectroscopic data. Raman data can be loaded from "asc", "sif", "json", "wdf", 
#' "sdf", "csv" and "txt" files.
#'
#' @template arg-headers
#' @template arg-workflow
#' @template arg-settings-and-list
#' @template arg-results
#' @template arg-analyses
#' @template arg-raman-target
#' @template arg-ms-minIntensity
#' @template arg-settings
#' @template arg-chromatograms
#' @template arg-title
#' @template arg-legendNames
#' @template arg-colorBy
#' @template arg-labs
#' @template arg-interactive
#' @template arg-xlim-ylim
#' @template arg-cex
#' @template arg-showLegend
#' @template arg-renderEngine
#' @template arg-useRawData
#' @template arg-raman-targets
#' @template arg-raman-target
#'
#' @references
#' \insertRef{orpl01}{StreamFind}
#'
#' @export
#'
RamanEngine <- R6::R6Class("RamanEngine",
  inherit = CoreEngine,

  # MARK: active bindings
  # _ active bindings -----
  active = list(

    # MARK: raw_spectra
    # __ raw_spectra -----
    #' @field raw_spectra Named list of raw spectra `data.table` objects for each analysis.
    raw_spectra = function() {
      self$analyses$raw_spectra
    },

    # MARK: spectra
    # __ spectra -----
    #' @field spectra Named list of spectra `data.table` objects for each analysis or replicate.
    spectra = function(value) {
      if (missing(value)) {
        return(self$analyses$spectra)
      }
      self$analyses$spectra <- value
      invisible(self)
    }
  ),

  # MARK: public fields
  # _ public fields -----
  public = list(

    # MARK: initialize
    # __ initialize -----
    #' @description Creates an R6 class *RamanEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param file Character (length 1) with the full path to the `sqlite` or `rds` save file of 
    #' the engine.
    #' @param analyses A `RamanAnalyses` S7 class object or a `character vector` with full file 
    #' paths to "asc", "sif", "json", "wdf", "sdf", "csv" and/or "txt" raman files or a `data.frame`
    #' with colnames `file`, `replicate` and `blank`. The "replicate" column is used to group the 
    #' analyses and the "blank" column is used to identify the blank samples. The "file" column is 
    #' the full to the raman files.
    #'
    initialize = function(file = NULL, headers = NULL, workflow = NULL, analyses = NULL) {
      super$initialize(file, headers, workflow, analyses)
      invisible(self)
    },

    # MARK: get_analysis_names
    ## __ get_analysis_names -----
    #' @description Gets the analysis replicate names.
    get_analysis_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      names(self$analyses)[analyses]
    },

    # MARK: get_replicate_names
    ## __ get_replicate_names -----
    #' @description Gets the analysis replicate names.
    get_replicate_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$replicates[analyses]
    },

    # MARK: get_blank_names
    ## __ get_blank_names -----
    #' @description Gets the analysis blank replicate names.
    get_blank_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$blanks[analyses]
    },

    # MARK: get_files
    ## __ get_files -----
    #' @description Gets the full file paths of each analysis.
    get_files = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$files[analyses]
    },

    # MARK: get_overview
    ## __ get_overview -----
    #' @description Gets an overview data.frame of all the analyses.
    get_overview = function() {
      self$analyses$info
    },
    
    # MARK: get_spectra
    ## __ get_spectra -----
    #' @description Gets a `data.table` with spectra from analyses.
    #'
    #' @param useRawData Logical of length one. Set to `TRUE` for parsing raw spectra not spectra 
    #' results/processed.
    #'
    get_spectra = function(analyses = NULL,
                           targets = NULL,
                           rt = NULL,
                           shift = NULL,
                           minIntensity = NULL,
                           useRawData = FALSE) {
      get_spectra(self$analyses, analyses, targets, rt, shift, minIntensity, useRawData)
    },
    
    # MARK: get_spectra_matrix
    ## __ get_spectra_matrix -----
    #' @description Gets a matrix with spectra from analyses.
    get_spectra_matrix = function(analyses = NULL,
                                  targets = NULL) {
      get_spectra_matrix(self$analyses, analyses, targets)
    },
    
    # MARK: get_chromatograms_peaks
    ## __ get_chromatograms_peaks -----
    #' @description Gets chromatographic peaks from analyses with spectra coupled to LC.
    #'
    get_chromatograms_peaks = function(analyses = NULL,
                                       targets = NULL,
                                       rt = NULL) {
      get_chromatograms_peaks(self$analyses, analyses, targets, rt)
    },
    
    # MARK: add_analyses
    ## __ add_analyses -----
    #' @description Adds analyses based on asc files. Note that when adding new files, any existing 
    #' results are removed.
    #'
    #' @param analyses A `RamanAnalyses` S7 class object or a `character vector` with full file 
    #' paths to "asc", "sif", "json", "wdf", "sdf", "csv" and/or "txt" raman files.
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      self$analyses <- add(self$analyses, analyses)
      invisible(self)
    },
    
    # MARK: remove_analyses
    ## __ remove_analyses -----
    #' @description Removes analyses.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses <- remove(self$analyses, analyses)
      invisible(self)
    },
    
    # MARK: add_replicate_names
    ## __ add_replicate_names -----
    #' @description Adds replicate names to the analysis.
    #' 
    #' @param value Character vector with the replicate names. Must have the same length as the
    #' number of analyses.
    #' 
    add_replicate_names = function(value) {
      self$analyses$replicates <- value
      invisible(self)
    },
    
    # MARK: add_blank_names
    ## __ add_blank_names -----
    #' @description Adds blank names to the analysis.
    #' 
    #' @param value Character vector with the replicate names. Must have the same length as the
    #' number of analyses and
    #' must be one of replicate names.
    #' 
    add_blank_names = function(value) {
      self$analyses$blanks <- value
      invisible(self)
    },
    
    # MARK: has_spectra
    ## __ has_spectra -----
    #' @description Checks if there are spectra results, returning `TRUE` or `FALSE`.
    has_spectra = function() {
      self$analyses$has_spectra
    },
    
    # MARK: plot_spectra
    ## __ plot_spectra -----
    #' @description Plots spectra for given *RamanAnalyses*.
    #'
    #' @param useRawData Logical of length one. Set to `TRUE` for parsing raw spectra not spectra
    #' results/processed.
    #'
    #' @return A spectra plot.
    #'
    plot_spectra = function(analyses = NULL,
                            targets = NULL,
                            rt = NULL,
                            shift = NULL,
                            minIntensity = NULL,
                            useRawData = FALSE,
                            xLab = NULL,
                            yLab = NULL,
                            title = NULL,
                            showLegend = TRUE,
                            colorBy = "analyses",
                            interactive = TRUE,
                            cex = 0.6,
                            renderEngine = "webgl") {
      plot_spectra(
        self$analyses,
        analyses,
        targets,
        rt,
        shift,
        minIntensity,
        useRawData,
        xLab,
        yLab,
        title,
        showLegend,
        colorBy,
        interactive,
        cex,
        renderEngine
      )
    },
    
    # MARK: plot_spectra_3d
    ## __ plot_spectra_3d -----
    #' @description Plots spectra in 3D, when a time dimension is available.
    #' 
    #' @param zLab Character (length 1). The label for the z-axis.
    #'
    #' @return A 3D plot.
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
        self$analyses,
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
    ## __ plot_spectra_baseline -----
    #' @description Plots spectra corrected for given *RamanAnalyses*.
    #'
    #' @return A plot.
    #'
    plot_spectra_baseline = function(analyses = NULL,
                                     targets = NULL,
                                     rt = NULL,
                                     shift = NULL,
                                     minIntensity = NULL,
                                     xLab = NULL,
                                     yLab = NULL,
                                     title = NULL,
                                     showLegend = TRUE,
                                     colorBy = "analyses",
                                     interactive = TRUE,
                                     cex = 0.6,
                                     renderEngine = "webgl") {
      plot_spectra_baseline(
        self$analyses,
        analyses,
        targets,
        rt,
        shift,
        minIntensity,
        xLab,
        yLab,
        title,
        showLegend,
        colorBy,
        interactive,
        cex,
        renderEngine
      )
    },
    
    
    
    # MARK: plot_chromatograms
    ## __ plot_chromatograms -----
    #' @description Plots chromatograms from analyses with spectra coupled to LC.
    #'
    #' @param useRawData Logical of length one. Set to `TRUE` for parsing raw spectra not spectra
    #' results/processed.
    #'
    #' @return A plot.
    #'
    plot_chromatograms = function(analyses = NULL,
                                  targets = NULL,
                                  rt = NULL,
                                  shift = NULL,
                                  minIntensity = NULL,
                                  useRawData = FALSE,
                                  xLab = NULL,
                                  yLab = NULL,
                                  title = NULL,
                                  showLegend = TRUE,
                                  colorBy = "analyses",
                                  interactive = TRUE,
                                  cex = 0.6,
                                  renderEngine = "webgl") {
      plot_chromatograms(
        self$analyses,
        analyses,
        targets,
        rt,
        shift,
        minIntensity,
        useRawData,
        xLab,
        yLab,
        title,
        showLegend,
        colorBy,
        interactive,
        cex,
        renderEngine
      )
    },

    # MARK: plot_chromatograms_peaks
    ## __ plot_chromatograms_peaks -----
    #' @description Plots peaks from chromatograms from analyses.
    #'
    plot_chromatograms_peaks = function(analyses = NULL,
                                        targets = NULL,
                                        rt = NULL,
                                        title = NULL,
                                        legendNames = TRUE,
                                        colorBy = "targets",
                                        showLegend = TRUE,
                                        xLab = NULL,
                                        yLab = NULL,
                                        interactive = TRUE,
                                        cex = 0.6,
                                        renderEngine = "webgl") {
      plot_chromatograms_peaks(
        self$analyses,
        analyses,
        targets,
        rt,
        title,
        legendNames,
        colorBy,
        showLegend,
        xlim = NULL,
        ylim = NULL,
        xLab,
        yLab,
        interactive,
        cex,
        renderEngine
      )
    }
  )
)
