#' **RamanEngine** R6 class and methods
#'
#' @description
#' The *RamanEngine* R6 class is a framework for parsing, processing, inspecting and storing Raman spectroscopic data.
#'
#' @template arg-headers
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
#' 
#'
#' @export
#'
RamanEngine <- R6::R6Class("RamanEngine",

  inherit = CoreEngine,
  
  # _ active bindings -----
  
  active = list(
    
    #' @field raw_spectra Named list of raw spectra `data.table` objects for each analysis.
    #' 
    raw_spectra = function() {
      self$analyses$raw_spectra
    },
    
    #' @field spectra Named list of spectra `data.table` objects for each analysis or replicate.
    #'
    spectra = function(value) {
      if (missing(value)) return(self$analyses$spectra)
      self$analyses$spectra <- value
      invisible(self)
    }
  ),

  # _ public fields -----
  public = list(

    #' @description Creates an R6 class *RamanEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param file Character of length one with the full path to the `sqlite` save file of the engine.
    #' @param headers A `ProjectHeaders` S7 class object.
    #' @param analyses A `RamanAnalyses` S7 class object or a `character vector` with full file paths to raman files or 
    #' a `data.frame` as described in `?RamanAnalyses`.
    #' @param workflow A `Workflow` S7 class object.
    #'
    initialize = function(file = NULL, headers = NULL, workflow = NULL, analyses = NULL) {
      super$initialize(file, headers, workflow, analyses)
      invisible(self)
    },
    
    ## ___ get -----
    
    #' @description Gets the analysis replicate names.
    #'
    get_analysis_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$names[analyses]
    },
    
    #' @description Gets the analysis replicate names.
    #'
    get_replicate_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$replicates[analyses]
    },
    
    #' @description Gets the analysis blank replicate names.
    #'
    get_blank_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$blanks[analyses]
    },
    
    #' @description Gets the full file paths of each analysis.
    #'
    get_files = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$files[analyses]
    },
    
    #' @description Gets an overview data.frame of all the analyses.
    #'
    get_overview = function() {
      self$analyses$info
    },

    #' @description Gets a `data.table` with spectra from analyses.
    #' 
    #' @param useRawData Logical of length one. Set to `TRUE` for parsing raw spectra not spectra results/processed.
    #'
    get_spectra = function(analyses = NULL, rt = NULL, shift = NULL, minIntensity = 0, useRawData = FALSE) {
      get_spectra(self$analyses, analyses, rt, shift, minIntensity, useRawData)
    },
    
    #' @description Gets a matrix with spectra from analyses.
    #' 
    get_spectra_matrix = function(analyses = NULL) {
      get_spectra_matrix(self$analyses, analyses)
    },
    
    ## ___ add/remove -----
    
    #' @description Adds analyses based on asc files. Note that when adding new files, any existing results are removed.
    #' 
    #' @param analyses A RamanAnalysis S3 class object or a list with RamanAnalysis S3 class objects as 
    #' elements (see `?RamanAnalysis` for more information) or a character vector with with full paths of **.asc** files 
    #' from Raman analyses.
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      self$analyses <- add(self$analyses, analyses)
      invisible(self)
    },
    
    #' @description Removes analyses.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses <- remove(self$analyses, analyses)
      invisible(self)
    },
    
    ## ___ has -----
    
    #' @description Checks if there are spectra results, returning `TRUE` or `FALSE`.
    #'
    has_spectra = function() {
      self$analyses$has_spectra
    },
    
    ## ___ plot -----
    
    #' @description Plots spectra for given *RamanAnalyses*.
    #'
    #' @param xVal Character of length one. Possible are "rt" or "shift" for using the retention time or the shift 
    #' as x axis, respectively.
    #' @param useRawData Logical of length one. Set to `TRUE` for parsing raw spectra not spectra results/processed.
    #'
    #' @return A plot.
    #' 
    plot_spectra = function(analyses = NULL,
                            rt = NULL,
                            shift = NULL,
                            minIntensity = 0,
                            useRawData = FALSE,
                            xVal = "shift",
                            xLab = NULL,
                            yLab = NULL,
                            title = NULL,
                            cex = 0.6,
                            showLegend = TRUE,
                            colorBy = "analyses",
                            interactive = TRUE) {
      
      plot_spectra(self$analyses, analyses, rt, shift, minIntensity, useRawData, xVal, xLab, yLab, title, cex, showLegend, colorBy, interactive)
    },
    
    #' @description Plots spectra corrected for given *RamanAnalyses*.
    #'
    #' @param xVal Character of length one. Possible are "rt" or "shift" for using the retention time or the shift 
    #' as x axis, respectively.
    #'
    #' @return A plot.
    #' 
    plot_spectra_baseline = function(analyses = NULL,
                                     rt = NULL,
                                     shift = NULL,
                                     minIntensity = 0,
                                     xVal = "shift",
                                     xLab = NULL,
                                     yLab = NULL,
                                     title = NULL,
                                     cex = 0.6,
                                     showLegend = TRUE,
                                     colorBy = "analyses",
                                     interactive = TRUE) {
      
      plot_spectra_baseline(self$analyses, analyses, rt, shift, minIntensity, xVal, xLab, yLab, title, cex, showLegend, colorBy, interactive)
    },
    
    #' @description Plots peaks from spectra from analyses.
    #'
    plot_spectra_peaks = function(analyses = NULL,
                                  legendNames = NULL,
                                  title = NULL,
                                  colorBy = "analyses",
                                  showLegend = TRUE,
                                  xlim = NULL,
                                  ylim = NULL,
                                  cex = 0.6,
                                  xLab = NULL,
                                  yLab = NULL,
                                  interactive = TRUE) {
      
      if (!self$has_spectra_peaks()) return(NULL)
      
      analyses <- .check_analyses_argument(self$analyses, analyses)
      
      if (is.null(analyses)) return(NULL)
      
      pks <- self$spectra_peaks
      
      pks <- pks[pks$analysis %in% analyses, ]
      
      if (nrow(pks) == 0) {
        message("\U2717 Peaks not found for the targets!")
        return(NULL)
      }
      
      setnames(pks, c("mass", "massmin", "massmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
      
      sp_data <- self$get_results("spectra")
      sp_data <- sp_data$spectra$data
      sp_data <- sp_data[unique(pks$analysis)]
      
      if (self$has_averaged_spectra()) {
        spec <- lapply(sp_data, function(x) x$average)
        spec <- rbindlist(spec, fill = TRUE)
        if ("rt" %in% colnames(spec)) spec$rt <- NULL
        setnames(spec, c("mass", "massmin", "massmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
        setnames(spec, c("mz", "mzmin", "mzmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
        
      } else {
        spec <- lapply(sp_data, function(x) x$raw)
        spec <- rbindlist(spec, fill = TRUE)
        if ("rt" %in% colnames(spec)) spec$rt <- NULL
        setnames(spec, c("mass", "massmin", "massmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
        setnames(spec, c("mz", "mzmin", "mzmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
      }
      
      if ("smoothed" %in% colnames(spec)) {
        spec$raw <- spec$smoothed
      }
      
      ids <- spec$id
      names(ids) <- spec$analysis
      ids <- ids[!duplicated(names(ids))]
      
      pks$id = ids[pks$analysis]
      
      if (is.null(xLab)) xLab <- "Mass / Da"
      if (is.null(yLab)) yLab <- "Intensity"
      
      if (!interactive) {
        .plot_chrom_peaks_static(spec, pks, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
      } else {
        .plot_chrom_peaks_interactive(spec, pks, legendNames, colorBy, title, showLegend, xLab, yLab)
      }
    },
    
    #' @description Plots chromatograms from analyses with spectra coupled to LC.
    #' 
    #' @param useRawData Logical of length one. Set to `TRUE` for parsing raw spectra not spectra results/processed.
    #'
    #' @return A plot.
    #' 
    plot_chromatograms = function(analyses = NULL,
                                  rt = NULL,
                                  shift = NULL,
                                  minIntensity = 0,
                                  useRawData = FALSE,
                                  xLab = NULL,
                                  yLab = NULL,
                                  title = NULL,
                                  cex = 0.6,
                                  showLegend = TRUE,
                                  colorBy = "analyses",
                                  interactive = TRUE) {
      
      plot_chromatograms(self$analyses, analyses, rt, shift, minIntensity, useRawData, xLab, yLab, title, cex, showLegend, colorBy, interactive)
    },
    
    #' @description Plots peaks from chromatograms from analyses.
    #'
    plot_chromatograms_peaks = function(analyses = NULL,
                                        chromatograms = NULL,
                                        legendNames = NULL,
                                        title = NULL,
                                        colorBy = "targets",
                                        showLegend = TRUE,
                                        xlim = NULL,
                                        ylim = NULL,
                                        cex = 0.6,
                                        xLab = NULL,
                                        yLab = NULL,
                                        interactive = TRUE) {
      
      if (!self$has_chromatograms_peaks()) return(NULL)
      
      analyses <- .check_analyses_argument(self$analyses, analyses)
      
      if (is.null(analyses)) return(NULL)
      
      pks <- self$chromatograms_peaks
      pks <- rbindlist(pks)
      pks <- pks[pks$analysis %in% analyses, ]
      
      if (is.numeric(chromatograms)) {
        which_pks <- pks$index %in% chromatograms
        pks <- pks[which_pks, ]
        
      } else if (is.character(chromatograms)) {
        which_pks <- pks$id %in% chromatograms
        pks <- pks[which_pks, ]
        
      } else if (!is.null(chromatograms)) {
        return(NULL)
      }
      
      if (nrow(pks) == 0) {
        message("\U2717 Peaks not found for the targets!")
        return(NULL)
      }
      
      chroms <- self$get_chromatograms(analyses = analyses, chromatograms = chromatograms)
      
      if ("replicates" %in% colorBy) {
        chroms$replicate <- self$get_replicate_names()[chroms$analysis]
        pks$replicate <- self$get_replicate_names()[pks$analysis]
      }
      
      if (!interactive) {
        .plot_chrom_peaks_static(chroms, pks, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
      } else {
        .plot_chrom_peaks_interactive(chroms, pks, legendNames, colorBy, title, showLegend, xLab, yLab)
      }
    }
  )
)
