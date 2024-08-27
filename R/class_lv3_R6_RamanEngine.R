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
      lapply(self$analyses$analyses, function(x) x$spectra)
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
      analyses <- .check_analyses_argument(self$analyses, analyses)
      if (is.null(analyses)) return(data.frame())
      
      if (self$analyses$has_processed_spectra && !useRawData) {
        spec <- self$spectra
        
        if (self$analyses$results$Spectra$is_averaged) {
          spec <- rbindlist(spec, idcol = "replicate", fill = TRUE)
        } else {
          spec <- rbindlist(spec, idcol = "analysis", fill = TRUE)
        }
        
        if ("analysis" %in% colnames(spec)) {
          spec <- spec[spec$analysis %in% analyses, ]
          if (!"replicate" %in% colnames(spec)) spec$replicate <- self$get_replicate_names()[spec$analysis]
          
        } else if ("replicate" %in% colnames(spec)) {
          rpl <- self$get_replicate_names()
          rpl <- rpl[analyses]
          spec <- spec[spec$replicate %in% unname(rpl)]
          if (!"analysis" %in% colnames(spec)) spec$analysis <- spec$replicate
        }
        
      } else {
        spec <- self$raw_spectra[analyses]
        spec <- rbindlist(spec, idcol = "analysis", fill = TRUE)
        spec$replicate <- self$get_replicate_names()[spec$analysis] 
      }
      
      if (nrow(spec) == 0) {
        warning("No spectra found!")
        return(spec)
      }
      
      if (!is.null(rt) && length(rt) == 2 && "rt" %in% colnames(spec)) {
        rt_range <- sort(rt)
        sel <- spec$rt >= rt_range[1] & spec$rt <= rt_range[2]
        spec <- spec[sel, ]
      }

      if (!is.null(shift) && length(shift) == 2) {
        shift_range <- sort(shift)
        sel <- spec$shift >= shift_range[1] & spec$shift <= shift_range[2]
        spec <- spec[sel, ]
      }
      
      if ("rt" %in% colnames(spec)) {
        setorder(spec, analysis, rt, shift)
      } else {
        setorder(spec, analysis, shift)
      }
      
      setcolorder(spec, c("analysis", "replicate"))
      spec
    },
    
    #' @description Gets a matrix with spectra from analyses.
    #' 
    get_spectra_matrix = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      mat <- self$get_spectra(analyses)
      mat <- mat[order(mat$analysis), ]
      matrix(
        mat$intensity,
        nrow = length(unique(mat$analysis)),
        ncol = length(unique(mat$shift)),
        byrow = TRUE,
        dimnames = list(
          as.character(unique(mat$analysis)),
          as.character(round(unique(mat$shift), digits = 1))
        )
      )
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
    
    #' @description Checks if there are spectra, returning `TRUE` or `FALSE`.
    #'
    has_processed_spectra = function() {
      self$analyses$has_processed_spectra
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
      
      spectra <- self$get_spectra(analyses, rt, shift, minIntensity, useRawData)
      
      if (nrow(spectra) == 0) {
        warning("No spectra found for the defined targets!")
        return(NULL)
      }
      
      if ("rt" %in% xVal) {
        spectra <- spectra[, .(intensity = sum(intensity)), by = c("analysis", "rt")]
        if (is.null(xLab)) xLab = "Retention time / seconds"
        setnames(spectra, "rt", "shift")
        
      } else if ("shift" %in% xVal) {
        spectra <- spectra[, .(intensity = mean(intensity)), by = c("analysis", "shift")]
        if (is.null(xLab)) {
          if (interactive) {
            xLab = "Raman shift / cm<sup>-1</sup>"
          } else {
            xLab = expression("Raman shift / cm"^"-1")
          }
        }
      }
      
      spectra <- unique(spectra)
      
      if (is.null(yLab)) yLab = "Raman intensity / A.U."
      
      if ("replicates" %in% colorBy) {
        if (!"replicate" %in% colnames(spectra)) {
          spectra$replicate <- self$get_replicate_names()[spectra$analysis]
        }
      }
      
      spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
      
      setnames(spectra, "shift", "x")
      
      if (!interactive) {
        return(.plot_x_spectra_static(spectra, xLab, yLab, title, cex, showLegend))
        
      } else {
        return(.plot_x_spectra_interactive(spectra, xLab, yLab, title, colorBy))
      }
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
      
      spectra <- self$get_spectra(analyses, rt, shift, minIntensity, useRawData = FALSE)
      
      if (!("baseline" %in% colnames(spectra) && "raw" %in% colnames(spectra))) {
        warning("Baseline not found!")
        return(NULL)
      }
      
      if ("rt" %in% xVal) {
        spectra <- spectra[, .(baseline = sum(baseline), raw = sum(raw)), by = c("analysis", "rt")]
        
        if (is.null(xLab)) xLab = "Retention time / seconds"
        
        setnames(spectra, "rt", "shift")
        
      } else if ("shift" %in% xVal) {
        
        spectra <- spectra[, .(baseline = mean(baseline), raw = mean(raw)), by = c("analysis", "shift")]
        
        if (is.null(xLab)) {
          if (interactive) {
            xLab = "Raman shift / cm<sup>-1</sup>"
          } else {
            xLab = expression("Raman shift / cm"^"-1")
          }
        }
      }
      
      spectra <- unique(spectra)
      
      if (is.null(yLab)) yLab = "Raman intensity / A.U."
      
      spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
      
      setnames(spectra, "shift", "x")
      
      if (!interactive) {
        return(.plot_x_spectra_baseline_static(spectra, xLab, yLab, title, cex, showLegend))
        
      } else {
        return(.plot_x_spectra_baseline_interactive(spectra, xLab, yLab, title, colorBy))
      }
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
      
      spectra <- self$get_spectra(analyses, rt, shift, minIntensity, useRawData)
      
      if ("rt" %in% colnames(spectra)) {
        
        intensity <- NULL
        
        spectra[["shift"]] <- NULL
        
        spectra <- spectra[, .(intensity = sum(intensity)), by = c("analysis", "rt")]
        
        spectra <- unique(spectra)
        
        if (is.null(xLab)) xLab = "Retention time / seconds"
        
        spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
        
        setnames(spectra, "rt", "x")
        
        if (!interactive) {
          return(.plot_x_spectra_static(spectra, xLab, yLab, title, cex, showLegend))
          
        } else {
          return(.plot_x_spectra_interactive(spectra, xLab, yLab, title, colorBy))
        }

      } else {
        warning("Column rt not found in spectra data.table!")
        NULL
      }
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
    },
    
    ## ___ info -----
    
    ### ___ processing_function_calls -----
    
    #' @description A data.table with available data processing methods.
    #'
    processing_methods = function() {
      ps <- list()
      ps[["MergeSpectraTimeSeries"]] <- 1
      ps[["AverageSpectra"]] <- 1
      ps[["SubtractBlankSpectra"]] <- 1
      ps[["CorrectSpectraBaseline"]] <- 1
      ps[["BinSpectra"]] <- 1
      ps[["SubtractSpectraSection"]] <- 1
      ps[["DeleteSpectraSection"]] <- Inf
      ps[["SmoothSpectra"]] <- Inf
      ps[["NormalizeSpectra"]] <- Inf
      data.table(name = names(ps), max = unlist(ps))
    }
  )
)
