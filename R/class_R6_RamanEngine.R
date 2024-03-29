#' **RamanEngine** R6 class and methods
#'
#' @description
#' The *RamanEngine* R6 class is a framework for parsing, processing, inspecting and storing Raman spectroscopic data.
#'
#' @template arg-headers
#' @template arg-settings-and-list
#' @template arg-results
#' @template arg-analyses
#' @template arg-verbose
#' @template arg-runParallel
#' 
#' @template arg-raman-target
#' @template arg-ms-minIntensity
#' 
#' @template arg-settings
#' 
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

  # _ private fields -----
  private = list(

    ## .averaged -----
    .averaged = NULL

  ),
  
  # _ active bindings -----
  
  active = list( ),

  # _ public fields/methods -----
  public = list(

    ## ___ system -----
    #' @description Creates an R6 class *RamanEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param files Vector with full paths of **.asc** files from Raman analyses.
    #'
    initialize = function(files = NULL, headers = NULL, settings = NULL, analyses = NULL, results = NULL, runParallel = FALSE) {
      
      if (is.null(analyses) & !is.null(files)) {
        
        cached_analyses <- FALSE
        
        analyses <- NULL
        
        if (.caches_data()) {
          hash <- patRoon::makeHash(files)
          
          analyses <- patRoon::loadCacheData("parsed_raman_analyses", hash)
          
          if (!is.null(analyses)) {
            message("\U2139 Raman analyses loaded from cache!")
            cached_analyses <- TRUE
          }
          
        } else {
          hash <- NULL
          analyses <- NULL
        }
        
        if (is.null(analyses)) {
          message("\U2699 Parsing ", length(files),  " Raman file/s..." ,appendLF = FALSE)
          
          if (!is.logical(runParallel)) runParallel <- FALSE
          
          if (runParallel & length(files) > 1) {
            workers <- parallel::detectCores() - 1
            if (length(files) < workers) workers <- length(files)
            par_type <- "PSOCK"
            if (parallelly::supportsMulticore()) par_type <- "FORK"
            cl <- parallel::makeCluster(workers, type = par_type)
            doParallel::registerDoParallel(cl)
          } else {
            registerDoSEQ()
          }
          
          x <- NULL
          
          vars <- c("rcpp_parse_asc_file")
          
          analyses <- foreach(
            x = files,
            .packages = "StreamFind",
            .export = vars
            ) %dopar% { rcpp_parse_asc_file(x) }
          
          names(analyses) <- vapply(analyses, function(x) x$name, "")
          
          if (runParallel & length(files) > 1 & !cached_analyses) parallel::stopCluster(cl)
          
          message(" Done!")
          
          if (!cached_analyses & !is.null(hash)) {
            if (!is.null(analyses)) {
              message("\U1f5ab Parsed Raman analyses cached!")
              patRoon::saveCacheData("parsed_raman_analyses", analyses, hash)
            }
          }
        }
        
        analyses <- lapply(analyses, function(x) as.RamanAnalysis(x))
        
        if (is.null(analyses)) {
          warning("No valid files were given! MassSpecEngine object is empty. \n")
        }
      }
      
      super$initialize(headers, settings, analyses, results)
      
      private$.register(
        "created",
        "RamanEngine",
        headers$name,
        "StreamFind",
        as.character(packageVersion("StreamFind")),
        paste(c(headers$author, headers$path), collapse = ", ")
      )
    },
    
    ## ___ get -----
    
    #' @description Gets an overview data.frame of all the analyses.
    #'
    get_overview = function() {
      
      if (length(private$.analyses) > 0) {
        
        ov <- super$get_overview()
        
        ov$spectra <- vapply(private$.analyses, function(x) {
          if ("rt" %in% colnames(x$spectra)) {
            length(unique(x$spectra$rt))
            
          } else if (nrow(x$spectra) > 0) {
            1
            
          } else {
            0
          }
        }, 0)
        
        ov$traces <- vapply(private$.analyses, function(x) nrow(x$spectra), 0)
        
        ov$file <- vapply(private$.analyses, function(x) x$file, NA_character_)
        
        row.names(ov) <- seq_len(nrow(ov))
        
        ov
        
      } else {
        data.frame()
      }
    },

    #' @description Gets a `data.table` with spectra from analyses.
    #'
    get_spectra = function(analyses = NULL, rt = NULL, shift = NULL, minIntensity = 0) {

      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(data.frame())
      
      if (self$has_averaged_spectra()) {
        spec <- self$averaged_spectra
        spec <- rbindlist(spec, fill = TRUE)
        
        if ("analysis" %in% colnames(spec)) {
          spec <- spec[spec$analysis %in% analyses, ]
          
        } else if ("replicate" %in% colnames(spec)) {
          rpl <- self$get_replicate_names()
          rpl <- rpl[analyses]
          spec <- spec[spec$replicate %in% unname(rpl)]
          spec$analysis <- spec$replicate
          setcolorder(spec, c("analysis", "replicate"))
        }
        
      } else if (self$has_spectra()) {
        spec <- self$spectra
        spec <- rbindlist(spec, fill = TRUE)
        spec <- spec[spec$analysis %in% analyses]
        
      } else {
        spec <- lapply(private$.analyses[analyses], function(x) x$spectra)
        spec <- rbindlist(spec, idcol = "analysis", fill = TRUE)
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
        setorder(spec, shift, rt, analysis)
      } else {
        setorder(spec, shift, analysis)
      }
      
      spec
    },
    
    ## ___ subset -----
    
    #' @description Subsets on analyses.
    #'
    #' @return A new cloned engine with only the analyses as defined by the analyses argument.
    #'
    subset_analyses = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (!is.null(analyses)) {
        
        allNames <- self$get_analysis_names()
        
        keepAnalyses <- unname(allNames[allNames %in% analyses])
        
        if (length(keepAnalyses) > 0) {
          
          newAnalyses <- self$get_analyses(keepAnalyses)
          
          new_ms <- suppressMessages(RamanEngine$new(
            headers = self$get_headers(),
            settings = self$get_settings(),
            analyses = newAnalyses
          ))
          
          if (self$has_results("spectra")) {
            
            analyses_left <- new_ms$get_analysis_names()
            
            replicates_left <- new_ms$get_replicate_names()[analyses_left]
            
            if (!is.null(analyses_left)) {
              sel <- names(private$.results$spectra$data) %in% c(analyses_left, replicates_left)
              res <- private$.results$spectra
              res <- res$data[sel]
              suppressMessages(new_ms$add_results(res))
            }
          }
          
          message("\U2713 Subset with ", new_ms$get_number_analyses(), " analyses created!")
          
          return(new_ms)
          
        } else {
          warning("No analyses selected to subset! Not done.")
        }
        
        NULL
      }
    },
    
    ## ___ remove -----
    
    #' @description Removes analyses.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (!is.null(analyses)) {
        
        super$remove_analyses(analyses)
        
        if (self$has_results("spectra")) {
          
          analyses_left <- self$get_analysis_names()
          
          replicates_left <- self$get_replicate_names()[analyses_left]
          
          if (is.null(analyses_left)) {
            private$.remove_results("spectra")
            
          } else {
            sel <- names(private$.results$spectra$data) %in% c(analyses_left, replicates_left)
            private$.results$spectra$data <- private$.results$spectra$data[sel]
            
          }
        }
      }
      
      invisible(self)
    },
    
    ## ___ processing -----
    
    #' @description Merges spectra for given *RamanAnalyses* from the same chromatographic separation when using 
    #' LC-Raman coupling.
    #'
    #' @return Invisible.
    #' 
    merge_spectra_time_series = function(settings) {
      
      if (missing(settings)) settings <- Settings_merge_spectra_time_series_StreamFind()
      
      .dispatch_process_method("merge_spectra_time_series", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Averages spectra based on assigned analysis replicates.
    #'
    #' @return Invisible.
    #' 
    average_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_average_spectra_StreamFind()
      
      .dispatch_process_method("average_spectra", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Subtracts spectra from correspondent blank analysis replicates.
    #'
    #' @return Invisible.
    #' 
    subtract_blank_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_subtract_blank_spectra_StreamFind()
      
      .dispatch_process_method("subtract_blank_spectra", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Corrects the spectra baseline.
    #'
    #' @return Invisible.
    #' 
    correct_spectra_baseline = function(settings) {
      
      if (missing(settings)) settings <- Settings_correct_spectra_baseline_StreamFind()
      
      .dispatch_process_method("correct_spectra_baseline", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Bins spectra in units, according to a given window size.
    #'
    #' @return Invisible.
    #' 
    bin_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_bin_spectra_StreamFind()
      
      .dispatch_process_method("bin_spectra", settings, self, private) 
      
      invisible(self)
    },
    
    #' @description Subtracts each spectra by a spectra section in each analysis.
    #'
    #' @return Invisible.
    #' 
    subtract_spectra_section = function(settings) {
      
      if (missing(settings)) settings <- Settings_subtract_spectra_section_StreamFind()
      
      .dispatch_process_method("subtract_spectra_section", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Deletes a section of spectra in each analysis.
    #'
    #' @return Invisible.
    #' 
    delete_spectra_section = function(settings) {
      
      if (missing(settings)) settings <- Settings_subtract_spectra_section_StreamFind()
      
      .dispatch_process_method("delete_spectra_section", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Smooths the spectra in each analysis/replicate.
    #'
    #' @return Invisible.
    #' 
    smooth_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_smooth_spectra_StreamFind()
      
      .dispatch_process_method("smooth_spectra", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Normalizes spectra in each analysis/replicate.
    #'
    #' @return Invisible.
    #' 
    normalize_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_normalize_spectra_StreamFind()
      
      .dispatch_process_method("normalize_spectra", settings, self, private)
      
      invisible(self)
    },
    
    ## ___ plot -----
    
    #' @description Plots spectra for given *RamanAnalyses*.
    #'
    #' @param xVal Character of length one. Possible are "rt" or "shift" for using the retention time or the shift 
    #' as x axis, respectively.
    #'
    #' @return A plot.
    #' 
    plot_spectra = function(analyses = NULL,
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
      
      spectra <- self$get_spectra(analyses, rt, shift, minIntensity)
      
      if ("rt" %in% xVal) {
        spectra <- spectra[, .(shift = unique(rt), intensity = sum(intensity)), by = c("analysis", "rt")]
        if (is.null(xLab)) xLab = "Retention time / seconds"
        
      } else if ("shift" %in% xVal) {
        spectra <- spectra[, .(shift = unique(shift), intensity = mean(intensity)), by = c("analysis", "shift")]
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
      
      spectra$intensity <- spectra$intensity - min(spectra$intensity)
      
      if ("replicates" %in% colorBy) {
        spectra$replicate <- self$get_replicate_names()[spectra$analysis]
      }
      
      spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
      
      if (!interactive) {
        return(.plot_raman_spectra_static(spectra, xLab, yLab, title, cex, showLegend))
        
      } else {
        return(.plot_raman_spectra_interactive(spectra, xLab, yLab, title, colorBy))
      }
    },
    
    #' @description Plots chromatograms from analyses with spectra coupled to LC.
    #'
    #' @return A plot.
    #' 
    plot_chromatograms = function(analyses = NULL,
                                  rt = NULL,
                                  shift = NULL,
                                  minIntensity = 0,
                                  xLab = NULL,
                                  yLab = NULL,
                                  title = NULL,
                                  cex = 0.6,
                                  showLegend = TRUE,
                                  colorBy = "analyses",
                                  interactive = TRUE) {
      
      spectra <- self$get_spectra(analyses, rt, shift, minIntensity)
      
      if ("rt" %in% colnames(spectra)) {
        
        intensity <- NULL
        
        spectra[["shift"]] <- NULL
        
        spectra <- spectra[, .(intensity = sum(intensity)), by = c("analysis", "rt")]
        
        spectra <- unique(spectra)
        
        if (is.null(xLab)) xLab = "Retention time / seconds"
        
        if ("replicates" %in% colorBy) {
          spectra$replicate <- self$get_replicate_names()[spectra$analysis]
        }
        
        spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
        
        setnames(spectra, "rt", "shift")
        
        if (!interactive) {
          return(.plot_raman_spectra_static(spectra, xLab, yLab, title, cex, showLegend))
          
        } else {
          return(.plot_raman_spectra_interactive(spectra, xLab, yLab, title, colorBy))
        }

      } else {
        warning("Column rt not found in spectra data.table!")
        NULL
      }
    },
    
    ## ___ info -----
    
    ### ___ processing_function_calls -----
    
    #' @description A data.table with available data processing methods.
    #'
    processing_methods = function() {
      
      data.table(
        name = c(
          "average_spectra",
          "subtract_blank_spectra",
          "correct_spectra_baseline",
          "bin_spectra",
          "subtract_spectra_section",
          "delete_spectra_section",
          "smooth_spectra",
          "normalize_spectra"
        ),
        max = c(
          1,
          1,
          1,
          1,
          1,
          Inf,
          Inf,
          1
        )
      )
    }
  )
)
