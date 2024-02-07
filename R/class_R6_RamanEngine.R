#' **RamanEngine** R6 class and methods
#'
#' @description
#' The *RamanEngine* R6 class is a framework for parsing, processing, inspecting
#' and storing Raman spectroscopic data.
#'
#' @template arg-headers
#' @template arg-runParallel
#' @template arg-analyses
#' @template arg-raman-target
#' @template arg-title
#' @template arg-ms-cex
#' @template arg-ms-showLegend
#' @template arg-ms-labs
#' @template arg-ms-minIntensity
#' @template arg-ms-interactive
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

  # _ public fields/methods -----
  public = list(

    ## ___ system -----
    #' @description
    #' Creates an R6 class *RamanEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param files Vector with full paths of **.asc** files from Raman analyses.
    #'
    initialize = function(files = NULL, headers = NULL, runParallel = FALSE) {

      if (is.null(headers)) headers <- ProjectHeaders()
      
      if (!is.null(headers)) suppressMessages(self$add_headers(headers))

      if (!is.null(files)) {
        
        # TODO add validation for Raman asc files
        
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
          
          if (runParallel & length(files) > 1 & !cached_analyses) {
            parallel::stopCluster(cl)
          }
          
          message(" Done!")
          
          if (!cached_analyses & !is.null(hash)) {
            if (!is.null(analyses)) {
              message("\U1f5ab Parsed Raman analyses cached!")
              patRoon::saveCacheData("parsed_raman_analyses", analyses, hash)
            }
          }
        }
        
        private$.analyses <- analyses
      }

      message("\U2713 RamanEngine created!")
    },

    #' @description
    #' Gets spectra from analyses.
    #'
    #' @return A data.frame.
    #'
    get_spectra = function(analyses = NULL,
                           rt = NULL,
                           shift = NULL,
                           minIntensity = 0) {

      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(data.frame())

      spec <- lapply(private$.analyses[analyses], function(x) x$spectra)
      
      spec <- rbindlist(spec, idcol = "analysis", fill = TRUE)
      
      spec <- spec[spec$intensity >= 0, ]
      
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

      spec
    },

    ## ___ add -----
    
    ## ___ processing -----
    
    #' @description
    #' Merges spectra for given *RamanAnalyses* from the same chromatographic 
    #' separation when using LC-Raman coupling.
    #'
    #' @param preCut The number of pre Raman scans to exclude when merging.
    #'
    #' @return Invisible.
    #' 
    merge_replicates = function(preCut = 2) {
      processed <- .merge_replicate_files(self, preCut)
      if (!processed) warning("Raman files were not merged!")
      invisible(self)
    },
    
    ## ___ plot -----
    
    #' @description
    #' Plots spectra for given *RamanAnalyses*.
    #'
    #' @param colorBy A string of length 1. One of `analyses` (the default) or 
    #' `replicates`.
    #' @param xVal Character of length one. Possible are "rt" or "shift" for 
    #' using the retention time or the shift as x axis, respectively.
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
    }  
  )
)
