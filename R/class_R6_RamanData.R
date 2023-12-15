#' **RamanData** R6 class and methods
#'
#' @description
#' The RamanData R6 class is a framework with methods for parsing, processing,
#' inspecting and storing RAMAN data.
#'
#' @template arg-headers
#' @template arg-ms-analyses
#' @template arg-raman-target
#' @template arg-ms-title
#' @template arg-runParallel
#' @template arg-ms-cex
#' @template arg-ms-showLegend
#' @template arg-ms-labs
#' @template arg-ms-minIntensity
#' @template arg-ms-interactive
#'
#' @export
#'
RamanData <- R6::R6Class("RamanData",

  # _ private fields -----
  private = list(

    ## .headers -----
    .headers = NULL,

    ## .settings -----
    .settings = NULL,

    ## .analyses -----
    .analyses = NULL,

    ## .averaged -----
    .averaged = NULL,

    ## ___ .utils -----

    # Checks the analyses argument as a character/integer vector to match
    # analyses names or indices from the `RamanData` object. Returns a valid
    # character vector with analysis names or `NULL` for non-matching.
    #
    .check_analyses_argument = function(analyses = NULL) {
      if (is.null(analyses)) {
        self$get_analysis_names()
      } else {
        analyses <- self$get_analysis_names(analyses)
        if (!all(analyses %in% self$get_analysis_names())) {
          warning("Defined analyses not found!")
          NULL
        } else {
          analyses
        }
      }
    },

    # Gets an entry from the analyses private field.
    #
    .get_analyses_entry = function(analyses = NULL, value = NA_character_) {
      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(NULL)
      output <- lapply(private$.analyses, function(x, value) {

        temp <- x[[value]]
        names(temp) <- rep(x$name, length(temp))
        temp

      }, value = value)

      output <- unname(output)

      output <- unlist(output, recursive = FALSE, use.names = TRUE)

      output[names(output) %in% analyses]
    }
  ),

  # _ public fields/methods -----
  public = list(

    ## ___ system -----
    #' @description
    #' Creates an R6 RamanData class object. When `headers` are not given
    #' (i.e., `NULL`), a default Headers S3 class object is generated with name
    #' as `NA_character`, path as `get_wd()` and date as `Sys.time()`.
    #' See `?Headers` for more information.
    #'
    #' @param files Full file paths of Raman analyses.
    #'
    initialize = function(files = NULL, headers = NULL, runParallel = FALSE) {

      if (!is.null(headers)) suppressMessages(self$add_headers(headers))

      if (is.null(private$.headers)) {
        private$.headers <- ProjectHeaders(
          name = NA_character_,
          path = getwd(),
          date = Sys.time()
        )
      }

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

      message("\U2713 RamanData class object created!")
    },

    #' @description
    #' Prints a summary of the `RamanData` object in the console.
    #'
    #' @return Console text.
    #'
    print = function() {
      cat(
        paste(is(self), collapse = "; "), "\n",
        "name          ", private$.headers$name, "\n",
        "author        ", private$.headers$author, "\n",
        "path          ", private$.headers$path, "\n",
        "date          ", as.character(private$.headers$date), "\n",
        sep = ""
      )

      cat("\n")

      if (length(private$.analyses) > 0) {
        overview <- self$get_overview()
        overview$file <- NULL
        cat("Analyses: \n")
        row.names(overview) <- paste0(" ", seq_len(nrow(overview)), ":")
        print(overview)

      } else {
        cat("Analyses: ", 0, "\n", sep = "")
      }
      cat("\n")
    },

    ## ___ get -----
    #' @description
    #' Gets the headers.
    #'
    #' @param value A character vector with the name/s of the header elements.
    #' When `NULL`, the entire headers list is returned.
    #'
    #' @return The headers list or the header elements as defined by `value`.
    #'
    get_headers = function(value = NULL) {
      if (is.null(value)) {
        private$.headers
      } else {
        private$.headers[value]
      }
    },

    #' @description
    #' Gets analyses.
    #'
    #' @return The list of analyses or the analyses as defined by `analyses`
    #' argument.
    #'
    get_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      private$.analyses[analyses]
    },

    #' @description
    #' Gets the number of analyses present.
    #'
    #' @return An integer value.
    #'
    get_number_analyses = function() {
      length(private$.analyses)
    },

    #' @description
    #' Gets the analysis names.
    #'
    #' @param analyses X.
    #'
    #' @return A character vector.
    #'
    get_analysis_names = function(analyses = NULL) {
      if (length(private$.analyses) > 0) {
        ana <- vapply(private$.analyses, function(x) x$name, "")
        names(ana) <- vapply(private$.analyses, function(x) x$name, "")
        if (!is.null(analyses)) {
          ana[analyses]
        } else {
          ana
        }
      } else {
        NULL
      }
    },
    
    #' @description
    #' Gets the analysis replicate names.
    #'
    #' @return A character vector.
    #'
    get_replicate_names = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "replicate")
    },
    
    #' @description
    #' Gets the analysis blank replicate names.
    #'
    #' @return A character vector.
    #'
    get_blank_names = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "blank")
    },

    #' @description
    #' Gets the full file paths of the analyses.
    #'
    #' @param analyses X.
    #'
    #' @return A character vector.
    #'
    get_files = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "file")
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

    #' @description
    #' Gets the overview data.frame with all the analysis types,
    #' names, replicates, associated blank replicates and full
    #' file paths.
    #'
    #' @return A data.frame with columns type, analysis, replicate, blank and
    #' file.
    #'
    get_overview = function() {

      if (length(private$.analyses) > 0) {
        df <- data.frame(
          "analysis" = vapply(private$.analyses, function(x) x$name, ""),
          "replicate" = vapply(private$.analyses, function(x) x$replicate, ""),
          "blank" = vapply(private$.analyses, function(x) x$blank, ""),
          "size" = vapply(private$.analyses, function(x) nrow(x$spectra), 0),
          "file" = vapply(private$.analyses, function(x) x$file, "")
        )
        row.names(df) <- seq_len(nrow(df))
        df
      } else {
        data.frame()
      }
    },

    ## ___ add -----
    #' @description
    #' Adds headers. If an argument or element "name" is given, it must
    #' be type character. If an argument or element path is given, it must be
    #' type character and exist. If an argument or element date is given, it
    #' must be class POSIXct or POSIXt. If given date is character, conversion
    #' to class POSIXct or POSIXt is attempted. See `?Headers` for more
    #' information.
    #'
    #' @template arg-headers-ellipsis
    #'
    #' @return Invisible.
    #'
    add_headers = function(...) {

      headers <- ProjectHeaders(...)

      if (is(headers, "ProjectHeaders")) {
        old_headers <- private$.headers
        if (is.null(old_headers)) old_headers <- list()

        if (length(old_headers) > 0) {
          new_headers <- old_headers[!names(old_headers) %in% names(headers)]
          new_headers[names(headers)] <- headers
        } else {
          new_headers <- headers
        }

        new_headers <- as.ProjectHeaders(new_headers)

        if (!identical(new_headers, old_headers) &
              is(new_headers, "ProjectHeaders")) {
          private$.headers <- new_headers
          message("\U2713 Added headers!")
        }

      } else {
        warning("Invalid headers content or structure! Not added.")
      }
      invisible(self)
    },
    
    #' @description
    #' Adds analyses.
    #'
    #' @param analyses A list.
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      
      if (is.list(analyses)) {
        if (all(c("name", "file") %in% names(analyses))) {
          # analyses <- as.MassSpecAnalysis(analyses)
          
          if (TRUE) {
            ana_name <- analyses$name
            analyses <- list(analyses)
            names(analyses) <- ana_name
            
          } else {
            warning("Not done, check the conformity of the analyses list!")
            analyses <- NULL
          }
          
        } else {
          # analyses <- lapply(analyses, as.MassSpecAnalysis)
          
          if (TRUE) {
            ana_names <- vapply(analyses, function(x) x$name, "")
            names(analyses) <- ana_names
            
          } else {
            warning("Not done, check the conformity of the analyses list!")
            analyses <- NULL
          }
        }
        
      } else {
        warning("Not done, check the conformity of the analyses list!")
        analyses <- NULL
      }
      
      if (!is.null(analyses)) {
        old_analyses <- self$get_analyses()
        old_names <- NULL
        
        if (length(old_analyses) > 0) {
          old_names <- vapply(old_analyses, function(x) x$name, "")
        }
        
        new_names <- c(old_names, vapply(analyses, function(x) x$name, ""))
        
        if (!any(duplicated(new_names))) {
          new_analyses <- c(old_analyses, analyses)
          names(new_analyses) <- new_names
          new_analyses <- new_analyses[order(names(new_analyses))]
          old_size <- length(private$.analyses)
          
          private$.analyses <- new_analyses
          
          # lapply(analyses, function(x) {
          #   private$.register(
          #     "added",
          #     class(x),
          #     x$name,
          #     "StreamFind",
          #     x$version,
          #     x$file
          #   )
          # })
          
          message(
            paste0(
              "\U2713 ",
              length(new_analyses) - old_size,
              " analyses added!"
            )
          )
        } else {
          warning("Duplicated analysis names not allowed! Not done.")
        }
      }
      invisible(self)
    },
    
    #' @description
    #' Adds or redefines the analysis replicate names.
    #'
    #' @param value A character vector with the analysis replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_replicate_names = function(value = NULL) {
      if (is.character(value) & length(value) == self$get_number_analyses()) {
        private$.analyses <- Map(
          function(x, y) {
            x$replicate <- y
            x
          },
          private$.analyses, value
        )
        
        message("\U2713 Replicate names added!")
        
      } else {
        warning("Not done, check the value!")
      }
      invisible(self)
    },
    
    #' @description
    #' Adds or redefines the analysis blank replicate names.
    #'
    #' @param value A character vector with the analysis blank replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_blank_names = function(value = NULL) {
      if (is.character(value) & length(value) == self$get_number_analyses()) {
        
        if (all(value %in% self$get_replicate_names())) {
          private$.analyses <- Map(
            function(x, y) {
              x$blank <- y
              x
            },
            private$.analyses, value
          )
          
          message("\U2713 Blank names added!")
          
        } else {
          warning("Not done, blank names not among replicate names!")
        }
        
      } else {
        warning("Not done, check the value!")
      }
      invisible(self)
    },
    
    ## ___ remove -----
    
    #' @description
    #' Removes analyses.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      
      if (!is.null(analyses)) {
        analyses <- private$.check_analyses_argument(analyses)
        allNames <- self$get_analysis_names()
        keepAnalyses <- unname(allNames[!(allNames %in% analyses)])
        removeAnalyses <- unname(allNames[allNames %in% analyses])
        analysesLeft <- self$get_analyses(keepAnalyses)
        
        if (length(removeAnalyses) > 0) {
          private$.analyses <- analysesLeft
          message("\U2713 Removed analyses:\n", paste(removeAnalyses, collapse = "\n"))
          
        } else {
          message("\U2717 There are no analyses to remove!")
        }
        
      } else {
        private$.analyses <- NULL
        message("\U2713 Removed all analyses!")
      }
      invisible(self)
    },
    
    ## ___ processing -----
    
    #' @description
    #' Merges spectra for given RAMAN analyses from the same chromatographic 
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
    #' Plots spectra for given RAMAN analyses.
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
                            interactive = FALSE) {
      
      spectra <- self$get_spectra(analyses, rt, shift, minIntensity)
      
      if ("rt" %in% xVal) {
        spectra <- spectra[, .(shift = unique(rt), intensity = sum(intensity)), by = c("analysis", "rt")]
        if (is.null(xLab)) xLab = "Retention time / seconds"
        
      } else if ("shift" %in% xVal) {
        spectra <- spectra[, .(shift = unique(shift), intensity = mean(intensity)), by = c("analysis", "shift")]
        if (is.null(xLab)) xLab = expression('Shift / cm' ^ -1)
      }
      
      spectra <- unique(spectra)
      
      if (is.null(yLab)) yLab = "Intensity / a.u."
      
      spectra$intensity <- spectra$intensity - min(spectra$intensity)
      
      if ("replicates" %in% colorBy) {
        spec$replicate <- self$get_replicate_names()[spec$analysis]
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
