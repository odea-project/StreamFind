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

  # _ private fields -----
  private = list( ),
  
  # _ active bindings -----
  
  active = list(
    
    #' @field raw_spectra List of raw spectra `data.table` objects for each analysis.
    #'
    raw_spectra = function() {
      
      res <- lapply(self$get_analyses(), function(x) x$spectra)
      
      names(res) <- self$get_analysis_names()
      
      res <- Map(function(x, y) {
        x$analysis <- y
        x
      }, res, names(res))
      
      res
    },
    
    #' @field spectra List of spectra `data.table` objects for each analysis.
    #'
    spectra = function(value) {
      
      if (missing(value)) {
        
        if (self$has_results("spectra")) {
          res <- private$.results$spectra
          res <- lapply(res, function(x) x$spectra)
          
        } else {
          res <- lapply(self$get_analyses(), function(x) x$spectra)
          names(res) <- self$get_analysis_names()
          res <- Map(function(x, y) {
            x$analysis <- y
            x
          }, res, names(res))
        }
        
        res
        
      } else {
        
        if (self$has_results("spectra")) {
          
          if (identical(names(private$.results$spectra), names(value))) {
            private$.results$spectra <- Map(function(x, y) {
              x$spectra <- y
              x
            }, private$.results$spectra, value)
            
          } else {
            private$.results[["spectra"]] <- Map(function(x, y) {
              x <- list("spectra" = y)
              x
            }, names(value), value)
          }
          
        } else {
          private$.results[["spectra"]] <- Map(function(x, y) {
            x <- list("spectra" = y)
            x
          }, names(value), value)
        }
        
        invisible(self)
      }
    }
  ),

  # _ public fields -----
  public = list(

    #' @description Creates an R6 class *RamanEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param files Vector with full paths of **.asc** files from Raman analyses.
    #'
    initialize = function(files = NULL, headers = NULL, settings = NULL, analyses = NULL, results = NULL) {
      
      if (!is.null(analyses)) {
        
        if (is(analyses, "RamanAnalysis")) analyses <- list(analyses)
        
        if (!all(vapply(analyses, is, "RamanAnalysis"))) {
          warning("The argument analyses must be a RamanAnalysis object or a list of RamanAnalysis objects! Not done.")
          analyses <- NULL
        }
      }
      
      super$initialize(headers, settings, analyses, results)
      
      if (!is.null(files)) self$add_files(files)
      
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
    #' @param raw_spectra Logical of length one. Set to `TRUE` for parsing raw spectra not spectra results/processed.
    #'
    get_spectra = function(analyses = NULL, rt = NULL, shift = NULL, minIntensity = 0, raw_spectra = FALSE) {

      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(data.frame())
      
      if (self$has_spectra() && !raw_spectra) {
        spec <- self$spectra
        spec <- rbindlist(spec, fill = TRUE)
        
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
        spec <- lapply(private$.analyses[analyses], function(x) x$spectra)
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
    
    ## ___ add -----
    
    #' @description Adds analyses based on asc files. Note that when adding new files, any existing results are removed.
    #' 
    #' @param files Vector with full paths of **.asc** files from Raman analyses.
    #'
    #' @return Invisible.
    #'
    add_files = function(files = NULL) {
      
      if (!is.null(files)) {
        
        if (is.data.frame(files)) {
          
          if ("file" %in% colnames(files)) {
            
            if ("replicate" %in% colnames(files)) {
              replicates <- as.character(files$replicate)
            } else {
              replicates <- rep(NA_character_, nrow(files))
            }
            
            if ("blank" %in% colnames(files)) {
              blanks <- as.character(files$blank)
            } else {
              blanks <- rep(NA_character_, nrow(files))
            }
            
            files <- files$file
            
          } else {
            files <- ""
          }
          
        } else {
          replicates <- rep(NA_character_, length(files))
          blanks <- rep(NA_character_, length(files))
        }
        
        possible_ms_file_formats <- ".asc"
        
        valid_files <- vapply(files,
          FUN.VALUE = FALSE,
          function(x, possible_ms_file_formats) {
            if (!file.exists(x)) {
              return(FALSE)
            }
            if (FALSE %in% grepl(possible_ms_file_formats, x)) {
              return(FALSE)
            }
            TRUE
          }, possible_ms_file_formats = possible_ms_file_formats
        )
        
        if (!all(valid_files)) {
          warning("File/s not valid!")
          return(NULL)
        }
        
        names(replicates) <- as.character(files)
        
        names(blanks) <- as.character(files)
        
        analyses <- lapply(files, function(x) {
          
          cache <- .load_chache("parsed_raman_analyses", x)
          
          if (!is.null(cache$data)) {
            message("\U2139 Analysis loaded from cache!")
            cache$data
            
          } else {
            
            message("\U2699 Parsing ", basename(x), "...", appendLF = FALSE)
            
            ana <- rcpp_parse_asc_file(x)
            
            class_ana <- class(ana)[1]
            
            if (!class_ana %in% "RamanAnalysis") {
              message(" Not Done!")
              return(NULL)
            }
            
            message(" Done!")
            
            rpl <- replicates[x]
            
            if (is.na(rpl)) {
              rpl <- ana$name
              rpl <- sub("-[^-]+$", "", rpl)
            }
            
            ana$replicate <- rpl
            
            blk <- blanks[x]
            
            if (!is.na(blk)) ana$blank <- blk
            
            ana$blank <- blk
            
            if (!is.null(cache$hash)) {
              .save_cache("parsed_raman_analyses", ana, cache$hash)
              message("\U1f5ab Parsed file cached!")
            }
            
            ana
          }
        })
        
        names(analyses) <- vapply(analyses, function(x) x[["name"]], "")
        
        analyses <- analyses[order(names(analyses))]
        
        if (all(vapply(analyses, function(x) "RamanAnalysis" %in% is(x), FALSE))) {
          self$add_analyses(analyses)
          
        } else {
          warning("Not all added files could be converted as RamanAnalysis!")
        }
        
      } else {
        warning("Files were not added!")
      }
      
      invisible(self)
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
      
      if (missing(settings)) settings <- Settings_correct_spectra_baseline_airpls()
      
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
      
      if (missing(settings)) settings <- Settings_smooth_spectra_movingaverage()
      
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
    
    ## ___ has -----
    
    #' @description Checks if there are spectra, returning `TRUE` or `FALSE`.
    #'
    has_spectra = function() {
      
      if (self$has_results("spectra")) {
        sum(vapply(private$.results$spectra, function(x) nrow(x$spectra), 0)) > 0
        
      } else {
        all(vapply(self$get_analyses(), function(x) nrow(x$spectra) > 0, FALSE))
      }
    },
    
    ## ___ plot -----
    
    #' @description Plots spectra for given *RamanAnalyses*.
    #'
    #' @param xVal Character of length one. Possible are "rt" or "shift" for using the retention time or the shift 
    #' as x axis, respectively.
    #' @param raw_spectra Logical of length one. Set to `TRUE` for parsing raw spectra not spectra results/processed.
    #'
    #' @return A plot.
    #' 
    plot_spectra = function(analyses = NULL,
                            rt = NULL,
                            shift = NULL,
                            minIntensity = 0,
                            raw_spectra = FALSE,
                            xVal = "shift",
                            xLab = NULL,
                            yLab = NULL,
                            title = NULL,
                            cex = 0.6,
                            showLegend = TRUE,
                            colorBy = "analyses",
                            interactive = TRUE) {
      
      spectra <- self$get_spectra(analyses, rt, shift, minIntensity, raw_spectra)
      
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
      
      spectra <- self$get_spectra(analyses, rt, shift, minIntensity, raw_spectra = FALSE)
      
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
      
      analyses <- private$.check_analyses_argument(analyses)
      
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
    #' @param raw_spectra Logical of length one. Set to `TRUE` for parsing raw spectra not spectra results/processed.
    #'
    #' @return A plot.
    #' 
    plot_chromatograms = function(analyses = NULL,
                                  rt = NULL,
                                  shift = NULL,
                                  minIntensity = 0,
                                  raw_spectra = FALSE,
                                  xLab = NULL,
                                  yLab = NULL,
                                  title = NULL,
                                  cex = 0.6,
                                  showLegend = TRUE,
                                  colorBy = "analyses",
                                  interactive = TRUE) {
      
      spectra <- self$get_spectra(analyses, rt, shift, minIntensity, raw_spectra)
      
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
      
      analyses <- private$.check_analyses_argument(analyses)
      
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
      
      data.table(
        name = c(
          "merge_spectra_time_series",
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
          1,
          Inf,
          Inf,
          Inf
        )
      )
    }
  )
)
