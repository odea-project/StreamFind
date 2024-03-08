#' **RamanEngine** R6 class and methods
#'
#' @description
#' The *RamanEngine* R6 class is a framework for parsing, processing, inspecting and storing Raman spectroscopic data.
#'
#' @template arg-headers
#' @template arg-runParallel
#' @template arg-analyses
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
    
    ## ___ processing -----
    
    #' @description Merges spectra for given *RamanAnalyses* from the same chromatographic separation when using 
    #' LC-Raman coupling.
    #'
    #' @param preCut The number of pre Raman scans to exclude when merging.
    #'
    #' @return Invisible.
    #' 
    merge_replicates = function(preCut = 2) {
      analyses <- .merge_replicate_files(self, preCut)
      analyses <- private$.validate_list_analyses(analyses, childClass = "RamanAnalysis")
      
      if (!is.null(analyses)) {
        
        if (all(vapply(analyses, function(x) is(x), NA_character_) %in% "RamanAnalysis")) {
          to_remove <- self$get_analysis_names()[self$get_replicate_names() %in% names(analyses)]
          suppressMessages(self$remove_analyses(to_remove))
          self$add_analyses(analyses)
        }
      }
      
      invisible(self)
    },
    
    #' @description Averages spectra based on assigned analysis replicates.
    #'
    #' @return Invisible.
    #' 
    average_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_average_spectra_StreamFind()
      
      settings <- private$.get_call_settings(settings, "average_spectra")
      
      if ("StreamFind" %in% settings$algorithm) {
        
        spec <- self$spectra
        spec <- rbindlist(spec, fill = TRUE)
        
        if (nrow(spec) == 0) {
          warning("Spectra not found! Not done.")
          return(invisible(self))
        }
        
        if ("analysis" %in% colnames(spec)) {
          
          rpl <- self$get_replicate_names()

          spec$replicate <- rpl[spec$analysis]
          
          spec_list <- split(spec, spec$replicate)
          
          av_list <- lapply(spec_list, function(x) {
            intensity <- NULL
            res <- copy(x)
            
            res[["analysis"]] <- NULL
            
            groupCols <- "shift"
            
            if ("rt" %in% colnames(x)) groupCols <- c("rt", groupCols)
            
            res <- res[, intensity := mean(intensity), by = groupCols]
            
            res <- unique(res)
            
            setcolorder(res, c("replicate"))
            
            res
            
            list("spectra" = x, "average" = res)
            
          })
          
          if (self$has_averaged_spectra()) {
            private$.modules$spectra$data <- av_list
            
          } else {
            self$add_modules_data(
              list("spectra" = list(
                "data" = av_list,
                "software" = "StreamFind",
                "version" = as.character(packageVersion("StreamFind"))
              ))
            )
          }
          
          message(paste0("\U2713 ", "Averaged spectra!"))
          
          if (!private$.settings_already_stored(settings)) self$add_settings(settings)
          
          if (requireNamespace(settings$software, quietly = TRUE)) {
            version <- as.character(packageVersion(settings$software))
          } else {
            version <- NA_character_
          }
          
          private$.register("processed", "spectra", settings$call, settings$software, version, settings$algorithm)
        }
      }
      
      invisible(self)
    },
    
    #' @description Subtracts spectra from correspondent blank analysis replicates.
    #'
    #' @return Invisible.
    #' 
    subtract_blank_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_subtract_blank_spectra_StreamFind()
      
      settings <- private$.get_call_settings(settings, "subtract_blank_spectra")
      
      if ("StreamFind" %in% settings$algorithm) {
        
        if (self$has_averaged_spectra()) {
          spec_list <- self$averaged_spectra
          
        } else if (self$has_spectra()) {
          spec_list <- self$spectra
          
        } else {
          warning("Spectra not found! Not done.")
          return(invisible(self))
        }
        
        blks <- self$get_blank_names()
        
        names(blks) <- self$get_replicate_names()
        
        spec_blk <- spec_list[names(spec_list) %in% blks]
        
        if (length(spec_blk) == 0) {
          warning("Blank spectra not found! Not done.")
          return(invisible(self))
        }
        
        spec_sub <- lapply(spec_list, function(x) {
          
          rp <- unique(x$replicate)
          
          if (rp %in% blks) return(data.table())
          
          blk <- spec_blk[blks[rp]]
          
          if (length(blk) > 1) {
            intensity <- NULL
            blk <- rbindlist(blk)
            blk[["analysis"]] <- NULL
            blk[["replicate"]] <- NULL
            blk <- blk[, intensity := mean(intensity), by = c("shift")][]
            blk <- blk$intensity
            
          } else {
            blk <- blk[[1]]$intensity
          }
          
          x$blank <- blk
          x$intensity <- x$intensity - blk
          
          if ("analysis" %in% colnames(x) && "replicate" %in% colnames(x)) {
            if (unique(x$analysis) == unique(x$replicate)) {
              x[["analysis"]] <- NULL
            }
          }
          
          x <- unique(x)
          
          x
        })
        
        if (self$has_averaged_spectra()) {
          
          private$.modules$spectra$data <- Map(
            function(x, y) {
              x$average <- y
              x
            },
            private$.modules$spectra$data, spec_sub
          )
          
          message(paste0("\U2713 ", "Blank spectra subtracted in averaged spectra!"))
          
          if (!private$.settings_already_stored(settings)) self$add_settings(settings)
          
          if (requireNamespace(settings$software, quietly = TRUE)) {
            version <- as.character(packageVersion(settings$software))
          } else {
            version <- NA_character_
          }
          
          private$.register("processed", "spectra", settings$call, settings$software, version, settings$algorithm)
          
        } else if (self$has_spectra()) {
          private$.modules$spectra$data <- Map(
            function(x, y) {
              x$spectra <- y
              x
            },
            private$.modules$spectra$data, spec_sub
          )
          
          message(paste0("\U2713 ", "Blank spectra subtracted in spectra!"))
          
          if (!private$.settings_already_stored(settings)) self$add_settings(settings)
          
          if (requireNamespace(settings$software, quietly = TRUE)) {
            version <- as.character(packageVersion(settings$software))
          } else {
            version <- NA_character_
          }
          
          private$.register("processed", "spectra", settings$call, settings$software, version, settings$algorithm)
        }
      }
      
      invisible(self)
    },
    
    #' @description Corrects the spectra baseline.
    #'
    #' @return Invisible.
    #' 
    correct_spectra_baseline = function(settings) {
      
      if (missing(settings)) settings <- Settings_correct_spectra_baseline_StreamFind()
      
      settings <- private$.get_call_settings(settings, "correct_spectra_baseline")
      
      if ("StreamFind" %in% settings$algorithm) {
        
        baseline_method <- settings$parameters$method
        
        baseline_args <- settings$parameters$args
        
        if (!(self$has_averaged_spectra() || self$has_spectra())) {
          warning("Spectra not found! Not done.")
          return(invisible(self))
        }
        
        private$.modules$spectra$data <- lapply(private$.modules$spectra$data, function(x, baseline_method, baseline_args) {
          
          if ("average" %in% names(x)) {
            
            if (nrow(x$average) > 0) {
              
              if ("rt" %in% colnames(x$average)) {
                temp_x <- split(x$average, x$average$rt)
                
                temp_x <- lapply(temp_x, function(z) {
                  baseline_data <- .baseline_correction(z$intensity, baseline_method, baseline_args)
                  z$baseline <- baseline_data$baseline
                  z$intensity <- baseline_data$corrected
                  z
                })
                
                x$average <- rbindlist(temp_x)
                
              } else {
                baseline_data <- .baseline_correction(x$average$intensity, baseline_method, baseline_args)
                x$average$baseline <- baseline_data$baseline
                x$average$intensity <- baseline_data$corrected
              }
            }
            
          } else (
            
            if (nrow(x$spectra) > 0) {
              
              if (nrow(x$average) > 0) {
                
                if ("rt" %in% colnames(x$spectra)) {
                  temp_x <- split(x$spectra, x$spectra$rt)
                  
                  temp_x <- lapply(temp_x, function(z) {
                    baseline_data <- .baseline_correction(z$intensity, baseline_method, baseline_args)
                    z$baseline <- baseline_data$baseline
                    z$intensity <- baseline_data$corrected
                    z
                  })
                  
                  x$spectra <- rbindlist(temp_x)
                  
                } else {
                  baseline_data <- .baseline_correction(x$spectra$intensity, baseline_method, baseline_args)
                  x$spectra$baseline <- baseline_data$baseline
                  x$spectra$intensity <- baseline_data$corrected
                }
              }
            }
          )
          
          x
        }, baseline_method = baseline_method, baseline_args = baseline_args)

        message(paste0("\U2713 ", "Spectra beseline corrected!"))
        
        if (!private$.settings_already_stored(settings)) self$add_settings(settings)
        
        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }
        
        private$.register("processed", "spectra", settings$call, settings$software, version, settings$algorithm)
      }
      
      invisible(self)
    },
    
    #' @description Bins spectra in units, according to a given window size.
    #'
    #' @return Invisible.
    #' 
    bin_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_bin_spectra_StreamFind()
      
      settings <- private$.get_call_settings(settings, "bin_spectra")
      
      if ("StreamFind" %in% settings$algorithm) {
        
        valSpectrumUnits = settings$parameters$valSpectrumUnits
        windowSpectrumUnits = settings$parameters$windowSpectrumUnits
        
        xVals = settings$parameters$xVals #c("rt", "shift")
        xWindows = settings$parameters$xWindows
        
        if (self$has_averaged_spectra()) {
          spec_list <- self$averaged_spectra
          
        } else if (self$has_spectra()) {
          spec_list <- self$spectra
          
        } else {
          warning("Spectra not found! Not done.")
          return(invisible(self))
        }
        
        cached_analyses <- FALSE
        
        if (.caches_data()) {
          hash <- patRoon::makeHash(spec_list, settings)
          spec_binned <- patRoon::loadCacheData("bin_spectra", hash)
          
          if (!is.null(spec_binned)) {
            check <- identical(names(spec_binned), names(spec_list))
            
            if (all(check)) {
              cached_analyses <- TRUE
              
            } else {
              spec_binned <- NULL
            }
          }
        } else {
          hash <- NULL
          spec_binned <- NULL
        }
        
        if (is.null(spec_binned)) {
          
          spec_binned <- lapply(spec_list, function(x) {
            
            if (nrow(x) == 0) return(data.table())
            
            if (!is.null(valSpectrumUnits) && !is.null(windowSpectrumUnits)) {
              
              unitVal <- unique(x[[valSpectrumUnits]])
              
              max_i <- length(unitVal) # maximum number of scan
              
              min_i <- 1
              
              unitSections <- seq(min_i, max_i, windowSpectrumUnits)
              
              idx <- seq_len(max_i)
              
              binKey <- rep(NA_integer_, max_i)
              
              for (i in unitSections) binKey[idx >= i] <- i
              
              names(binKey) <- as.character(unitVal)
              
              res <- data.table("intensity" = x$intensity, "bin_key" = binKey[as.character(x[[valSpectrumUnits]])])
              
              if (is.null(xVals)) {
                xVals <- colnames(x)
                xVals <- xVals[!xVals %in% c("analysis", "replicate", "intensity")]
              }
              
              for (i in xVals) res[[i]] <- x[[i]]
              
              valKeys <- xVals[!xVals %in% valSpectrumUnits]
              
              res <- res[, .(x = mean(x), intensity = mean(intensity)), by = c("bin_key", valKeys), env = list(x = valSpectrumUnits)]
              
              res <- unique(res)
              
              setcolorder(res, c(valSpectrumUnits, valKeys, "intensity"))
              
              res$bin_key <- NULL
              
              if ("replicate" %in% colnames(x)) {
                res$replicate <- unique(x$replicate)
                setcolorder(res, c("replicate"))
              }
              
              if ("analysis" %in% colnames(x)) {
                res$analysis <- unique(x$analysis)
                setcolorder(res, c("analysis"))
              }
              
              res
              
            } else {
              
              max_x <- max(x[[xVal]])
              min_x <- min(x[[xVal]])
              
              max_x2 <- max(x[[x2Val]])
              min_x2 <- min(x[[x2Val]])
              
              
              
              
              
              
              
              
              
              
            }
            
            # x_all <- seq(round(min_x, digits = 0), round(max_x , digits = 0), rt_bin_size)
            # x2_all <- seq(round(min_x2, digits = 0), round(max_x2 , digits = 0), mz_bin_size)
            # 
            # bins_number <- length(rts_all) * length(mzs_all)
            # bins_id <- rep(NA_character_, bins_number)
            # mat <- matrix(rep(1, bins_number * 2), nrow = bins_number, ncol = 2)
            # counter <- 0
            # for (i in seq_len(length(rts_all))) {
            #   for (j in seq_len(length(mzs_all))) {
            #     bins_id[counter + j] <- paste0(rts_all[i], "-", mzs_all[j])
            #     mat[counter + j, 1] <- rts_all[i]
            #     mat[counter + j, 2] <- mzs_all[j]
            #   }
            #   counter <- counter + j
            # }
            # dimnames(mat) <- list(bins_id, c("rt", "mz"))
            # as.data.frame(mat)
            
          })
          
          if (!is.null(hash)) {
            patRoon::saveCacheData("bin_spectra", spec_binned, hash)
            message("\U1f5ab Binned spectra cached!")
          }
        }
        
        if (self$has_averaged_spectra()) {
          
          private$.modules$spectra$data <- Map(
            function(x, y) {
              x$average <- y
              x
            },#
            private$.modules$spectra$data, spec_binned
          )
          
        } else {
          
          spec_list <- Map(function(x, y) list("spectra" = x, "average" = y), spec_list, spec_binned)
          
          self$add_modules_data(
            list("spectra" = list(
              "data" = spec_list,
              "software" = "StreamFind",
              "version" = as.character(packageVersion("StreamFind"))
            ))
          )
        }
        
        message(paste0("\U2713 ", "Spectra binned!"))
        
        if (!private$.settings_already_stored(settings)) self$add_settings(settings)
        
        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }
        
        private$.register("processed", "spectra", settings$call, settings$software, version, settings$algorithm)
      }
      
      invisible(self)
    },
    
    #' @description Subtracts each spectra by a spectra section in each analysis.
    #'
    #' @return Invisible.
    #' 
    subtract_spectra_section = function(settings) {
      
      if (missing(settings)) settings <- Settings_subtract_spectra_section_StreamFind()
      
      settings <- private$.get_call_settings(settings, "subtract_spectra_section")
      
      if ("StreamFind" %in% settings$algorithm) {
        
        if (self$has_averaged_spectra()) {
          spec_list <- self$averaged_spectra
          
        } else if (self$has_spectra()) {
          spec_list <- self$spectra
          
        } else {
          warning("Spectra not found! Not done.")
          return(invisible(self))
        }
        
        sectionVal = settings$parameters$sectionVal
        
        sectionWindow = settings$parameters$sectionWindow
        
        cached_analyses <- FALSE
        
        if (.caches_data()) {
          hash <- patRoon::makeHash(spec_list, settings)
          spec_cut <- patRoon::loadCacheData("subtract_spectra_section", hash)
          
          if (!is.null(spec_cut)) {
            check <- identical(names(spec_cut), names(spec_list))
            
            if (all(check)) {
              cached_analyses <- TRUE
              
            } else {
              spec_cut <- NULL
            }
          }
        } else {
          hash <- NULL
          spec_cut <- NULL
        }
        
        if (is.null(spec_cut)) {
          
          spec_cut <- lapply(spec_list, function(x) {
            
            if (nrow(x) == 0) return(data.table())
            
            res <- copy(x)
            
            if (!is.null(sectionVal) && !is.null(sectionWindow)) {
              
              if (sectionVal %in% colnames(x)) {
                
                if (length(sectionWindow) == 2 && is.numeric(sectionWindow)) {
                  
                  sectionWindow <- sort(sectionWindow)
                  
                  cutSec <- res[res[[sectionVal]] >= sectionWindow[1] & res[[sectionVal]] <= sectionWindow[2], ]
                  
                  if (nrow(cutSec) > 0) {
                    
                    cutSec <- cutSec[, .(intensity = mean(intensity)), by = "shift"]
                    
                    res <- res[res[[sectionVal]] < sectionWindow[1] | res[[sectionVal]] > sectionWindow[2], ]
                    
                    if (nrow(res) > 0) {
                      
                      for (i in unique(res$rt)) res$intensity[res$rt == i] <- res$intensity[res$rt == i] - cutSec$intensity
                      
                    }
                  }
                }
              }
            }
            
            res
          })
          
          if (!is.null(hash)) {
            patRoon::saveCacheData("subtract_spectra_section", spec_cut, hash)
            message("\U1f5ab Subtrated spectra section cached!")
          }
        }
        
        if (self$has_averaged_spectra()) {
          private$.modules$spectra$data <- Map(
            function(x, y) {
              x$average <- y
              x
            },#
            private$.modules$spectra$data, spec_cut
          )
          
        } else {
          
          spec_list <- Map(function(x, y) list("spectra" = x, "average" = y), spec_list, spec_cut)
          
          self$add_modules_data(
            list("spectra" = list(
              "data" = spec_list,
              "software" = "StreamFind",
              "version" = as.character(packageVersion("StreamFind"))
            ))
          )
        }
        
        message(paste0("\U2713 ", "Spectra section subtracted!"))
        
        if (!private$.settings_already_stored(settings)) self$add_settings(settings)
        
        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }
        
        private$.register("processed", "spectra", settings$call, settings$software, version, settings$algorithm)
      }
      
      invisible(self)
    },
    
    #' @description Deletes a section of spectra in each analysis.
    #'
    #' @return Invisible.
    #' 
    delete_spectra_section = function(settings) {
      
      if (missing(settings)) settings <- Settings_subtract_spectra_section_StreamFind()

      settings <- private$.get_call_settings(settings, "delete_spectra_section")

      if ("StreamFind" %in% settings$algorithm) {
        
        section <- settings$parameters$section
        
        if (length(section) == 0) {
          warning("Sections not found! Not done.")
          return(invisible(self))
        }
        
        if (self$has_averaged_spectra()) {
          spec_list <- self$averaged_spectra
          
        } else if (self$has_spectra()) {
          spec_list <- self$spectra
          
        } else {
          warning("Spectra not found! Not done.")
          return(invisible(self))
        }
        
        spec_del <- lapply(spec_list, function(x) {
          
          if (nrow(x) > 0) {
            
            sel <- logical()
            
            for (i in names(section)) {
              if (i %in% colnames(x)) {
                section[[i]] <- sort(section[[i]])
                
                if (length(sel) == 0) {
                  sel <- (x[[i]] >= section[[i]][1]) & (x[[i]] <= section[[i]][2])
                  
                } else {
                  sel <- sel & (x[[i]] >= section[[i]][1]) & (x[[i]] <= section[[i]][2])
                }
              }
            }
            
            if (length(sel) > 0) x <- x[!sel, ]
          }
          
          x
        })
        
        if (self$has_averaged_spectra()) {
          
          private$.modules$spectra$data <- Map(
            function(x, y) {
              x$average <- y
              x
            },#
            private$.modules$spectra$data, spec_del
          )
          
        } else {
          spec_list <- Map(function(x, y) list("spectra" = x, "average" = y), spec_list, spec_del)
          
          self$add_modules_data(
            list("spectra" = list(
              "data" = spec_list,
              "software" = "StreamFind",
              "version" = as.character(packageVersion("StreamFind"))
            ))
          )
        }
        
        message(paste0("\U2713 ", "Spectra section deleted!"))
        
        if (!private$.settings_already_stored(settings)) self$add_settings(settings)
        
        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }
        
        private$.register("processed", "spectra", settings$call, settings$software, version, settings$algorithm)
      }
      
      invisible(self)
    },
    
    #' @description Smooths the spectra in each analysis/replicate.
    #'
    #' @return Invisible.
    #' 
    smooth_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_smooth_spectra_StreamFind()
      
      settings <- private$.get_call_settings(settings, "smooth_spectra")
      
      if ("StreamFind" %in% settings$algorithm) {
        
        windowSize <- settings$parameters$windowSize
        
        if (!(self$has_averaged_spectra() || self$has_spectra())) {
          warning("Spectra not found! Not done.")
          return(invisible(self))
        }
        
        private$.modules$spectra$data <- lapply(private$.modules$spectra$data, function(x, windowSize) {
          
          if ("average" %in% names(x)) {
            
            if (nrow(x$average) > 0) {
              
              if ("rt" %in% colnames(x$average)) {
                temp_x <- split(x$average, x$average$rt)
                
                temp_x <- lapply(temp_x, function(z) {
                  z$intensity <- .moving_average(z$intensity, windowSize = windowSize)
                  z
                })
                
                x$average <- rbindlist(temp_x)
                
              } else {
                x$average$intensity <- .moving_average(x$average$intensity, windowSize = windowSize)
              }
            }
            
          } else (
            
            if (nrow(x$spectra) > 0) {
              
              if (nrow(x$average) > 0) {
                
                if ("rt" %in% colnames(x$spectra)) {
                  temp_x <- split(x$spectra, x$spectra$rt)
                  
                  temp_x <- lapply(temp_x, function(z) {
                    z$intensity <- .moving_average(z$intensity, windowSize = windowSize)
                    z
                  })
                  
                  x$spectra <- rbindlist(temp_x)
                  
                } else {
                  x$average$intensity <- .moving_average(x$average$intensity, windowSize = windowSize)
                }
              }
            }
          )
          
          x
        }, windowSize = windowSize)
        
        message(paste0("\U2713 ", "Spectra smoothed!"))
        
        if (!private$.settings_already_stored(settings)) self$add_settings(settings)
        
        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }
        
        private$.register("processed", "spectra", settings$call, settings$software, version, settings$algorithm)
      }
      
      invisible(self)
    },
    
    #' @description Normalizes spectra in each analysis/replicate.
    #'
    #' @return Invisible.
    #' 
    normalize_spectra = function(settings) {
      
      if (missing(settings)) settings <- Settings_normalize_spectra_StreamFind()
      
      settings <- private$.get_call_settings(settings, "normalize_spectra")
      
      if ("StreamFind" %in% settings$algorithm) {
        
        if (!(self$has_averaged_spectra() || self$has_spectra())) {
          warning("Spectra not found! Not done.")
          return(invisible(self))
        }
        
        private$.modules$spectra$data <- lapply(private$.modules$spectra$data, function(x) {
          
          if ("average" %in% names(x)) {
            
            if (nrow(x$average) > 0) {
              
              if ("rt" %in% colnames(x$average)) {
                temp_x <- split(x$average, x$average$rt)
                
                temp_x <- lapply(temp_x, function(z) {
                  z$intensity <- z$intensity / max(z$intensity)
                  z
                })
                
                x$average <- rbindlist(temp_x)
                
              } else {
                x$average$intensity <- x$average$intensity / max(x$average$intensity)
              }
            }
            
          } else (
            
            if (nrow(x$spectra) > 0) {
              
              if (nrow(x$average) > 0) {
                
                if ("rt" %in% colnames(x$spectra)) {
                  temp_x <- split(x$spectra, x$spectra$rt)
                  
                  temp_x <- lapply(temp_x, function(z) {
                    z$intensity <- z$intensity / max(z$intensity)
                    z
                  })
                  
                  x$spectra <- rbindlist(temp_x)
                  
                } else {
                  x$average$intensity <- x$average$intensity / max(x$average$intensity)
                }
              }
            }
          )
          
          x
        })
        
        message(paste0("\U2713 ", "Spectra normalized!"))
        
        if (!private$.settings_already_stored(settings)) self$add_settings(settings)
        
        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }
        
        private$.register("processed", "spectra", settings$call, settings$software, version, settings$algorithm)
      }
      
      invisible(self)
    },
    
    ## ___ has -----
    
    ## ___ plot -----
    
    #' @description Plots spectra for given *RamanAnalyses*.
    #'
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
