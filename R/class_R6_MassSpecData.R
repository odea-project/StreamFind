#' **MassSpecData** R6 class and methods
#'
#' @description
#' The MassSpecData R6 class is a framework with methods for parsing,
#' processing, visualizing and storing mass spectrometry (MS) data.
#'
#' @template arg-ms-files
#' @template arg-runParallel
#' @template arg-headers
#' @template arg-ms-alignment
#' @template arg-ms-analyses
#' @template arg-ms-levels
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-drift
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-allTraces
#' @template arg-ms-isolationWindow
#' @template arg-ms-minIntensityMS1
#' @template arg-ms-minIntensityMS2
#' @template arg-ms-index
#' @template arg-ms-minIntensity
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-verbose
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-filtered
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-loaded
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-loadedMS1
#' @template arg-ms-loadedMS2
#' @template arg-ms-groups
#' @template arg-ms-intensities
#' @template arg-ms-average
#' @template arg-ms-groupBy
#' @template arg-ms-mzClustFeatures
#' @template arg-ms-presenceFeatures
#' @template arg-ms-minIntensityFeatures
#' @template arg-ms-loadedFeaturesMS1
#' @template arg-ms-mzClustGroups
#' @template arg-ms-presenceGroups
#' @template arg-ms-minIntensityGroups
#' @template arg-ms-loadedGroupsMS1
#' @template arg-ms-loadedFeaturesMS2
#' @template arg-ms-loadedGroupsMS2
#' @template arg-ms-settings
#' @template arg-ms-settingsFeatures
#' @template arg-ms-legendNames
#' @template arg-ms-colorBy
#' @template arg-ms-title
#' @template arg-ms-labs
#' @template arg-ms-interactive
#' @template arg-ms-save-format
#' @template arg-ms-save-name
#' @template arg-ms-save-path
#' @template arg-ms-import-file
#' @template arg-ms-xlim-ylim
#' @template arg-ms-cex
#' @template arg-ms-showLegend
#' @template arg-ms-clusters
#' @template arg-ms-onGroups
#' @template arg-ms-addSuspects
#' @template arg-ms-ppmMS2
#' @template arg-ms-minFragments
#'
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' \insertRef{pugixml01}{StreamFind}
#'
#' @export
#'
MassSpecData <- R6::R6Class("MassSpecData",

  # _ private fields -----
  private = list(
    ## ___ .headers -----
    .headers = NULL,

    ## ___ .settings -----
    .settings = NULL,

    ## ___ .history -----
    .history = NULL,

    ## ___ .analyses -----
    .analyses = NULL,

    ## ___ .alignment -----
    .alignment = NULL,

    ## ___ .modules -----
    .modules = NULL,

    ## ___ .utils -----

    # Registers changes in the history private field.
    #
    .register = function(
      action = NA_character_,
      object = NA_character_,
      name = NA_character_,
      software = NA_character_,
      version = NA_character_,
      details = NA_character_) {

      date_time <- Sys.time()
      if (is.null(private$.history)) private$.history <- list()

      private$.history[[as.character.POSIXt(date_time)]] <- data.table(
        "time" = date_time,
        "action" = action,
        "object" = object,
        "name" = name,
        "software" = software,
        "version" = version,
        "details" = details
      )

      invisible(self)
    },

    # Checks the analyses argument as a character/integer vector to match
    # analyses names or indices from the `MassSpecData` object. Returns a valid
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
    },

    # Extracts and validates ProcessingSettings for a given call.
    #
    .get_call_settings = function(settings, call) {
      
      checkmate::assert_choice(call, self$processing_function_calls())

      if (is.null(settings)) settings <- self$get_settings(call)
      
      if (is.null(settings)) return(NULL)
      
      cols_check <- c("call", "algorithm", "parameters")
      
      if (all(cols_check %in% names(settings))) settings <- list(settings)
      
      if (length(settings) > 1) {
        warning("More then one settings for ", call, " found!")
        return(NULL)
      }
      
      settings <- as.ProcessingSettings(settings)
      
      if (checkmate::test_choice(call, settings$call)) {
        settings
        
      } else {
        warning("Settings call must be ", call, "!")
        NULL
      }
    },
    
    # Checks if settings are already stored.
    #
    .settings_already_stored = function(settings) {
      any(vapply(private$.settings, function(x, settings) {
        identical(x, settings)
      }, settings = settings, FALSE))
    },

    ## ___ .filters -----

    # Tags features and feature groups with filter based on logical vector of
    # feature groups.
    #
    .tag_filtered = function(groups, tag) {

      private$.analyses <- lapply(private$.analyses,
        function(x, groups, tag) {
          sel <- (x$features$group %in% groups) & (!x$features$filtered)
          x$features$filtered[sel] <- TRUE
          x$features$filter[sel] <- tag
          x
        },
        groups = groups, tag = tag
      )
    },

    # Filters features and feature groups with minimum intensity.
    #
    .filter_minIntensity = function(value = 5000) {

      if (any(self$has_features())) {

        if (is.numeric(value) & length(value) == 1) {
          
          message("\U2699 Filtering by minIntensity...", appendLF = FALSE)

          if (self$has_groups()) {
            rpl <- unique(self$get_replicate_names())
            groups <- self$get_groups(filtered = FALSE, intensities = TRUE, average = TRUE, metadata = FALSE)
            groups_sel <- apply(groups[, rpl, with = FALSE], MARGIN = 1,function(x) max(x) <= value)
            groups <- groups$group[groups_sel]
            private$.tag_filtered(groups, "minIntensity")

          } else {
            private$.analyses <- lapply(private$.analyses, function(x) {
              sel <- (x$features$intensity <= value) & (!x$features$filtered)
              x$features$filtered[sel] <- TRUE
              x$features$filter[sel] <- "minIntensity"
              x
            })
          }

          private$.register(
            "filter_features",
            "features",
            "minIntensity",
            "StreamFind",
            as.character(packageVersion("StreamFind")),
            paste0(value, " counts")
          )
          
          message(" Done!")

        } else {
          stop("The value for minimum intensity filtering must be numeric and of length one!")
        }
      } else {
        warning("There are no features in the MassSpecData!")
      }
    },

    # Filters features and feature groups with minimum signal-to-noise ratio.
    #
    .filter_minSnRatio = function(value = 3) {

      features <- self$get_features(filtered = TRUE)

      if (any(self$has_features())) {

        if ("quality" %in% colnames(features)) {

          if (is.numeric(value) & length(value) == 1) {
            
            message("\U2699 Filtering by minSnRatio...", appendLF = FALSE)

            if (self$has_groups()) {
              groups <- self$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, metadata = TRUE)
              groups_sel <- groups$sn <= value
              groups <- groups$group
              groups <- groups[groups_sel]
              private$.tag_filtered(groups, "minSnRatio")

            } else {
              private$.analyses <- lapply(private$.analyses, function(x) {
                qlt <- x$features$quality$sn
                qlt[is.null(qlt)] <- 0
                sel <- qlt <= value & !x$features$filtered
                x$features$filtered[sel] <- TRUE
                x$features$filter[sel] <- "minSnRatio"
                x
              })
            }

            private$.register(
              "filter_features",
              "features",
              "minSnRatio",
              "StreamFind",
              as.character(packageVersion("StreamFind")),
              value
            )
            
            message(" Done!")

          } else {
            stop("The value for sn filtering must be numeric and of length one!")
          }
        } else {
          stop("The qlt_sn column was not found in the features data.table!")
        }
      } else {
        warning("There are no features in the MassSpecData!")
      }


    },

    # Filters features annotated as isotopes when groups are present the
    # isotopes are filtered if present in the all samples of a replicate.
    #
    .filter_excludeIsotopes = function(value = TRUE) {

      features <- self$get_features(filtered = TRUE)

      if (nrow(features) > 0) {

        if ("isotope" %in% colnames(features) & isTRUE(value)) {
          
          message("\U2699 Excluding isotopes...", appendLF = FALSE)

          if (self$has_groups()) {
            groups <- self$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, metadata = TRUE)
            groups_sel <- groups$iso > 0
            groups <- groups$group
            groups <- groups[groups_sel]
            private$.tag_filtered(groups, "isotope")

          } else {
            private$.analyses <- lapply(private$.analyses, function(x) {
              iso <- x$features$isotope$step
              iso[is.null(iso)] <- 0
              sel <- iso > 0 & !x$features$filtered
              x$features$filtered[sel] <- TRUE
              x$features$filter[sel] <- "isotope"
              x
            })
          }

          private$.register(
            "filter_features",
            "features",
            "excludeIsotopes",
            "StreamFind",
            as.character(packageVersion("StreamFind")),
            TRUE
          )
          
          message(" Done!")

        } else {
          warning("Isotopic step column was not found in the features data.table!")
        }
      } else {
        warning("There are no features in the MassSpecData!")
      }
    },

    # Filters feature groups with max replicate group intensity deviation.
    #
    .filter_maxGroupSd = function(value = 30) {

      if (self$has_groups()) {

        if (is.numeric(value) & length(value) == 1) {
          
          message("\U2699 Filtering by maxGroupSd...", appendLF = FALSE)
          
          rpl <- self$get_replicate_names()
          blk <- self$get_blank_names()
          rpl <- rpl[!rpl %in% blk]
          rpl <- paste0(rpl, "_sd")

          groups <- self$get_groups(
            filtered = FALSE, intensities = TRUE,
            average = TRUE, sdValues = TRUE,
            metadata = FALSE
          )

          groups_sel <- apply(groups[, rpl, with = FALSE], MARGIN = 1,
            function(x, value) {
              all(x >= value | x == 0, na.rm = TRUE)
            }, value = value
          )
          
          groups <- groups$group[groups_sel]

          private$.tag_filtered(groups, "maxGroupSd")

          private$.register(
            "filter_features",
            "features",
            "maxGroupSd",
            "StreamFind",
            as.character(packageVersion("StreamFind")),
            paste0(value, "%")
          )
          
          message(" Done!")

        } else {
          warning("The value for maxGroupSd filtering must be numeric and of length one!")
        }
      } else {
        warning("There are no feature groups in the MassSpecData!")
      }
    },

    # Filters feature groups with max replicate group abundance.
    #
    .filter_minGroupAbundance = function(value = 3) {

      if (self$has_groups()) {

        if (is.numeric(value) & length(value) == 1) {
          
          message("\U2699 Filtering by minGroupAbundance...", appendLF = FALSE)
          
          groups <- self$get_groups(filtered = FALSE, intensities = TRUE, average = FALSE, metadata = FALSE)
          
          features <- self$get_features(filtered = TRUE)
          
          # TODO add abundance to group metadata output

          groups_sel <- vapply(groups$group,
            function(x, features, rpl, value) {
              which_fts <- which(features$group %in% x)
              analyses <- features$analysis[which_fts]
              r <- rpl[analyses]
              r <- table(r)
              !any(apply(r, 1, function(x) max(x) >= value))
            },
            features = features,
            rpl = self$get_replicate_names(),
            value = value,
            FALSE
          )
          
          groups <- groups$group[groups_sel]

          private$.tag_filtered(groups, "minGroupAbundance")

          private$.register(
            "filter_features",
            "features",
            "minGroupAbundance",
            "StreamFind",
            as.character(packageVersion("StreamFind")),
            value
          )
          
          message(" Done!")

        } else {
          warning("The value for minGroupAbundance filtering must be numeric and of length one!")
        }
      } else {
        warning("There are no feature groups in the MassSpecData!")
      }
    },

    # Filters feature groups which not higher then the defined threshold of the
    # corresponding blank replicate group.
    #
    .filter_blank = function(value = 30) {

      if (self$has_groups()) {

        if (is.numeric(value) & length(value) == 1) {
          
          blk <- self$get_blank_names()
          
          rpl <- self$get_replicate_names()
          
          names(blk) <- rpl
          
          blk <- blk[!rpl %in% unique(blk)]
          
          blk <- blk[!duplicated(names(blk))]
          
          if (length(blk) != 0) {
          
            message("\U2699 Subtracting blank...", appendLF = FALSE)
  
            groups <- self$get_groups(filtered = FALSE, intensities = TRUE, average = TRUE, sdValues = FALSE, metadata = FALSE)
  
            for (r in seq_len(length(blk))) {
              rp <- names(blk)[r]
              bl <- blk[r]
              groups[, (rp) := groups[[rp]] <= (groups[[bl]] * value)][]
            }
  
            groups_sel <- apply(groups[, names(blk), with = FALSE], MARGIN = 1,
              function(x) { all(x, na.rm = TRUE) }
            )
            
            groups <- groups$group[groups_sel]
  
            private$.tag_filtered(groups, "blank")
  
            private$.register(
              "filter_features",
              "features",
              "blank",
              "StreamFind",
              as.character(packageVersion("StreamFind")),
              paste0("multiplier ", value)
            )
            
            message(" Done!")
          
          
          } else {
            warning("There are no blank analysis replicates!")
          }
        } else {
          warning("The value for blank filtering must be numeric and of length one!")
        }
      } else {
        warning("There are no feature groups in the MassSpecData!")
      }
    },

    # Filters features and feature groups within a retention time range.
    #
    .filter_rtFilter = function(value = c(0, 0)) {

      if (any(self$has_features())) {

        if (is.numeric(value) & length(value) == 2) {
          
          message("\U2699 Filtering by rtFilter...", appendLF = FALSE)
          
          value <- sort(value)

          if (self$has_groups()) {
            rpl <- self$get_replicate_names()

            groups <- self$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = TRUE)

            groups_sel <- (groups$rt <= value[2]) & (groups$rt >= value[1])
            
            groups <- groups$group[groups_sel]

            private$.tag_filtered(groups, "rtFilter")

          } else {
            private$.analyses <- lapply(private$.analyses, function(x) {
              sel <- (x$features$rt <= value[2]) & (x$features$rt >= value[1]) & (!x$features$filtered)
              x$features$filtered[sel] <- TRUE
              x$features$filter[sel] <- "rtFilter"
              x
            })
          }

          private$.register(
            "filter_features",
            "features",
            "rtFilter",
            "StreamFind",
            as.character(packageVersion("StreamFind")),
            paste(value, collapse = "; ")
          )
          
          message(" Done!")

        } else {
          warning("The value for rt filtering must be numeric and of length two!")
        }
      } else {
        warning("There are no features in the MassSpecData!")
      }
    },

    # Filters features and feature groups within a mass range.
    #
    .filter_massFilter = function(value = c(0, 0)) {

      if (any(self$has_features())) {

        if (is.numeric(value) & length(value) == 2) {

          message("\U2699 Filtering by massFilter...", appendLF = FALSE)
          
          value <- sort(value)

          if (self$has_groups()) {
            rpl <- self$get_replicate_names()

            groups <- self$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = TRUE)

            groups_sel <- (groups$mass <= value[2]) & (groups$mass >= value[1])
            
            groups <- groups$group[groups_sel]

            private$.tag_filtered(groups, "massFilter")

          } else {
            private$.analyses <- lapply(private$.analyses, function(x) {
              sel <- (x$features$mass <= value[2]) &
                (x$features$mass >= value[1]) &
                (!x$features$filtered)
              x$features$filtered[sel] <- TRUE
              x$features$filter[sel] <- "massFilter"
              x
            })
          }

          private$.register(
            "filter_features",
            "features",
            "massFilter",
            "StreamFind",
            as.character(packageVersion("StreamFind")),
            paste(value, collapse = "; ")
          )
          
          message(" Done!")

        } else {
          warning("The value for neutral mass filtering must be numeric and of length two!")
        }
      } else {
        warning("There are no features in the MassSpecData!")
      }
    },
    
    # Filters features and feature groups within a mass range.
    #
    .filter_onlySuspects = function(value = NULL) {
      
      if (any(self$has_suspects())) {
        
        if (is.logical(value) & length(value) == 1) {
          
          message("\U2699 Filtering by onlySuspects...", appendLF = FALSE)
          
          if (self$has_groups()) {
            
            sus <- self$get_suspects(onGroups = TRUE)
            
            groups <- self$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE)
            
            groups_sel <- !groups$group %in% sus$group
            
            groups <- groups$group[groups_sel]
            
            private$.tag_filtered(groups, "onlySuspects")
            
          } else {
            private$.analyses <- lapply(private$.analyses, function(x) {
              sel <- vapply(x$features$suspects, function(x) is.null(x), FALSE) & !x$features$filtered
              x$features$filtered[sel] <- TRUE
              x$features$filter[sel] <- "onlySuspects"
              x
            })
          }
          
          private$.register(
            "filter_features",
            "features",
            "onlySuspects",
            "StreamFind",
            as.character(packageVersion("StreamFind")),
            paste(value, collapse = "; ")
          )
          
          message(" Done!")
          
        } else {
          warning("The value for onlySuspects must be logical and of length one!")
        }
      } else {
        warning("There are no suspects in the MassSpecData!")
      }
    }
  ),

  # _ public fields/methods -----
  public = list(
    ## ___ create -----

    #' @description
    #' Creates an R6 MassSpecData class object. When `headers` are not given
    #' (i.e., `NULL`), a default ProjectHeaders S3 class object is generated with name
    #' as `NA_character`, path as `get_wd()` and date as `Sys.time()`.
    #' See `?ProjectHeaders` for more information.
    #'
    #' @param settings  A named list of ProcessingSettings S3 class objects or a
    #' single ProcessingSettings S3 class object. The list names should match
    #' the call name of each ProcessingSettings object. Alternatively, a named
    #' list with call name, algorithm and parameters to be transformed and added
    #' as ProcessingSettings S3 class object.
    #'
    #' @param analyses A MassSpecAnalysis S3 class object or a list with
    #' MassSpecAnalysis S3 class objects as elements (see `?MassSpecAnalysis`
    #' for more information).
    #'
    #' @param groups A data.table with feature groups from correspondence across
    #' MS analyses as obtained by the method `get_groups()`. Note that
    #' correspondence of features across MS analyses is performed with the
    #' method `group_features()`.
    #'
    #' @return A new `MassSpecData` class object.
    #'
    initialize = function(files = NULL,
                          runParallel = FALSE,
                          headers = NULL,
                          settings = NULL,
                          analyses = NULL,
                          groups = NULL,
                          alignment = NULL) {

      if (is.null(headers)) headers <- ProjectHeaders()

      if (!is.null(headers)) suppressMessages(self$add_headers(headers))

      if (!is.null(settings)) suppressMessages(self$add_settings(settings))

      if (is.null(analyses) & !is.null(files)) {
        analyses <- parse.MassSpecAnalysis(files, runParallel)
        if (is.null(analyses)) {
          warning("No valid files were given! MassSpecData object is empty. \n")
        }
      }

      if (!is.null(analyses)) suppressMessages(self$add_analyses(analyses))

      if (!is.null(groups)) suppressMessages(self$add_groups(groups))

      if (!is.null(alignment)) suppressMessages(self$add_alignment(alignment))

      private$.register(
        "created",
        "MassSpecData",
        headers$name,
        "StreamFind",
        as.character(packageVersion("StreamFind")),
        paste(c(headers$author, headers$path), collapse = ", ")
      )

      message("\U2713 MassSpecData class object created!")
    },
    
    ## ___ print -----

    #' @description
    #' Prints a summary of the `MassSpecData` object in the console.
    #'
    #' @return Console text.
    #'
    print = function() {
      cat("\n")
      cat(paste(is(self), collapse = "; "))
      cat("\n")
      cat(
        "name          ", private$.headers$name, "\n",
        "author        ", private$.headers$author, "\n",
        "path          ", private$.headers$path, "\n",
        "date          ", as.character(private$.headers$date), "\n",
        sep = ""
      )

      self$print_workflow()
      
      self$print_analyses()
    },
    
    #' @description Prints the headers.
    #'
    #' @return Console text.
    #'
    print_headers = function() {
      cat("\nHeaders: ")
      
      if (length(private$.headers) > 0) {
        cat("\n")
        names <- names(private$.headers)
        vals <- unlist(private$.headers)
        cat(paste0(" ", names, ": ", vals), sep = "\n")
        cat("\n")
        
      } else {
        cat("No headers found! \n")
      }
    },
    
    #' @description Prints the analyses present.
    #'
    #' @return Console text.
    #'
    print_analyses = function() {
      cat("\nAnalyses: ")
      
      if (length(private$.analyses) > 0) {
        cat("\n")
        overview <- self$get_overview()
        overview$file <- NULL
        
        if (all(self$has_loaded_spectra())) {
          overview$spectra <- paste(overview$spectra, "loaded", sep = " ")
        }

        row.names(overview) <- paste0(" ", seq_len(nrow(overview)), ":")
        
        print(overview)
        
      } else {
        cat("No files found! \n")
      }
    },
    
    #' @description Prints all processing methods present by order.
    #'
    #' @return Console text.
    #'
    print_workflow = function() {
      cat("\nWorkflow: ")
      
      if (self$has_settings()) {
        cat("\n")
        names_settings <- vapply(private$.settings, function(x) x$call, "")
        algorithms <- vapply(private$.settings, function(x) x$algorithm, "")
        cat(
          paste0(" ", seq_len(length(names_settings)), ": ", names_settings, " (", algorithms, ")"),
          sep = "\n"
        )
        cat("\n")
        
      } else {
        cat("No methods found! \n")
      }
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
    #' Gets the object history.
    #'
    #' @return The history list of processing steps applied.
    #'
    get_history = function() {
      
      if (is.list(private$.history)) {
        rbindlist(private$.history, fill = TRUE)
        
      } else {
        private$.history
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
    #' Gets the overview data.frame with all the analysis types,
    #' names, replicates, associated blank replicates, polarities and full
    #' file paths.
    #'
    #' @return A data.frame with columns type, analysis, replicate, blank
    #' polarity and file.
    #'
    get_overview = function() {
      if (length(private$.analyses) > 0) {

        if (self$has_groups()) {
          groups <- vapply(private$.analyses, function(x) {
            grs <- x$features$group[!x$features$filtered]
            length(grs[!is.na(grs)])
          }, 0)
          
        } else {
          groups <- 0
        }

        features <- vapply(private$.analyses, function(x) {
          nrow(x$features[!x$features$filtered])
        }, 0)

        filtered <- vapply(private$.analyses, function(x) {
          nrow(x$features[x$features$filtered])
        }, 0)

        df <- data.frame(
          "type" = vapply(private$.analyses, function(x) x$type, ""),
          "analysis" = vapply(private$.analyses, function(x) x$name, ""),
          "replicate" = vapply(private$.analyses, function(x) x$replicate, ""),
          "blank" = vapply(private$.analyses, function(x) x$blank, ""),
          "polarity" = vapply(private$.analyses, function(x) {
            paste(x$polarity, collapse = "; ")
          }, ""),
          "spectra" = vapply(private$.analyses, function(x) {
            x$spectra_number
          }, 0),
          "features" = features,
          "filtered" = filtered,
          "groups" = groups,
          "file" = vapply(private$.analyses, function(x) x$file, "")
        )

        row.names(df) <- seq_len(nrow(df))
        df
      } else {
        data.frame()
      }
    },

    #' @description
    #' Gets the analysis names.
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
    #' Gets the full file paths of each analysis.
    #'
    #' @return A character vector.
    #'
    get_files = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "file")
    },

    #' @description
    #' Gets the file format of each analysis.
    #'
    #' @return A character vector.
    #'
    get_formats = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "format")
    },

    #' @description
    #' Gets the type of each analysis.
    #'
    #' @return A character vector.
    #'
    get_types = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "type")
    },

    #' @description
    #' Gets the time stamp of the each analysis.
    #'
    #' @return A character vector.
    #'
    get_time_stamps = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "time_stamp")
    },

    #' @description
    #' Gets the number of spectra in each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_number = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "spectra_number")
    },

    #' @description
    #' Gets the spectra mode of each analysis (i.e., profile or centroided).
    #'
    #' @return A character vector.
    #'
    get_spectra_mode = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "spectra_mode")
    },

    #' @description
    #' Gets the spectra levels of each analysis.
    #'
    #' @return A list for each analysis with an integer vector.
    #'
    get_spectra_levels = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "spectra_levels")
    },

    #' @description
    #' Gets the lower \emph{m/z} value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_mz_low = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "mz_low")
    },

    #' @description
    #' Gets the higher \emph{m/z} value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_mz_high = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "mz_high")
    },

    #' @description
    #' Gets the start retention time value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_rt_start = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "rt_start")
    },

    #' @description
    #' Gets the end retention time value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_rt_end = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "rt_end")
    },

    #' @description
    #' Gets the polarity of each analysis.
    #'
    #' @return A character vector.
    #'
    get_polarities = function(analyses = NULL) {

      polarities <- private$.get_analyses_entry(analyses, "polarity")

      analyses <- unique(names(polarities))

      if (length(polarities) > length(analyses)) {
        message("\U2139 Multiple polarities detected in each analysis! Some find_features algorithms cannot handled multiple polarities properly.")
      }

      # if (length(polarities) > length(analyses)) {
      #
      #   polarities <- vapply(private$.analyses[analyses], function(x) {
      #     run <- x$run
      #     polarity <- run$polarity
      #
      #     scans_pos <- length(polarity[polarity == 1])
      #     scans_neg <- length(polarity[polarity == -1])
      #
      #     ratio <- scans_pos/scans_neg
      #
      #     if (ratio < 1.2 & ratio > 0.8) {
      #       warning("Multiple polarities detected! Currently, find_features algorithms cannot handled multiple polarities properly.", )
      #       return(NA_character_)
      #
      #     } else if (ratio > 1.2) {
      #       per_pos_pol <- round((scans_pos / nrow(run)) * 100, digits = 0)
      #       warning("Multiple polarities detected but positive polarity is present in ", per_pos_pol, "% of the spectra! Advisable to remove data from negative ionization." )
      #       return("positive")
      #
      #     } else {
      #       per_neg_pol <- round((scans_neg / nrow(run)) * 100, digits = 0)
      #       warning("Multiple polarities detected but negative polarity is present in ", per_neg_pol, "% of the spectra! Advisable to remove data from positive ionization." )
      #       return("negative")
      #     }
      #   }, "")
      #
      #   names(polarities) <- analyses
      # }
      polarities
    },

    #' @description
    #' Gets the number of chromatograms in each analysis.
    #'
    #' @return A character vector.
    #'
    get_chromatograms_number = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "chromatograms_number")
    },

    #' @description
    #' Gets the instrument information of each analysis.
    #'
    #' @return A data.table.
    #'
    get_instrument_info = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      value <- lapply(private$.analyses[analyses], function(x) {
        x$instrument
      })
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      value
    },

    #' @description
    #' Gets the software information of each analysis.
    #'
    #' @return A data.table.
    #'
    get_software_info = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      value <- lapply(private$.analyses[analyses], function(x) {
        x$software
      })
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      value
    },

    #' @description
    #' Gets the run summary data.table of each analysis.
    #'
    #' @return A data.table.
    #'
    get_run = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      value <- lapply(private$.analyses[analyses], function(x) {
        x$run
      })
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      value
    },

    #' @description
    #' Gets the total ion chromatogram (TIC) of each analysis.
    #'
    #' @return A data.table with the TIC chromatogram.
    #'
    get_tic = function(analyses = NULL, levels = c(1, 2)) {

      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(data.table())

      tic <- lapply(private$.analyses[analyses], function(x) {
        data.table(
          "polarity" = x$run$polarity,
          "level" = x$run$level,
          "rt" = x$run$rt,
          "intensity" = x$run$tic_intensity
        )
      })

      tic <- rbindlist(tic, idcol = "analysis", fill = TRUE)

      tic <- tic[tic$level %in% levels, ]

      tic
    },

    #' @description
    #' Gets the base peak chromatogram (BPC) of each analysis.
    #'
    #' @return A character vector.
    #'
    get_bpc = function(analyses = NULL, levels = c(1, 2)) {
      
      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(data.table())

      bpc <- lapply(private$.analyses[analyses], function(x) {
        data.table(
          "polarity" = x$run$polarity,
          "level" = x$run$level,
          "rt" = x$run$rt,
          "mz" = x$run$bpc_mz,
          "intensity" = x$run$bpc_intensity
        )
      })

      bpc <- rbindlist(bpc, idcol = "analysis", fill = TRUE)

      bpc <- bpc[bpc$level %in% levels, ]

      bpc
    },

    #' @description
    #' Gets metadata from each analysis.
    #'
    #' @return A data.table.
    #'
    get_metadata = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      metadata <- lapply(private$.analyses[analyses], function(x) {
        as.data.table(x$metadata)
      })
      metadata <- rbindlist(metadata, idcol = "analysis", fill = TRUE)

      metadata
    },

    #' @description
    #' Gets spectra from each analysis.
    #'
    #' @return A data.table with spectra for each analyses and
    #' targets, when defined.
    #'
    get_spectra = function(analyses = NULL,
                           levels = NULL,
                           mass = NULL,
                           mz = NULL,
                           rt = NULL,
                           drift = NULL,
                           ppm = 20,
                           sec = 60,
                           millisec = 5,
                           id = NULL,
                           allTraces = TRUE,
                           isolationWindow = 1.3,
                           minIntensityMS1 = 0,
                           minIntensityMS2 = 0,
                           runParallel = FALSE) {

      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())

      if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) {
        minIntensityMS1 <- 0
      }

      if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) {
        minIntensityMS2 <- 0
      }

      polarities <- unique(self$get_polarities(analyses))

      if (!is.null(mass)) {

        if (is.data.frame(mass)) {
          if ("mass" %in% colnames(mass)) {
            mass$mz <- mass$mass
          }
        }

        neutral_targets <- make_ms_targets(mass, rt, drift, ppm, sec, millisec, id)

        targets <- list()

        for (i in polarities) {

          if (i %in% "positive") {
            temp_tar <- copy(neutral_targets)
            temp_tar$mz <- temp_tar$mz + 1.00726
            temp_tar$mzmax <- temp_tar$mzmax + 1.00726
            temp_tar$mzmin <- temp_tar$mzmin + 1.00726
            temp_tar$polarity <- 1
            targets[[length(targets) + 1]] <- temp_tar

          } else if (i %in% "negative") {
            temp_tar <- copy(neutral_targets)
            temp_tar$mz <- temp_tar$mz - 1.00726
            temp_tar$mzmax <- temp_tar$mzmax - 1.00726
            temp_tar$mzmin <- temp_tar$mzmin - 1.00726
            temp_tar$polarity <- -1
            targets[[length(targets) + 1]] <- temp_tar
          }
        }

        if (length(targets) > 0) {
          targets <- rbindlist(targets, fill = TRUE)

        } else {
          warning("Targets could not be made from mass as polarities are not defined!")
          return(data.table())
        }

      } else {
        mz_targets <- make_ms_targets(mz, rt, drift, ppm, sec, millisec, id)

        if (!"polarity" %in% colnames(mz_targets)) {
          targets <- list()

          for (i in polarities) {

            if (i %in% "positive") {
              temp_tar <- copy(mz_targets)
              temp_tar$polarity <- 1
              targets[[length(targets) + 1]] <- temp_tar

            } else if (i %in% "negative") {
              temp_tar <- copy(mz_targets)
              temp_tar$polarity <- -1
              targets[[length(targets) + 1]] <- temp_tar
            }
          }

          targets <- rbindlist(targets, fill = TRUE)

        } else {
          targets <- mz_targets
        }
      }

      num_cols <- c("mz", "rt", "drift", "mzmin", "mzmax", "rtmin", "rtmax", "driftmin", "driftmax")

      if (all(apply(targets[, num_cols, with = FALSE], 1, function(x) sum(x, na.rm = TRUE)) != 0)) {
        
        if (TRUE %in% is.na(targets$mz)) {
          targets$mz[is.na(targets$mz)] <- 0
        }
        
        if (TRUE %in% is.na(targets$mzmax)) {
          targets$mzmax[is.na(targets$mzmax)] <- max(self$get_mz_high(analyses))
        }
        
        if (TRUE %in% is.na(targets$mzmin)) {
          targets$mzmin[is.na(targets$mzmin)] <- min(self$get_mz_low(analyses))
        }
        
        if (TRUE %in% (targets$mzmax == 0)) {
          targets$mzmax[targets$mzmax == 0] <- max(self$get_mz_high(analyses))
        }
        
        if (TRUE %in% is.na(targets$rt)) {
          targets$rt[is.na(targets$rt)] <- 0
        }
        
        if (TRUE %in% is.na(targets$rtmax)) {
          targets$rtmax[is.na(targets$rtmax)] <- max(self$get_rt_end(analyses))
        }
        
        if (TRUE %in% is.na(targets$rtmin)) {
          targets$rtmin[is.na(targets$rtmin)] <- min(self$get_rt_start(analyses))
        }

        if (TRUE %in% (targets$rtmax == 0)) {
          targets$rtmax[targets$rtmax == 0] <- max(self$get_rt_end(analyses))
        }
        
        if (TRUE %in% is.na(targets$drift)) {
          targets$drift[is.na(targets$drift)] <- 0
        }
        
        if (TRUE %in% is.na(targets$driftmax) & any(self$has_ion_mobility())) {
          targets$driftmax[is.na(targets$driftmax)] <- max(self$get_run(analyses)[["drift"]], na.rm = TRUE)
        }
        
        if (TRUE %in% is.na(targets$driftmin) & any(self$has_ion_mobility())) {
          targets$driftmin[is.na(targets$driftmin)] <- min(self$get_run(analyses)[["drift"]], na.rm = TRUE)
        }

        if (TRUE %in% (targets$driftmax == 0) & any(self$has_ion_mobility())) {
          targets$driftmax[targets$driftmax == 0] <- max(self$get_run(analyses)[["drift"]], na.rm = TRUE)
        }

      } else {
        targets <- NULL
      }

      if (!2 %in% levels) allTraces <- TRUE

      if (!is.logical(allTraces)) allTraces <- TRUE

      if (!allTraces & !is.null(targets)) {

        if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) {
          isolationWindow <- 0
        }

        # TODO make case for DIA when pre_mz is not available
        preMZr <- targets[, c("mzmin", "mzmax")]
        preMZr$mzmin <- preMZr$mzmin - (isolationWindow / 2)
        preMZr$mzmax <- preMZr$mzmax + (isolationWindow / 2)

        if (nrow(preMZr) == 1 & TRUE %in% (targets$mzmax == 0)) {
          preMZr <- NULL
        }

      } else {
        preMZr <- NULL
      }

      cached_spectra <- FALSE

      has_spectra <- self$has_loaded_spectra(analyses)
      
      if (.caches_data() & !all(has_spectra)) {
        hash <- patRoon::makeHash(
          analyses, levels, targets, allTraces,
          isolationWindow, minIntensityMS1, minIntensityMS2
        )

        spec_list <- patRoon::loadCacheData("parsed_ms_spectra", hash)

        if (!is.null(spec_list)) {
          message("\U2139 Spectra loaded from cache!")
          return(spec_list)
        }

      } else {
        hash <- NULL
        spec_list <- NULL
      }

      message("\U2699 Parsing spectra from ", length(analyses),  " MS file/s..." ,
        appendLF = FALSE
      )

      if (!is.logical(runParallel)) runParallel <- FALSE

      has_spectra <- all(self$has_loaded_spectra(analyses))

      if (runParallel & length(analyses) > 1 & !has_spectra) {
        workers <- parallel::detectCores() - 1
        if (length(analyses) < workers) workers <- length(analyses)
        par_type <- "PSOCK"
        if (parallelly::supportsMulticore()) par_type <- "FORK"
        cl <- parallel::makeCluster(workers, type = par_type)
        doParallel::registerDoParallel(cl)
      } else {
        registerDoSEQ()
      }

      i <- NULL

      if (has_spectra) {
        spec_list <- lapply(self$get_analyses(analyses),
          function(x, levels, targets, preMZr) {

            temp <- x$run

            with_im <- x$has_ion_mobility

            if (!is.null(levels)) temp <- temp[temp$level %in% levels, ]

            if (!is.null(targets)) {

              if ("polarity" %in% colnames(targets)) {
                polarities_targets <- unique(targets$polarity)

                if (length(polarities_targets) == 1) {
                  temp <- temp[temp$polarity == polarities_targets, ]
                }
              }

              if ("analysis" %in% colnames(targets)) {
                tp_tar <- targets[targets$analysis %in% x$name, ]

                if (!is.null(preMZr)) {
                  pre_tar <- preMZr[targets$analysis %in% x$name, ]

                } else {
                  pre_tar <- NULL
                }

                if (nrow(tp_tar) > 0) {
                  temp <- temp[x$spectra, on = .(scan)]
                  temp <- .trim_spectra_targets(temp, tp_tar, pre_tar, with_im)

                } else {
                  temp <- data.frame()
                }
              } else {
                temp <- temp[x$spectra, on = .(scan)]
                temp <- .trim_spectra_targets(temp, targets, preMZr, with_im)
              }
            } else {
              temp <- temp[x$spectra, on = .(scan)]
            }

            if (!with_im) temp[["drift"]] <- NULL

            temp
          },
          levels = levels,
          targets = targets,
          preMZr = preMZr
        )

      } else {

        vars <- c(
          "rcpp_parse_ms_analysis_spectra",
          ".trim_spectra_targets"
        )

        spec_list <- foreach(i = self$get_analyses(analyses), .packages = "StreamFind", .export = vars) %dopar% {
          run <- i$run
          
          if (nrow(run) > 0) {

            if (!is.null(levels)) run <- run[run$level %in% levels, ]

            if (!is.null(targets)) {

              trim <- function(v, a, b) rowSums(as.matrix(mapply(function(a, b) v >= a & v <= b, a = a, b = b))) > 0
              
              tp_tar <- targets
              pre_tar <- preMZr

              if ("polarity" %in% colnames(tp_tar)) {
                tar_polarities <- unique(tp_tar$polarity)
                ana_polarities <- unique(run$polarity)
                sel_tar <- tp_tar$polarity %in% ana_polarities
                run <- run[run$polarity %in% tar_polarities, ]
                tp_tar <- tp_tar[sel_tar, ]
                
                if (!is.null(pre_tar)) {
                  pre_tar <- pre_tar[sel_tar, ]  
                }
              }
              
              if (nrow(tp_tar) == 0) return(data.frame())

              if ("analysis" %in% colnames(tp_tar)) {
                sel_tar <- tp_tar$analysis %in% i$name
                tp_tar <- tp_tar[sel_tar, ]

                if (nrow(tp_tar) > 0) {
                  run <- run[trim(run$rt, tp_tar$rtmin, tp_tar$rtmax), ]

                  if (i$has_ion_mobility && nrow(run) > 0) {
                    run <- run[trim(run$drift, tp_tar$driftmin, tp_tar$driftmax), ]
                  }
                  
                  if (!is.null(pre_tar)) {
                    pre_tar <- pre_tar[sel_tar, ]
                    pre_tar_check <- trim(run$pre_mz, pre_tar$mzmin, pre_tar$mzmax)
                    run <- run[(pre_tar_check %in% TRUE) | is.na(pre_tar_check), ]
                  }

                } else {
                  return(data.frame())
                }

              } else {
                run <- run[trim(run$rt, tp_tar$rtmin, tp_tar$rtmax), ]

                if (i$has_ion_mobility && nrow(run) > 0) {
                  run <- run[trim(run$drift, tp_tar$driftmin, tp_tar$driftmax), ]
                }
                
                if (!is.null(pre_tar)) {
                  pre_tar_check <- trim(run$pre_mz, pre_tar$mzmin, pre_tar$mzmax)
                  run <- run[(pre_tar_check %in% TRUE) | is.na(pre_tar_check), ]
                }
              }
            }

            if (nrow(run) > 0) {

              run <- rcpp_parse_ms_analysis_spectra(i, run$index)

              if (!is.null(targets)) {
                run <- .trim_spectra_targets(run, tp_tar, pre_tar, i$has_ion_mobility)
              }

              if (!i$has_ion_mobility) run[["drift"]] <- NULL

              run

            } else {
              data.frame()
            }
          } else {
            data.frame()
          }
        }

        if (runParallel & length(analyses) > 1) parallel::stopCluster(cl)
      }

      if (length(spec_list) == length(analyses)) {

        spec_list <- lapply(spec_list, function(x, minMS1, minMS2) {
          x <- x[!(x$intensity <= minMS1 & x$level == 1), ]
          x <- x[!(x$intensity <= minMS2 & x$level == 2), ]
          x
        }, minMS1 = minIntensityMS1, minMS2 = minIntensityMS2)

        names(spec_list) <- analyses

        spec <- rbindlist(spec_list, idcol = "analysis", fill = TRUE)
        
        message(" Done!")

        if (!cached_spectra & !is.null(hash)) {
          if (!is.null(spec)) {
            message("\U1f5ab Parsed spectra cached!")
            patRoon::saveCacheData("parsed_ms_spectra", spec, hash)
          }
        }

        spec

      } else {
        warning("Defined analyses not found!")
        data.table()
      }
    },

    #' @description
    #' Gets chromatograms from each analysis.
    #'
    #' @return A data.table with chromatogram/s.
    #'
    get_chromatograms = function(analyses = NULL,
                                 index = NA_integer_,
                                 minIntensity = 0,
                                 runParallel = FALSE) {

      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())

      index <- as.integer(index)

      if (!is.integer(index)) {
        warning("Index must be an integer vector!")
        return(data.table())
      }

      has_chroms <- self$has_loaded_chromatograms(analyses)

      if (all(has_chroms)) {
        chrom_list <- lapply(self$get_analyses(analyses), function(x, index) {
          chroms <- x$chromatograms
          if (!is.na(index[1])) {
            which_chroms <- chroms$index %in% index
            chroms <- chroms[which_chroms, ]
          }
          chroms
        }, index = index)

      } else {

        message("\U2699 Parsing chromatograms from ", length(analyses),  " MS file/s..." ,
          appendLF = FALSE
        )

        if (!is.logical(runParallel)) runParallel <- FALSE

        if (runParallel & length(analyses) > 1) {
          workers <- parallel::detectCores() - 1
          if (length(files) < workers) workers <- length(files)
          par_type <- "PSOCK"
          if (parallelly::supportsMulticore()) par_type <- "FORK"
          cl <- parallel::makeCluster(workers, type = par_type)
          doParallel::registerDoParallel(cl)
        } else {
          registerDoSEQ()
        }

        i = NULL

        chrom_list <- foreach(i = self$get_analyses(analyses)) %dopar% {
          chroms <- rcpp_parse_ms_analysis_chromatograms(i, index)
          chroms
        }

        message(" Done!")
      }

      if (length(chrom_list) == length(analyses)) {
        names(chrom_list) <- analyses
        chrom_df <- rbindlist(chrom_list, idcol = "analysis", fill = TRUE)
        chrom_df <- chrom_df[chrom_df$intensity > minIntensity, ]
        chrom_df
      } else {
        warning("Defined analyses not found!")
        data.table()
      }
    },

    #' @description
    #' Gets extract ion chromatograms (EIC) from the analyses based
    #' on targets.
    #'
    #' @return A data.table.
    #'
    get_eic = function(analyses = NULL,
                       mass = NULL,
                       mz = NULL,
                       rt = NULL,
                       drift = NULL,
                       ppm = 20,
                       sec = 60,
                       millisec = 5,
                       id = NULL,
                       runParallel = FALSE) {

      eic <- self$get_spectra(
        analyses, levels = 1,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        allTraces = TRUE,
        isolationWindow = 1.3,
        minIntensityMS1 = 0,
        minIntensityMS2 = 0,
        runParallel = runParallel
      )

      if (nrow(eic) > 0) {
        eic <- as.data.table(eic)

        if (!"id" %in% colnames(eic)) eic$id <- NA_character_

        if (!"polarity" %in% colnames(eic)) eic$polarity <- 0

        eic <- eic[, `:=`(intensity = sum(intensity)),
          by = c("analysis", "polarity", "id", "rt")
        ][]

        eic <- eic[, c("analysis", "polarity", "id", "rt", "intensity"), with = FALSE]

        eic <- unique(eic)
      }

      eic
    },

    #' @description
    #' Gets MS1 data from the analyses based on targets.
    #'
    #' @return A data.frame.
    #'
    get_ms1 = function(analyses = NULL,
                       mass = NULL,
                       mz = NULL,
                       rt = NULL,
                       drift = NULL,
                       ppm = 20,
                       sec = 60,
                       millisec = 5,
                       id = NULL,
                       mzClust = 0.003,
                       presence = 0.8,
                       verbose = FALSE,
                       minIntensity = 1000,
                       runParallel = FALSE) {

      ms1 <- self$get_spectra(
        analyses, levels = 1,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        allTraces = TRUE,
        minIntensityMS1 = minIntensity,
        minIntensityMS2 = 0,
        runParallel = runParallel
      )

      if (nrow(ms1) == 0) return(ms1)

      if (!"id" %in% colnames(ms1)) {

        if (any(self$has_ion_mobility())) {
          ms1$id <- paste(
            round(min(ms1$mz), 4),
            "-",
            round(max(ms1$mz), 4),
            "/",
            round(max(ms1$rt), 0),
            "-",
            round(min(ms1$rt), 0),
            "/",
            round(max(ms1$drift), 0),
            "-",
            round(min(ms1$drift), 0),
            sep = ""
          )

        } else {
          ms1$id <- paste(
            round(min(ms1$mz), 4),
            "-",
            round(max(ms1$mz), 4),
            "/",
            round(max(ms1$rt), 0),
            "-",
            round(min(ms1$rt), 0),
            sep = ""
          )
        }
      }

      if (!is.logical(verbose)) verbose = FALSE

      if (!is.numeric(mzClust)) mzClust = 0.01

      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$id, "_", ms1$polarity)

      ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, verbose)

      ms1_df <- rbindlist(ms1_list, fill = TRUE)

      ms1_df <- ms1_df[order(ms1_df$mz), ]

      ms1_df <- ms1_df[order(ms1_df$id), ]

      ms1_df <- ms1_df[order(ms1_df$analysis), ]

      ms1_df
    },

    #' @description
    #' Gets MS2 data from the analyses based on targets.
    #'
    #' @return A data.frame.
    #'
    get_ms2 = function(analyses = NULL,
                       mass = NULL,
                       mz = NULL,
                       rt = NULL,
                       drift = NULL,
                       ppm = 20,
                       sec = 60,
                       millisec = 5,
                       id = NULL,
                       isolationWindow = 1.3,
                       mzClust = 0.005,
                       presence = 0.8,
                       verbose = FALSE,
                       minIntensity = 0,
                       runParallel = FALSE) {

      ms2 <- self$get_spectra(
        analyses, levels = 2,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        isolationWindow = isolationWindow,
        allTraces = FALSE,
        minIntensityMS1 = 0,
        minIntensityMS2 = minIntensity,
        runParallel = runParallel
      )

      if (nrow(ms2) == 0) return(ms2)

      if (!"id" %in% colnames(ms2)) {
        if (any(self$has_ion_mobility())) {
          ms2$id <- paste(
            round(min(ms2$mz), 4),
            "-",
            round(max(ms2$mz), 4),
            "/",
            round(max(ms2$rt), 0),
            "-",
            round(min(ms2$rt), 0),
            "/",
            round(max(ms2$drift), 0),
            "-",
            round(min(ms2$drift), 0),
            sep = ""
          )

        } else {
          ms2$id <- paste(
            round(min(ms2$mz), 4),
            "-",
            round(max(ms2$mz), 4),
            "/",
            round(max(ms2$rt), 0),
            "-",
            round(min(ms2$rt), 0),
            sep = ""
          )
        }
      }

      if (!is.logical(verbose)) verbose = FALSE

      if (!is.numeric(mzClust)) mzClust = 0.01

      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id, "_", ms2$polarity)

      ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, verbose)

      ms2_df <- rbindlist(ms2_list, fill = TRUE)

      ms2_df <- ms2_df[order(ms2_df$mz), ]

      ms2_df <- ms2_df[order(ms2_df$id), ]

      ms2_df <- ms2_df[order(ms2_df$analysis), ]

      ms2_df
    },

    #' @description
    #' Gets processing settings.
    #'
    #' @param call A string or a vector of strings with the name/s of the
    #' processing method/s.
    #'
    #' @return A list with ProcessingSettings S3 class object/s.
    #'
    get_settings = function(call = NULL) {
      
      if (is.null(call)) {
        private$.settings
        
      } else {
        call_names <- vapply(private$.settings, function(x) x$call, NA_character_)
        
        if (any(call %in% call_names)) {
          private$.settings[call_names %in% call]
          
        } else {
          warning("Settings for ", call, " not found!")
          NULL
        }
      }
    },

    #' @description
    #' Gets the names of all present processing settings.
    #'
    #' @return A character vector with the name of with the ProcessingSettings.
    #'
    get_settings_names = function() {
      vapply(private$.settings, function(x) x$call, NA_character_)
    },

    #' @description
    #' Gets features from analyses.
    #'
    #' @return A data.frame.
    #'
    get_features = function(analyses = NULL,
                            features = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE) {

      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(data.frame())

      fts <- lapply(private$.analyses[analyses], function(x) x$features)

      fts <- rbindlist(fts, idcol = "analysis", fill = TRUE)

      if (!filtered) fts <- fts[!fts$filtered, ]

      if (!is.null(features)) {
        target_id <- features

        if (is.character(target_id)) {

          if ("group" %in% colnames(fts)) {
            fts <- fts[fts$feature %in% target_id | fts$group %in% target_id, ]

          } else {
            fts <- fts[fts$feature %in% target_id, ]
          }

          return(fts)

        } else if (is.numeric(target_id)) {
          fts <- fts[target_id, ]

          return(fts)
        }

        if (is.data.frame(target_id)) {

          if (all(colnames(fts) %in% colnames(target_id))) return(target_id)

          if ("analysis" %in% colnames(target_id)) {
            sel <- rep(FALSE, nrow(fts))

            for (i in seq_len(nrow(target_id))) {
              sel[(fts$feature %in% target_id$feature[i] &
                fts$analysis %in% target_id$analysis[i]) |
                fts$group %in% target_id$group] <- TRUE
            }

            fts <- fts[sel, ]

            if ("name" %in% colnames(target_id)) {
              ids <- target_id$name
              names(ids) <- target_id$feature
              fts$name <- ids[fts$feature]
            }

            return(fts)

          } else if ("group" %in% colnames(target_id)) {
            sel <- rep(FALSE, nrow(fts))

            for (i in seq_len(nrow(target_id))) {
              sel[fts$feature %in% target_id$feature[i] |
                fts$group %in% target_id$group] <- TRUE
            }

            fts <- fts[sel, ]

            if ("name" %in% colnames(target_id)) {
              ids <- unique(target_id$name)
              names(ids) <- unique(target_id$group)
              fts$name <- ids[fts$group]
            }

            return(fts)
          }
        }

        return(data.frame())
      }

      if (!is.null(mass)) {

        if (is.data.frame(mass)) {
          colnames(mass) <- gsub("mass", "mz", colnames(mass))
          colnames(mass) <- gsub("neutralMass", "mz", colnames(mass))
          colnames(mass) <- gsub("min", "mzmin", colnames(mass))
          colnames(mass) <- gsub("max", "mzmax", colnames(mass))
        }

        targets <- make_ms_targets(mass, rt, drift, ppm, sec, millisec)

        for (i in seq_len(nrow(targets))) {

          if (targets$rtmax[i] == 0) targets$rtmax[i] <- max(fts$rtmax)

          if (targets$mzmax[i] == 0) targets$mzmax[i] <- max(fts$mass)

          if ("drift" %in% colnames(fts)) {
            if (targets$driftmax[i] == 0) targets$driftmax[i] <- max(fts$drift)
          }
        }

        sel <- rep(FALSE, nrow(fts))

        ids <- rep(NA_character_, nrow(fts))

        for (i in seq_len(nrow(targets))) {

          if ("drift" %in% colnames(fts)) {
            sel[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
                    between(fts$drift, targets$driftmin[i], targets$driftmax[i])] <- TRUE

            ids[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
                    between(fts$drift, targets$driftmin[i], targets$driftmax[i])] <- targets$id[i]

          } else {
            sel[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE

            ids[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- targets$id[i]
          }
        }

        fts$name <- ids

        return(fts[sel])
      }

      if (!is.null(mz)) {
        targets <- make_ms_targets(mz, rt, drift, ppm, sec, millisec)

        for (i in seq_len(nrow(targets))) {

          if (targets$rtmax[i] == 0) targets$rtmax[i] <- max(fts$rtmax)

          if (targets$mzmax[i] == 0) targets$mzmax[i] <- max(fts$mzmax)

          if ("drift" %in% colnames(fts)) {
            if (targets$driftmax[i] == 0) targets$driftmax[i] <- max(fts$drift)
          }
        }

        sel <- rep(FALSE, nrow(fts))

        ids <- rep(NA_character_, nrow(fts))

        for (i in seq_len(nrow(targets))) {

          if ("drift" %in% colnames(fts)) {
            sel[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
                  between(fts$drift, targets$driftmin[i], targets$driftmax[i])] <- TRUE

            ids[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
                  between(fts$drift, targets$driftmin[i], targets$driftmax[i])] <- targets$id[i]

          } else {
            sel[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE

            ids[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- targets$id[i]
          }
        }

        fts$name <- ids

        return(fts[sel])
      }

      fts
    },

    #' @description
    #' Gets EIC of features from analyses.
    #'
    #' @return A data.table.
    #'
    get_features_eic = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                drift = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                rtExpand = 120,
                                mzExpand = NULL,
                                filtered = FALSE,
                                loaded = TRUE,
                                runParallel = FALSE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) return(data.table())

      analysis_names <- unique(fts$analysis)

      if (loaded & any(self$has_features_eic(analysis_names))) {

        eic_list <- private$.analyses[analysis_names]

        eic_list <- lapply(eic_list, function(x, fts) {

          tf <- fts$feature

          ana <- x$name

          tf <- tf[fts$analysis %in% ana]

          x <- x$features_eic[tf]

          if (length(x) > 0) {
            x <- rbindlist(x, idcol = "feature", fill = TRUE)

            if (nrow(x) > 0) x[["analysis"]] <- ana

          } else {
            x <- data.table()
          }

          x
        }, fts = fts)

        eic <- rbindlist(eic_list, fill = TRUE)

        add_eic_filtered <- FALSE

        if (any(fts$filtered)) {
          
          if (nrow(eic) == 0) {
            add_eic_filtered <- TRUE
            
          } else if (any(!(fts$feature[fts$filtered] %in% eic$feature))) {
            add_eic_filtered <- TRUE
          }
        }

        if (add_eic_filtered) {
          fts_filtered <- fts[fts$filtered, ]
          fts_filtered <- fts_filtered[!(fts_filtered$feature %in% eic$feature), ]

          settings <- self$get_settings("load_features_eic")[[1]]
          
          if (!is.null(settings)) {
            
            parameters <- settings$parameters
            
            if ("rtExpand" %in% names(parameters)) {
              rtExpand <- parameters[["rtExpand"]]
            } else {
              if (is.null(rtExpand)) rtExpand <- 0
            }
            
            if ("mzExpand" %in% names(parameters)) {
              mzExpand <- parameters[["mzExpand"]]
            } else {
              if (is.null(mzExpand)) mzExpand <- 0
            }
            
            fts_filtered$rtmin <- fts_filtered$rtmin - rtExpand
            fts_filtered$rtmax <- fts_filtered$rtmax + rtExpand
            fts_filtered$mzmin <- fts_filtered$mzmin - mzExpand
            fts_filtered$mzmax <- fts_filtered$mzmax + mzExpand
          }

          eic_2 <- self$get_spectra(
            analyses = analyses,
            levels = 1,
            mz = fts_filtered, id = fts_filtered$feature,
            runParallel = runParallel
          )

          eic_2 <- eic_2[, c("analysis", "polarity", "id", "rt", "mz", "intensity"), with = FALSE]

          setnames(eic_2, "id", "feature")

          eic <- list(eic, eic_2)

          eic <- rbindlist(eic, fill = TRUE)
        }

        if (nrow(eic) == 0) return(data.table())

      } else {

        if (is.null(rtExpand)) rtExpand <- 0
        if (is.null(mzExpand)) mzExpand <- 0

        fts$rtmin <- fts$rtmin - rtExpand
        fts$rtmax <- fts$rtmax + rtExpand
        fts$mzmin <- fts$mzmin - mzExpand
        fts$mzmax <- fts$mzmax + mzExpand

        eic <- self$get_spectra(
          analyses = analyses,
          levels = 1,
          mz = fts,
          id = fts$feature,
          runParallel = runParallel
        )

        eic <- eic[, c("analysis", "polarity", "id", "rt", "mz", "intensity"), with = FALSE]

        setnames(eic, "id", "feature")
      }

      if ("group" %in% colnames(fts)) {
        fgs <- fts$group
        names(fgs) <- fts$feature
        eic$group <- fgs[eic$feature]
      }

      if ("name" %in% colnames(fts)) {
        tar_ids <- fts$name
        names(tar_ids) <- fts$feature
        eic$name <- tar_ids[eic$feature]
      }

      eic
    },

    #' @description
    #' Gets an averaged MS1 spectrum for features in the analyses.
    #'
    #' @return A data.table.
    #'
    get_features_ms1 = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                drift = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                rtWindow = c(-2, 2),
                                mzWindow = c(-5, 100),
                                mzClust = 0.003,
                                presence = 0.8,
                                minIntensity = 1000,
                                verbose = FALSE,
                                filtered = FALSE,
                                loadedMS1 = TRUE,
                                runParallel = FALSE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) return(data.table())

      if (!is.null(rtWindow) & length(rtWindow) == 2 & is.numeric(rtWindow)) {
        fts$rtmin <- fts$rt + rtWindow[1]
        fts$rtmax <- fts$rt + rtWindow[2]
      }

      if (!is.null(mzWindow) & length(mzWindow) == 2 & is.numeric(mzWindow)) {
        fts$mzmin <- fts$mz + mzWindow[1]
        fts$mzmax <- fts$mz + mzWindow[2]
      }

      analysis_names <- unique(fts$analysis)

      if (loadedMS1 & any(self$has_loaded_features_ms1(analysis_names))) {

        ms1_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
          temp <- fts[x, ]

          temp_ms <- temp[["ms1"]][[1]]

          if (is.null(temp_ms)) return(data.table())

          temp_ms$analysis <- temp$analysis

          temp_ms$feature <- temp$feature

          temp_ms
        }, fts = fts)

        ms1 <- rbindlist(ms1_list, fill = TRUE)

        add_filtered <- FALSE

        if (any(fts$filtered)) {
          if (nrow(ms1) == 0) {
            add_filtered <- TRUE
          } else if (any(!(fts$feature[fts$filtered] %in% ms1$feature))) {
            add_filtered <- TRUE
          }
        }

        if (add_filtered) {
          fts_filtered <- fts[fts$filtered, ]
          fts_filtered <- fts_filtered[!(fts_filtered$feature %in% ms1$feature), ]

          settings <- self$get_settings("load_features_ms1")[[1]]
          
          if (!is.null(settings)) {

            parameters <- settings$parameters
            
            if ("mzClust" %in% names(parameters)) {
              mzClust <- parameters[["mzClust"]]
            }
            
            if ("presence" %in% names(parameters)) {
              presence <- parameters[["presence"]]
            }
            
            if ("minIntensity" %in% names(parameters)) {
              minIntensity <- parameters[["minIntensity"]]
            }
          }

          ms1_2 <- self$get_ms1(
            analyses = unique(fts$analysis),
            mz = fts_filtered,
            id = fts_filtered$feature,
            mzClust = mzClust,
            presence = presence,
            minIntensity = minIntensity,
            verbose = verbose,
            runParallel = runParallel
          )

          setnames(ms1_2, "id", "feature")

          ms1 <- list(ms1, ms1_2)

          ms1 <- rbindlist(ms1, fill = TRUE)
        }

        if (nrow(ms1) == 0) return(data.table())

      } else {
        ms1 <- self$get_ms1(
          analyses = unique(fts$analysis),
          mz = fts,
          id = fts$feature,
          mzClust = mzClust,
          presence = presence,
          minIntensity = minIntensity,
          verbose = verbose,
          runParallel = runParallel
        )

        setnames(ms1, "id", "feature")
      }

      unique_fts_id <- paste0(fts$analysis, "-", fts$feature)

      unique_ms1_id <- paste0(ms1$analysis, "-", ms1$feature)

      if ("group" %in% colnames(fts)) {
        fgs <- fts$group
        names(fgs) <- unique_fts_id
        ms1$group <- fgs[unique_ms1_id]
      }

      if ("name" %in% colnames(fts)) {
        tar_ids <- fts$name
        names(tar_ids) <- unique_fts_id
        ms1$name <- tar_ids[unique_ms1_id]
      }

      copy(ms1)
    },

    #' @description
    #' Gets an averaged MS2 spectrum for features in the analyses.
    #'
    #' @return A data.table.
    #'
    get_features_ms2 = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                drift = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                isolationWindow = 1.3,
                                mzClust = 0.003,
                                presence = 0.8,
                                minIntensity = 0,
                                verbose = FALSE,
                                filtered = FALSE,
                                loadedMS2 = TRUE,
                                runParallel = FALSE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) return(data.table())

      analysis_names <- unique(fts$analysis)

      if (loadedMS2 & any(self$has_loaded_features_ms2(analysis_names))) {

        ms2_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
          temp <- fts[x, ]

          temp_ms <- temp[["ms2"]][[1]]

          if (is.null(temp_ms)) return(data.table())

          temp_ms$analysis <- temp$analysis

          temp_ms$feature <- temp$feature

          temp_ms
        }, fts = fts)

        ms2 <- rbindlist(ms2_list, fill = TRUE)

        add_filtered <- FALSE

        if (any(fts$filtered)) {
          if (nrow(ms2) == 0) {
            add_filtered <- TRUE
          } else if (any(!(fts$feature[fts$filtered] %in% ms2$feature))) {
            add_filtered <- TRUE
          }
        }

        if (add_filtered) {
          fts_filtered <- fts[fts$filtered, ]
          fts_filtered <- fts_filtered[!(fts_filtered$feature %in% ms2$feature), ]

          settings <- self$get_settings("load_features_ms2")[[1]]

          if (!is.null(settings)) {
            
            parameters <- settings$parameters
            
            if ("isolationWindow" %in% names(parameters)) {
              isolationWindow <- parameters[["isolationWindow"]]
            }
            
            if ("mzClust" %in% names(parameters)) {
              mzClust <- parameters[["mzClust"]]
            }
            
            if ("presence" %in% names(parameters)) {
              presence <- parameters[["presence"]]
            }
            
            if ("minIntensity" %in% names(parameters)) {
              minIntensity <- parameters[["minIntensity"]]
            }
          }

          ms2_2 <- self$get_ms2(
            analyses = unique(fts$analysis),
            mz = fts_filtered,
            id = fts_filtered$feature,
            isolationWindow = isolationWindow,
            mzClust = mzClust,
            presence = presence,
            minIntensity = minIntensity,
            verbose = verbose,
            runParallel = runParallel
          )

          setnames(ms2_2, "id", "feature")

          ms2 <- list(ms2, ms2_2)

          ms2 <- rbindlist(ms2, fill = TRUE)
        }

        if (nrow(ms2) == 0) return(data.table())

      } else {

        ms2 <- self$get_ms2(
          analyses = unique(fts$analysis),
          mz = fts,
          id = fts$feature,
          isolationWindow = isolationWindow,
          mzClust = mzClust,
          presence = presence,
          minIntensity = minIntensity,
          verbose = verbose,
          runParallel = runParallel
        )

        setnames(ms2, "id", "feature")
      }

      unique_fts_id <- paste0(fts$analysis, "-", fts$feature)

      unique_ms2_id <- paste0(ms2$analysis, "-", ms2$feature)

      if ("group" %in% colnames(fts)) {
        fgs <- fts$group
        names(fgs) <- unique_fts_id
        ms2$group <- fgs[unique_ms2_id]
      }

      if ("name" %in% colnames(fts)) {
        tar_ids <- fts$name
        names(tar_ids) <- unique_fts_id
        ms2$name <- tar_ids[unique_ms2_id]
      }

      copy(ms2)
    },

    #' @description
    #' Gets alignment.
    #'
    #' @return A data.frame.
    #'
    get_alignment = function() {
      private$.alignment
    },

    #' @description
    #' Gets feature groups from the analyses.
    #' 
    #' @param sdValues Logical length 1. Set to `TRUE` for returning the sd 
    #' values when averaging the intensity within analysis replicates.
    #' @param metadata Logical length 1. Set to `TRUE` for returning extra 
    #' metadata from feature groups (e.g., presence in each analysis replicate
    #' and mass and time widths).
    #'
    #' @return A data.table.
    #'
    get_groups = function(groups = NULL,
                          mass = NULL,
                          mz = NULL,
                          rt = NULL,
                          drift = NULL,
                          ppm = 20,
                          sec = 60,
                          millisec = 5,
                          filtered = FALSE,
                          intensities = TRUE,
                          average = FALSE,
                          sdValues = FALSE,
                          metadata = FALSE) {

      if (!self$has_groups()) return(data.table())

      fts <- self$get_features(
        analyses = NULL,
        features = groups,
        mass, mz, rt, drift, ppm, sec, millisec,
        filtered = filtered
      )
      
      if (nrow(fts) > 0) {
        g_ids <- unique(fts$group)
        
        n_g <- length(g_ids)
        
        fgroups <- data.table("group" = g_ids)
        
        if (intensities) {
          fts_av <- fts[, .(intensity = max(intensity)), by = c("group", "analysis")]
          
          if (average) {
            rpls <- self$get_replicate_names()
            fts_av$analysis <- rpls[fts_av$analysis]
            fts_av <- fts_av[, .(intensity = mean(intensity, na.rm = TRUE), iSD = sd(intensity, na.rm = TRUE)), by = c("group", "analysis")]
            fts_av$iSD <- round(fts_av$iSD / fts_av$intensity * 100, digits = 0)
          }
          
          mat_cols <- self$get_analysis_names()
          
          if (average) mat_cols <- unique(rpls)
          
          mat <- matrix(rep(0, n_g * length(mat_cols)), nrow = n_g)
          rownames(mat) <- g_ids
          colnames(mat) <- mat_cols
          
          if (average && sdValues) {
            mat_sd <- mat
            for (i in seq_len(nrow(fts_av))) mat_sd[fts_av$group[i], fts_av$analysis[i]] <- fts_av$iSD[i]
            colnames(mat_sd) <- paste0(mat_cols, "_sd")
            iSD <- cbind(fgroups, mat_sd)
          }
          
          for (i in seq_len(nrow(fts_av))) mat[fts_av$group[i], fts_av$analysis[i]] <- fts_av$intensity[i]
          ints <- data.table("group" = g_ids)
          ints <- cbind(fgroups, mat)
        }

        if ("name" %in% colnames(fts)) {
          g_names <- fts$name
          names(g_names) <- fts$group
          g_names <- g_names[!duplicated(names(g_names))]
          fgroups$name <- g_names[fgroups$group]
        }
        
        # adds rt and mass
        if (metadata) {
          cols <- colnames(fts)
          if (!"istd" %in% cols) fts[["istd"]] <- list(NULL)
          if (!"quality" %in% cols) fts[["quality"]] <- list(NULL)
          if (!"isotope" %in% cols) fts[["isotope"]] <- list(NULL)
          
          fts_meta <- fts[, .(
            rt = round(mean(rt), digits = 0),
            mass = round(mean(mass), digits = 4),
            rtdev = round(max(rtmax - rtmin), digits = 0),
            massdev = round(max(mzmax - mzmin), digits = 4),
            presence = round(length(feature) / self$get_number_analyses() * 100, digits = 0),
            sn = round(max(vapply(quality, function(x) if (!is.null(x)) x$sn else 0, 0), na.rm = TRUE), digits = 1),
            iso = min(vapply(isotope, function(x) if (!is.null(x)) x$step else 0, 0)),
            istd = paste0(unique(vapply(istd, function(x) if (!is.null(x)) x$name else NA_character_, NA_character_)), collapse = "; "),
            filtered = all(filtered),
            filter = paste0(unique(filter))
          ), by = "group"]
          
          fgroups <- fgroups[fts_meta, on = "group"]
        }
        
        if (intensities) fgroups <- fgroups[ints, on = "group"]
        
        if (average && sdValues) fgroups <- fgroups[iSD, on = "group"]
        
        if (filtered) {
          ftag <- fts[, .(filter = paste(unique(filter), collapse = "; ")), by = "group"]
          fgroups <- fgroups[ftag, on = "group"]
        }
        
        fgroups
        
      } else {
        data.table()
      }
    },

    #' @description
    #' Gets an averaged MS1 spectrum for feature groups in the analyses.
    #'
    #' @return A data.table.
    #'
    get_groups_ms1 = function(groups = NULL,
                              mass = NULL,
                              mz = NULL,
                              rt = NULL,
                              drift = NULL,
                              ppm = 20,
                              sec = 60,
                              millisec = 5,
                              rtWindow = c(-2, 2),
                              mzWindow = c(-5, 90),
                              mzClustFeatures = 0.003,
                              presenceFeatures = 0.8,
                              minIntensityFeatures = 1000,
                              loadedFeaturesMS1 = TRUE,
                              mzClust = 0.003,
                              presence = 0.8,
                              minIntensity = 1000,
                              groupBy = "groups",
                              verbose = FALSE,
                              filtered = FALSE,
                              runParallel = FALSE) {

      fgs <- self$get_groups(
        groups, mass, mz, rt, drift, ppm, sec, millisec, filtered,
        intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
      )

      if (nrow(fgs) == 0) return(data.table())

      fts <- self$get_features(features = fgs$group)

      if (nrow(fts) == 0) return(data.table())

      ms1 <- self$get_features_ms1(
        analyses = unique(fts$analysis),
        features = fts$feature,
        rtWindow = rtWindow,
        mzWindow = mzWindow,
        mzClust = mzClustFeatures,
        presence = presenceFeatures,
        minIntensity = minIntensityFeatures,
        verbose = verbose,
        filtered = filtered,
        loadedMS1 = loadedFeaturesMS1,
        runParallel = runParallel
      )

      ms1 <- ms1[ms1$intensity > minIntensity, ]

      if (nrow(ms1) == 0) return(data.table())

      polarities <- unique(self$get_polarities(analyses = unique(ms1$analysis)))

      multiple_polarities <- FALSE

      if (length(polarities) > 1) multiple_polarities <- TRUE

      if ("groups" %in% groupBy) {

        if (multiple_polarities) {
          ms1$unique_id <- paste0(ms1$group, "_", ms1$polarity)
          ms1$analysis <- NA_character_

        } else {
          ms1$unique_id <- ms1$group
          ms1$analysis <- NA_character_
        }

      } else {
        rpls <- self$get_replicate_names()
        ms1$analysis <- rpls[ms1$analysis]

        if (multiple_polarities) {
          ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group, "", ms1$polarity)
        } else {
          ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group)
        }
      }

      ms1$id <- ms1$group

      ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, verbose)

      ms1_df <- rbindlist(ms1_list, fill = TRUE)

      ms1_df$group <- ms1_df$id

      ms1_df[["id"]] <- NULL

      ms1_df <- ms1_df[order(ms1_df$mz), ]

      ms1_df <- ms1_df[order(ms1_df$group), ]

      if ("groups" %in% groupBy) {
        ms1_df[["analysis"]] <- NULL

      } else {
        ms1_df <- ms1_df[order(ms1_df$analysis), ]
        setnames(ms1_df, "analysis", "replicate")
      }

      if ("name" %in% colnames(fgs)) {
        tar_ids <- fgs$name
        names(tar_ids) <- fgs$group
        ms1_df$name <- tar_ids[ms1_df$group]
      }

      copy(ms1_df)
    },

    #' @description
    #' Gets an averaged MS2 spectrum for feature groups in the analyses.
    #'
    #' @return A data.table.
    #'
    get_groups_ms2 = function(groups = NULL,
                              mass = NULL,
                              mz = NULL,
                              rt = NULL,
                              drift = NULL,
                              ppm = 20,
                              sec = 60,
                              millisec = 5,
                              isolationWindow = 1.3,
                              mzClustFeatures = 0.003,
                              presenceFeatures = 0.8,
                              minIntensityFeatures = 100,
                              loadedFeaturesMS2 = TRUE,
                              mzClust = 0.003,
                              presence = 0.8,
                              minIntensity = 100,
                              groupBy = "groups",
                              verbose = FALSE,
                              filtered = FALSE,
                              runParallel = FALSE) {

      fgs <- self$get_groups(
        groups, mass, mz, rt, drift, ppm, sec, millisec, filtered,
        intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
      )

      if (nrow(fgs) == 0) return(data.table())

      fts <- self$get_features(features = fgs$group, filtered = filtered)

      if (nrow(fts) == 0) return(data.table())

      ms2 <- self$get_features_ms2(
        analyses = unique(fts$analysis),
        features = fts$feature,
        isolationWindow = isolationWindow,
        mzClust = mzClustFeatures,
        presence = presenceFeatures,
        minIntensity = minIntensityFeatures,
        verbose = verbose,
        filtered = filtered,
        loadedMS2 = loadedFeaturesMS2,
        runParallel = runParallel
      )

      ms2 <- ms2[ms2$intensity > minIntensity, ]

      if (nrow(ms2) == 0) return(data.table())

      polarities <- unique(self$get_polarities(analyses = unique(ms2$analysis)))

      multiple_polarities <- FALSE

      if (length(polarities) > 1) multiple_polarities <- TRUE

      if ("groups" %in% groupBy) {
        if (multiple_polarities) {
          ms2$unique_id <- paste0(ms2$group, "_", ms2$polarity)
          ms2$analysis <- NA_character_

        } else {
          ms2$unique_id <- ms2$group
          ms2$analysis <- NA_character_
        }

      } else {
        rpls <- self$get_replicate_names()
        ms2$analysis <- rpls[ms2$analysis]

        if (multiple_polarities) {
          ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group, "", ms2$polarity)
        } else {
          ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group)
        }
      }

      ms2$id <- ms2$group

      ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, verbose)

      ms2_df <- rbindlist(ms2_list, fill = TRUE)

      ms2_df$group <- ms2_df$id

      ms2_df[["id"]] <- NULL

      ms2_df <- ms2_df[order(ms2_df$mz), ]

      ms2_df <- ms2_df[order(ms2_df$group), ]

      if ("groups" %in% groupBy) {
        ms2_df[["analysis"]] <- NULL

      } else {
        ms2_df <- ms2_df[order(ms2_df$analysis), ]
        setnames(ms2_df, "analysis", "replicate")
      }

      if ("name" %in% colnames(fgs)) {
        tar_ids <- fgs$name
        names(tar_ids) <- fgs$group
        ms2_df$name <- tar_ids[ms2_df$group]
      }

      copy(ms2_df)
    },

    #' @description
    #' Gets the percentage coverage of feature groups in the analyses (i.e.
    #' 100% means that a feature group is present in all the analyses).
    #'
    #' @return A data.table.
    #'
    get_groups_coverage = function(groups = NULL,
                                   mass = NULL,
                                   mz = NULL,
                                   rt = NULL,
                                   drift = NULL,
                                   ppm = 20,
                                   sec = 60,
                                   millisec = 5,
                                   filtered = FALSE,
                                   minIntensity = 0) {

      groups <- self$get_groups(
        groups, mass, mz, rt, drift, ppm, sec, millisec, filtered,
        intensities = TRUE, average = FALSE, sdValues = FALSE, metadata = FALSE
      )

      cols_id_ints <- unname(self$get_analysis_names())

      groups_ints <- groups[, cols_id_ints, with = FALSE]

      cov <- apply(groups_ints, 1, function(x, n) {
        x <- x[x > 0]
        round((length(x) / n) * 100, digits = 0)
      }, n = length(cols_id_ints))

      res <- data.table::data.table("group" = groups$group)

      if ("name" %in% colnames(groups)) {
        res$name <- groups$name
      }

      res$coverage <- cov

      res <- cbind(res, groups_ints)

      # TODO improve coverage eval by given the coverage within each replicate group

      res
    },

    #' @description
    #' Gets feature isotopes (i.e., isotope clusters) in the analyses.
    #'
    #' @return A data.table.
    #'
    get_isotopes = function(analyses = NULL,
                            groups = NULL,
                            features = NULL,
                            clusters = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE) {

      if (is.null(features) & !is.null(groups)) {
        features <- groups
      }

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) return(data.table())
      
      if (!("isotope" %in% colnames(fts))) {
        warning("Isotopes not found! Run annotate_features.")
        return(data.table())
      }
      
      if (!is.null(clusters)) {
        if (is.numeric(clusters)) {
          fts_clusters <- vapply(fts$isotope, function(x) x$cluster, NA_real_)
          fts <- fts[fts_clusters == clusters, ]
        }
      }
      
      if (nrow(fts) == 0) return(data.table())
      
      clusters <- vapply(fts$isotope, function(x) x$cluster, NA_real_)
      which_clusters <- unique(clusters)
      which_clusters <- which_clusters[!(which_clusters == 0)]

      if (length(which_clusters) == 0) return(data.table())

      all_fts <- self$get_features(filtered = TRUE)
      all_iso <- all_fts$isotope
      sel <- vapply(all_iso, function(x) x$cluster, NA_real_) %in% which_clusters
      
      iso <- all_iso[sel]
      iso_df <- lapply(iso, function(x) as.data.table(x))
      iso_df <- rbindlist(iso_df)
      iso_df$feature <- NULL
      colnames(iso_df) <- paste0("iso_", colnames(iso_df))
      
      fts_sel <- all_fts[sel, ]
      
      iso_df <- cbind(
        fts_sel[, c("analysis", "feature"), with = FALSE],
        iso_df,
        fts_sel[, c("rt", "mz", "intensity", "rtmin", "rtmax", "mzmin", "mzmax"), with = FALSE]
      )

      if ("name" %in% colnames(fts)) {
        tar_names <- fts$name
        names(tar_names) <- as.character(clusters)
        iso_df$name <- tar_names[as.character(iso_df$iso_cluster)]
      }

      iso_df
    },

    #' @description Gets suspects from features according to a defined database
    #' and mass (`ppm`) and time (`sec`) deviations.
    #'
    #' @param database A data.frame with at least the columns name
    #' and mass, indicating the name and neutral monoisotopic
    #' mass of the suspect targets.
    #'
    #' @details The `ppm` and `sec` which indicate the
    #' mass (im ppm) and time (in seconds) deviations applied during the
    #' screening.
    #'
    #' @return A data.frame with the suspects and matched features.
    #'
    get_suspects = function(analyses = NULL,
                            database = NULL,
                            features = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 5,
                            sec = 10,
                            millisec = 5,
                            ppmMS2 = 10,
                            minFragments = 3,
                            isolationWindow = 1.3,
                            mzClust = 0.003,
                            presence = 0.8,
                            minIntensity = 0,
                            runParallel = FALSE,
                            filtered = FALSE,
                            onGroups = TRUE) {
      
      if (!any(self$has_features(analyses))) {
        warning("Features not found in the MassSpecData object!")
        return(data.table())
      }

      valid_db <- FALSE
      
      # Check if suspects are available in features
      if (is.null(database)) {
        features <- self$get_features(
          analyses,
          features,
          mass,
          mz,
          rt,
          drift,
          ppm,
          sec,
          millisec,
          filtered
        )
        
        if (nrow(features) == 0) {
          message("\U2717 Features not found for targets!")
          return(data.table())
        }
        
        features[["name"]] <- NULL
        
        if ("suspects" %in% colnames(features)) {
          
          sel <- !vapply(features$suspects, function(x) {
            if (is.null(x)) {
              TRUE
            } else if (is.data.frame(x)) {
              if (nrow(x) == 0) { TRUE } else { FALSE }
            } else { TRUE }
          }, TRUE)
          
          if (any(sel)) {
            features <- features[sel, ]
            
            suspects_l <- features[["suspects"]]
            
            suspects_l2 <- lapply(seq_len(length(suspects_l)),
              function(x, suspects_l, features) {
                temp <- suspects_l[[x]]
                temp_ft <- features[x, ]
                temp_ft[["suspects"]] <- NULL
                temp$feature <- temp_ft$feature
                temp <- merge(temp, temp_ft, by = "feature", all = TRUE)
                setcolorder(temp, colnames(temp)[2:(which(colnames(temp) %in% "analysis"))])
                temp
              },
              suspects_l = suspects_l, features = features
            )
            
            analyses_df <- rbindlist(suspects_l2, fill = TRUE)

          } else {
            warning("Suspects were not found! Run suspect_screening or give a database.")
            return(data.table())
          }
          
        } else {
          warning("Suspects were not found! Run suspect_screening or give a database.")
          return(data.table())
        }

      } else {
        
        database <- as.data.table(database)
        
        if (is.data.frame(database)) {
          database <- as.data.table(database)
          if (any(c("mass", "neutralMass") %in% colnames(database)) | "mz" %in% colnames(database)) {
            if ("name" %in% colnames(database)) {
              if ("neutralMass" %in% colnames(database)) {
                setnames(database, "neutralMass", "mass")
              }
              valid_db = TRUE
            }
          }
        }
        
        if (!valid_db) {
          warning("Argument database must be a data.frame with at least the columns name and mass or mz!")
          return(data.table())
        }
        
        if (!"rt" %in% colnames(database)) {
          database$rt <- NA_real_
          
        } else {
          database$rt[database$rt == ""] <- NA_real_
        }
        
        database$rt <- as.numeric(database$rt)
        
        analyses <- lapply(self$get_analysis_names(analyses),
          function(analysis, database, ppm, sec) {
           
            cols_db <- colnames(database)
            
            pol <- self$get_polarities(analysis)
            
            it <- seq_len(nrow(database))
            
            suspects <- lapply(it, function(x, analysis, database, ppm, sec) {

            x_rt <- database$rt[x]
            
            if ("mz" %in% cols_db) x_mz <- database$mz[x] else x_mz <- NA_real_
            
            if (!is.na(x_mz)) {
              x_mz <- database$mz[x]
              
              temp <- self$get_features(
                analyses = analysis,
                mz = x_mz,
                rt = x_rt,
                ppm = ppm,
                sec = sec,
                filtered = filtered
              )
             
            } else {
              x_mass <- database$mass[x]
              
              temp <- self$get_features(
                analyses = analysis,
                mass = x_mass,
                rt = x_rt,
                ppm = ppm,
                sec = sec,
                filtered = filtered
              )
            }
            
            if (is.na(x_rt)) x_rt <- NULL
            
            if (nrow(temp) > 0) {
             
              temp$name <- database$name[x]
              cols_front <- c("name")
              
              if ("formula" %in% colnames(database)) {
                temp$formula <- database$formula[x]
                cols_front <- c(cols_front, "formula")
              }
              
              if ("SMILES" %in% colnames(database)) {
                temp$SMILES <- database$SMILES[x]
                cols_front <- c(cols_front, "SMILES")
              }
              
              if ("mz" %in% cols_db) {
               
                temp$exp_mass <- database$mz[x]
               
                temp$error_mass <- round(
                  ((temp$mz - x_mz) / temp$mz) * 1E6, digits = 1
                )
               
              } else {
               
                temp$exp_mass <- database$mass[x]
                
                temp$error_mass <- round(
                  ((temp$mass - x_mass) / temp$mass) * 1E6, digits = 1
                )
              }
              
              cols_front <- c(cols_front, "exp_mass")
              
              temp$exp_rt <- database$rt[x]
              
              cols_front <- c(cols_front, "exp_rt")
              
              temp$id_level <- NA_character_
              
              temp$error_rt <- NA_real_
              
              temp$shared_fragments <- 0
              
              temp$fragments <- NA_character_
              
              cols_front <- c(cols_front, "id_level", "error_mass", "error_rt", "shared_fragments", "fragments")
              
              setcolorder(temp, cols_front)
              
              for (i in seq_len(nrow(temp))) {
                temp$id_level[i] = "4"
                
                if (!is.null(x_rt)) {
                  temp$id_level[i] = "3b"
                  temp$error_rt[i] = round(temp$rt[i] - x_rt, digits = 1)
                }
                
                if ("fragments" %in% colnames(database)) {
                  fragments <- database$fragments[x]
                  
                  if (!is.na(fragments)) {
                    
                    if ("ms2" %in% colnames(temp)) {
                      ms2 <- temp$ms2[[1]]
                      
                      if (is.null(ms2)) ms2 <- data.table()
                      
                    } else {
                      ms2 <- self$get_features_ms2(
                        temp$analysis,
                        temp$feature,
                        isolationWindow = isolationWindow,
                        mzClust = mzClust,
                        presence = presence,
                        minIntensity = minIntensity,
                        runParallel = runParallel
                      )
                    }
                    
                    if (nrow(ms2) > 0) {
                      fragments <- unlist(strsplit(fragments, split = "; ", fixed = TRUE))
                      fragments <- strsplit(fragments, " ")
                      fragments <- data.table(
                        "mz" = vapply(fragments, function(x) as.numeric(x[1]), NA_real_),
                        "intensity" = vapply(fragments, function(x) as.numeric(x[2]), NA_real_)
                      )
                      
                      setorder(fragments, -intensity)
                      
                      setorder(ms2, -intensity)
                      
                      fragments$intensity <- -fragments$intensity
                      
                      mzr <- fragments$mz * ppm / 1E6
                      fragments$mzmin <- fragments$mz - mzr
                      fragments$mzmax <- fragments$mz + mzr
                      
                      fragments$shared <- apply(fragments, 1, function(x) {
                        any(ms2$mz >= x[3] & ms2$mz <= x[4])
                      })
                      
                      # plot(
                      #   intensity ~ mz, ms2,
                      #   type = "h",
                      #   xlab = expression(italic("m/z ") / " Da"),
                      #   ylab = "Intensity / counts",
                      #   col = "black",
                      #   lwd = 2,
                      #   ylim = c(min(fragments$intensity) * 1.5, max(ms2$intensity) * 1.5),
                      #   yaxs = "i",
                      #   xaxt = "n"
                      # )
                      # 
                      # lines(
                      #   intensity ~ mz, fragments,
                      #   type = "h",
                      #   pch = 19,
                      #   lwd = 2,
                      #   cex = 0.5,
                      #   col = "red",
                      #   yaxs = "i",
                      #   xaxt = "n"
                      # )
                      
                      temp$shared_fragments[i] = sum(fragments$shared)
                      
                      if (temp$shared_fragments[i] > 3) {
                        
                        temp$fragments <- database$fragments[x]
                        
                        if (temp$id_level[i] == "3b") {
                          temp$id_level[i] = "1"
                          
                        } else if (temp$id_level[i] == "4") {
                          temp$id_level[i] = "2"
                        }
                      }
                    }
                  }
                }
              }
            } else {
             temp <- data.table()
            }
            
            temp
          
          },
          analysis = analysis,
          database = database,
          ppm = ppm,
          sec = sec
        )
        
        suspects <- rbindlist(suspects, fill = TRUE)

        },
        database = database,
        ppm = ppm,
        sec = sec
        )
        
        analyses_df <- rbindlist(analyses, fill = TRUE)
        
      }
      
      if (nrow(analyses_df) > 0 && !filtered && self$has_groups() && onGroups) {
        
        if (all(!is.na(analyses_df$group))) {
          
          keep_cols <- colnames(analyses_df)
          keep_cols <- c(keep_cols[1:which(keep_cols %in% "id_level") - 1], "group")
          
          order_cols <- colnames(analyses_df)
          order_cols <- c(order_cols[1:which(order_cols %in% "shared_fragments")], "group")
          
          temp_fts <- analyses_df[, keep_cols, with = FALSE]

          analyses_df$id_level <- factor(
            analyses_df$id_level,
            levels = c("1", "2", "3a", "3b", "4"),
            ordered = TRUE
          )
          
          error_vals <- analyses_df[, .(
            id_level = min(id_level),
            error_mass = max(abs(error_mass)),
            error_rt = max(abs(error_rt)),
            shared_fragments = max(shared_fragments)
          ), by = "group"]
          
          groups_df <- self$get_groups(groups = unique(temp_fts$group),
            intensities = TRUE, average = TRUE, sdValues = FALSE, metadata = FALSE
          )
          
          groups_df <- groups_df[error_vals, on = .(group)]
          
          groups_df <- groups_df[temp_fts, on = .(group)]
          
          data.table::setkey(groups_df, group)
          
          groups_df <- groups_df[unique(group), mult = "first"]
          
          setcolorder(groups_df, order_cols)
          
          return(groups_df)
        }
      }
      
      analyses_df
    },

    #' @description
    #' Gets modules data.
    #'
    #' @param modules X.
    #'
    #' @return The list of modules data as defined by `modules` argument when
    #' `NULL` all data in modules is returned.
    #'
    get_modules_data = function(modules = NULL) {
      if (is.null(modules)) modules <- names(private$.modules)
      private$.modules[modules]
    },

    #' @description
    #' Gets modules data.
    #'
    #' @param modules X.
    #'
    #' @return The list of modules data as defined by `modules` argument when
    #' `NULL` all data in modules is returned.
    #'
    get_internal_standards = function(average = TRUE) {
      istd <- self$get_features(filtered = TRUE)

      if ("istd" %in% colnames(istd)) {
        
        sel <- !vapply(istd$istd, is.null, TRUE)

        istd <- istd[sel, ]

        if (nrow(istd) > 0) {
          
          istd_l <- istd[["istd"]]
          
          istd_l2 <- lapply(seq_len(length(istd_l)), function(x, istd_l, istd) {
            temp <- istd_l[[x]]
            temp_ft <- istd[x, ]
            temp <- cbind(temp, temp_ft)
            temp
          }, istd = istd, istd_l = istd_l)
          
          istd <- rbindlist(istd_l2, fill = TRUE)

          istd$rtr <- round(istd$rtmax - istd$rtmin, digits = 1)

          istd$mzr <- round(istd$mzmax - istd$mzmin, digits = 4)

          if ("isotope" %in% colnames(istd)) {
            istd$iso_n <- vapply(istd$isotope, function(x) x$cluster_size, 0)
            istd$iso_c <- vapply(istd$isotope, function(x) x$carbons, 0)
            
          } else {
            istd$iso_n <- NA
            istd$iso_c <- NA
          }

          if (self$has_groups() & average) {

            rpl <- self$get_replicate_names()

            istd$replicate <- rpl[istd$analysis]

            cols <- c(
              "name",
              "intensity",
              "area",
              "rtr",
              "mzr",
              "error_rt",
              "error_mass",
              "rec",
              "iso_n",
              "iso_c",
              "replicate",
              "group"
            )

            istd <- istd[, cols, with = FALSE]

            istd <- istd[, `:=`(
                freq = length(area),
                intensity = round(mean(intensity, na.rm = TRUE), digits = 0),
                intensity_sd = round(sd(intensity, na.rm = TRUE), digits = 0),
                area = round(mean(area, na.rm = TRUE), digits = 0),
                area_sd = round(sd(area, na.rm = TRUE), digits = 0),
                rtr = round(mean(rtr, na.rm = TRUE), digits = 1),
                rtr_sd = round(sd(rtr, na.rm = TRUE), digits = 1),
                mzr = round(mean(mzr, na.rm = TRUE), digits = 4),
                mzr_sd = round(sd(mzr, na.rm = TRUE), digits = 4),
                error_rt = round(mean(error_rt, na.rm = TRUE), digits = 1),
                error_rt_sd = round(sd(error_rt, na.rm = TRUE), digits = 1),
                error_mass = round(mean(error_mass, na.rm = TRUE), digits = 1),
                error_mass_sd = round(sd(error_mass, na.rm = TRUE), digits = 1),
                rec = round(mean(rec, na.rm = TRUE), digits = 1),
                rec_sd = round(sd(rec, na.rm = TRUE), digits = 1),
                iso_n = round(mean(iso_n, na.rm = TRUE), digits = 0),
                iso_n_sd = round(sd(iso_n, na.rm = TRUE), digits = 0),
                iso_c = round(mean(iso_c, na.rm = TRUE), digits = 0),
                iso_c_sd = round(sd(iso_c, na.rm = TRUE), digits = 0)
              ),
              by = c("name", "group", "replicate")
            ][]

            istd <- unique(istd)

            istd$rec[is.nan(istd$rec)] <- NA_real_

          } else {
            cols <- c(
              "name",
              "intensity",
              "area",
              "rtr",
              "mzr",
              "error_rt",
              "error_mass",
              "rec",
              "iso_n",
              "iso_c",
              "analysis",
              "feature"
            )

            if (self$has_groups()) cols <- c(cols, "group")

            istd <- istd[, cols, with = FALSE]
            istd$intensity <- round(istd$intensity, digits = 0)
            istd$area <- round(istd$area, digits = 0)
          }

          setorder(istd, "name")

          istd

        } else {
          warning("Internal standards not found!")
          data.table()
        }

      } else {
        warning("Not present! Run find_internal_standards method to tag the internal standards!")
        data.table()
      }
    },
    
    #' @description Data.table with the overview of all processing methods 
    #' present.
    #'
    #' @return A data.table.
    #'
    get_workflow_overview = function() {
      if (self$has_settings()) {
        data.table(
          "call" = vapply(private$.settings, function(x) x$call, ""),
          "algorithm" = vapply(private$.settings, function(x) x$algorithm, ""),
          "developer" = vapply(private$.settings, function(x) paste(x$developer, collapse = "; "), ""),
          "contact" = vapply(private$.settings, function(x) x$contact, ""),
          "link" = vapply(private$.settings, function(x) x$link, "")
        )
      } else {
        data.table()
      }
    },

    ## ___ add -----

    #' @description
    #' Adds headers. If an argument or element "name" is given, it must
    #' be type character. If an argument or element path is given, it must be
    #' type character and exist. If an argument or element date is given, it
    #' must be class POSIXct or POSIXt. If given date is character, conversion
    #' to class POSIXct or POSIXt is attempted. See `?ProjectHeaders` for more
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

        if (!identical(new_headers, old_headers) & is(new_headers, "ProjectHeaders")) {
          private$.headers <- new_headers

          lapply(names(headers), function(x, new_headers) {
            private$.register(
              "added",
              "ProjectHeaders",
              x,
              NA_character_,
              NA_character_,
              new_headers[x]
            )
          }, new_headers = new_headers)


          message("\U2713 Added headers!")
        }

      } else {
        warning("Invalid headers content or structure! Not added.")
      }
      invisible(self)
    },

    #' @description
    #' Adds processing settings.
    #'
    #' @param settings  A named list of ProcessingSettings S3 class objects or a
    #' single ProcessingSettings S3 class object. The list names should match
    #' the call name of each ProcessingSettings object. Alternatively, a named
    #' list with call name, algorithm and parameters to be transformed and added
    #' as ProcessingSettings S3 class object.
    #'
    #' @param replace Logical. When `TRUE`, existing settings are replaced by
    #' the new settings with the same call name.
    #'
    #' @return Invisible.
    #'
    add_settings = function(settings = NULL, replace = FALSE) {

      if (is.list(settings)) {

        cols_check <- c("call", "algorithm", "parameters")
        if (all(cols_check %in% names(settings))) {
          settings <- list(settings)
        }

        settings <- lapply(settings, as.ProcessingSettings)
        
        names(settings) <- NULL

        all_ps <- vapply(settings, function(x) class(x)[1], "")

        if (all(all_ps %in% "ProcessingSettings")) {

          only_one_possible <- c(
            "centroid_spectra",
            "bin_spectra",
            "find_features",
            "annotate_features",
            "load_features_eic",
            "load_features_ms1",
            "load_features_ms2",
            "load_groups_ms1",
            "load_groups_ms2",
            "group_features",
            "fill_features",
            "calculate_quality"
          )

          call_names <- vapply(settings, function(x) x$call, NA_character_)

          duplicated_names <- call_names[duplicated(call_names)]

          if (any(duplicated_names %in% only_one_possible)) {

            if (length(duplicated_names) == 1) {
              message("\U2139 ", duplicated_names, " duplicate not added as only one is possible!")

            } else {
              message(paste0("\U2713 Duplicate settings for the following not added as only one is possible!\n",
                paste(duplicated_names, collapse = "\n"))
              )
            }
            
            settings <- settings[!(duplicated(call_names) & call_names %in% only_one_possible)]

            call_names <- vapply(settings, function(x) x$call, NA_character_)
          }

          if (is.null(private$.settings)) private$.settings <- list()
          
          lapply(settings, function(x, only_one_possible, replace) {
            
            stored_calls <- vapply(private$.settings, function(z) z$call, NA_character_)

            if (replace) {

              if (x$call %in% stored_calls) {
                
                # case when repeating can happen with replace = TRUE as long as duplicated in call_names
                if (!(x$call %in% only_one_possible) & any(duplicated(call_names))) {
                  private$.settings <- c(private$.settings, list(x))
                  
                  private$.register(
                    "added",
                    "ProcessingSettings",
                    x$call,
                    "StreamFind",
                    x$version,
                    x$algorithm
                  )
                  
                  message(
                    paste0("\U2713 ", x$call, " processing settings added!")
                  )
                  
                } else {
                  private$.settings[which(x$call %in% stored_calls)] <- list(x)
                  
                  private$.register(
                    "replaced",
                    "ProcessingSettings",
                    x$call,
                    "StreamFind",
                    x$version,
                    x$algorithm
                  )
                  
                  message(
                    paste0("\U2713 ", x$call, " processing settings replaced!")
                  )
                }

              } else {
                private$.settings <- c(private$.settings, list(x))
                
                private$.register(
                  "added",
                  "ProcessingSettings",
                  x$call,
                  "StreamFind",
                  x$version,
                  x$algorithm
                )
                
                message(
                  paste0("\U2713 ", x$call, " processing settings added!")
                )
              }

            } else {
              
              if (x$call %in% only_one_possible && x$call %in% stored_calls) {
                message("\U2139 ", x$call, " replaced as only one is possible!")
                
                private$.settings[which(x$call %in% stored_calls)] <- list(x)
                
                private$.register(
                  "replaced",
                  "ProcessingSettings",
                  x$call,
                  "StreamFind",
                  x$version,
                  x$algorithm
                )
                
                message(
                  paste0("\U2713 ", x$call, " processing settings replaced!")
                )
                
              } else {
                private$.settings <- c(private$.settings, list(x))
                
                private$.register(
                  "added",
                  "ProcessingSettings",
                  x$call,
                  "StreamFind",
                  x$version,
                  x$algorithm
                )
                
                message(
                  paste0("\U2713 ", x$call, " processing settings added!")
                )
              }
            }
          }, only_one_possible = only_one_possible, replace = replace)

        } else {
          not_conform <- which(!all_ps %in% "ProcessingSettings")
          warning("Settings (number/s: ", paste(not_conform, collapse = "; "), ") content or structure not conform! Not added.")
        }
      } else {
        warning("Settings must be a list! Not added.")
      }
      
      invisible(self)
    },

    #' @description
    #' Adds analyses.
    #'
    #' @param analyses A MassSpecAnalysis S3 class object or a list with
    #' MassSpecAnalysis S3 class objects as elements (see `?MassSpecAnalysis` for
    #' more information).
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {

      if (is.list(analyses)) {
        if (all(c("name", "file") %in% names(analyses))) {
          analyses <- as.MassSpecAnalysis(analyses)

          if (is(analyses, "MassSpecAnalysis")) {
            ana_name <- analyses$name
            analyses <- list(analyses)
            names(analyses) <- ana_name

          } else {
            warning("Not done, check the conformity of the analyses list!")
            analyses <- NULL
          }

        } else {
          analyses <- lapply(analyses, as.MassSpecAnalysis)

          if (all(vapply(analyses, function(x) is(x, "MassSpecAnalysis"), FALSE))) {
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

          lapply(analyses, function(x) {
            private$.register(
              "added",
              class(x),
              x$name,
              "StreamFind",
              x$version,
              x$file
            )
          })

          message(
            paste0(
              "\U2713 ",
              length(new_analyses) - old_size,
              " analyses added!"
            )
          )

          if (old_size < length(new_analyses)) {

            if (any(self$has_features())) {
              n_feats_old <- vapply(old_analyses, function(x) nrow(x$features), 0)
              n_feats_new <- vapply(analyses, function(x) nrow(x$features), 0)

              if (sum(n_feats_old) == 0 & sum(n_feats_new) != 0 & old_size != 0) {
                warning("New analyses have features but there are no features in the MassSpecData! Consider running find_features.")

              } else if (sum(n_feats_old) != 0 & sum(n_feats_new) == 0 & old_size != 0) {
                warning("New analyses do not have features but there are features in the MassSpecData! Consider running the find_features.")

              } else if (any(c(n_feats_old, n_feats_new) %in% 0) & sum(c(n_feats_old, n_feats_new)) > 0) {
                warning("There are analyses without features! Consider running find_features.")
              }
            }

            has_features <- all(self$has_features())

            no_groups_in_all_analyses <- !all(vapply(new_analyses,
              function(x) "group" %in% colnames(x$features), FALSE)
            )
            
            if (self$has_groups() && old_size != 0) {
              warning("Feature groups cleared as new analyses were added!")
              suppressMessages(self$remove_groups())

            } else if (has_features && self$has_groups() && no_groups_in_all_analyses) {
              warning("Feature groups cleared as were not present in all the analyses!")
              suppressMessages(self$remove_groups())

            } else if (has_features && self$has_groups() && !self$check_correspondence()) {
              warning("Feature groups cleared as correspondence over the analyses did not match!")
              suppressMessages(self$remove_groups())
            }
          }
        } else {
          warning("Duplicated analysis names not allowed! Not done.")
        }
      }
      invisible(self)
    },

    #' @description
    #' Adds *MassSpecAnalysis* objects based on mzML/mzXML files. Note that
    #' when adding new mzML/mzXML files or *MassSpecAnalysis* are added any
    #' existing grouping of features is removed.
    #'
    #' @return Invisible.
    #'
    add_files = function(files = NULL, runParallel = FALSE) {

      if (!is.null(files)) {

        new_analyses <- parse.MassSpecAnalysis(files, runParallel)

        if (all(vapply(new_analyses, function(x) "MassSpecAnalysis" %in% is(x), FALSE))) {
          self$add_analyses(new_analyses)

        } else {
          warning("Not all added files could be converted as MassSpecAnalysis!")
        }

      } else {
        warning("Files were not added!")
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

        private$.register(
          "added",
          "analyses",
          "replicate names",
          NA_character_,
          NA_character_,
          NA_character_
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

          private$.register(
            "added",
            "analyses",
            "blank names",
            NA_character_,
            NA_character_,
            NA_character_
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

    #' @description
    #' Adds metadata to analyses.
    #'
    #' @param value A data.frame or data.table with metadata for the analyses.
    #' The data.frame must have an analysis column and the same number of rows
    #' as the number of analyses in the MassSpecData. Metadata is added using
    #' any extra columns of the data.frame.
    #'
    #' @return Invisible.
    #'
    add_metadata = function(value = NULL) {

      if (is.data.frame(value)) {
        if (nrow(value) == self$get_number_analyses()) {
          if ("analysis" %in% colnames(value)) {
            if (ncol(value) >= 2) {
              value <- value[order(value$analysis), ]
              value <- as.data.table(value)
              setcolorder(value, "analysis")

              col_names <- colnames(value)
              col_names <- col_names[2:length(col_names)]

              value <- split(value, value$analysis)

              private$.analyses <- Map(
                function(x, y) {

                  cols <- colnames(y)
                  cols <- cols[2:length(cols)]

                  for (i in cols) x$metadata[[i]] <- y[[i]][1]

                  x
                },
                private$.analyses, value
              )

              private$.register(
                "added",
                "analyses",
                "metadata",
                NA_character_,
                NA_character_,
                paste(col_names, collapse = "; ")
              )

              message("\U2713 Metadata ", paste(col_names, collapse = ", "), " added!")

            } else {
              warning("No metadata found in the data.frame/data.table!")
            }
          } else {
            warning("The column analysis must be in the data.frame/data.table!")
          }
        } else {
          warning("The data.frame/data.table must have the same number of rows as the number of analyses in the MassSpecData!")
        }
      } else {
        warning("The argument value must be a data.frame/data.table!")
      }
      invisible(self)
    },

    #' @description
    #' Adds spectra to analyses.
    #'
    #' @param spectra A data.table with spectra from MS analyses as obtained
    #' by the method `get_spectra()` with columns "scan", "mz" and "intensity".
    #' Other columns might be added from specific processing algorithms!
    #'
    #' @param replace Logical. When `TRUE`, existing spectra are replaced by
    #' the new features.
    #'
    #' @return Invisible.
    #'
    add_spectra = function(spectra = NULL, replace = TRUE) {

      valid <- FALSE

      org_analysis_names <- unname(self$get_analysis_names())

      must_have_cols <- c("scan", "mz", "intensity")

      if (is.data.frame(spectra)) {
        must_have_cols <- c("analysis", must_have_cols)

        if (all(must_have_cols %in% colnames(spectra))) {
          spectra <- spectra[order(spectra$analysis), ]
          analysis_names <- unique(spectra$analysis)

          if (all(analysis_names %in% org_analysis_names)) {
            valid <- TRUE
            split_vector <- spectra$analysis
            spectra$analysis <- NULL
            spectra <- split(spectra, split_vector)
          }

        } else {
          warning("Features data frame does not have all mandatory columns!")
        }

      } else if (is.list(spectra)) {
        analysis_names <- sort(names(spectra))

        if (all(analysis_names %in% org_analysis_names)) {
          spectra <- spectra[analysis_names]
          valid <- vapply(spectra, function(x, must_have_cols) {

            if (is.data.frame(x)) {
              if (all(must_have_cols %in% colnames(x))) {
                return(TRUE)
              }
            }
            FALSE
          }, must_have_cols = must_have_cols, FALSE)

          valid <- all(valid)
        }
      }

      if (valid) {
        n_data <- sum(vapply(spectra, function(x) nrow(x), 0))

        org_spectra <- lapply(private$.analyses, function(x) x$spectra)
        names(org_spectra) <- names(private$.analyses)

        if (replace | n_data == 0) {
          rem_run_cols <- lapply(private$.analyses, function(x) colnames(x$run))
          rem_run_cols <- unique(unname(unlist(rem_run_cols)))
          rem_run_cols <- rem_run_cols[!rem_run_cols %in% "scan"]

          spectra <- lapply(spectra, function(x, rem_run_cols) {
            if (nrow(x) == 0) return(data.table())
            keep_cols <- colnames(x)[!colnames(x) %in% rem_run_cols]
            x <- x[, keep_cols, with = FALSE]
            x
          }, rem_run_cols = rem_run_cols)

          org_spectra[names(spectra)] <- spectra

          private$.analyses <- Map(
            function(x, y) {
              x$spectra <- y
              x
            },
            private$.analyses, org_spectra
          )

          private$.register(
            "added",
            "analyses",
            "spectra",
            NA_character_,
            NA_character_,
            n_data
          )

          message("\U2713 ", n_data, " spectra added!")

        } else {
          warning("Spectra already presence and not replaced!")
        }
      } else {
        warning("Invalid spectra content or structure! Not added.")
      }
      invisible(self)
    },

    #' @description
    #' Adds extracted ion chromatograms (EICs) of features to analyses.
    #'
    #' @param eics A data.table with features from MS analyses as obtained
    #' by the method `get_features_eics()`.
    #'
    #' @param replace Logical. When `TRUE`, existing EICs of features are
    #' replaced by the new EICs.
    #'
    #' @return Invisible.
    #'
    add_features_eic = function(eics = NULL, replace = TRUE) {

      if (!any(self$has_features())) {
        warning("Features must be present in at least one analysis! Not Added.")
        return(invisible(self))
      }

      valid <- FALSE

      org_analysis_names <- unname(self$get_analysis_names())

      must_have_cols <- c("polarity", "mz", "rt", "intensity")

      if (is.data.frame(eics)) {

        if (nrow(eics) == 0) {
          warning("Feature EICs empty! Not Added.")
          return(invisible(self))
        }

        must_have_cols <- c("analysis", "feature", must_have_cols)

        if (all(must_have_cols %in% colnames(eics))) {
          eics <- eics[order(eics$analysis), ]
          analysis_names <- unique(eics$analysis)

          if (all(analysis_names %in% org_analysis_names)) {
            valid <- TRUE
            split_vector <- eics$analysis
            eics$analysis <- NULL
            eics <- split(eics, split_vector)
          }

        } else {
          warning("EICs data frame does not have all mandatory columns!")
        }
      }

      if (is.list(eics)) {

        if (length(eics) == 0) {
          warning("Feature EICs empty! Not Added.")
          return(invisible(self))
        }

        analysis_names <- sort(names(eics))

        if (all(analysis_names %in% org_analysis_names)) {
          eics <- eics[analysis_names]

          eics <- lapply(eics, function(x, must_have_cols) {

            if (is.data.frame(x)) {

              if (all(c("feature", must_have_cols) %in% colnames(x)) | nrow(x) == 0) {

                if (nrow(x) == 0) {
                  warning("Feature EIC/s without traces added! Not added.")
                  return(NULL)
                }

                if (all(c("feature", must_have_cols) %in% colnames(x))) {
                  split_vector <- x$feature
                  x$feature <- NULL
                  x <- split(x, split_vector)
                }

                x

              } else {
                NULL
              }
            }

            if (is.list(x)) {
              x <- lapply(x, function(z) {

                if (is.data.frame(z)) {

                  if (nrow(z) == 0) {
                    warning("Feature EIC without traces added! Not added.")
                    return(NULL)
                  }

                  if (!all(must_have_cols %in% colnames(z))) return(NULL)

                  z

                } else {
                  NULL
                }
              })

              x <- x[!is.null(x)]

              if (length(x) == 0) {
                NULL
              } else {
                x
              }

            } else {
              NULL
            }
          }, must_have_cols = must_have_cols)

          valid <- !all(vapply(eics, is.null, TRUE))

        } else {
          warning("Analysis name/s in feature EICs not present! Not added.")
          return(invisible(self))
        }
      }

      if (!valid) {
        warning("Feature EICs must be a data.frame or a list object! Not added.")
      }

      if (valid) {
        n_fts <- sum(vapply(eics, function(x) length(x), 0))

        org_features_eic <- lapply(private$.analyses, function(x) x$features_eic)
        names(org_features_eic) <- names(private$.analyses)

        if (replace | n_fts == 0) {
          org_features_eic[names(eics)] <- eics

          private$.analyses <- Map(
            function(x, y) {
              x$features_eic <- y
              x
            },
            private$.analyses, org_features_eic
          )

          valid <- vapply(private$.analyses, function(x) {
            ids <- x$features[["feature"]]
            eic_ids <- names(x$features_eic)

            if (all(eic_ids %in% ids)) {
              TRUE
            } else {
              FALSE
            }
          }, FALSE)

          if (all(valid)) {
            private$.register(
              "added",
              "analyses",
              "features_eic",
              NA_character_,
              NA_character_,
              n_fts
            )

            message("\U2713 ", n_fts, " feature EICs added!")

          } else {
            private$.analyses <- lapply(private$.analyses, function(x) {
              x$features_eic <- list()
            })

            warning("Features ID in EICs did not match feature ID in analyses! Not added.")
          }

        } else {
          warning("rbind for feature EICs not implemented yet!")
          # TODO add rbind option for features_eic
          # Possibly needed to redo the index and amend the features ID
        }

      } else {
        warning("Invalid EICs content or structure! Not added.")
      }

      invisible(self)
    },

    #' @description
    #' Adds features to analyses.
    #'
    #' @param features A data.table with features from MS analyses as obtained
    #' by the method `get_features()`. Note that features are obtained with the
    #' method `find_features()`.
    #'
    #' @param replace Logical. When `TRUE`, existing features are replaced by
    #' the new features.
    #'
    #' @return Invisible.
    #'
    add_features = function(features = NULL, replace = TRUE) {

      valid <- FALSE

      org_analysis_names <- unname(self$get_analysis_names())

      must_have_cols <- c(
        "feature",
        "index",
        "rt",
        "mz",
        "mass",
        "intensity",
        "area",
        "rtmin",
        "rtmax",
        "mzmin",
        "mzmax",
        "filled",
        "polarity",
        "filtered",
        "filter"
      )

      if (is.data.frame(features)) {
        must_have_cols <- c("analysis", must_have_cols)

        if (all(must_have_cols %in% colnames(features))) {
          features <- features[order(features$analysis), ]
          analysis_names <- unique(features$analysis)

          if (all(analysis_names %in% org_analysis_names)) {
            valid <- TRUE
            split_vector <- features$analysis
            features$analysis <- NULL
            features <- split(features, split_vector)
          }

        } else {
          warning("Features data frame does not have all mandatory columns!")
        }

      } else if (is.list(features)) {
        analysis_names <- sort(names(features))

        if (all(analysis_names %in% org_analysis_names)) {
          features <- features[analysis_names]
          valid <- vapply(features, function(x, must_have_cols) {

            if (is.data.frame(x)) {
              if (all(must_have_cols %in% colnames(x))) {
                return(TRUE)
              }
            }
            FALSE
          }, must_have_cols = must_have_cols, FALSE)

          valid <- all(valid)
        }
      }

      if (valid) {
        n_fts <- sum(vapply(features, function(x) nrow(x), 0))

        org_features <- lapply(private$.analyses, function(x) x$features)
        names(org_features) <- names(private$.analyses)

        if (replace) {
          org_features[names(features)] <- features

          private$.analyses <- Map(
            function(x, y) {
              x$features <- y
              x
            },
            private$.analyses, org_features
          )

          private$.register(
            "added",
            "analyses",
            "features",
            NA_character_,
            NA_character_,
            n_fts
          )

          message("\U2713 ", n_fts, " features added!")

        } else {
          warning("rbind for features not implemented yet!")
          # TODO add rbind option for features
          # Possibly needed to redo the index and amend the features ID
        }
      } else {
        warning("Invalid features content or structure! Not added.")
      }
      invisible(self)
    },

    #' @description
    #' Adds a group column from correspondence analysis to the features
    #' data.table. Note that existing features groups are replaced!
    #'
    #' @param groups A data.table with columns analysis, feature and group.
    #'
    #' @return Invisible.
    #'
    add_groups = function(groups = NULL) {

      if (is.data.frame(groups)) {

        if (all(c("analysis", "feature", "group") %in% colnames(groups))) {

          groups <- split(groups, groups$analysis)

          analyses <- self$get_analysis_names()

          groups <- groups[analyses]

          private$.analyses <- Map(
            function(x, y) {

              y <- y[!is.na(y$group), ]

              g_id <- y$group

              names(g_id) <- y$feature

              x$features$group <- NA_character_

              x_f_in_y <- x$features$feature %in% y$feature

              f_to_group <- x$features$feature[x_f_in_y]

              x$features$group[x_f_in_y] <- g_id[f_to_group]

              reg_filter <- is.na(x$features$group) & !x$features$filtered

              x$features$filtered[reg_filter] <- TRUE

              x$features$filter[reg_filter] <- "grouping"

              x
            },
            private$.analyses, groups
          )

          private$.register(
            "added",
            "analyses",
            "group_to_features",
            NA_character_,
            NA_character_,
            NA_character_
          )

          message(paste0("\U2713 ", length(unique(groups$group)), " unique groups added to features!"))
        }
      }
      invisible(self)
    },

    #' @description
    #' Adds time alignment results.
    #'
    #' @return Invisible.
    #'
    add_alignment = function(alignment = NULL) {
      if (is.list(alignment) &
        all(unname(self$get_analysis_names()) %in% names(alignment)) &
        self$has_groups()) {
        must_have_cols <- c(
          "rt_original", "rt_adjusted",
          "adjustment", "adjPoints"
        )

        alignment <- lapply(alignment, as.data.table)

        valid <- vapply(alignment, function(x, must_have_cols) {
          all(must_have_cols %in% colnames(x))
        }, FALSE, must_have_cols = must_have_cols)

        if (all(valid)) {
          private$.alignment <- alignment

          private$.register(
            "added",
            "MassSpecData",
            "alignment",
            NA_character_,
            NA_character_,
            NA_character_
          )

          message("\U2713 Alignment added!")

        } else {
          warning("Invalid alignment structure or content! Not added.")
        }

      } else {
        warning("Groups not present or alignment not valid! Not added.")
      }
      invisible(self)
    },

    #' @description
    #' Adds data from modules to the MassSpecData.
    #'
    #' @param value A named list with data from modules.
    #'
    #' @return Invisible.
    #'
    add_modules_data = function(value = NULL) {

      value_names <- names(value)

      if (!is.null(value_names)) {

        if (is.null(private$.modules)) private$.modules <- list()

        lapply(value_names, function(x, value) {

          if (is.null(value[[x]]$software)) value[[x]]$software <- NA_character_
          if (is.null(value[[x]]$version)) value[[x]]$version <- NA_character_

          private$.modules <- c(private$.modules, value[x])

          private$.register(
            "added",
            "module",
            x,
            value[[x]]$software,
            value[[x]]$version,
            length(value[[x]])
          )

          message(paste0("\U2713 ", x, " data added to modules!"))

        }, value = value)

      } else {
        warning("Not done, the value must be a named list!")
      }

      invisible(self)
    },

    ## ___ load -----

    #' @description
    #' Loads all spectra from all analyses.
    #'
    #' @return Invisible.
    #'
    load_spectra = function(runParallel = FALSE) {
      
      spec <- self$get_spectra(
        analyses = NULL, levels = NULL, mass = NULL,
        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
        allTraces = TRUE, isolationWindow = 1.3,
        minIntensityMS1 = 0, minIntensityMS2 = 0,
        runParallel = runParallel
      )

      split_vector <- spec$analysis
      
      spec$analysis <- NULL
      
      spec_list <- split(spec, split_vector)

      if (length(spec_list) == self$get_number_analyses()) {
        
        suppressMessages(self$add_spectra(spec_list, replace = TRUE))

        private$.register(
          "loaded",
          "analyses",
          "raw spectra",
          NA_character_,
          NA_character_,
          NA_character_
        )

        message("\U2713 Spectra loaded to all analyses!")

      } else {
        warning("Not done, check the MS file paths and formats!")
      }
      invisible(self)
    },

    #' @description
    #' Loads all chromatograms from all analyses.
    #'
    #' @return Invisible.
    #'
    load_chromatograms = function(runParallel = FALSE) {

      chrom <- self$get_chromatograms(
        analyses = NULL, minIntensity = 0,
        runParallel = runParallel
      )

      if (nrow(chrom) > 0) {
        split_vector <- chrom$analysis
        chrom$analysis <- NULL
        chrom_list <- split(chrom, split_vector)

        if (length(chrom_list) == self$get_number_analyses()) {
          private$.analyses <- Map(
            function(x, y) {
              x$chromatograms <- y
              x
            },
            private$.analyses, chrom_list
          )

          private$.register(
            "loaded",
            "analyses",
            "raw chromatograms",
            NA_character_,
            NA_character_,
            NA_character_
          )

          message("\U2713 Chromatograms loaded to all analyses!")

        } else {
          warning("Not done! Chromatograms not found.")
        }

      } else {
        warning("Not done! Chromatograms not found.")
      }
      invisible(self)
    },

    #' @description
    #' Loads features EICs in each analyses.
    #'
    #' @return Invisible.
    #'
    load_features_eic = function(settings = NULL) {
      
      if (!any(self$has_features())) {
        warning("Features not found! Not loaded.")
        return(invisible(self))
      }
      
      settings <- private$.get_call_settings(settings, "load_features_eic")
      
      if (is.null(settings)) return(invisible(self))

      algorithm <- settings$algorithm
      
      parameters <- settings$parameters

      if ("StreamFind" %in% algorithm) {

        feat_eics <- self$get_features_eic(
          rtExpand = parameters$rtExpand,
          mzExpand = parameters$mzExpand,
          filtered = parameters$filtered,
          loaded = FALSE,
          runParallel = parameters$runParallel
        )

        feat_eics_list <- split(feat_eics, feat_eics$analysis)

        feat_eics_list <- lapply(feat_eics_list, function(x) {
          x[["analysis"]] <- NULL
          x[["group"]] <- NULL
          x[["name"]] <- NULL
          x_l <- split(x, x$feature)
          x_l <- lapply(x_l, function(z) {
            z[["feature"]] <- NULL
            z
          })
          x_l
        })
        
        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }

        private$.register(
          "loaded",
          "features",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )

        message("\U2713 Feature EICs loaded!")

        self$add_features_eic(feat_eics_list, replace = TRUE)
        
        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }
      }

      invisible(self)
    },

    #' @description
    #' Loads and average MS1 spectra from features in the analyses.
    #'
    #' @return Invisible.
    #'
    load_features_ms1 = function(settings = NULL) {
      
      if (!any(self$has_features())) {
        warning("Features not found! Not loaded.")
        return(invisible(self))
      }

      settings <- private$.get_call_settings(settings, "load_features_ms1")
      
      if (is.null(settings)) return(invisible(self))

      algorithm <- settings$algorithm
      
      parameters <- settings$parameters

      if ("StreamFind" %in% algorithm) {

        cached_ms1 <- FALSE

        if (.caches_data()) {
          ana_feats <- self$get_features(filtered = TRUE)
          ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
          hash <- patRoon::makeHash(ana_feats, parameters)
          ms1 <- patRoon::loadCacheData("load_features_ms1", hash)

          if (!is.null(ms1)) {
            if (all(ms1$id %in% ana_feats$feature)) {
              message("\U2139 Features MS1 spectra loaded from cache!")
              cached_ms1 <- TRUE
            } else {
              ms1 <- NULL
            }
          } else {
            ms1 <- NULL
          }

        } else {
          hash <- NULL
          ms1 <- NULL
        }

        if (is.null(ms1)) {
          ms1 <- self$get_features_ms1(
            rtWindow = parameters$rtWindow,
            mzWindow = parameters$mzWindow,
            mzClust = parameters$mzClust,
            presence = parameters$presence,
            minIntensity = parameters$minIntensity,
            verbose = parameters$verbose,
            filtered = parameters$filtered,
            loadedMS1 = FALSE,
            runParallel = parameters$runParallel
          )
        }

        analyses <- self$get_analyses()

        analyses <- lapply(analyses, function(x, ms1) {

          ana <- x$name

          ana_ms1 <- ms1[ms1$analysis %in% ana, ]

          fts_all <- x$features$feature

          fts_ms1 <- lapply(fts_all, function(x2, ana_ms1) {

            ft_ms1 <- ana_ms1[ana_ms1$feature %in% x2, ]

            if (nrow(ft_ms1) > 0) {
              ft_ms1[["group"]] <- NULL
              ft_ms1[["analysis"]] <- NULL
              ft_ms1[["feature"]] <- NULL
              ft_ms1

            } else {
              NULL
            }
          }, ana_ms1 = ana_ms1)
          x$features$ms1 <- fts_ms1
          x
        }, ms1 = ms1)

        added_ms1 <- vapply(analyses, function(x) {
          "ms1" %in% colnames(x$features)
        }, FALSE)

        if (all(added_ms1)) {

          private$.analyses <- analyses

          if (!cached_ms1 & !is.null(hash)) {
            ms1_for_cache <- self$get_features_ms1(loadedMS1 = TRUE)
            patRoon::saveCacheData("load_features_ms1", ms1_for_cache, hash)
            message("\U1f5ab Features MS1 spectra cached!")
          }

          if (requireNamespace(settings$software, quietly = TRUE)) {
            version <- as.character(packageVersion(settings$software))
          } else {
            version <- NA_character_
          }

          private$.register(
            "loaded",
            "features",
            settings$call,
            settings$software,
            version,
            settings$algorithm
          )

          message("\U2713 MS1 spectra added to features in analyses!")
          
          if (!private$.settings_already_stored(settings)) {
            self$add_settings(settings)
          }
        }
      }
      invisible(self)
    },

    #' @description
    #' Loads and average MS2 spectra from features in the analyses.
    #'
    #' @return Invisible.
    #'
    load_features_ms2 = function(settings = NULL) {
      
      if (!any(self$has_features())) {
        warning("Features not found! Not loaded.")
        return(invisible(self))
      }

      settings <- private$.get_call_settings(settings, "load_features_ms2")
      
      if (is.null(settings)) return(invisible(self))

      algorithm <- settings$algorithm
      
      parameters <- settings$parameters

      if ("StreamFind" %in% algorithm) {

        cached_ms2 <- FALSE

        if (.caches_data()) {
          ana_feats <- self$get_features(filtered = TRUE)
          ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
          hash <- patRoon::makeHash(ana_feats, parameters)
          ms2 <- patRoon::loadCacheData("load_features_ms2", hash)

          if (!is.null(ms2)) {
            if (all(ms2$id %in% ana_feats$feature)) {
              message("\U2139 Features MS2 spectra loaded from cache!")
              cached_ms2 <- TRUE
            } else {
              ms2 <- NULL
            }
          } else {
            ms2 <- NULL
          }

        } else {
          hash <- NULL
          ms2 <- NULL
        }

        if (is.null(ms2)) {
          ms2 <- self$get_features_ms2(
            isolationWindow =  parameters$isolationWindow,
            mzClust = parameters$mzClust,
            presence = parameters$presence,
            minIntensity = parameters$minIntensity,
            verbose = parameters$verbose,
            filtered = parameters$filtered,
            loadedMS2 = FALSE,
            runParallel = parameters$runParallel
          )
        }

        analyses <- self$get_analyses()

        analyses <- lapply(analyses, function(x, ms2) {

          ana <- x$name

          ana_ms2 <- ms2[ms2$analysis %in% ana, ]

          fts_all <- x$features$feature

          fts_ms2 <- lapply(fts_all, function(x2, ana_ms2) {

            ft_ms2 <- ana_ms2[ana_ms2$feature %in% x2, ]

            if (nrow(ft_ms2) > 0) {
              ft_ms2[["group"]] <- NULL
              ft_ms2[["analysis"]] <- NULL
              ft_ms2[["feature"]] <- NULL
              ft_ms2

            } else {
              NULL
            }
          }, ana_ms2 = ana_ms2)

          x$features[["ms2"]] <- fts_ms2

          x
        }, ms2 = ms2)

        added_ms2 <- vapply(analyses, function(x) {
          "ms2" %in% colnames(x$features)
        }, FALSE)

        if (all(added_ms2)) {

          private$.analyses <- analyses

          if (!cached_ms2 & !is.null(hash)) {
            ms2_for_cache <- self$get_features_ms2(loadedMS2 = TRUE)
            message("\U1f5ab Features MS2 spectra cached!")
            patRoon::saveCacheData("load_features_ms2", ms2_for_cache, hash)
          }

          if (requireNamespace(settings$software, quietly = TRUE)) {
            version <- as.character(packageVersion(settings$software))
          } else {
            version <- NA_character_
          }

          private$.register(
            "loaded",
            "features",
            settings$call,
            settings$software,
            version,
            settings$algorithm
          )

          message("\U2713 MS2 spectra added to features in analyses!")
          
          if (!private$.settings_already_stored(settings)) {
            self$add_settings(settings)
          }
        }
      }
      invisible(self)
    },

    ## ___ remove -----

    #' @description
    #' Removes headers entries. Note that the name, path and date headers
    #' cannot be removed only changed.
    #'
    #' @param value A character vector with the name/s of the elements in headers
    #' to be removed.
    #'
    #' @return Invisible.
    #'
    remove_headers = function(value = NULL) {
      if (!is.null(value)) {
        value <- value[!(value %in% c("name", "author", "path", "date"))]

        if (length(value) == 0) {
          warning("Name, author, path and date headers cannot be removed!")
          value <- NA_character_
        }

        message("\U2713 Removed headers: ",
          paste(value[value %in% names(private$.headers)], collapse = ", ")
        )

        lapply(value, function(x) {
          if (x %in% names(private$.headers)) {
            private$.register(
              "removed",
              "ProjectHeaders",
              x,
              NA_character_,
              NA_character_,
              private$.headers[[x]]
            )

            private$.headers[x] <- NULL
          }
        })

      } else {
        to_remove <- names(private$.headers) %in% c("name", "author", "path", "date")
        to_remove <- names(private$.headers)[!to_remove]
        private$.headers[to_remove] <- NULL

        if (length(to_remove) > 1) {
          details <- paste(to_remove, collapse = ", ")

          private$.register(
            "removed",
            "ProjectHeaders",
            "all",
            NA_character_,
            NA_character_,
            details
          )

          message("\U2713 Removed headers: \n",
            paste(to_remove, collapse = "\n")
          )
        } else {
          message("\U2713 Removed all headers except name, author, path and date!")
        }
      }
      invisible(self)
    },

    #' @description
    #' Removes settings.
    #'
    #' @param call A string or a vector of strings with the name/s of the
    #' processing method/s to be removed. Alternatively, an integer vector
    #' with the index/es of the settings to be removed. When `call` is
    #' \code{NULL} all settings are removed.
    #'
    #' @return Invisible.
    #'
    remove_settings = function(call = NULL) {

      if (is.null(call)) {
        lapply(private$.settings, function(x) {
          private$.register(
            "removed",
            "ProcessingSettings",
            x$call,
            "StreamFind",
            x$version,
            x$algorithm
          )
        })

        private$.settings <- NULL

        cat("Removed all processing settings! \n")

      } else {
        all_calls <- vapply(private$.settings, function(x) x$call, NA_character_)

        if (is.numeric(call)) {
          to_remove <- call
        } else {
          to_remove <- which(all_calls %in% call)
        }

        if (length(call) > 0) {
          lapply(private$.settings[to_remove], function(x) {
            private$.register(
              "removed",
              "ProcessingSettings",
              x$call,
              "StreamFind",
              x$version,
              x$algorithm
            )
          })

          private$.settings[to_remove] <- NULL

          message("\U2713 Removed settings for:\n",
            paste(all_calls[to_remove], collapse = "\n")
          )

        } else {
          message("\U2717 There are no settings to remove!")
        }
      }
      invisible(self)
    },

    #' @description
    #' Removes analyses. Note that unique feature
    #' groups from the removed analyses are also removed.
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
          private$.alignment <- private$.alignment[keepAnalyses]

          lapply(removeAnalyses, function(x) {
            private$.register(
              "removed",
              "MassSpecAnalysis",
              x,
              NA_character_,
              NA_character_,
              NA_character_
            )
          })

          message("\U2713 Removed analyses:\n", paste(removeAnalyses, collapse = "\n"))

        } else {
          message("\U2717 There are no analyses to remove!")
        }

      } else {
        lapply(private$.analyses, function(x) {
          private$.register(
            "removed",
            "MassSpecAnalysis",
            x,
            NA_character_,
            NA_character_,
            NA_character_
          )
        })

        private$.analyses <- NULL
        private$.alignment <- NULL

        message("\U2713 Removed all analyses!")
      }

      invisible(self)
    },

    #' @description
    #' Remove features.
    #'
    #' @return Invisible.
    #'
    remove_features = function(features = NULL, filtered = FALSE) {

      if (is.null(features) & !filtered) {

        private$.analyses <- lapply(private$.analyses, function(x) {
          x$features <- data.table()
          x$features_eic <- list()
          x
        })

        private$.register(
          "removed",
          "features",
          "all",
          NA_character_,
          NA_character_,
          NA_character_
        )

        message("\U2713 Removed all features!")

        return(invisible(self))
      }

      if (is.data.frame(features) | filtered) {
        org_fts <- self$get_features(filtered = TRUE)
        n_org <- nrow(org_fts)

        if (n_org > 0) {
          unique_fts_ids <- paste0(org_fts$analysis, org_fts$feature)

          if (is.data.frame(features)) {
            if (all(c("analysis", "feature") %in% colnames(features))) {
              rem_fts <- paste0(features$analysis, features$feature)
              rem_fts <- !unique_fts_ids %in% rem_fts
              org_fts <- org_fts[rem_fts, ]
            }
          }

          if (filtered) {
            org_fts <- org_fts[!org_fts$filtered, ]
          }

          n_org_new <- nrow(org_fts)

          if (n_org_new < n_org) {

            private$.analyses <- lapply(private$.analyses, function(x, org_fts) {
              temp <- org_fts[org_fts$analysis %in% x$name, ]
              temp[["analysis"]] <- NULL
              x$features <- temp
              if (length(x$features_eic) > 0) x$features_eic <- x$features_eic[temp$feature]
              x
            }, org_fts = org_fts)

            private$.register(
              "removed",
              "features",
              n_org - n_org_new,
              NA_character_,
              NA_character_,
              NA_character_
            )

            message("\U2713 Removed ", n_org - n_org_new, " features!")

          } else {
            message("\U2717 There are no features to remove!")
          }
        } else {
          message("\U2717 There are no features to remove!")
        }
      } else {
        message("\U2717 There are no features to remove!")
      }
      invisible(self)
    },

    #' @description
    #' Removes loaded MS1 spectra from features in the analyses. In practice,
    #' the column \emph{ms1} in the features data.table of each analysis object
    #' is removed.
    #'
    #' @return Invisible.
    #'
    remove_features_ms1 = function() {
      if (any(self$has_features())) {
        if (any(self$has_loaded_features_ms1())) {
          private$.analyses <- lapply(private$.analyses, function(x) {
            x$features$ms1 <- NULL
            x
          })

          private$.register(
            "removed",
            "features_ms1",
            "all",
            NA_character_,
            NA_character_,
            NA_character_
          )

          message("\U2713 Removed all MS1 spectra from features!")

        } else {
          message("\U2717 Features MS1 spectra not loaded!")
        }
      } else {
        message("\U2717 Features not present!")
      }
      invisible(self)
    },

    #' @description
    #' Removes loaded MS2 spectra from features in the analyses. In practice,
    #' the column \emph{ms2} in the features data.table of each analysis object
    #' is removed.
    #'
    #' @return Invisible.
    #'
    remove_features_ms2 = function() {
      if (any(self$has_features())) {
        if (any(self$has_loaded_features_ms2())) {
          private$.analyses <- lapply(private$.analyses, function(x) {
            x$features$ms2 <- NULL
            x
          })

          private$.register(
            "removed",
            "features_ms2",
            "all",
            NA_character_,
            NA_character_,
            NA_character_
          )

          message("\U2713 Removed all MS2 spectra from features!")
        } else {
          message("\U2717 Features MS2 spectra not loaded!")
        }
      } else {
        message("\U2717 Features not present!")
      }
      invisible(self)
    },

    #' @description
    #' Removes feature groups.
    #'
    #' @return Invisible.
    #'
    remove_groups = function(groups = NULL, filtered = FALSE) {

      if (is.null(groups) & !filtered) {
        private$.alignment <- NULL
        private$.analyses <- lapply(private$.analyses, function(x) {
          x$features[["group"]] <- NULL
          x$features$filter[x$features$filter %in% "grouping"] <- NA_character_
          x$features$filtered[is.na(x$features$filter)] <- FALSE
          x
        })

        private$.register(
          "removed",
          "feature groups",
          "all",
          NA_character_,
          NA_character_,
          NA_character_
        )

        message("\U2713 Removed all groups!")

        return(invisible(self))
      }
      
      if (self$has_groups()) {
        all_features <- self$get_features(filtered = TRUE)
        all_groups <- sort(unique(all_features$group[!is.na(all_features$group)]))
        
        if (is.numeric(groups)) groups <- all_groups[groups]
        
        if (filtered) {
          filtered_groups <- all_features$group[all_features$filtered]
          groups <- c(groups, filtered_groups)
          groups <- unique(groups)
        }
        
        if (is.character(groups) & length(groups) > 0) {
          n_org_g <- length(all_groups)
          keep_groups <- !all_groups %in% groups
          
          if (!all(keep_groups)) {
            
            private$.analyses <- lapply(private$.analyses, function(x, groups) {
              x$features$group[x$features$group %in% groups] <- NA
              NA_groups <- is.na(x$features$group)
              x$features$filter[NA_groups & !x$features$filtered] <- "grouping"
              x$features$filtered[NA_groups & !x$features$filtered] <- TRUE
              x$features <- x$features[order(x$features$mz), ]
              x$features <- x$features[order(x$features$rt), ]
              x$features <- x$features[order(x$features$filtered), ]
              
              if (length(x$features_eic) > 0) x$features_eic <- x$features_eic[x$features$feature]
              
              x
            }, groups = groups)
            n_g <- nrow(private$.groups)
            
            private$.register(
              "removed",
              "feature groups",
              n_org_g - n_g,
              NA_character_,
              NA_character_,
              NA_character_
            )
            
            message("\U2713 Removed ", n_org_g - n_g, " groups!")
            
          } else {
            message("\U2717 There are no groups to remove!")
          }
        } else {
          message("\U2717 There are no groups to remove!")
        }
      } else {
        message("\U2717 There are no groups to remove!")
      }
      
      invisible(self)
    },

    #' @description
    #' Removes alignment results.
    #'
    #' @return Invisible.
    #'
    remove_alignment = function() {
      private$.alignment <- NULL
      private$.register(
        "removed",
        "alignment",
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_
      )
      message("\U2713 Removed alignment!")
      invisible(self)
    },

    ## ___ subset -----

    #' @description
    #' Subsets a `MassSpecData` object on analyses.
    #'
    #' @return A new cloned `MassSpecData` object with only the analyses as
    #' defined by the `analyses` argument.
    #'
    subset_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)

      if (!is.null(analyses)) {
        allNames <- self$get_analysis_names()
        removeAnalyses <- unname(allNames[!(allNames %in% analyses)])
        keepAnalyses <- unname(allNames[allNames %in% analyses])

        if (length(keepAnalyses) > 0) {
          newAnalyses <- self$get_analyses(keepAnalyses)
          newAlignment <- self$get_alignment()[keepAnalyses]

          new_ms <- suppressMessages(MassSpecData$new(
            files = NULL,
            headers = self$get_headers(),
            settings = self$get_settings(),
            analyses = newAnalyses,
            groups = NULL,
            alignment = newAlignment
          ))

          message("\U2713 Subset with ", new_ms$get_number_analyses(), " analyses created!")

          return(new_ms)
        }
      }

      message("\U2717 There are no analyses selected to subset!")
      
      suppressMessages(MassSpecData$new(
        files = NULL,
        headers = self$get_headers(),
        settings = self$get_settings())
      )
    },

    #' @description
    #' Subsets a `MassSpecData` object on features from analyses.
    #'
    #' @return A new cloned `MassSpecData` object with only the features as
    #' defined by the `features` argument.
    #'
    subset_features = function(features = NULL) {
      if (is.data.frame(features)) {
        cols_must_have <- c("analysis", "feature")
        if (all(cols_must_have %in% colnames(features))) {
          all_fts <- self$get_features()
          n_all <- nrow(all_fts)

          if (n_all > 0) {
            unique_fts_ids <- paste0(all_fts$analysis, all_fts$feature)
            keep_fts <- paste0(features$analysis, features$feature)
            rem_fts <- !(unique_fts_ids %in% keep_fts)
            rem_fts <- all_fts[rem_fts, cols_must_have, with = FALSE]

            if (nrow(rem_fts) > 0) {
              new_ms <- self$clone()
              new_ms <- suppressMessages(new_ms$remove_features(rem_fts, filtered = TRUE))
              message("\U2713 Subset with ", nrow(new_ms$get_features()), " features created!")
              return(new_ms)

            } else {
              message("\U2717 There are no features to subset!")
            }
          } else {
            message("\U2717 There are no features to subset!")
          }
        } else {
          message("\U2717 Data.frame with analysis and feature IDs not given!")
        }
      } else {
        message("\U2717 Data.frame with analysis and feature IDs not given!")
      }
      suppressMessages(MassSpecData$new())
    },

    #' @description
    #' Subsets a `MassSpecData` object on groups from correspondence of features
    #' across analyses. Note that when sub-setting groups, features that lose
    #' correspondence are not removed but filtered with "grouping" added as
    #' filter category/tag. Filtered features can be removed with the method
    #' `remove_features(filtered = TRUE)`.
    #'
    #' @return A new cloned `MassSpecData` object with only the groups as
    #' defined by the `groups` argument.
    #'
    subset_groups = function(groups = NULL) {
      if (self$has_groups() & !is.null(groups)) {
        all_features <- self$get_features(filtered = TRUE)
        all_groups <- sort(unique(all_features$group[!is.na(all_features$group)]))
        
        if (is.numeric(groups)) groups <- all_groups[groups]
        
        groups_rem <- all_groups[!all_groups %in% groups]

        if (length(groups_rem) > 0) {
          new_ms <- self$clone(deep = TRUE)
          new_ms <- suppressMessages(new_ms$remove_groups(groups_rem, filtered = TRUE))
          message("\U2713 Subset with ", length(all_groups) - length(groups_rem), " feature groups created!")
          return(new_ms)

        } else {
          message("\U2717 There are no groups to subset!")
        }
      } else {
        message("\U2717 There are no groups to subset!")
      }
      suppressMessages(MassSpecData$new())
    },

    ## ___ has -----

    #' @description
    #' Checks if analyses have have drift time from ion mobility.
    #'
    #' @return Logical value.
    #'
    has_ion_mobility = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(FALSE)

      has_im <- vapply(
        private$.analyses[analyses],
        function(x) x$has_ion_mobility, FALSE
      )

      names(has_im) <- self$get_analysis_names(analyses)

      has_im
    },

    #' @description
    #' Checks if analyses are present.
    #'
    #' @return Logical value.
    #'
    has_analyses = function() {
      length(private$.analyses) > 0
    },

    #' @description
    #' Checks for loaded spectra in given analyses names/indices.
    #'
    #' @return Logical value.
    #'
    has_loaded_spectra = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) {
        return(FALSE)
      }

      has_spectra <- vapply(
        private$.analyses[analyses],
        function(x) nrow(x$spectra) > 0, FALSE
      )

      names(has_spectra) <- self$get_analysis_names(analyses)
      has_spectra
    },

    #' @description
    #' Checks for loaded chromatograms in given analyses names/indices.
    #'
    #' @return Logical value.
    #'
    has_loaded_chromatograms = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) {
        return(FALSE)
      }

      has_chromatograms <- vapply(
        private$.analyses[analyses],
        function(x) nrow(x$chromatograms) > 0, FALSE
      )

      names(has_chromatograms) <- self$get_analysis_names(analyses)
      has_chromatograms
    },

    #' @description
    #' Checks for loaded features MS1 in given analyses names/indices.
    #'
    #' @return Logical value.
    #'
    has_loaded_features_ms1 = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) {
        return(FALSE)
      }

      has_loaded_ms1 <- vapply(private$.analyses[analyses], function(x) {
        if ("ms1" %in% colnames(x$features)) {
          any(vapply(x$features$ms1, is.data.frame, FALSE))
        } else {
          FALSE
        }
      }, FALSE)

      has_loaded_ms1
    },

    #' @description
    #' Checks for loaded features MS2 in given analyses names/indices.
    #'
    #' @return Logical value.
    #'
    has_loaded_features_ms2 = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) {
        return(FALSE)
      }

      has_loaded_ms2 <- vapply(private$.analyses[analyses], function(x) {
        if ("ms2" %in% colnames(x$features)) {
          any(vapply(x$features$ms2, is.data.frame, FALSE))
        } else {
          FALSE
        }
      }, FALSE)

      has_loaded_ms2
    },

    #' @description
    #' Checks if there are processing settings.
    #'
    #' @param call A string or a vector of strings with the name/s of the
    #' processing method/s.
    #'
    #' @return Logical value.
    #'
    has_settings = function(call = NULL) {
      if (is.null(call)) {
        length(private$.settings) > 0
      } else if (length(private$.settings) > 0) {
        call %in% vapply(private$.settings, function(x) x$call, NA_character_)
      } else {
        FALSE
      }
    },

    #' @description
    #' Checks for presence of feature extracted ion chromatograms (EICs) in
    #' given analyses names/indices.
    #'
    #' @return Logical value.
    #'
    has_features_eic = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(FALSE)

      has_eics <- vapply(private$.analyses[analyses], function(x) {

        if (length(x$features_eic) > 0) {

          if (!all(vapply(x$features_eic, is.null, TRUE))) {
            TRUE
          } else {
            FALSE
          }
        } else {
          FALSE
        }
      }, FALSE)

      names(has_eics) <- self$get_analysis_names(analyses)
      has_eics
    },

    #' @description
    #' Checks if given analyses have features.
    #'
    #' @return Logical value.
    #'
    has_features = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) {
        return(FALSE)
      }

      has_fts <- vapply(
        private$.analyses[analyses],
        function(x) nrow(x$features) > 0, FALSE
      )

      names(has_fts) <- self$get_analysis_names(analyses)
      has_fts
    },
    
    #' @description
    #' Checks for presence of suspects in given analyses names/indices.
    #'
    #' @return Logical value.
    #'
    has_suspects = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(FALSE)
      
      has_sus <- vapply(private$.analyses[analyses], function(x) {
        
        if ("suspects" %in% colnames(x$features)) {
          any(vapply(x$features$suspects, function(x) !is.null(x), FALSE))
        } else {
          FALSE
        }
        
      }, FALSE)
      
      names(has_sus) <- self$get_analysis_names(analyses)
      has_sus
    },

    #' @description
    #' Checks if there is alignment of retention time from grouping
    #' features across analyses.
    #'
    #' @return Logical value.
    #'
    has_alignment = function() {
      !is.null(private$.alignment)
    },

    #' @description
    #' Checks if there are feature groups from grouping features
    #' across analyses.
    #'
    #' @return Logical value.
    #'
    has_groups = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(FALSE)
      
      if (!all(self$has_features())) return(FALSE)
      
      if (all(vapply(private$.analyses[analyses], function(x) "group" %in% colnames(x$features), FALSE))) {
        any(vapply(private$.analyses[analyses], function(x) any(!is.na(x$features$group)), FALSE))
      } else {
        FALSE 
      }
    },

    ## ___ plot -----

    #' @description
    #' Plots spectra for given MS analyses.
    #'
    #' @param colorBy A string of length 1. One of `analyses` (the default),
    #' `polarities`, `levels`, `targets` or `replicates`.
    #' @param xVal Character length one. Possible values are "mz", "rt" or "drift".
    #' @param yVal Character length one. Possible values are "mz", "rt" or "drift".
    #' @param zLab A string with the title for the z axis.
    #'
    #'
    #'
    #' @return A 3D interactive plot.
    #'
    plot_spectra = function(analyses = NULL,
                            levels = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            id = NULL,
                            allTraces = TRUE,
                            isolationWindow = 1.3,
                            minIntensityMS1 = 0,
                            minIntensityMS2 = 0,
                            runParallel = FALSE,
                            legendNames = NULL,
                            colorBy = "analyses",
                            xVal = "rt",
                            yVal = "mz",
                            xLab = NULL,
                            yLab = NULL,
                            zLab = NULL) {

      spec <- self$get_spectra(
        analyses, levels,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        allTraces = allTraces,
        isolationWindow,
        minIntensityMS1,
        minIntensityMS2,
        runParallel
      )

      if (nrow(spec) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      if ("drift" %in% c(xVal, yVal)) {
        if (!"drift" %in% colnames(spec)) {
          warning("Drift time values not found!")
          return(NULL)
        }
      }

      if ("feature" %in% colnames(spec)) spec$id <- spec$feature

      if ("replicates" %in% colorBy) {
        spec$replicate <- self$get_replicate_names()[spec$analysis]
      }

      .plot_spectra_interactive(spec, colorBy, legendNames, xVal, yVal, xLab, yLab, zLab)
    },
    
    #' @description
    #' Plots chromatograms in the analyses.
    #'
    #' @return A plot.
    #'
    plot_chromatograms = function(analyses = NULL,
                                  title = NULL,
                                  colorBy = "targets",
                                  legendNames = NULL,
                                  showLegend = TRUE,
                                  xlim = NULL,
                                  ylim = NULL,
                                  cex = 0.6,
                                  interactive = TRUE) {
      
      chromatograms <- self$get_chromatograms(analyses)
      
      if (nrow(chromatograms) == 0) {
        message("\U2717 Chromatograms not found for the analyses!")
        return(NULL)
      }
      
      if ("replicates" %in% colorBy) {
        chromatograms$replicate <- self$get_replicate_names()[chromatograms$analysis]
      }
      
      # if (!"id" %in% colnames(chromatograms)) tic$id <- tic$analysis
      
      # polarities <- self$get_polarities()
      # polarities_names <- unique(names(polarities))
      
      pol_key <- c("positive", "negative", "nd")
      names(pol_key) <- c("1", "-1", "0")
      chromatograms$polarity <- as.character(chromatograms$polarity)
      chromatograms$polarity <- pol_key[chromatograms$polarity]
      
      if (!interactive) {
        .plot_eic_static(chromatograms, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
        NULL
      } else {
        .plot_chromatograms_interactive(chromatograms, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description
    #' Plots extract ion chromatograms (EIC) and \emph{m/z} vs
    #' retention time from the analyses.
    #'
    #' @param plotTargetMark Logical, set to \code{TRUE} to plot a target mark.
    #'
    #' @param targetsMark A data.frame with columns `mz` and `rt`, defining the
    #' \emph{m/z} and retention time values of each target. Note that the number
    #'  of rows should match with the number of targets.
    #'
    #' @param ppmMark A numeric vector of length one to define the mass deviation,
    #' in ppm, of the target mark. The default is 5 ppm.
    #'
    #' @param secMark A numeric vector of length one to define the time deviation,
    #' in seconds, of the target mark. The default is 10 ppm.
    #'
    #' @param numberRows An integer vector of length one to define the number of
    #' rows to grid the plots. Note that each target is always plotted in one row
    #' for all selected analyses.
    #'
    #' @return An interactive plot.
    #'
    plot_xic = function(analyses = NULL,
                        mass = NULL,
                        mz = NULL,
                        rt = NULL,
                        drift = NULL,
                        ppm = 20,
                        sec = 60,
                        millisec = 5,
                        id = NULL,
                        runParallel = FALSE,
                        legendNames = NULL,
                        plotTargetMark = TRUE,
                        targetsMark = NULL,
                        ppmMark = 5,
                        secMark = 10,
                        numberRows = 1) {

      xic <- self$get_spectra(
        analyses,
        levels = 1,
        mass,
        mz, rt, drift, ppm, sec, millisec, id,
        allTraces = TRUE,
        isolationWindow = 1.3,
        minIntensityMS1 = 0,
        minIntensityMS2 = 0,
        runParallel = runParallel
      )

      if (nrow(xic) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      .plot_xic_interactive(
        xic,
        legendNames,
        plotTargetMark,
        targetsMark,
        ppmMark,
        secMark,
        numberRows
      )
    },

    #' @description
    #' Plots extract ion chromatograms (EIC) from the analyses based on targets.
    #'
    #' @return A plot.
    #'
    plot_eic = function(analyses = NULL,
                        mass = NULL,
                        mz = NULL,
                        rt = NULL,
                        drift = NULL,
                        ppm = 20,
                        sec = 60,
                        millisec = 5,
                        id = NULL,
                        runParallel = FALSE,
                        legendNames = NULL,
                        title = NULL,
                        colorBy = "targets",
                        showLegend = TRUE,
                        xlim = NULL,
                        ylim = NULL,
                        cex = 0.6,
                        interactive = TRUE) {

      eic <- self$get_eic(analyses, mass, mz, rt, drift, ppm, sec, millisec, id, runParallel)

      if (nrow(eic) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        eic$replicate <- self$get_replicate_names()[eic$analysis]
      }

      if (!interactive) {
        .plot_eic_static(eic, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_eic_interactive(eic, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description
    #' Plots the total ion chromatogram (TIC) of the analyses.
    #'
    #' @return A plot.
    #'
    plot_tic = function(analyses = NULL,
                        levels = c(1, 2),
                        title = NULL,
                        colorBy = "analyses",
                        legendNames = NULL,
                        showLegend = TRUE,
                        xlim = NULL,
                        ylim = NULL,
                        cex = 0.6,
                        interactive = TRUE) {

      tic <- self$get_tic(analyses, levels)

      if (nrow(tic) == 0) {
        message("\U2717 TIC not found for the analyses!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        tic$replicate <- self$get_replicate_names()[tic$analysis]
      }

      if (!"id" %in% colnames(tic)) tic$id <- tic$analysis

      # polarities <- self$get_polarities()
      # polarities_names <- unique(names(polarities))
      # 
      # if (length(polarities) > length(polarities_names) &
      #   colorBy %in% c("analyses", "levels", "replicates")) {
      # 
      #   pol_key <- c("positive", "negative", "nd")
      #   names(pol_key) <- c("1", "-1", "0")
      #   tic$polarity <- as.character(tic$polarity)
      #   tic$polarity <- pol_key[tic$polarity]
      # 
      #   if (grepl("analyses", colorBy)) {
      #     tic$analysis <- paste0(tic$analysis, "/", tic$polarity)
      #   } else if (grepl("replicates", colorBy)) {
      #     tic$replicate <- paste0(tic$replicate, "/", tic$polarity)
      #   } else {
      #     tic$level <- paste("MS", tic$level, sep = "")
      #     tic$level <- factor(tic$level)
      #     tic$level <- paste0(tic$level, "/", tic$polarity)
      #   }
      # }

      if (!interactive) {
        .plot_eic_static(tic, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_eic_interactive(tic, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description
    #' Plots the base peak chromatogram (BPC) of the analyses.
    #'
    #' @return A plot.
    #'
    plot_bpc = function(analyses = NULL,
                        levels = c(1, 2),
                        title = NULL,
                        colorBy = "analyses",
                        legendNames = NULL,
                        showLegend = TRUE,
                        xlim = NULL,
                        ylim = NULL,
                        cex = 0.6,
                        interactive = TRUE) {

      bpc <- self$get_bpc(analyses, levels)

      if (nrow(bpc) == 0) {
        message("\U2717 BPC not found for the analyses!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        bpc$replicate <- self$get_replicate_names()[bpc$analysis]
      }

      if (!"id" %in% colnames(bpc)) bpc$id <- bpc$analysis

      # polarities <- self$get_polarities()
      # polarities_names <- unique(names(polarities))
      # 
      # if (length(polarities) > length(polarities_names) &
      #     colorBy %in% c("analyses", "levels", "replicates")) {
      # 
      #   pol_key <- c("positive", "negative", "nd")
      #   names(pol_key) <- c("1", "-1", "0")
      #   bpc$polarity <- as.character(bpc$polarity)
      #   bpc$polarity <- pol_key[bpc$polarity]
      # 
      #   if (grepl("analyses", colorBy)) {
      #     bpc$analysis <- paste0(bpc$analysis, "/", bpc$polarity)
      #   } else if (grepl("replicates", colorBy)) {
      #     bpc$replicate <- paste0(bpc$replicate, "/", bpc$polarity)
      #   } else {
      #     bpc$level <- paste("MS", bpc$level, sep = "")
      #     bpc$level <- factor(bpc$level)
      #     bpc$level <- paste0(bpc$level, "/", bpc$polarity)
      #   }
      # }

      if (!interactive) {
        .plot_eic_static(bpc, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_bpc_interactive(bpc, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description
    #' Plots MS2 spectra from the analyses based on targets.
    #'
    #' @return A plot.
    #'
    plot_ms2 = function(analyses = NULL,
                        mass = NULL,
                        mz = NULL,
                        rt = NULL,
                        drift = NULL,
                        ppm = 20,
                        sec = 60,
                        millisec = 5,
                        id = NULL,
                        isolationWindow = 1.3,
                        mzClust = 0.005,
                        presence = 0.8,
                        verbose = FALSE,
                        minIntensity = 0,
                        runParallel = FALSE,
                        legendNames = NULL,
                        title = NULL,
                        colorBy = "targets",
                        interactive = TRUE) {

      ms2 <- self$get_ms2(
        analyses, mass, mz, rt, drift, ppm, sec, millisec, id, isolationWindow,
        mzClust, presence, verbose, minIntensity, runParallel
      )

      if (nrow(ms2) == 0) {
        message("\U2717 MS2 traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        ms2$replicate <- self$get_replicate_names()[ms2$analysis]
      }

      if (!interactive) {
        .plot_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        .plot_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Plots MS1 spectra from the analyses based on targets.
    #' 
    #' @param showText X.
    #'
    #' @return A plot.
    #'
    plot_ms1 = function(analyses = NULL,
                        mass = NULL,
                        mz = NULL,
                        rt = NULL,
                        drift = NULL,
                        ppm = 20,
                        sec = 60,
                        millisec = 5,
                        id = NULL,
                        mzClust = 0.003,
                        presence = 0.8,
                        verbose = FALSE,
                        minIntensity = 1000,
                        runParallel = FALSE,
                        legendNames = NULL,
                        title = NULL,
                        colorBy = "targets",
                        showText = TRUE,
                        interactive = TRUE) {

      ms1 <- self$get_ms1(
        analyses, mass, mz, rt, drift, ppm, sec, millisec, id, mzClust, presence,
        verbose, minIntensity, runParallel
      )

      if (nrow(ms1) == 0) {
        message("\U2717 MS1 traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        ms1$replicate <- self$get_replicate_names()[ms1$analysis]
      }

      if (!interactive) {
        .plot_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        .plot_ms1_interactive(ms1, legendNames, colorBy, title, showText)
      }
    },

    #' @description
    #' Plots features from analyses.
    #'
    #' @return plot.
    #'
    plot_features = function(analyses = NULL,
                             features = NULL,
                             mass = NULL,
                             mz = NULL,
                             rt = NULL,
                             drift = NULL,
                             ppm = 20,
                             sec = 60,
                             millisec = 5,
                             rtExpand = 120,
                             mzExpand = NULL,
                             loaded = TRUE,
                             filtered = FALSE,
                             runParallel = FALSE,
                             legendNames = NULL,
                             title = NULL,
                             colorBy = "targets",
                             showLegend = TRUE,
                             xlim = NULL,
                             ylim = NULL,
                             cex = 0.6,
                             interactive = TRUE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) {
        message("\U2717 Features not found for the targets!")
        return(NULL)
      }

      eic <- self$get_features_eic(
        analyses = unique(fts$analysis),
        features = fts,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        filtered = filtered,
        loaded = loaded,
        runParallel = runParallel
      )

      eic <- eic[, `:=`(intensity = sum(intensity)),
        by = c("analysis", "polarity", "feature", "rt")
      ][]

      if (nrow(eic) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        eic$replicate <- self$get_replicate_names()[eic$analysis]
      }

      if (!interactive) {
        .plot_features_static(eic, fts, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_features_interactive(eic, fts, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description
    #' Plots a map of the retention time vs \emph{m/z} of features from analyses.
    #'
    #' @return A plot.
    #'
    map_features = function(analyses = NULL,
                            features = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE,
                            legendNames = NULL,
                            title = NULL,
                            colorBy = "targets",
                            showLegend = TRUE,
                            xlim = 30,
                            ylim = 0.05,
                            cex = 0.6,
                            interactive = TRUE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) {
        message("\U2717 Features not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        fts$replicate <- self$get_replicate_names()[fts$analysis]
      }

      if (!interactive) {
        .map_features_static(fts, colorBy, legendNames, title, showLegend, xlim, ylim, cex)
      } else {
        .map_features_interactive(fts, colorBy, legendNames, xlim, ylim, title)
      }
    },

    #' @description
    #' Plots MS1 spectra from features in the analyses.
    #'
    #' @return A plot.
    #'
    plot_features_ms1 = function(analyses = NULL,
                                 features = NULL,
                                 mass = NULL,
                                 mz = NULL,
                                 rt = NULL,
                                 drift = NULL,
                                 ppm = 20,
                                 sec = 60,
                                 millisec = 5,
                                 rtWindow = c(-2, 2),
                                 mzWindow = c(-5, 100),
                                 mzClust = 0.003,
                                 presence = 0.8,
                                 minIntensity = 1000,
                                 verbose = FALSE,
                                 filtered = FALSE,
                                 loadedMS1 = TRUE,
                                 runParallel = FALSE,
                                 legendNames = NULL,
                                 title = NULL,
                                 colorBy = "targets",
                                 interactive = TRUE) {

      ms1 <- self$get_features_ms1(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec,
        rtWindow, mzWindow, mzClust, presence, minIntensity,
        verbose, filtered, loadedMS1, runParallel
      )

      if (nrow(ms1) == 0) {
        message("\U2717 MS1 traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        ms1$replicate <- self$get_replicate_names()[ms1$analysis]
      }

      if (!interactive) {
        .plot_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        .plot_ms1_interactive(ms1, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Plots MS2 spectra from features in the analyses.
    #'
    #' @return A plot.
    #'
    plot_features_ms2 = function(analyses = NULL,
                                 features = NULL,
                                 mass = NULL,
                                 mz = NULL,
                                 rt = NULL,
                                 drift = NULL,
                                 ppm = 20,
                                 sec = 60,
                                 millisec = 5,
                                 isolationWindow = 1.3,
                                 mzClust = 0.005,
                                 presence = 0.8,
                                 minIntensity = 0,
                                 verbose = FALSE,
                                 filtered = FALSE,
                                 loadedMS2 = TRUE,
                                 runParallel = FALSE,
                                 legendNames = NULL,
                                 title = NULL,
                                 colorBy = "targets",
                                 interactive = TRUE) {

      ms2 <- self$get_features_ms2(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec,
        isolationWindow, mzClust, presence, minIntensity,
        verbose, filtered, loadedMS2, runParallel
      )

      if (nrow(ms2) == 0) {
        message("\U2717 MS2 traces not found for the targets!")
        return(NULL)
      }
      if (grepl("replicates", colorBy)) {
        ms2$replicate <- self$get_replicate_names()[ms2$analysis]
      }

      if (!interactive) {
        .plot_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        .plot_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Plots the results from the retention time alignment across analyses.
    #'
    #' @return A plot with the retention time alignment differences
    #' for each sample.
    #'
    plot_alignment = function() {
      if (!self$has_alignment()) {
        warning("\U2717 Adjusted retention time not found!")
        return(NULL)
      }

      alignment <- private$.alignment
      colors <- .get_colors(names(alignment))

      xaxis <- list(
        linecolor = toRGB("black"),
        linewidth = 2,
        title = "Retention time / seconds",
        titlefont = list(size = 12, color = "black")
      )
      yaxis <- list(
        linecolor = toRGB("black"),
        linewidth = 2,
        title = "RT<sub>Raw</sub> - RT<sub>Adjusted</sub> / seconds",
        titlefont = list(size = 12, color = "black")
      )

      plot <- plot_ly()

      for (i in names(alignment)) {
        df <- alignment[[i]]

        plot <- plot %>% add_trace(
          x = df$rt_original,
          y = df$adjustment,
          type = "scatter",
          mode = "lines",
          line = list(
            shape = "spline", width = 0.5,
            color = colors[i]
          ),
          name = i,
          legendgroup = i,
          showlegend = TRUE
        )

        df_pt <- df[!is.na(df$adjPoints), ]

        plot <- plot %>% add_trace(
          x = df_pt$adjPoints,
          y = df_pt$adjustment,
          type = "scatter",
          mode = "markers",
          marker = list(
            size = 5,
            color = colors[i]
          ),
          name = i,
          legendgroup = i,
          showlegend = FALSE
        )
      }

      plot <- plot %>% plotly::layout(
        legend = list(title = list(text = "<b> Analyses: </b>")),
        xaxis = xaxis, yaxis = yaxis
      )

      plot
    },

    #' @description
    #' Plots feature groups EIC.
    #'
    #' @return A plot.
    #'
    plot_groups = function(groups = NULL,
                           mass = NULL,
                           mz = NULL,
                           rt = NULL,
                           drift = NULL,
                           ppm = 20,
                           sec = 60,
                           millisec = 5,
                           rtExpand = 15,
                           mzExpand = 0.005,
                           filtered = FALSE,
                           runParallel = FALSE,
                           legendNames = NULL,
                           title = NULL,
                           colorBy = "targets",
                           showLegend = TRUE,
                           xlim = NULL,
                           ylim = NULL,
                           cex = 0.6,
                           interactive = TRUE) {

      fts <- self$get_features(
        analyses = NULL,
        groups, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (grepl("targets", colorBy) & !isTRUE(legendNames)) {
        fts$name <- fts$group
        if (is.null(legendNames)) legendNames <- TRUE
      }

      self$plot_features(
        features = fts,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        filtered = filtered,
        runParallel = runParallel,
        legendNames = legendNames,
        title = title,
        colorBy = colorBy,
        showLegend = showLegend,
        xlim = xlim,
        ylim = ylim,
        cex = cex,
        interactive = interactive
      )
    },

    #' @description
    #' Plots MS1 spectra from feature groups in the analyses.
    #'
    #' @return A plot.
    #'
    plot_groups_ms1 = function(groups = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               drift = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               rtWindow = c(-2, 2),
                               mzWindow = c(-5, 90),
                               mzClustFeatures = 0.005,
                               presenceFeatures = 0.8,
                               minIntensityFeatures = 1000,
                               loadedFeaturesMS1 = TRUE,
                               mzClust = 0.005,
                               presence = 0.8,
                               minIntensity = 1000,
                               groupBy = "groups",
                               verbose = FALSE,
                               filtered = FALSE,
                               runParallel = FALSE,
                               legendNames = NULL,
                               title = NULL,
                               colorBy = "targets",
                               interactive = TRUE) {

      if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
        groupBy <- "groups"
      } else {
        groupBy <- "replicates"
      }

      ms1 <- self$get_groups_ms1(
        groups, mass, mz, rt, drift, ppm, sec, millisec,
        rtWindow, mzWindow,
        mzClustFeatures,
        presenceFeatures,
        minIntensityFeatures,
        loadedFeaturesMS1,
        mzClust,
        presence,
        minIntensity,
        groupBy,
        verbose,
        filtered,
        runParallel
      )

      if (nrow(ms1) == 0) {
        message("\U2717 MS1 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) colorBy <- "replicates"
      
      if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"

      if (!interactive) {
        .plot_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        .plot_ms1_interactive(ms1, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Plots MS1 spectra from feature groups in the analyses.
    #'
    #' @return A plot.
    #'
    plot_groups_ms2 = function(groups = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               drift = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               isolationWindow = 1.3,
                               mzClustFeatures = 0.003,
                               presenceFeatures = 0.8,
                               minIntensityFeatures = 100,
                               loadedFeaturesMS2 = TRUE,
                               mzClust = 0.003,
                               presence = TRUE,
                               minIntensity = 100,
                               groupBy = "groups",
                               verbose = FALSE,
                               filtered = FALSE,
                               runParallel = FALSE,
                               legendNames = NULL,
                               title = NULL,
                               colorBy = "targets",
                               interactive = TRUE) {
      
      if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
        groupBy <- "groups"
      } else {
        groupBy <- "replicates"
      }

      ms2 <- self$get_groups_ms2(
        groups, mass, mz, rt, drift, ppm, sec, millisec,
        isolationWindow,
        mzClustFeatures,
        presenceFeatures,
        minIntensityFeatures,
        loadedFeaturesMS2,
        mzClust,
        presence,
        minIntensity,
        groupBy,
        verbose,
        filtered,
        runParallel
      )

      if (nrow(ms2) == 0) {
        message("\U2717 MS2 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) colorBy <- "replicates"
      
      if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"

      if (!interactive) {
        .plot_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        .plot_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to give an overview of the EIC, alignment and intensity variance
    #' from features within target feature groups.
    #'
    #' @param heights A numeric vector of length 3 to control the height of
    #' the first, second and third plot, respectively.
    #'
    #' @return A plot.
    #'
    plot_groups_overview = function(analyses = NULL,
                                    groups = NULL,
                                    mass = NULL,
                                    mz = NULL,
                                    rt = NULL,
                                    drift = NULL,
                                    ppm = 20,
                                    sec = 60,
                                    millisec = 5,
                                    rtExpand = 120,
                                    mzExpand = 0.005,
                                    loaded = TRUE,
                                    filtered = FALSE,
                                    runParallel = FALSE,
                                    legendNames = NULL,
                                    title = NULL,
                                    heights = c(0.35, 0.5, 0.15)) {

      fts <- self$get_features(analyses, groups, mass, mz, rt, drift, ppm, sec, millisec, filtered)

      eic <- self$get_features_eic(
        analyses = unique(fts$analysis),
        features = fts,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        filtered = filtered,
        loaded = loaded,
        runParallel = runParallel
      )

      eic <- eic[, `:=`(intensity = sum(intensity)),
        by = c("analysis", "polarity", "feature", "rt")
      ][]

      if (nrow(eic) == 0) {
        message("\U2717 Traces and/or features not found for targets!")
        return(NULL)
      }


      if (is.character(legendNames) & length(legendNames) == length(unique(fts$group))) {
        leg <- legendNames
        names(leg) <- unique(fts$group)
        leg <- leg[fts$group]
      } else if (isTRUE(legendNames) & "name" %in% colnames(fts)) {
        leg <- fts$name
      } else {
        leg <- fts$group
      }

      names(leg) <- paste0(fts$feature, "_", fts$analysis)
      eic$uid <- paste0(eic$feature, "_", eic$analysis)
      fts$uid <- paste0(fts$feature, "_", fts$analysis)
      eic$var <- leg[eic$uid]
      fts$var <- leg

      analyses <- private$.check_analyses_argument(analyses)

      .plot_groups_overview_aux(fts, eic, heights, analyses)
    },

    #' @description
    #' Plots the quality control assessment of internal standards.
    #'
    #' @return A plot.
    #'
    plot_internal_standards_qc = function() {

      if (self$has_groups()) {
        istd <- self$get_internal_standards(average = TRUE)
        .plot_internal_standards_qc_interactive(istd, self$get_replicate_names())
      } else {
        istd <- self$get_internal_standards(average = FALSE)
        .plot_internal_standards_qc_interactive(istd, self$get_analysis_names())
      }
    },
    
    #' @description
    #' Plots suspects.
    #' 
    #' @param database A data.frame with at least the columns name
    #' and mass, indicating the name and neutral monoisotopic
    #' mass of the suspect targets.
    #'
    #' @details The `ppm` and `sec` which indicate the
    #' mass (im ppm) and time (in seconds) deviations applied during the
    #' screening.
    #'
    #' @return A plot.
    #'
    plot_suspects = function(analyses = NULL,
                             database = NULL,
                             features = NULL,
                             mass = NULL,
                             mz = NULL,
                             rt = NULL,
                             drift = NULL,
                             ppm = 4,
                             sec = 10,
                             millisec = 5,
                             ppmMS2 = 10,
                             minFragments = 3,
                             isolationWindow = 1.3,
                             mzClust = 0.003,
                             presence = 0.8,
                             minIntensity = 0,
                             filtered = FALSE,
                             rtExpand = 120,
                             mzExpand = 0.005,
                             loaded = TRUE,
                             runParallel = FALSE,
                             colorBy = "targets") {
      
      if (any(self$has_suspects())) {
        
        suspects <- self$get_suspects(
          analyses,
          database,
          features,
          mass,
          mz,
          rt,
          drift,
          ppm,
          sec,
          millisec,
          ppmMS2,
          minFragments,
          isolationWindow ,
          mzClust,
          presence,
          minIntensity,
          runParallel,
          filtered,
          onGroups = FALSE
        )
        
        if (nrow(suspects) == 0) return(NULL)
        
        eic <- self$get_features_eic(
          analyses = unique(suspects$analysis),
          features = suspects$feature,
          rtExpand = rtExpand,
          mzExpand = mzExpand,
          filtered = filtered,
          loaded = loaded,
          runParallel = runParallel
        )
        
        eic <- eic[, `:=`(intensity = sum(intensity)),
          by = c("analysis", "polarity", "feature", "rt")
        ][]
        
        if (nrow(eic) == 0) {
          message("\U2717 Traces and/or features not found for targets!")
          return(NULL)
        }
        
        if (grepl("replicates", colorBy)) {
          eic$replicate <- self$get_replicate_names()[eic$analysis]
          suspects$replicate <- self$get_replicate_names()[suspects$analysis]
        }
        
        suspects <- .make_colorBy_varkey(suspects, colorBy, TRUE)
        
        leg <- suspects$var
        names(leg) <- paste0(suspects$feature, "_", suspects$analysis)
        eic$uid <- paste0(eic$feature, "_", eic$analysis)
        suspects$uid <- paste0(suspects$feature, "_", suspects$analysis)
        eic$var <- leg[eic$uid]
        
        .plot_suspects_interactive(suspects, eic, heights = c(0.5, 0.5))
      }
    },

    #' @description
    #' Maps isotopic clusters in the analyses.
    #'
    #' @return A plot.
    #'
    map_isotopes = function(analyses = NULL,
                            groups = NULL,
                            features = NULL,
                            clusters = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE,
                            xlim = 30,
                            ylim = 0.05,
                            showLegend = TRUE,
                            legendNames = NULL,
                            title = NULL,
                            colorBy = "targets",
                            interactive = TRUE) {

      isotopes <- self$get_isotopes(
        analyses, groups, features, clusters,
        mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(isotopes) == 0) {
        message("\U2717 Feature isotopes not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        isotopes$replicate <- self$get_replicate_names()[isotopes$analysis]
      }

      if (!interactive) {
        .map_isotopes_static(
          isotopes, colorBy, legendNames,
          xlim, ylim, title, showLegend
        )
      } else {
        .map_isotopes_interactive(
          isotopes, colorBy, legendNames,
          xlim, ylim, title
        )
      }
    },

    ## ___ processing -----
    
    #' @description Runs all modules represented by the added ProcessingSettings.
    #'
    #' @return Invisible.
    #'
    run_workflow = function() {

      if (self$has_settings()) {

        lapply(self$get_settings(), function(x) {
          call <- x$call
          message("\U2699 Running ", call, " with ", x$algorithm)
          do.call(self[[call]], list("settings" = x))
        })

      } else {
        warning("There are no processing settings to run!")
      }

      invisible(self)
    },

    ### _____ centroid_spectra -----

    #' @description Centroids profile spectra data for each MS analysis.
    #'
    #' @return Invisible.
    #'
    centroid_spectra = function(settings = NULL) {

      if (self$get_number_analyses() == 0) {
        warning("There are no analyses! Add MS analyses as mzML or mzXML files!")
        return(invisible(self))
      }
      
      if (!all(self$get_spectra_mode() %in% "profile")) {
        warning("MS analyses must be all in profile mode for centroiding! Not done.")
        return(invisible(self))
      }
      
      settings <- private$.get_call_settings(settings, "centroid_spectra")

      if (is.null(settings)) return(invisible(self))

      processed <- .s3_ms_centroid_spectra(settings, self)

      if (processed) {
        
        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }

        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }

        private$.register(
          "processed",
          "spectra",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )
      }

      invisible(self)
    },

    ### _____ bin_spectra -----

    #' @description Bins centroided spectra for each MS analysis.
    #'
    #' @return Invisible.
    #'
    bin_spectra = function(settings = NULL) {

      if (self$get_number_analyses() == 0) {
        warning("There are no analyses! Add MS analyses as mzML or mzXML files!")
        return(invisible(self))
      }
      
      settings <- private$.get_call_settings(settings, "bin_spectra")

      if (is.null(settings)) return(invisible(self))

      processed <- .s3_ms_bin_spectra(settings, self)

      if (processed) {
        
        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }

        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))

        } else {
          version <- NA_character_
        }

        private$.register(
          "processed",
          "spectra",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )
      }

      invisible(self)
    },

    ### _____ find_features -----

    #' @description Finds features (i.e., chromatographic peaks) in the spectra
    #' data of the analyses. Note, MS data structure requirements vary between
    #' the available processing settings for finding features.
    #'
    #' @return Invisible.
    #'
    find_features = function(settings = NULL) {
      
      if (self$get_number_analyses() == 0) {
        warning("There are no analyses! Add MS analyses as mzML or mzXML files!")
        return(invisible(self))
      }

      settings <- private$.get_call_settings(settings, "find_features")

      if (is.null(settings)) return(invisible(self))
      
      processed <- .s3_ms_find_features(settings, self)

      if (processed) {

        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }

        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))

        } else {
          version <- NA_character_
        }

        private$.register(
          "processed",
          "spectra",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )
      }

      invisible(self)
    },

    ### _____ group_features -----

    #' @description Groups and possibly aligns features across analyses.
    #'
    #' @return Invisible.
    #'
    group_features = function(settings = NULL) {
      
      if (!any(self$has_features())) {
        warning("There are no features! Run find_features first!")
        return(invisible(self))
      }

      settings <- private$.get_call_settings(settings, "group_features")

      if (is.null(settings)) return(invisible(self))

      processed <- .s3_ms_group_features(settings, self)

      if (processed) {
        
        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }

        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }

        private$.register(
          "processed",
          "features",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )
      }

      invisible(self)
    },

    ### _____ filter_features -----

    #' @description Filters features and feature groups according to defined
    #' settings.
    #'
    #' @return Invisible.
    #'
    #' @details The features are not entirely removed but tagged as filtered. 
    #' See columns `filtered` and `filter` of the features data.table as 
    #' obtained with the method `get_features()`.
    #'
    filter_features = function(settings = NULL) {

      if (!any(self$has_features())) {
        warning("There are no features! Run find_features first!")
        return(invisible(self))
      }
      
      processed <- FALSE

      settings <- private$.get_call_settings(settings, "filter_features")

      if (is.null(settings)) return(invisible(self))

      if ("Settings_filter_features_StreamFind" %in% class(settings)) {
        
        if (!any(self$has_features())) {
          warning("Features were not found! Run find_features method first!")
          return(invisible(self))
        }

        parameters <- settings$parameters
        
        filters <- names(parameters)

        possible_feature_filters <- c(
          "minIntensity",
          "minSnRatio",
          "blank",
          "maxGroupSd",
          "minGroupAbundance",
          "excludeIsotopes",
          "excludeAdducts",
          "rtFilter",
          "massFilter",
          "onlySuspects"
        )

        if (!all(filters %in% possible_feature_filters)) {
          warning("At least one of the filters is not recognized.")
          return(invisible(self))
        }

        n_features <- nrow(self$get_features(filtered = FALSE))

        for (i in seq_len(length(filters))) {

          switch(filters[i],
            minIntensity = (private$.filter_minIntensity(parameters[[filters[i]]])),

            minSnRatio = (private$.filter_minSnRatio(parameters[[filters[i]]])),

            maxGroupSd = (private$.filter_maxGroupSd(parameters[[filters[i]]])),

            blank = (private$.filter_blank(parameters[[filters[i]]])),

            minGroupAbundance = (private$.filter_minGroupAbundance(parameters[[filters[i]]])),

            excludeIsotopes = (private$.filter_excludeIsotopes(parameters[[filters[i]]])),

            rtFilter = private$.filter_rtFilter(parameters[[filters[i]]]),

            massFilter = private$.filter_massFilter(parameters[[filters[i]]]),
            
            onlySuspects = private$.filter_onlySuspects(parameters[[filters[i]]])

            # TODO add more filters
          )
        }

        n_features_after <- nrow(self$get_features(filtered = FALSE))
        
        n_features_filtered <- n_features - n_features_after
        
        if (n_features_filtered < 0) n_features_filtered <- 0

        message(paste0("\U2713 ", n_features_filtered, " features filtered!"))

        processed <- TRUE

      } else {
        processed <- .s3_ms_filter_features(settings, self)
      }

      if (processed) {
        
        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }

        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }

        private$.register(
          "processed",
          "features",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )
      }

      invisible(self)
    },

    ### _____ annotate_features -----

    #' @description Annotates isotopic features according to defined settings.
    #'
    #' @return Invisible.
    #'
    #' @details Extra columns are added to the features data.table in each
    #' analysis.
    #'
    annotate_features = function(settings = NULL) {

      if (!any(self$has_features())) {
        warning("There are no features! Run find_features first!")
        return(invisible(self))
      }
      
      settings <- private$.get_call_settings(settings, "annotate_features")

      if (is.null(settings)) return(invisible(self))

      processed <- .s3_ms_annotate_features(settings, self)

      if (processed) {
        
        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }

        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }

        private$.register(
          "processed",
          "features",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )

        message(paste0("\U2713 ", "Features annotated!"))
      }

      invisible(self)
    },

    ### _____ find_internal_standards -----

    #' @description Finds internal standards in features according to defined
    #' settings.
    #'
    find_internal_standards = function(settings = NULL) {

      if (!any(self$has_features())) {
        warning("There are no features! Run find_features first!")
        return(invisible(self))
      }
      
      settings <- private$.get_call_settings(settings, "find_internal_standards")

      if (is.null(settings)) return(invisible(self))

      processed <- .s3_ms_find_internal_standards(settings, self)

      if (processed) {
        
        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }

        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }

        private$.register(
          "processed",
          "features",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )
      }

      invisible(self)
    },

    ### _____ suspect_screening -----

    #' @description Screens for suspect targets in features according to defined
    #' settings.
    #'
    suspect_screening = function(settings = NULL) {

      if (!any(self$has_features())) {
        warning("There are no features! Run find_features first!")
        return(invisible(self))
      }
      
      settings <- private$.get_call_settings(settings, "suspect_screening")

      if (is.null(settings)) return(invisible(self))

      processed <- .s3_ms_suspect_screening(settings, self)

      if (processed) {
        
        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }

        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }

        private$.register(
          "processed",
          "features",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )
      }

      invisible(self)
    },

    ### _____ calculate_quality -----

    #' @description Calculates quality parameters of features that can be used
    #' for filtering/prioritization.
    #'
    calculate_quality = function(settings = NULL) {

      if (!any(self$has_features())) {
        warning("Features not found! Not calculated.")
        return(invisible(self))
      }

      settings <- private$.get_call_settings(settings, "calculate_quality")

      if (is.null(settings)) {
        warning("Settings not found to calculate quality of features!")
        return(invisible(self))
      }

      processed <- .s3_ms_calculate_quality(settings, self)

      if (processed) {

        if (!private$.settings_already_stored(settings)) {
          self$add_settings(settings)
        }

        if (requireNamespace(settings$software, quietly = TRUE)) {
          version <- as.character(packageVersion(settings$software))
        } else {
          version <- NA_character_
        }

        private$.register(
          "processed",
          "features",
          settings$call,
          settings$software,
          version,
          settings$algorithm
        )

        message(paste0("\U2713 ", "Features quality added!"))
      }

      invisible(self)
    },

    ## ___ as -----

    #' @description
    #' Creates an object with S4 class `features` from the package \pkg{patRoon}
    #' with the features in the analyses.
    #'
    #' @return An object with S4 class `features`.
    #'
    as_patRoon_features = function(filtered = FALSE) {

      if (!requireNamespace("patRoon", quietly = TRUE)) {
        warning("Package patRoon is not installed! Install it via https://github.com/rickhelmus/patRoon.")
        return(NULL)
      }

      if (self$get_number_analyses() == 0) {
        warning("There are no MS analyses!")
        return(NULL)
      }

      if (!any(self$has_features())) {
        warning("There are no features in the MS analyses!")
        return(NULL)
      }

      anaInfo <- self$get_overview()

      anaInfo <- data.frame(
        "path" = dirname(anaInfo$file),
        "analysis" = anaInfo$analysis,
        "group" = anaInfo$replicate,
        "blank" = anaInfo$blank
      )

      anaInfo$blank[is.na(anaInfo$blank)] <- ""

      multiple_polarities <- FALSE

      polarities <- self$get_polarities()

      if (length(polarities) > self$get_number_analyses()) {
        stop("Multiple polarities detected within one file! Not permited in patRoon currently.")
      }

      if (length(unique(polarities)) > 1) {
        multiple_polarities <- TRUE
        anaInfo$set <- polarities
      }

      anaInfo$file <- self$get_files()

      rownames(anaInfo) <- seq_len(nrow(anaInfo))

      features <- lapply(self$get_analyses(), function(x, without_filtered_features) {

        ft <- copy(x$features)

        if (without_filtered_features) {
          if ("filtered" %in% colnames(ft)) ft <- ft[!ft$filtered, ]
        }

        setnames(ft, c("feature", "rt", "rtmin", "rtmax"),
          c("ID", "ret", "retmin", "retmax"),
          skip_absent = TRUE
        )

        setcolorder(
          ft,
          c(
            "ID", "mz", "mzmin", "mzmax", "ret", "retmin", "retmax",
            "intensity", "area", "polarity"
          )
        )

        ft <- ft[order(ft$mz), ]
        ft <- ft[order(ft$ret), ]
        ft$index <- seq_len(nrow(ft))

        ft
      }, without_filtered_features = !filtered)

      if (multiple_polarities) {

        pol_key <- c("[M+H]+", "[M-H]-", "[M]")
        names(pol_key) <- c("1", "-1", "0")

        features <- lapply(features, function(x, pol_key) {

          if (nrow(x) == 0) {
            x$mass <- NULL
            x[, adduct := character(.N)]
            return(x)
          }

          x$mzmin <- x$mass - (x$mz - x$mzmin)
          x$mzmax <- x$mass + (x$mzmax - x$mz)
          x$mz <- x$mass
          x$mass <- NULL

          pol_char <- as.character(x$polarity)
          x$adduct <- pol_key[pol_char]

          x
        }, pol_key = pol_key)

        features_obj <- new("featuresSet",
          features = features, analysisInfo = anaInfo,
          algorithm = "openms-set"
        )

      } else {
        features_obj <- new("featuresOpenMS",
          features = features, analysisInfo = anaInfo
        )
      }

      return(features_obj)
    },

    #' @description
    #' Creates an object with S4 class `featureGroups` from the package
    #' \pkg{patRoon} with the features in the analyses and feature groups.
    #' 
    #' 
    #'
    #' @return An object with S4 class `featureGroups`.
    #'
    as_patRoon_featureGroups = function(filtered = FALSE, addSuspects = TRUE) {

      if (!requireNamespace("patRoon", quietly = TRUE)) {
        warning("Package patRoon is not installed! Install it via https://github.com/rickhelmus/patRoon.")
        return(NULL)
      }

      if (self$get_number_analyses() == 0) {
        warning("There are no MS analyses!")
        return(NULL)
      }

      if (!any(self$has_features())) {
        warning("There are no features in the MS analyses!")
        return(NULL)
      }

      if (!self$has_groups()) {
        warning("No feature groups found!")
        return(NULL)
      }

      pat_features <- self$as_patRoon_features(filtered)

      features <- pat_features@features

      n_analyses <- length(features)

      groups <- self$get_groups(
        filtered = filtered,
        intensities = TRUE, average = FALSE, sdValues = FALSE, metadata = TRUE
      )
      
      groups_info <- copy(groups)
      groups_cols <- groups$group
      groups <- groups[, self$get_analysis_names(), with = FALSE]
      groups_trans <- data.table::transpose(groups)
      colnames(groups_trans) <- groups_cols

      groups_info <- groups_info[, 1:3]
      groups_info_rows <- groups_info$group
      groups_info[["group"]] <- NULL
      groups_info <- as.data.frame(groups_info)
      rownames(groups_info) <- groups_info_rows
      colnames(groups_info) <- c("mzs", "rts")
      # Note that here the mzs is still neutral mass
      
      ftindex <- matrix(rep(0, n_analyses * length(groups_cols)), nrow = n_analyses)
      colnames(ftindex) <- groups_cols

      for (i in seq_len(n_analyses)) {
        fts_temp <- features[[i]]
        if (nrow(fts_temp) > 0) {
          for (j in groups_cols) {
            idx <- which(fts_temp$group %in% j)
            if (length(idx) > 0) {
              if (length(idx) > 1) {
                warning("More than one feature per analyses assigned to a feature group!")
                ftindex[i, j] <- idx
              }
            }
          }
        }
      }
      
      ftindex <- as.data.table(ftindex)
      
      add_suspects_to_pat <- FALSE
      
      if (addSuspects && any(self$has_suspects())) {
        suspects_df <- self$get_suspects(onGroups = FALSE)
        
        cols_keep <- colnames(suspects_df)
        cols_keep <- c(cols_keep[1:(which(cols_keep %in% "analysis") - 1)], "group", "polarity")
        suspects_df <- suspects_df[, cols_keep, with = FALSE]
        
        pol_key <- c("positive", "negative", "nd")
        names(pol_key) <- c("1", "-1", "0")
        suspects_df$polarity <- as.character(suspects_df$polarity)
        suspects_df$polarity <- pol_key[suspects_df$polarity]
        
        setnames(suspects_df, c("exp_rt", "exp_mass"), c("rt", "neutralMass"), skip_absent = TRUE)
        
        suspects_df$error_mass <- suspects_df$error_mass * suspects_df$neutralMass / 1E6
        
        suspects_df_av <- data.table::copy(suspects_df)
        suspects_df_av <- suspects_df_av[, `:=`(d_rt = max(abs(error_rt)), d_mz = max(abs(error_mass))), by = list(group)]
        
        suspects_df_av[["error_mass"]] <- NULL
        suspects_df_av[["error_rt"]] <- NULL
        
        cols_group_by <- colnames(suspects_df_av)
        cols_group_by <- cols_group_by[!cols_group_by %in% "polarity"]
        
        polarity <- NULL
        
        suspects_df_av <- suspects_df_av[, polarity := paste(unique(polarity), collapse = ","), by = cols_group_by]
        
        suspects_df_av <- unique(suspects_df_av)
        
        setcolorder(suspects_df_av, c("name", "group"))
        
        setnames(suspects_df_av, "polarity", "sets")
        
        setnames(suspects_df, c("error_rt", "error_mass"), c("d_rt", "d_mz"), skip_absent = TRUE)
        
        setcolorder(suspects_df, c("name", "group"))
        
        add_suspects_to_pat <- TRUE
      }

      if (TRUE %in% grepl("Set", is(pat_features))) {
        polarity <- c("[M+H]+", "[M-H]-", "[M]")
        names(polarity) <- c("1", "-1", "0")

        polarity_set <- c("positive", "negative", "not defined")
        names(polarity_set) <- c("1", "-1", "0")

        neutralMasses <- groups_info$mzs
        names(neutralMasses) <- groups_cols

        annotations_entry <- data.table::rbindlist(pat_features@features, fill = TRUE)
        annotations_entry$neutralMass <- neutralMasses[annotations_entry$group]
        polarity_column <- as.character(annotations_entry$polarity)
        annotations_entry$adduct <- polarity[polarity_column]
        annotations_entry$set <- polarity_set[polarity_column]

        cols_to_keep <- c("set", "group", "adduct", "neutralMass")
        annotations_entry <- annotations_entry[, cols_to_keep, with = FALSE]
        annotations_entry <- unique(annotations_entry)
        
        pat <- new("featureGroupsSet",
          groups = groups_trans,
          analysisInfo = pat_features@analysisInfo,
          groupInfo = groups_info,
          features = pat_features,
          ftindex = ftindex,
          groupQualities = data.table(),
          groupScores = data.table(),
          annotations = annotations_entry,
          ISTDs = data.table(),
          ISTDAssignments = list(),
          concentrations = data.table(),
          toxicities = data.table(),
          algorithm = "openms-set"
        )
        
        if (add_suspects_to_pat) {
          
          pat_positive <- patRoon::unset(pat, "positive")
          suspects_df_pos <- suspects_df[suspects_df$polarity %in% "positive", ]
          if (nrow(suspects_df_pos) > 0) {
            suspects_df_pos <- suspects_df_pos[, `:=`(d_rt = max(abs(d_rt)), d_mz = max(abs(d_mz))), by = list(group)]
            suspects_df_pos <- unique(suspects_df_pos)
          }
          
          pat_negative <- patRoon::unset(pat, "negative")
          suspects_df_neg <- suspects_df[suspects_df$polarity %in% "negative", ]
          if (nrow(suspects_df_neg) > 0) {
            suspects_df_neg <- suspects_df_neg[, `:=`(d_rt = max(abs(d_rt)), d_mz = max(abs(d_mz))), by = list(group)]
            suspects_df_neg <- unique(suspects_df_neg)
          }

          pat_positive_sus <- new("featureGroupsScreening",
            screenInfo = suspects_df_pos,
            groups = pat_positive@groups,
            analysisInfo = pat_positive@analysisInfo,
            groupInfo = pat_positive@groupInfo,
            features = pat_positive@features,
            ftindex = pat_positive@ftindex,
            groupQualities = data.table(),
            groupScores = data.table(),
            annotations = pat_positive@annotations,
            ISTDs = data.table(),
            ISTDAssignments = list(),
            concentrations = data.table(),
            toxicities = data.table()
          )
          
          pat_negative_sus <- new("featureGroupsScreening",
            screenInfo = suspects_df_neg,
            groups = pat_negative@groups,
            analysisInfo = pat_negative@analysisInfo,
            groupInfo = pat_negative@groupInfo,
            features = pat_negative@features,
            ftindex = pat_negative@ftindex,
            groupQualities = data.table(),
            groupScores = data.table(),
            annotations = pat_negative@annotations,
            ISTDs = data.table(),
            ISTDAssignments = list(),
            concentrations = data.table(),
            toxicities = data.table()
          )
          
          pat_sus <- new("featureGroupsScreeningSet",
            screenInfo = suspects_df_av,
            groups = groups_trans,
            analysisInfo = pat_features@analysisInfo,
            groupInfo = groups_info,
            features = pat_features,
            ftindex = ftindex,
            annotations = annotations_entry,
            ISTDs = data.table(),
            ISTDAssignments = list(),
            concentrations = data.table(),
            toxicities = data.table(),
            setObjects = list(
              "positive" = pat_positive_sus,
              "negative" = pat_negative_sus
            )
          )
          
          return(pat_sus)
        }
        
        pat

      } else {

        polarity <- unique(self$get_polarities())

        if (length(polarity) > 1) {
          stop("Multiple polarities detected but Features is not a set!")
        }

        if (polarity %in% "positive") {
          groups_info$mzs <- groups_info$mzs + 1.00726
        } else if (polarity %in% "negative") {
          groups_info$mzs <- groups_info$mzs - 1.00726
        } else {
          stop("Polarity should be defined as positive or negative!")
        }

        pat <- new("featureGroupsOpenMS",
          groups = groups_trans,
          analysisInfo = pat_features@analysisInfo,
          groupInfo = groups_info,
          features = pat_features,
          ftindex = ftindex
        )
        
        if (add_suspects_to_pat) {
          suspects_df_av[["sets"]] <- NULL

          pat_sus <- new("featureGroupsScreening",
            screenInfo = suspects_df_av,
            groups = pat@groups,
            analysisInfo = pat@analysisInfo,
            groupInfo = pat@groupInfo,
            features = pat@features,
            ftindex = pat@ftindex,
            groupQualities = data.table(),
            groupScores = data.table(),
            annotations = pat@annotations,
            ISTDs = data.table(),
            ISTDAssignments = list(),
            concentrations = data.table(),
            toxicities = data.table()
          )
          
          return(pat_sus)
        }
        
        pat
      }
    },

    #' @description
    #' Creates an object with S4 class `MSPeakLists` from the package
    #' \pkg{patRoon} with MS and MSMS data from features in the analyses.
    #' Note that feature groups are required. The MS and MSMS spectra of each
    #' feature are then average by \pkg{patRoon} to produce the feature group
    #' spectra using the parameters of the function
    #' \link[patRoon]{getDefAvgPListParams}. The arguments described below are
    #' taken from the documentation available in \pkg{patRoon}.
    #'
    #' @param clusterMzWindow m/z window (in Da) used for clustering m/z values
    #' when spectra are averaged. For method="hclust" this corresponds to the
    #' cluster height, while for method="distance" this value is used to find
    #' nearby masses (+/- window). Too small windows will prevent clustering
    #' m/z values (thus erroneously treating equal masses along spectra as
    #' different), whereas too big windows may cluster unrelated m/z values
    #' from different or even the same spectrum together.
    #' @param topMost Only retain this maximum number of MS peaks when generating
    #' averaged spectra. Lowering this number may exclude more irrelevant (noisy)
    #' MS peaks and decrease processing time, whereas higher values may avoid
    #' excluding lower intense MS peaks that may still be of interest.
    #' @param minIntensityPre MS peaks with intensities below this value will
    #' be removed (applied prior to selection by `topMost`) before averaging.
    #' @param minIntensityPost MS peaks with intensities below this value will
    #' be removed after averaging.
    #' @param avgFun Function that is used to calculate average m/z values.
    #' @param method Method used for producing averaged MS spectra. Valid
    #' values are "hclust", used for hierarchical clustering (using the
    #' fastcluster package), and "distance", to use the between peak distance.
    #' The latter method may reduces processing time and memory requirements,
    #' at the potential cost of reduced accuracy.
    #' @param pruneMissingPrecursorMS For MS data only: if TRUE then peak lists
    #' without a precursor peak are removed. Note that even when this is set to
    #' FALSE, functionality that relies on MS (not MS/MS) peak lists (e.g.
    #' formulae calculation) will still skip calculation if a precursor is not
    #' found.
    #' @param retainPrecursorMSMS For MS/MS data only: if TRUE then always
    #' retain the precursor mass peak even if is not among the `topMost` peaks.
    #' Note that MS precursor mass peaks are always kept. Furthermore, note
    #' that precursor peaks in both MS and MS/MS data may still be removed by
    #' intensity thresholds (this is unlike the filter method function).
    #'
    #' @return An object with S4 class `MSPeakLists`.
    #'
    as_patRoon_MSPeakLists = function(filtered = FALSE,
                                      clusterMzWindow = 0.005,
                                      topMost = 100,
                                      minIntensityPre = 50,
                                      minIntensityPost = 50,
                                      avgFun = mean,
                                      method = "hclust",
                                      retainPrecursorMSMS = TRUE) {

      if (!requireNamespace("patRoon", quietly = TRUE)) {
        warning("Package patRoon is not installed! Install it via https://github.com/rickhelmus/patRoon.")
        return(NULL)
      }
      
      if (self$has_groups()) {

        pruneMissingPrecursorMS = FALSE

        correct_spectrum <- function(s, t, out) {

          if (length(s) > 1) s <- s[1]

          names(s) <- t

          if (!is.null(s[[1]])) {
            n_traces <- nrow(s[[1]])

            if (n_traces > 0) {
              s[[1]][["id"]] <- seq_len(n_traces)

              if (!"is_pre" %in% colnames(s[[1]])) {
                s[[1]][["is_pre"]] <- rep(FALSE, n_traces)
              }

              cols_to_keep <- c("id", "mz", "intensity", "is_pre")
              s[[1]] <- s[[1]][, cols_to_keep, with = FALSE]

              colnames(s[[1]]) <- c("ID", "mz", "intensity", "precursor")
            }
          }

          out <- c(out, s)

          out
        }

        plist <- lapply(self$get_analyses(), function(x, filtered, correct_spectrum) {

          features <- x$features

          if (!filtered) features <- features[!features$filtered, ]

          groups <- unique(features$group)
          groups <- groups[!is.na(groups)]

          glist <- lapply(groups, function(x2, features, correct_spectrum) {
            out <- list()

            MS <- features$ms1[features$group %in% x2]

            if (length(MS) > 1) MS <- MS[1]

            if (!is.null(MS[[1]])) {

              if (!"is_pre" %in% colnames(MS[[1]])) {

                t_mz_min <- features$mzmin[features$group %in% x2]
                t_mz_max <- features$mzmax[features$group %in% x2]

                MS[[1]][["is_pre"]] <- vapply(MS[[1]][["mz"]],
                  function(x, t_mz_min, t_mz_max) {
                    x >= t_mz_min & x <= t_mz_max
                  }, t_mz_min = t_mz_min,
                  t_mz_max = t_mz_max,
                  FALSE
                )
              }
            }

            MSMS <- features$ms2[features$group %in% x2]

            out <- correct_spectrum(MS, "MS", out)

            out <- correct_spectrum(MSMS, "MSMS", out)

            out

          }, features = features, correct_spectrum = correct_spectrum)

          names(glist) <- groups

          glist = glist[order(names(glist))]

          glist
        }, filtered = filtered, correct_spectrum = correct_spectrum)

        names(plist) <- self$get_analysis_names()

        plist <- plist[vapply(plist, function(x) length(x) > 0, FALSE)]



        mlist <- lapply(self$get_analyses(), function(x, filtered) {

          features <- x$features

          run <- x$run

          pol_col <- as.character(run$polarity)
          pol_key = c(1, 0, -1)
          names(pol_key) <- c("1", "-1", "0")
          run$polarity <- pol_key[pol_col]

          setnames(run,
            c("index", "level", "ce", "pre_mz"),
            c("seqNum", "msLevel", "collisionEnergy", "precursorMZ"),
            skip_absent = TRUE
          )

          if (!filtered) features <- features[!features$filtered, ]

          groups <- unique(features$group)
          groups <- groups[!is.na(groups)]

          glist <- lapply(groups, function(x2, features, run) {
            out <- list()

            ft <- features[features$group %in% x2, ]

            if (nrow(ft) > 0) {
              MS <- run[run$rt >= ft$rtmin &
                          run$rt <= ft$rtmax &
                            run$msLevel == 1, ]

              if (nrow(MS) > 0) out[["MS"]] <- MS

              MSMS <- run[run$rt >= ft$rtmin &
                            run$rt <= ft$rtmax &
                              run$precursorMZ >= ft$mzmin - 1.3/2 &
                                run$precursorMZ <= ft$mzmax + 1.3/2 &
                                  run$msLevel == 2, ]

              if (nrow(MSMS) > 0) out[["MSMS"]] <- MSMS
            }

            out

          }, features = features, run = run)

          names(glist) <- groups

          glist = glist[order(names(glist))]

          glist
        }, filtered = filtered)

        names(mlist) <- self$get_analysis_names()

        mlist <- mlist[vapply(mlist, function(x) length(x) > 0, FALSE)]

        groups <- self$get_groups(
          filtered = filtered,
          intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
        )

        # group_names <- unique(groups$group)
        #
        # group_names = group_names[order(group_names)]
        #
        # aplist <- lapply(seq_len(nrow(groups)), function(x, groups, correct_spectrum) {
        #
        #   out <- list()
        #
        #   MS <- groups$ms1[x]
        #
        #   MSMS <- groups$ms2[x]
        #
        #   out <- correct_spectrum(MS, "MS", out)
        #
        #   out <- correct_spectrum(MSMS, "MSMS", out)
        #
        #   out
        #
        # }, groups = groups, correct_spectrum = correct_spectrum)
        #
        # names(aplist) <- groups$group
        #
        # aplist <- aplist[vapply(aplist, function(x) length(x) > 0, FALSE)]
        #
        # aplist = aplist[order(names(aplist))]

        settings <- self$get_settings("load_groups_ms2")

        parameters <- settings$load_groups_ms2$parameters

        pat_param <- list(
          "clusterMzWindow" = clusterMzWindow,
          "topMost" = topMost,
          "minIntensityPre" = minIntensityPre,
          "minIntensityPost" = minIntensityPost,
          "avgFun" = avgFun,
          "method" = method,
          "pruneMissingPrecursorMS" = pruneMissingPrecursorMS,
          "retainPrecursorMSMS" = retainPrecursorMSMS
        )

        if ("mzClust" %in% names(parameters)) {
          pat_param$clusterMzWindow <- parameters$mzClust
        }

        if ("minIntensity" %in% names(parameters)) {
          pat_param$minIntensityPre <- parameters$minIntensity
          pat_param$minIntensityPost <- parameters$minIntensity
        }

        plfinal <- new("MSPeakLists",
          peakLists = plist,
          metadata = mlist,
          # averagedPeakLists = aplist,
          avgPeakListArgs = pat_param,
          origFGNames = groups$group,
          algorithm = "mzr"
        )

        # plfinal@averagedPeakLists <- aplist

        plfinal

      } else {
        warning("No feature groups found to make the MSPeakLists S4 object!")
      }
    },

    ## ___ check -----

    #' @description
    #' Checks the correspondence of features within feature groups.
    #'
    #' @return \code{TRUE} or \code{FALSE}.
    #'
    check_correspondence = function() {
      valid <- FALSE
      
      if (all(self$has_features()) & self$has_groups()) {
        valid <- rcpp_ms_groups_correspondence(
          groups = self$get_groups(intensities = TRUE, average = FALSE, metadata = TRUE),
          features = self$get_features(),
          verbose = TRUE
        )
      }
      valid
    },

    ## ___ save -----

    #' @description
    #' Saves the headers list.
    #'
    #' @return Saves the headers list as the defined \code{format} in
    #' \code{path} and returns invisible.
    #'
    save_headers = function(format = "json", name = "headers", path = getwd()) {
      if (format %in% "json") {
        js_headers <- toJSON(
          private$.headers,
          dataframe = "columns",
          Date = "ISO8601",
          POSIXt = "string",
          factor = "string",
          complex = "string",
          null = "null",
          na = "null",
          auto_unbox = FALSE,
          digits = 8,
          pretty = TRUE,
          force = TRUE
        )
        write(js_headers, file = paste0(path, "/", name, ".json"))
      }

      if (format %in% "rds") {
        saveRDS(private$.headers, file = paste0(path, "/", name, ".rds"))
      }

      invisible(self)
    },

    #' @description
    #' Saves settings.
    #'
    #' @param call A string or a vector of strings with the name/s of the
    #' processing method/s to be saved. When `call` is \code{NULL} all
    #' settings are saved.
    #'
    #' @return Saves the settings list as the defined \code{format} in
    #' \code{path} and returns invisible.
    #'
    save_settings = function(call = NULL, format = "json",
                             name = "settings", path = getwd()) {

      js_settings <- self$get_settings(call)
      
      names(js_settings) <- vapply(js_settings, function(x) x$call, NA_character_)

      if (format %in% "json") {
        js_settings <- toJSON(
          js_settings,
          dataframe = "columns",
          Date = "ISO8601",
          POSIXt = "string",
          factor = "string",
          complex = "string",
          null = "null",
          na = "null",
          auto_unbox = FALSE,
          digits = 8,
          pretty = TRUE,
          force = TRUE
        )
        write(js_settings, file = paste0(path, "/", name, ".json"))
      }

      if (format %in% "rds") {
        saveRDS(self$get_settings(call), file = paste0(path, "/", name, ".rds"))
      }

      invisible(self)
    },

    #' @description
    #' Saves analyses.
    #'
    #' @return Saves the list of analyses as the defined \code{format} in
    #' \code{path} and returns invisible.
    #'
    save_analyses = function(analyses = NULL, format = "json",
                             name = "analyses", path = getwd()) {

      analyses <- self$get_analyses(analyses)

      if (format %in% "json") {
        js_analyses <- toJSON(
          analyses,
          dataframe = "columns",
          Date = "ISO8601",
          POSIXt = "string",
          factor = "string",
          complex = "string",
          null = "null",
          na = "null",
          auto_unbox = FALSE,
          digits = 8,
          pretty = TRUE,
          force = TRUE
        )

        write(js_analyses, file = paste0(path, "/", name, ".json"))
      }

      if (format %in% "rds") {
        saveRDS(analyses, file = paste0(path, "/", name, ".rds"))
      }

      invisible(self)
    },

    #' @description
    #' Saves the private fields (i.e., headers, settings, analyses,
    #' groups and alignment) of the MassSpecData object.
    #'
    #' @return Saves the private fields of the msdata as the defined `format`
    #' in the \code{path} and returns invisible.
    #'
    save = function(format = "json", name = "MassSpecData", path = getwd()) {

      if (format %in% "json") {
        list_all <- list()

        headers <- private$.headers
        settings <- private$.settings
        analyses <- private$.analyses
        alignment <- private$.alignment
        history <- private$.history
        modules <- private$.modules

        if (length(headers) > 0) list_all$headers <- headers
        if (!is.null(settings)) list_all$settings <- settings
        if (!is.null(analyses)) list_all$analyses <- analyses
        if (!is.null(alignment)) list_all$alignment <- alignment
        if (!is.null(history)) list_all$history <- history
        if (!is.null(modules)) list_all$modules <- modules

        js_all <- toJSON(
          list_all,
          dataframe = "columns",
          Date = "ISO8601",
          POSIXt = "string",
          factor = "string",
          complex = "string",
          null = "null",
          na = "null",
          auto_unbox = FALSE,
          digits = 8,
          pretty = TRUE,
          force = TRUE
        )

        write(js_all, file = paste0(path, "/", name, ".", "json"))
      }

      if (format %in% "rds") {
        saveRDS(self, file = paste0(path, "/", name, ".rds"))
      }

      invisible(self)
    },

    ## ___ import -----

    #' @description
    #' Imports headers from a \emph{rds} or \emph{json} file.
    #'
    #' @return Invisible.
    #'
    import_headers = function(file = NA_character_) {
      if (file.exists(file)) {
        headers <- NULL
        if (file_ext(file) %in% "json") headers <- fromJSON(file)
        if (file_ext(file) %in% "rds") headers <- readRDS(file)
        self$add_headers(headers)

      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },

    #' @description
    #' Imports processing settings from a \emph{rds} or \emph{json} file.
    #'
    #' @param replace Logical. When `TRUE`, existing settings are replaced by
    #' the new settings with the same call name.
    #'
    #' @return Invisible.
    #'
    import_settings = function(file = NA_character_, replace = TRUE) {
      if (file.exists(file)) {
        settings <- NULL
        if (file_ext(file) %in% "json") settings <- fromJSON(file)
        if (file_ext(file) %in% "rds") settings <- readRDS(file)
        self$add_settings(settings, replace)

      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },

    #' @description
    #' Imports analyses from a \emph{rds} or \emph{json} file.
    #'
    #' @return Invisible.
    #'
    import_analyses = function(file = NA_character_) {
      if (file.exists(file)) {
        analyses <- NULL
        if (file_ext(file) %in% "json") {
          analyses <- fromJSON(file, simplifyDataFrame = FALSE)
        }
        if (file_ext(file) %in% "rds") analyses <- readRDS(file)
        # TODO check in features data.table is converted from JSON properly
        self$add_analyses(analyses)
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },

    #' @description
    #' Imports a `MassSpecData` object saved as \emph{json}.
    #'
    #' @param file A \emph{json} file representing a `MassSpecData` object.
    #'
    #' @return Invisible.
    #'
    import = function(file = NA_character_) {

      if (file.exists(file)) {
        if (file_ext(file) %in% "json") {
          js_ms <- fromJSON(file, simplifyDataFrame = FALSE)

          fields_present <- names(js_ms)

          if ("headers" %in% fields_present) self$add_headers(js_ms[["headers"]])

          if ("settings" %in% fields_present) {
            if (!is.null(js_ms[["settings"]])) {
              self$add_settings(js_ms[["settings"]], replace = TRUE)
            }
          }

          if ("analyses" %in% fields_present) {
            if (!is.null(js_ms[["analyses"]])) {
              self$add_analyses(js_ms[["analyses"]])
            }
          }

          if ("alignment" %in% fields_present) {
            if (!is.null(js_ms[["alignment"]])) {
              self$add_alignment(js_ms[["alignment"]])
            }
          }

          if ("history" %in% fields_present) {
            private$.history <- js_ms[["history"]]
          }

          if ("modules" %in% fields_present) {
            private$.modules <- js_ms[["modules"]]
          }
        }
      } else {
        warning("File not found in given path!")
        NULL
      }

      invisible(self)
    },

    ## ___ report -----

    #' @description
    #' Saves the HTML report from the function \link[patRoon]{report} from the
    #' package \pkg{patRoon}. The interface is exactly the same and the
    #' arguments description are taken from the documentation in \pkg{patRoon}.
    #' Therefore, for further information, we recommend to consult directly the
    #' function \link[patRoon]{report} in \pkg{patRoon}.
    #'
    #' @param settingsFile The path to the report settings file used for report
    #' configuration (see Report settings in \link[patRoon]{report}).
    #' @param EICParams A named list with parameters used for extracted ion
    #' chromatogram (EIC) creation. See \link[patRoon]{getDefEICParams}.
    #' @param specSimParams A named list with parameters that influence the
    #' calculation of MS spectra similarities. See \link[patRoon]{getDefSpecSimParams}.
    #' @param clearPath If TRUE then the report destination path will be
    #' (recursively) removed prior to reporting.
    #' @param openReport If set to TRUE then the output report file will be
    #' opened with the system browser.
    #' @param parallel If set to TRUE then code is executed in parallel.
    #' @param overrideSettings A list with settings that override those from
    #' the report settings file. See \link[patRoon]{report}.
    #'
    #' @return An interactive HTML report from the package \pkg{patRoon}.
    #'
    patRoon_report = function(path = paste0(getwd(), "/report"),
                              filtered = FALSE,
                              settingsFile = system.file("report", "settings.yml", package = "patRoon"),
                              EICParams = patRoon::getDefEICParams(topMost = 1, topMostByRGroup = TRUE),
                              specSimParams = patRoon::getDefSpecSimParams(),
                              clearPath = FALSE,
                              openReport = TRUE,
                              parallel = TRUE,
                              overrideSettings = list()) {

      if (!requireNamespace("patRoon", quietly = TRUE)) {
        return(invisible(self))
      }

      if (self$get_number_analyses() == 0) {
        warning("There are no MS analyses!")
        return(invisible(self))
      }

      if (!any(self$has_features())) {
        warning("There are no features in the MS analyses!")
        return(invisible(self))
      }

      if (!self$has_groups()) {
        warning("No feature groups found!")
        return(invisible(self))
      }

      fGroups <- self$as_patRoon_featureGroups(filtered)

      if (is.null(fGroups) | length(fGroups) == 0) {
        warning("Feature groups empty!")
        return(invisible(self))
      }

      if (!(any(self$has_loaded_features_ms2()) | any(self$has_loaded_features_ms1()))) {
        warning("MS or MSMS spectra for features not loaded!")
        # TODO add possibility to generate using patRoon
        MSPeakLists <- NULL

      } else {
        MSPeakLists <- self$as_patRoon_MSPeakLists(filtered)
      }

      patRoon::report(
        fGroups,
        MSPeakLists,
        formulas = NULL,
        compounds = NULL,
        compsCluster = NULL,
        components = NULL,
        TPs = NULL,
        settingsFile = settingsFile,
        path = path,
        EICParams = EICParams,
        specSimParams = specSimParams,
        clearPath = clearPath,
        openReport = openReport,
        parallel = parallel,
        overrideSettings = overrideSettings
      )

      invisible(self)
    },

    ## ___ info -----

    ### ___ processing_function_calls -----

    #' @description
    #' Possible processing function calls.
    #'
    #' @return A character vector with ordered possible function calls for data
    #' pre and post-processing.
    #'
    processing_function_calls = function() {
      c(
        "centroid_spectra",
        "bin_spectra",
        "find_features",
        "annotate_features",
        "load_features_eic",
        "load_features_ms1",
        "load_features_ms2",
        "load_groups_ms1",
        "load_groups_ms2",
        "group_features",
        "fill_features",
        "filter_features",
        "suspect_screening",
        "find_internal_standards",
        "calculate_quality"
      )
    },

    ### ___ help -----

    #' @field help (`list()`)\cr
    #' List of function elements to access specific reference help pages.
    help = list(
      methods = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/MassSpecData.html#methods")
      },
      get_groups = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/MassSpecData.html#method-MassSpecData-get_groups")
      },
      get_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/MassSpecData.html#method-MassSpecData-get_features")
      },
      settings_centroid_spectra = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#centroid-spectra")
      },
      settings_bin_spectra = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#bin-spectra")
      },
      settings_find_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#find-features")
      },
      settings_annotate_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#annotate-features")
      },
      settings_load_features_eic = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#load-features-eic")
      },
      settings_load_features_ms1 = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#load-features-ms1")
      },
      settings_load_features_ms2 = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#load-features-ms2")
      },
      settings_group_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#group-features")
      },
      settings_fill_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#fill-features")
      },
      settings_filter_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#filter-features")
      },
      settings_suspect_screening = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#suspect-screening")
      },
      settings_find_internal_standards = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#find-internal-standards")
      },
      settings_calculate_quality = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#calculate-quality")
      }
    )
  )
)

# _ import MassSpecData class -----

#' Function to import a MassSpecData class object from a *json* or *rds* file.
#'
#' @description Function to import a `MassSpecData` class object from a saved
#' *json* or *rds* file.
#'
#' @template arg-ms-import-file
#'
#' @return A `MassSpecData` class object.
#'
#' @export
#'
import_MassSpecData <- function(file) {

  if (file.exists(file)) {
    if (file_ext(file) %in% "json") {

      new_ms <- MassSpecData$new()
      new_ms$import_all(file)
      message("\U2713 MassSpecData class object imported from json file!")
    }

    if (file_ext(file) %in% "rds") {
      new_ms <- readRDS(file)
      # TODO is it important to validate object
      message("\U2713 MassSpecData class object imported from rds file!")
    }

    new_ms

  } else {
    warning("File not found in given path!")
    NULL
  }
}


#' Function to combine MassSpecData class objects.
#'
#' @param combineFeatureLists Logical, set to TRUE to combine feature lists.
#' @param ... *MassSpecData* class object.
#'
#' @return A *MassSpecData* class object.
#'
combine_MassSpecData <- function(combineFeatureLists = TRUE, ...) {

  combined_analyses <- list()

  for (obj in list(...)) {

    if (inherits(obj, "MassSpecData")) {
      combined_analyses <- c(combined_analyses, obj$get_analyses())
    }

    # if duplicated files, and cflists is TRUE {
    #  each duplicated files return grouped feature list.
    #  how to combine, using the first group_features settings?
    #}
  }

  return(MassSpecData$new(analyses = combined_analyses))
}
