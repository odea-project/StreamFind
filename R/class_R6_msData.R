#' **msData** R6 class and methods
#'
#' @description
#' The msData R6 class is a framework with methods for parsing, processing,
#' visualizing and storing MS data.
#'
#' @export
#'
msData <- R6::R6Class("msData",

  # _ private fields -----
  private = list(

    ## ___ .headers -----
    .headers = NULL,

    ## ___ .settings -----
    .settings = NULL,

    ## ___ .analyses -----
    .analyses = NULL,

    ## ___ .groups -----
    .groups = NULL,

    ## ___ .alignment -----
    .alignment = NULL,

    ## ___ .utils -----

    #' @description
    #' Checks the analyses argument as a character/integer vector to match
    #' analyses names or indices from the `msData` object. Returns a valid
    #' character vector with analysis names or `NULL` for non-matching.
    #'
    #' @param analyses X.
    #'
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
    }
  ),

  # _ public fields/methods -----

  public = list(

    ## ___ system -----

    #' @description
    #' Creates an msData class object. When `headers` are not given (i.e.,
    #' `NULL`), a default headers S3 class object is generated with name as
    #' `NA_character`, path as `get_wd()` and date as `Sys.time()`.
    #' See `?headers` for more information.
    #'
    #' @template arg-ms-files
    #' @template arg-runParallel
    #' @template arg-headers-list
    #' @template arg-settings-list
    #' @template arg-ms-analyses-list
    #' @template arg-ms-groups
    #' @param alignment X.
    #'
    #' @return A new `msData` class object.
    #'
    initialize = function(files = NULL,
                          runParallel = FALSE,
                          headers = NULL,
                          settings = NULL,
                          analyses = NULL,
                          groups = NULL,
                          alignment = NULL) {

      if (!is.null(headers)) self$add_headers(headers)

      if (is.null(private$.headers)) {
        private$.headers <- headers(
          name = NA_character_,
          path = getwd(),
          date = Sys.time()
        )
      }

      if (!is.null(settings)) self$add_settings(settings)

      if (is.null(analyses) & !is.null(files)) {
        analyses <- parse.msAnalysis(files, runParallel)
        if (is.null(analyses)) {
          warning("No valid files were given! msData object is empty. \n")
        }
      }

      if (!is.null(analyses)) self$add_analyses(analyses)

      if (!is.null(groups)) self$add_groups(groups)

      if (!is.null(alignment)) self$add_alignment(alignment)

      message("\U2713 msData class object created!")
    },

    #' @description
    #' Prints a summary of the `msData` object in the console.
    #'
    #' @return Console text.
    #'
    print = function() {
      cat(
        "Class         ", paste(is(self), collapse = "; "), "\n",
        "Name          ", private$.headers$name, "\n",
        "Date          ", as.character(private$.headers$date), "\n",
        sep = ""
      )

      cat("\n")

      if (self$has_settings()) {
        cat("Settings: \n")
        names_settings <- names(private$.settings)
        cat(
          paste0(" ", seq_len(length(names_settings)), ": ", names_settings),
          sep = "\n"
        )
        cat("\n")
      }

      if (length(private$.analyses) > 0) {
        overview <- self$get_overview()
        overview$file <- NULL
        row.names(overview) <- paste0("Analyses ", seq_len(nrow(overview)), ":")
        print(overview)

      } else {
        cat("Analyses      ", 0, "\n", sep = "")
      }
      cat("\n")
    },

    ## ___ get -----

    #' @description
    #' Method to get the headers of the `msData` object.
    #'
    #' @param value A character vector with the name of the entry.
    #' Possible values are name, author, description, path and date.
    #'
    #' @return The headers list as defined by `value`.
    #'
    get_headers = function(value = NULL) {
      if (is.null(value)) {
        private$.headers
      } else {
        private$.headers[value]
      }
    },

    #' @description
    #' Method to get specific analyses from the `msData` object.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return The list of analyses defined by `value`.
    #'
    get_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      private$.analyses[analyses]
    },

    #' @description
    #' Method to get the number of analysis in the `msData` object.
    #'
    #' @return An integer value.
    #'
    get_number_analyses = function() {
      length(private$.analyses)
    },

    #' @description
    #' Method to get the overview data.frame with all the analysis types,
    #' names, replicates, associated blank replicates, polarities and full
    #' file paths.
    #'
    #' @return A data.frame with columns type, analysis, replicate, blank
    #' polarity and file.
    #'
    get_overview = function() {
      if (length(private$.analyses) > 0) {

        if (!is.null(private$.groups)) {
          groups <- apply(
            private$.groups[, self$get_analysis_names(), with = FALSE],
            2, function(x) {
              length(x[x > 0])
            }
          )
        } else {
          groups <- 0
        }

        df <- data.frame(
          "type" = vapply(private$.analyses, function(x) x$type, ""),
          "analysis" = vapply(private$.analyses, function(x) x$name, ""),
          "replicate" = vapply(private$.analyses, function(x) x$replicate, ""),
          "blank" = vapply(private$.analyses, function(x) x$blank, ""),
          "polarity" = vapply(private$.analyses, function(x) {
            paste(x$polarity, collapse = "; ")
          }, ""),
          "traces" = vapply(private$.analyses, function(x) {
            x$spectra_number
          }, 0),
          "features" = vapply(private$.analyses, function(x) {
            nrow(x$features)
          }, 0),
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
    #' Method to get the analysis names.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
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
    #' Method to get the analysis replicate names.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_replicate_names = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      rpl <- vapply(private$.analyses, function(x) x$replicate, "")
      names(rpl) <- vapply(private$.analyses, function(x) x$name, "")
      rpl[analyses]
    },

    #' @description
    #' Method to get the analysis blank replicate names.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_blank_names = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      blk <- vapply(private$.analyses, function(x) x$blank, "")
      names(blk) <- vapply(private$.analyses, function(x) x$name, "")
      blk[analyses]
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_polarities = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      pol <- vapply(private$.analyses, function(x) {
        paste(x$polarity, collapse = "; ")
      }, "")
      names(pol) <- vapply(private$.analyses, function(x) x$name, "")
      pol[analyses]
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_file_paths = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      fls <- vapply(private$.analyses, function(x) x$file, "")
      names(fls) <- vapply(private$.analyses, function(x) x$name, "")
      fls[analyses]
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_mz_low = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      value <- vapply(private$.analyses, function(x) x$mz_low, 0)
      names(value) <- vapply(private$.analyses, function(x) x$name, "")
      value[analyses]
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_mz_high = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      value <- vapply(private$.analyses, function(x) x$mz_high, 0)
      names(value) <- vapply(private$.analyses, function(x) x$name, "")
      value[analyses]
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_rt_start = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      value <- vapply(private$.analyses, function(x) x$rt_start, 0)
      names(value) <- vapply(private$.analyses, function(x) x$name, "")
      value[analyses]
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_rt_end = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      value <- vapply(private$.analyses, function(x) x$rt_end, 0)
      names(value) <- vapply(private$.analyses, function(x) x$name, "")
      value[analyses]
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_spectra_mode = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      value <- vapply(private$.analyses, function(x) x$spectra_mode, "")
      names(value) <- vapply(private$.analyses, function(x) x$name, "")
      value[analyses]
    },

    #' @description
    #' Method to get the spectra levels of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A list for each analysis with an integer vector.
    #'
    get_spectra_levels = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      value <- lapply(private$.analyses, function(x) x$spectra_levels)
      names(value) <- vapply(private$.analyses, function(x) x$name, "")
      value[analyses]
    },

    #' @description
    #' Method to get spectra from the MS analyses.
    #'
    #' @param analyses X.
    #' @param levels Name.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param allTraces Logical, when \code{TRUE} all level 2 data is returned.
    #' When \code{FALSE} and level has 2, only the MS2 traces of MS1 targets
    #' are returned, using the `isolationWindow` to calculate the mass window.
    #' @param isolationWindow X.
    #' @param minIntensityMS1 X.
    #' @param minIntensityMS2 X.
    #' @param runParallel X.
    #'
    #' @return A data.frame with spectra for each analyses and
    #' targets when defined.
    #'
    get_spectra = function(analyses = NULL, levels = NULL,
                           mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                           allTraces = TRUE, isolationWindow = 1.3,
                           minIntensityMS1 = 0, minIntensityMS2 = 0,
                           runParallel = FALSE) {

      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())

      if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) {
        minIntensityMS1 <- 0
      }

      if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) {
        minIntensityMS1 <- 0
      }

      targets <- make_ms_targets(mz, rt, ppm, sec, id)

      if (TRUE %in% (targets$mzmax == 0)) {
        targets$mzmax[targets$mzmax == 0] <- max(self$get_mz_high(analyses))
      }

      if (TRUE %in% (targets$rtmax == 0)) {
        targets$rtmax[targets$rtmax == 0] <- max(self$get_rt_end(analyses))
      }

      has_spectra <- self$has_loaded_spectra(analyses)

      if (all(has_spectra)) {
        if (!2 %in% levels) allTraces <- TRUE

        if (!is.logical(allTraces)) allTraces <- TRUE

        if (!allTraces) {
          if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) {
            isolationWindow <- 0
          }
          preMZr <- targets[, c("mzmin", "mzmax")]
          preMZr$mzmin <- preMZr$mzmin - (isolationWindow / 2)
          preMZr$mzmax <- preMZr$mzmax + (isolationWindow / 2)
          if (nrow(preMZr) == 1 & TRUE %in% (targets$mzmax == 0)) {
            preMZr <- NULL
          }
        } else {
          preMZr <- NULL
        }

        spec_list <- lapply(self$get_analyses(analyses),
          function(x, levels, targets, preMZr, minMS1, minMS2) {
            temp <- x$spectra
            if (!is.null(levels)) temp <- temp[temp$level %in% levels, ]
            if (!is.null(targets)) {
              if ("analysis" %in% colnames(targets)) {
                tp_tar <- targets[targets$analysis %in% x$name, ]
                if (!is.null(preMZr)) {
                  pre_tar <- preMZr[targets$analysis %in% x$name, ]
                } else {
                  pre_tar <- NULL
                }
                if (nrow(tp_tar) > 0) {
                  temp <- trim_spectra_targets(temp, tp_tar, pre_tar)
                } else {
                  temp <- data.frame()
                }
              } else {
                temp <- trim_spectra_targets(temp, targets, preMZr)
              }
            }
            temp <- temp[!(temp$intensity <= minMS1 & temp$level == 1), ]
            temp <- temp[!(temp$intensity <= minMS2 & temp$level == 2), ]
            temp
          },
          levels = levels,
          targets = targets,
          preMZr = preMZr,
          minMS1 = minIntensityMS1,
          minMS2 = minIntensityMS2
        )

      } else {
        files <- unname(self$get_file_paths(analyses))
        spec_list <- parse_ms_spectra(
          files, levels, targets, allTraces, isolationWindow, runParallel,
          minIntensityMS1, minIntensityMS2
        )
      }

      if (length(spec_list) == length(analyses)) {
        names(spec_list) <- analyses
        spec <- rbindlist(spec_list, idcol = "analysis", fill = TRUE)
        spec
      } else {
        warning("Defined analyses not found!")
        data.table()
      }
    },

    #' @description
    #' Method to get spectra from the MS analyses.
    #'
    #' @param analyses X.
    #' @param minIntensity X.
    #' @param runParallel X.
    #'
    #' @return A data.frame with spectra.
    #'
    get_chromatograms = function(analyses = NULL, minIntensity = 0,
                                 runParallel = FALSE) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())

      files <- unname(self$get_file_paths(analyses))

      chrom_list <- parse_ms_chromatograms(files, runParallel)

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
    #' Method to get the total ion chromatograms (TIC) from the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_tic = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      tic <- lapply(private$.analyses[analyses], function(x) x$tic)
      tic <- rbindlist(tic, idcol = "analysis", fill = TRUE)
      tic
    },

    #' @description
    #' Method to get the base peak chromatograms (BPC) from the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    #'
    get_bpc = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      bpc <- lapply(private$.analyses[analyses], function(x) x$bpc)
      bpc <- rbindlist(bpc, idcol = "analysis", fill = TRUE)
      bpc
    },

    #' @description
    #' Method to get extract ion chromatograms (EIC) from the analyses based
    #' on targets.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param runParallel X.
    #'
    #' @return A data.frame.
    #'
    get_eic = function(analyses = NULL,
                       mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                       runParallel = FALSE) {

      eic <- self$get_spectra(
        analyses,
        levels = 1,
        mz, rt, ppm, sec, id,
        allTraces = TRUE,
        isolationWindow = 1.3,
        minIntensityMS1 = 0,
        minIntensityMS2 = 0,
        runParallel = runParallel
      )

      if (nrow(eic) > 0) {
        eic <- as.data.table(eic)
        if (!"id" %in% colnames(eic)) eic$id <- NA_character_
        eic <- eic[, `:=`(intensity = sum(intensity)),
          by = c("analysis", "id", "rt")
        ][]
        eic <- eic[, c("analysis", "id", "rt", "intensity"), with = FALSE]
        eic <- unique(eic)
      }

      eic
    },

    #' @description
    #' Method to get MS1 data from the analyses based on targets.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param mzClust X.
    #' @param verbose X.
    #' @param minIntensity X.
    #' @param runParallel X.
    #'
    #' @return A data.frame.
    #'
    get_ms1 = function(analyses = NULL,
                       mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                       mzClust = 0.003, verbose = FALSE,
                       minIntensity = 1000, runParallel = FALSE) {

      ms1 <- self$get_spectra(
        analyses = analyses, levels = 1,
        mz = mz, rt = rt, ppm = ppm, sec = sec, id = id, allTraces = TRUE,
        minIntensityMS1 = minIntensity, minIntensityMS2 = 0,
        runParallel = runParallel
      )

      if (nrow(ms1) == 0) return(ms1)

      if (!"id" %in% colnames(ms1)) {
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

      if (!is.logical(verbose)) verbose = FALSE
      if (!is.numeric(mzClust)) mzClust = 0.01
      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$id)
      ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, verbose)
      ms1_df <- rbindlist(ms1_list, fill = TRUE)

      ms1_df <- ms1_df[order(ms1_df$mz), ]
      ms1_df <- ms1_df[order(ms1_df$id), ]
      ms1_df <- ms1_df[order(ms1_df$analysis), ]

      ms1_df
    },

    #' @description
    #' Method to get MS2 data from the analyses based on targets.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param isolationWindow X.
    #' @param mzClust X.
    #' @param verbose X.
    #' @param minIntensity X.
    #' @param runParallel X.
    #'
    #' @return A data.frame.
    #'
    get_ms2 = function(analyses = NULL,
                       mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                       isolationWindow = 1.3, mzClust = 0.005, verbose = FALSE,
                       minIntensity = 0, runParallel = FALSE) {

      ms2 <- self$get_spectra(
        analyses = analyses, levels = 2,
        mz = mz, rt = rt, ppm = ppm, sec = sec, id = id,
        isolationWindow = isolationWindow, allTraces = FALSE,
        minIntensityMS1 = 0, minIntensityMS2 = minIntensity,
        runParallel = runParallel
      )

      if (nrow(ms2) == 0) return(ms2)

      if (!"id" %in% colnames(ms2)) {
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

      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id)
      ms2_list <- rcpp_ms_cluster_ms2(ms2, mzClust, verbose)
      ms2_df <- rbindlist(ms2_list, fill = TRUE)

      ms2_df <- ms2_df[order(ms2_df$mz), ]
      ms2_df <- ms2_df[order(ms2_df$id), ]
      ms2_df <- ms2_df[order(ms2_df$analysis), ]

      ms2_df
    },

    #' @description
    #' Method to get settings from analyses.
    #'
    #' @param call A string with the name of function call.
    #'
    #' @return A data.frame.
    #'
    get_settings = function(call = NULL) {
      if (is.null(call)) {
        private$.settings
      } else {
        private$.settings[call]
      }
    },

    #' @description
    #' Method to get features from analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param features X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param filtered X.
    #'
    #' @return A data.frame.
    #'
    get_features = function(analyses = NULL, features = NULL, mass = NULL,
                            mz = NULL, rt = NULL, ppm = 20, sec = 60,
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
        }

        if (is.data.frame(target_id)) {
          if ("analysis" %in% colnames(target_id)) {
            sel <- rep(FALSE, nrow(fts))
            for (i in seq_len(nrow(target_id))) {
              sel[(fts$feature %in% target_id$feature[i] &
                fts$analysis %in% target_id$analysis[i]) |
                fts$group %in% target_id$group] <- TRUE
            }
            fts <- fts[sel, ]
            return(fts)
          }
        }

        return(data.frame())
      }

      if (!is.null(mass)) {
        if (is.data.frame(mass)) {
          colnames(mass) <- gsub("mass", "mz", colnames(mass))
          colnames(mass) <- gsub("neutralMass", "mz", colnames(mass))
        }
        targets <- make_ms_targets(mass, rt, ppm, sec)
        sel <- rep(FALSE, nrow(fts))
        for (i in seq_len(nrow(targets))) {
          sel[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
            between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
        }
        return(fts[sel])
      }

      if (!is.null(mz)) {
        targets <- make_ms_targets(mz, rt, ppm, sec)
        sel <- rep(FALSE, nrow(fts))
        for (i in seq_len(nrow(targets))) {
          sel[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
            between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
        }
        return(fts[sel])
      }

      fts
    },

    #' @description
    #' Method to get EIC of features from analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param features X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param rtExpand X.
    #' @param mzExpand X.
    #' @param filtered X.
    #' @param runParallel X.
    #'
    #' @return A data.frame/data.table.
    #'
    get_features_eic = function(analyses = NULL, features = NULL, mass = NULL,
                                mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                rtExpand = 120, mzExpand = 0.005,
                                filtered = FALSE, runParallel = FALSE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, ppm, sec, filtered
      )

      if (nrow(fts) == 0) return(data.table())

      fts$rtmin <- fts$rtmin - rtExpand
      fts$rtmax <- fts$rtmax + rtExpand
      fts$mzmin <- fts$mzmin - mzExpand
      fts$mzmax <- fts$mzmax + mzExpand

      eic <- self$get_eic(
        analyses = analyses,
        mz = fts, id = fts$feature,
        runParallel = runParallel
      )

      eic
    },

    #' @description
    #' Method to get an averaged MS1 spectrum for features in analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param features X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param rtWindow X.
    #' @param mzWindow X.
    #' @param mzClust X.
    #' @param minIntensity X.
    #' @param verbose X.
    #' @param filtered X.
    #' @param loadedMS1 X.
    #' @param runParallel X.
    #'
    #' @return A data.frame/data.table.
    #'
    get_features_ms1 = function(analyses = NULL, features = NULL, mass = NULL,
                                mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                rtWindow = c(-2, 2), mzWindow = c(-5, 100),
                                mzClust = 0.003, minIntensity = 1000,
                                verbose = FALSE, filtered = FALSE,
                                loadedMS1 = TRUE, runParallel = FALSE) {

      fts <- self$get_features(analyses, features, mass, mz, rt, ppm, sec, filtered)
      if (nrow(fts) == 0) return(data.frame())

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
        ms1 <- fts$ms1
        unique_ids <- paste0(fts$analysis, fts$index)
        names(ms1) <- unique_ids
        ms1 <- lapply(ms1, function(x) {
          if (is.null(x)) x <- data.table()
          x
        })
        ms1 <- rbindlist(ms1, idcol = "unique_id")
        analysis_col <- fts$analysis
        names(analysis_col) <- unique_ids
        id_col <- fts$feature
        names(id_col) <- unique_ids
        ms1$analysis <- analysis_col[ms1$unique_id]
        ms1$id <- id_col[ms1$unique_id]
        ms1$unique_id <- NULL
        setcolorder(ms1, c("analysis", "id"))

      } else {
        ms1 <- self$get_ms1(
          analyses = unique(fts$analysis), mz = fts, id = fts$feature,
          mzClust = mzClust, minIntensity = minIntensity,
          verbose = verbose, runParallel = runParallel
        )
      }

      if ("group" %in% colnames(fts)) {
        fgs <- fts$group
        names(fgs) <- fts$feature
        ms1$group <- fgs[ms1$id]
      }

      ms1
    },

    #' @description
    #' Method to get an averaged MS2 spectrum for features in analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param features X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param isolationWindow X.
    #' @param mzClust X.
    #' @param minIntensity X.
    #' @param verbose X.
    #' @param filtered X.
    #' @param loadedMS2 X.
    #' @param runParallel X.
    #'
    #' @return A data.frame/data.table.
    #'
    get_features_ms2 = function(analyses = NULL, features = NULL, mass = NULL,
                                mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                isolationWindow = 1.3, mzClust = 0.003,
                                minIntensity = 0, verbose = FALSE,
                                filtered = FALSE, loadedMS2 = TRUE,
                                runParallel = FALSE) {

      fts <- self$get_features(analyses, features, mass, mz, rt, ppm, sec, filtered)

      if (nrow(fts) == 0) return(data.frame())

      analysis_names <- unique(fts$analysis)

      if (loadedMS2 & any(self$has_loaded_features_ms2(analysis_names))) {
        ms2 <- fts$ms2
        unique_ids <- paste0(fts$analysis, fts$index)
        names(ms2) <- unique_ids
        ms2 <- lapply(ms2, function(x) {
          if (is.null(x)) x <- data.table()
          x
        })
        ms2 <- rbindlist(ms2, idcol = "unique_id")
        analysis_col <- fts$analysis
        names(analysis_col) <- unique_ids
        id_col <- fts$feature
        names(id_col) <- unique_ids
        ms2$analysis <- analysis_col[ms2$unique_id]
        ms2$id <- id_col[ms2$unique_id]
        ms2$unique_id <- NULL
        setcolorder(ms2, c("analysis", "id"))

      } else {
        ms2 <- self$get_ms2(
          analyses = unique(fts$analysis), mz = fts, id = fts$feature,
          isolationWindow = isolationWindow, mzClust = mzClust,
          minIntensity = minIntensity, verbose = verbose,
          runParallel = runParallel
        )
      }

      if ("group" %in% colnames(fts)) {
        fgs <- fts$group
        names(fgs) <- fts$feature
        ms2$group <- fgs[ms2$id]
      }

      ms2
    },

    #' @description
    #' Method to get alignment.
    #'
    #' @return A data.frame.
    #'
    get_alignment = function() {
      private$.alignment
    },

    #' @description
    #' Method to get feature groups from analyses.
    #'
    #' @param groups X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param filtered X.
    #' @param onlyIntensities X.
    #' @param average X.
    #'
    #' @return A data.frame/data.table.
    #'
    get_groups = function(groups = NULL, mass = NULL,
                          mz = NULL, rt = NULL, ppm = 20, sec = 60,
                          filtered = FALSE, onlyIntensities = FALSE,
                          average = FALSE) {

      fgroups <- copy(private$.groups)
      if (self$has_groups()) {
        if (!filtered) fgroups <- fgroups[!fgroups$filtered, ]

        if (!is.null(groups)) {
          fgroups <- fgroups[fgroups$group %in% groups, ]
        } else if (!is.null(mass)) {
          if (is.data.frame(mass)) {
            colnames(mass) <- gsub("mass", "mz", colnames(mass))
            colnames(mass) <- gsub("neutralMass", "mz", colnames(mass))
          }
          targets <- make_ms_targets(mass, rt, ppm, sec)
          sel <- rep(FALSE, nrow(fgroups))
          for (i in seq_len(nrow(targets))) {
            sel[between(
              fgroups$mass,
              targets$mzmin[i],
              targets$mzmax[i]
            ) &
              between(
                fgroups$rt,
                targets$rtmin[i],
                targets$rtmax[i]
              )] <- TRUE
          }
          fgroups <- fgroups[sel, ]
        } else if (!is.null(mz)) {
          targets <- make_ms_targets(mz, rt, ppm, sec)
          sel <- rep(FALSE, nrow(fgroups))

          if (!"mz" %in% colnames(fgroups)) {
            adduct <- paste(unique(fgroups$adduct), collapse = ",")

            if (grepl("\\[M\\+H\\]\\+", adduct)) {
              for (i in seq_len(nrow(targets))) {
                if (targets$rtmax[i] > 0) {
                  sel[between(
                    fgroups$mass,
                    targets$mzmin[i] - 1.007276,
                    targets$mzmax[i] - 1.007276
                  ) &
                    between(
                      fgroups$rt,
                      targets$rtmin[i],
                      targets$rtmax[i]
                    )] <- TRUE
                } else {
                  sel[between(
                    fgroups$mass,
                    targets$mzmin[i] - 1.007276,
                    targets$mzmax[i] - 1.007276
                  )] <- TRUE
                }
              }
            }

            if (grepl("\\[M-H\\]-", adduct)) {
              for (i in seq_len(nrow(targets))) {
                if (targets$rtmax[i] > 0) {
                  sel[between(
                    fgroups$mass,
                    targets$mzmin[i] + 1.007276,
                    targets$mzmax[i] + 1.007276
                  ) &
                    between(
                      fgroups$rt,
                      targets$rtmin[i],
                      targets$rtmax[i]
                    )] <- TRUE
                } else {
                  sel[between(
                    fgroups$mass,
                    targets$mzmin[i] + 1.007276,
                    targets$mzmax[i] + 1.007276
                  )] <- TRUE
                }
              }
            }
          } else {
            for (i in seq_len(nrow(targets))) {
              if (targets$rtmax[i] > 0) {
                sel[between(
                  fgroups$mz,
                  targets$mzmin[i],
                  targets$mzmax[i]
                ) &
                  between(
                    fgroups$rt,
                    targets$rtmin[i],
                    targets$rtmax[i]
                  )] <- TRUE
              } else {
                sel[between(
                  feats@metadata$mz,
                  targets$mzmin[i],
                  targets$mzmax[i]
                )] <- TRUE
              }
            }
          }

          fgroups <- fgroups[sel, ]
        }

        if (onlyIntensities) {
          cols_id_ints <- unname(c("group", self$get_analysis_names()))
          fgroups <- fgroups[, cols_id_ints, with = FALSE]
        }

        if (average) {
          rpl_ana <- self$get_overview()[, c("analysis", "replicate")]
          rpl_ana <- split(rpl_ana, rpl_ana$replicate)
          rpl_ana <- lapply(rpl_ana, function(x) x$analysis)

          sd_vals <- lapply(rpl_ana, function(x, fgroups) {
            temp <- fgroups[, x, with = FALSE]
            temp <- apply(temp, 1, function(x) sd(x) / mean(x) * 100)
            temp[is.nan(temp)] <- 0
            temp <- round(temp, digits = 0)
            temp
          }, fgroups = fgroups)

          for (r in names(rpl_ana)) {
            ana <- rpl_ana[[r]]
            fgroups[[r]] <- apply(fgroups[, ana, with = FALSE], 1, mean)
          }

          to_keep <- colnames(fgroups)
          to_keep <- to_keep[!to_keep %in% self$get_analysis_names()]
          fgroups <- fgroups[, to_keep, with = FALSE]

          names(sd_vals) <- paste0(rpl, "_sd")
          fgroups <- cbind(fgroups, as.data.table(sd_vals))
        }
      }
      if (is.null(fgroups)) fgroups <- data.table()
      fgroups
    },

    #' @description
    #' Method to get an averaged MS1 spectrum for feature groups in analyses.
    #'
    #' @param groups X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param rtWindow X.
    #' @param mzWindow X.
    #' @param mzClustFeatures X.
    #' @param minIntensityFeatures X.
    #' @param loadedFeaturesMS1 X.
    #' @param mzClustGroups X.
    #' @param minIntensityGroups X.
    #' @param groupBy X.
    #' @param verbose X.
    #' @param filtered X.
    #' @param loadedGroupsMS1 X.
    #' @param runParallel X.
    #'
    #' @return A data.frame/data.table.
    #'
    get_groups_ms1 = function(groups = NULL, mass = NULL,
                              mz = NULL, rt = NULL, ppm = 20, sec = 60,
                              rtWindow = c(-2, 2), mzWindow = c(-5, 90),
                              mzClustFeatures = 0.003,
                              minIntensityFeatures = 1000,
                              loadedFeaturesMS1 = TRUE,
                              mzClustGroups = 0.003,
                              minIntensityGroups = 1000,
                              groupBy = "groups",
                              verbose = FALSE, filtered = FALSE,
                              loadedGroupsMS1 = TRUE, runParallel = FALSE) {

      fgs <- self$get_groups(
        groups, mass, mz, rt, ppm, sec, filtered,
        onlyIntensities = FALSE, average = FALSE
      )

      if (nrow(fgs) == 0) {
        return(data.table())
      }

      if (loadedGroupsMS1 & self$has_loaded_groups_ms1()) {
        ms1 <- fgs$ms1
        ids <- fgs$group
        names(ms1) <- ids
        ms1 <- lapply(ms1, function(x) {
          if (is.null(x)) x <- data.table()
          x
        })
        ms1 <- rbindlist(ms1, idcol = "id")
        return(ms1)
      }

      fts <- self$get_features(features = fgs$group)

      if (nrow(fts) == 0) {
        return(data.table())
      }

      ms1 <- self$get_features_ms1( analyses = unique(fts$analysis),
        features = fts$feature, rtWindow = rtWindow, mzWindow = mzWindow,
        mzClust = mzClustFeatures, minIntensity = minIntensityFeatures,
        verbose = verbose, filtered = filtered, loadedMS1 = loadedFeaturesMS1,
        runParallel = runParallel
      )

      ms1$id <- ms1$group
      ms1$group <- NULL

      ms1 <- ms1[ms1$intensity > minIntensityGroups, ]

      if (nrow(ms1) == 0) {
        return(data.table())
      }

      polarities <- unique(self$get_polarities(analyses = unique(ms1$analysis)))
      if (length(polarities) != 1 | "both" %in% polarities) groupBy <- NULL

      if ("groups" %in% groupBy) {
        ms1$unique_id <- ms1$id
        ms1$analysis <- NA_character_
      } else {
        rpls <- self$get_replicate_names()
        ms1$analysis <- rpls[ms1$analysis]
        ms1$unique_id <- paste0(ms1$analysis, "_", ms1$id)
      }

      ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClustGroups, verbose)
      ms1_df <- rbindlist(ms1_list, fill = TRUE)
      ms1_df <- ms1_df[order(ms1_df$mz), ]
      ms1_df <- ms1_df[order(ms1_df$id), ]

      if ("groups" %in% groupBy) {
        ms1_df[["analysis"]] <- NULL
      } else {
        ms1_df <- ms1_df[order(ms1_df$analysis), ]
        setnames(ms1_df, "analysis", "replicate")
      }

      ms1_df
    },

    #' @description
    #' Method to get an averaged MS2 spectrum for feature groups in analyses.
    #'
    #' @param groups X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param isolationWindow X.
    #' @param mzClustFeatures X.
    #' @param minIntensityFeatures X.
    #' @param loadedFeaturesMS2 X.
    #' @param mzClustGroups X.
    #' @param minIntensityGroups X.
    #' @param groupBy X.
    #' @param verbose X.
    #' @param filtered X.
    #' @param loadedGroupsMS2 X.
    #' @param runParallel X.
    #'
    #' @return A data.frame/data.table.
    #'
    get_groups_ms2 = function(groups = NULL, mass = NULL,
                              mz = NULL, rt = NULL, ppm = 20, sec = 60,
                              isolationWindow = 1.3,
                              mzClustFeatures = 0.003,
                              minIntensityFeatures = 100,
                              loadedFeaturesMS2 = TRUE,
                              mzClustGroups = 0.003,
                              minIntensityGroups = 100,
                              groupBy = "groups",
                              verbose = FALSE, filtered = FALSE,
                              loadedGroupsMS2 = TRUE, runParallel = FALSE) {

      fgs <- self$get_groups(
        groups, mass, mz, rt, ppm, sec, filtered,
        onlyIntensities = FALSE, average = FALSE
      )

      if (nrow(fgs) == 0) {
        return(data.table())
      }

      if (loadedGroupsMS2 & self$has_loaded_groups_ms2()) {
        ms2 <- fgs$ms2
        ids <- fgs$group
        names(ms2) <- ids
        ms2 <- lapply(ms2, function(x) {
          if (is.null(x)) x <- data.table()
          x
        })
        ms2 <- rbindlist(ms2, idcol = "id")
        return(ms2)
      }

      fts <- self$get_features(features = fgs$group)

      if (nrow(fts) == 0) {
        return(data.table())
      }

      ms2 <- self$get_features_ms2(analyses = unique(fts$analysis),
        features = fts$feature, isolationWindow = isolationWindow,
        mzClust = mzClustFeatures, minIntensity = minIntensityFeatures,
        verbose = verbose, filtered = filtered, loadedMS2 = loadedFeaturesMS2,
        runParallel = runParallel
      )

      ms2$id <- ms2$group
      ms2$group <- NULL

      ms2 <- ms2[ms2$intensity > minIntensityGroups, ]

      if (nrow(ms2) == 0) {
        return(data.table())
      }

      polarities <- unique(self$get_polarities(analyses = unique(ms2$analysis)))
      if (length(polarities) != 1 | "both" %in% polarities) groupBy <- NULL

      if ("groups" %in% groupBy) {
        ms2$unique_id <- ms2$id
        ms2$analysis <- NA_character_
      } else {
        rpls <- self$get_replicate_names()
        ms2$analysis <- rpls[ms2$analysis]
        ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id)
      }

      ms2_list <- rcpp_ms_cluster_ms2(ms2, mzClustGroups, verbose)
      ms2_df <- rbindlist(ms2_list, fill = TRUE)
      ms2_df <- ms2_df[order(ms2_df$mz), ]
      ms2_df <- ms2_df[order(ms2_df$id), ]

      if ("groups" %in% groupBy) {
        ms2_df[["analysis"]] <- NULL
      } else {
        ms2_df <- ms2_df[order(ms2_df$analysis), ]
        setnames(ms2_df, "analysis", "replicate")
      }

      ms2_df
    },

    ## ___ add -----

    #' @description
    #' Method to add headers information to the `msData` object. If an argument
    #' or element name is given, it must be type character. If an argument or
    #' element path is given, it must be type character and exist. If an
    #' argument or element date is given, it must be class POSIXct or POSIXt.
    #' If given date is character, conversion to class POSIXct or POSIXt is
    #' attempted. See `?headers` for more information.
    #'
    #' @template arg-headers-ellipsis
    #'
    #' @return Invisible.
    #'
    add_headers = function(...) {

      headers <- headers(...)

      if (is(headers, "headers")) {
        old_headers <- private$.headers
        if (is.null(old_headers)) old_headers <- list()

        if (length(old_headers) > 0) {
          new_headers <- old_headers[!names(old_headers) %in% names(headers)]
          new_headers[names(headers)] <- headers
        } else {
          new_headers <- headers
        }

        new_headers <- as.headers(new_headers)

        if (!identical(new_headers, old_headers) & is(new_headers, "headers")) {
          private$.headers <- new_headers
          message("\U2713 Added headers!")
        }

      } else {
        warning("Invalid headers content or structure! Not added.")
      }
      invisible(self)
    },

    #' @description
    #' Method to add processing settings to the `msData` object.
    #'
    #' @template arg-settings-list
    #'
    #' @return Invisible.
    #'
    add_settings = function(settings = NULL) {
      if (is.list(settings)) {

        if (all(c("call", "algorithm", "parameters") %in% names(settings))) {
          settings <- as.settings(settings)

          if (!is.null(settings)) {
            private$.settings[[settings$call]] <- settings
            message(
              paste0("\U2713 ", settings$call, " processing settings added!")
            )
          }

        } else if (all(vapply(settings, validate.settings, FALSE))) {
          settings <- lapply(settings, as.settings)
          call_names <- vapply(settings, function(x) x$call, NA_character_)
          private$.settings[call_names] <- settings
          message(paste0("\U2713 Added settings for:\n",
            paste(call_names, collapse = "\n"))
          )

        } else {
          warning("Settings content or structure not conform! Not added.")
        }
      } else {
        warning("Settings content or structure not conform! Not added.")
      }
      invisible(self)
    },

    #' @description
    #' Method to add analyses to the `msData` object.
    #'
    #' @template arg-ms-analyses-list
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {

      if (is.list(analyses)) {
        if (all(c("name", "file") %in% names(analyses))) {
          analyses <- as.msAnalysis(analyses)

          if (is(analyses, "msAnalysis")) {
            ana_name <- analyses$name
            analyses <- list(analyses)
            names(analyses) <- ana_name

          } else {
            warning("Not done, check the conformity of the analyses list!")
            analyses <- NULL
          }

        } else {
          analyses <- lapply(analyses, as.msAnalysis)

          if (all(vapply(analyses, function(x) is(x, "msAnalysis"), FALSE))) {
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

        if (!any(duplicated(c(new_names)))) {
          new_analyses <- c(old_analyses, analyses)
          names(new_analyses) <- new_names
          new_analyses <- new_analyses[order(names(new_analyses))]
          old_size <- length(private$.analyses)

          private$.analyses <- new_analyses
          message(
            paste0(
              "\U2713 ",
              length(new_analyses) - old_size,
              " analyses added!"
            )
          )

          if (old_size < length(new_analyses)) {
            if (self$has_groups()) {
              warning("Feature groups cleared as new analyses were added!")
              private$.analyses <- lapply(private$.analyses, function(x) {
                x$features[["feature"]] <- NULL
                x
              })
              private$.groups <- NULL
              private$.alignment <- NULL
            }
          }

        } else {
          warning("Duplicated analysis names not allowed! Not done.")
        }
      }
      invisible(self)
    },

    #' @description
    #' Method to add or redefine the analysis replicate names. Changes the
    #' `msData` object.
    #'
    #' @param value A character vector with the analysis replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_replicate_names = function(value = NULL) {
      if (is.character(value) &
        length(value) == self$get_number_analyses()) {
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
    #' Method to add or redefine the analysis blank replicate names.
    #' Changes the `msData` object.
    #'
    #' @param value A character vector with the analysis blank replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_blank_names = function(value = NULL) {
      if (is.character(value) &
        length(value) == self$get_number_analyses()) {

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

    #' @description
    #' Method to add features to each analysis in the `msData` object.
    #'
    #' @param features X.
    #'
    #' @return Invisible.
    #'
    add_features = function(features = NULL) {
      valid <- FALSE

      if (is.data.frame(features)) {
        must_have_cols <- c(
          "analysis", "feature", "mz", "rt", "mzmin", "mzmax",
          "rtmin", "rtmax", "intensity", "area"
        )

        if (all(must_have_cols %in% colnames(features))) {
          features <- features[order(features$analysis), ]
          analysis_names <- unique(features$analysis)
          org_analysis_names <- unname(self$get_analysis_names())

          if (identical(analysis_names, org_analysis_names)) {
            valid <- TRUE
          }
        }
      }

      if (valid) {
        features <- split(features, features$analysis)
        private$.analyses <- Map(
          function(x, y) {
            x$features <- y
            x
          },
          private$.analyses, features
        )
        message("\U2713 Features added!")

      } else {
        warning("Invalid features content or structure! Not added.")
      }
      invisible(self)
    },

    #' @description
    #' Method to add feature groups in the `msData` object.
    #'
    #' @template arg-ms-groups
    #'
    #' @return Invisible.
    #'
    add_groups = function(groups = NULL) {

      if (is.list(groups)) {
        if ("ms1" %in% names(groups)) {
          groups$ms1 <- lapply(groups$ms1, as.data.table)
        }

        if ("ms2" %in% names(groups)) {
          groups$ms2 <- lapply(groups$ms2, as.data.table)
        }

        groups <- as.data.table(groups)

      } else if (is.data.frame(groups)) {

        if ("ms1" %in% colnames(groups)) {
          groups$ms1 <- lapply(groups$ms1, as.data.table)
        }

        if ("ms2" %in% colnames(groups)) {
          groups$ms2 <- lapply(groups$ms2, as.data.table)
        }
      }

      if (is.data.table(groups)) {
        must_have_cols <- c(
          "group", "rt", unname(self$get_analysis_names()),
          "dppm", "drt", "index", "hasFilled", "filtered", "filter",
          "adduct", "mass"
        )

        if (all(must_have_cols %in% colnames(groups))) {
          groups <- groups[order(groups$index), ]
          old_groups <- private$.groups
          private$.groups <- copy(groups)

          if (!self$check_correspondence()) {

            if (is.null(old_groups)) {
              self$remove_groups()
              warning("Removed groups as correspondence did not match!.")

            } else {
              private$.groups <- old_groups
              warning("Correspondence did not match! Groups not added.")
            }

          } else {
            message(paste0("\U2713 ", nrow(groups), " feature groups added!"))
          }

        } else {
          warning("Columns of groups data.frame not as required! Not added.")
        }

      } else {
        warning("Groups must be a data.frame! Not added.")
      }
      invisible(self)
    },

    #' @description
    #' Method to add time alignment results in the `msData` object.
    #'
    #' @param alignment list.
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
          message("\U2713 Alignment added!")

        } else {
          warning("Invalid alignment structure or content! Not added.")
        }

      } else {
        warning("Groups not present or alignment not valid! Not added.")
      }
      invisible(self)
    },

    ## ___ load -----

    #' @description
    #' Method to load all spectra from analyses to the `msData` object.
    #'
    #' @param runParallel X.
    #'
    #' @return Invisible.
    #'
    load_spectra = function(runParallel = FALSE) {
      spec <- self$get_spectra(
        analyses = NULL, levels = NULL,
        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
        allTraces = TRUE, isolationWindow = 1.3,
        minIntensityMS1 = 0, minIntensityMS2 = 0,
        runParallel = runParallel
      )

      split_vector <- spec$analysis
      spec$analysis <- NULL
      spec_list <- split(spec, split_vector)

      if (length(spec_list) == self$get_number_analyses()) {
        private$.analyses <- Map(
          function(x, y) {
            x$spectra <- y
            x
          },
          private$.analyses, spec_list
        )

        message("\U2713 Spectra loaded to all analyses!")

      } else {
        warning("Not done, check the MS file paths and formats!")
      }
      invisible(self)
    },

    #' @description
    #' Method to load all chromatograms from analyses to the `msData` object.
    #'
    #' @param runParallel X.
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
    #' Method to load and average MS1 spectra from features in the analyses.
    #'
    #' @param settings X.
    #'
    #' @return Invisible.
    #'
    load_features_ms1 = function(settings = NULL) {
      valid <- TRUE

      if (is.null(settings)) {
        settings <- self$get_settings(call = "load_features_ms1")[[1]]
      } else if ("load_features_ms1" %in% names(settings)) {
        settings <- settings[["load_features_ms1"]]
      }

      if (validate.settings(settings)) {
        if (!"load_features_ms1" %in% settings$call) {
          warning("Settings call must be load_features_ms1!")
          valid <- FALSE
        }
      } else {
        warning("Settings content or structure not conform!")
        valid <- FALSE
      }

      if (!valid) {
        invisible(self)
      }

      algorithm <- settings$algorithm
      parameters <- settings$parameters

      if ("streamFind" %in% algorithm) {

        if (requireNamespace("patRoon", quietly = TRUE)) {
          ana_feats <- self$get_features()
          ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
          hash <- patRoon:::makeHash(ana_feats, parameters)
          ms1 <- patRoon:::loadCacheData("loadFeaturesMS1", hash)
          if (!all(ms1$id %in% ana_feats$feature)) ms1 <- NULL

        } else {
          hash <- NULL
          ms1 <- NULL
        }

        if (is.null(ms1)) {
          ms1 <- self$get_features_ms1(
            rtWindow = parameters$rtWindow,
            mzWindow = parameters$mzWindow,
            mzClust = parameters$mzClust,
            minIntensity = parameters$minIntensity,
            verbose = parameters$verbose,
            filtered = parameters$filtered,
            loadedMS1 = FALSE,
            runParallel = parameters$runParallel
          )

          if (!is.null(hash)) {
            patRoon:::saveCacheData("loadFeaturesMS1", ms1, hash)
          }
        }

        analyses <- self$get_analyses()

        analyses <- lapply(analyses, function(x, ms1) {
          ana <- x$name
          ana_ms1 <- ms1[ms1$analysis %in% ana, ]
          fts_all <- x$features$feature
          fts_ms1 <- lapply(fts_all, function(x2, ana_ms1) {
            ft_ms1 <- ana_ms1[ana_ms1$id %in% x2, ]
            if (nrow(ft_ms1) > 0) {
              cols <- c("rt","mz", "intensity")
              ft_ms1 <- ft_ms1[, cols, with = FALSE]
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
          message("\U2713 MS1 data added to features in analyses!")
          self$add_settings(settings)
        }
      }
      invisible(self)
    },

    #' @description
    #' Method to load and average MS2 spectra from features in the analyses.
    #'
    #' @param settings X.
    #'
    #' @return Invisible.
    #'
    load_features_ms2 = function(settings = NULL) {
      valid <- TRUE

      if (is.null(settings)) {
        settings <- self$get_settings(call = "load_features_ms2")[[1]]
      } else if ("load_features_ms2" %in% names(settings)) {
        settings <- settings[["load_features_ms2"]]
      }

      if (validate.settings(settings)) {
        if (!"load_features_ms2" %in% settings$call) {
          warning("Settings call must be 'load_features_ms2'!")
          valid <- FALSE
        }
      } else {
        warning("Settings content or structure not conform!")
        valid <- FALSE
      }

      if (!valid) {
        invisible(self)
      }

      algorithm <- settings$algorithm
      parameters <- settings$parameters

      if ("streamFind" %in% algorithm) {

        if (requireNamespace("patRoon", quietly = TRUE)) {
          ana_feats <- self$get_features()
          ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
          hash <- patRoon:::makeHash(ana_feats, parameters)
          ms2 <- patRoon:::loadCacheData("loadFeaturesMS2", hash)
          if (!all(ms2$id %in% ana_feats$feature)) ms2 <- NULL

        } else {
          hash <- NULL
          ms2 <- NULL
        }

        if (is.null(ms2)) {
          ms2 <- self$get_features_ms2(
            isolationWindow =  parameters$isolationWindow,
            mzClust = parameters$mzClust,
            minIntensity = parameters$minIntensity,
            verbose = parameters$verbose,
            filtered = parameters$filtered,
            loadedMS2 = FALSE,
            runParallel = parameters$runParallel
          )

          if (!is.null(hash)) {
            patRoon:::saveCacheData("loadFeaturesMS2", ms2, hash)
          }
        }

        analyses <- self$get_analyses()

        analyses <- lapply(analyses, function(x, ms2) {

          ana <- x$name
          ana_ms2 <- ms2[ms2$analysis %in% ana, ]

          fts_all <- x$features$feature
          fts_ms2 <- lapply(fts_all, function(x2, ana_ms2) {
            ft_ms2 <- ana_ms2[ana_ms2$id %in% x2, ]

            if (nrow(ft_ms2) > 0) {
              cols <- c("preMZ", "rt","mz", "intensity", "isPre")
              ft_ms2 <- ft_ms2[, cols, with = FALSE]
              ft_ms2
            } else {
              NULL
            }
          }, ana_ms2 = ana_ms2)
          x$features$ms2 <- fts_ms2
          x
        }, ms2 = ms2)

        added_ms2 <- vapply(analyses, function(x) {
          "ms2" %in% colnames(x$features)
        }, FALSE)

        if (all(added_ms2)) {
          private$.analyses <- analyses
          message("\U2713 MS2 data added to features in analyses!")
          self$add_settings(settings)
        }
      }
      invisible(self)
    },

    #' @description
    #' Method to load and average MS1 spectra from feature groups. If MS1
    #' spectra from features are already loaded, the feature MS1 spectra is
    #' used for averaging into the respective feature group. If features MS1
    #' are not present, settings for loading and averaging features MS1
    #' spectra (i.e., settings with call name "load_features_ms1") must
    #' be given in the `settingsFeatures` argument or added beforehand to the
    #' msData object.
    #'
    #' @param settings X.
    #' @param settingsFeatures X.
    #'
    #' @return Invisible.
    #'
    load_groups_ms1 = function(settings = NULL, settingsFeatures = NULL) {
      valid <- TRUE

      if (is.null(settings)) {
        settings <- self$get_settings(call = "load_groups_ms1")[[1]]
      } else if ("load_groups_ms1" %in% names(settings)) {
        settings <- settings[["load_groups_ms1"]]
      }

      if (validate.settings(settings)) {
        if (!"load_groups_ms1" %in% settings$call) {
          warning("Settings call must be 'load_groups_ms1'!")
          valid <- FALSE
        }
      } else {
        warning("Settings content or structure not conform!")
        valid <- FALSE
      }

      if (!valid) {
        invisible(self)
      }

      algorithm <- settings$algorithm
      parameters <- settings$parameters

      if ("streamFind" %in% algorithm) {

        if (requireNamespace("patRoon", quietly = TRUE)) {
          ana_feats <- self$get_features()
          ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
          group_ids <- self$get_groups()[["groups"]]
          hash <- patRoon:::makeHash(ana_feats, group_ids,  parameters)
          ms1 <- patRoon:::loadCacheData("loadGroupsMS1", hash)
          if (!all(ms1$id %in% group_ids)) ms1 <- NULL

        } else {
          hash <- NULL
          ms1 <- NULL
        }

        if (is.null(ms1)) {

          if (!any(self$has_loaded_features_ms1())) {
            self$load_features_ms1(settings = settingsFeatures)
          }

          if (any(self$has_loaded_features_ms1())) {
            ms1 <- self$get_groups_ms1(
              rtWindow = NULL, mzWindow = NULL, mzClustFeatures = NULL,
              minIntensityFeatures = NULL, loadedFeaturesMS1 = TRUE,
              groupBy = "groups",
              loadedGroupsMS1 = FALSE,
              mzClustGroups = parameters$mzClust,
              minIntensityGroups = parameters$minIntensity,
              verbose = parameters$verbose,
              filtered = parameters$filtered,
              runParallel = parameters$runParallel
            )

            if (!is.null(hash)) {
              patRoon:::saveCacheData("loadGroupsMS1", ms1, hash)
            }

          } else {
            warning("Features MS1 are not presensent and could not be loaded!")
          }
        }

        if (nrow(ms1) > 0) {
          ms1 <- split(ms1, ms1$id)
          groups <- self$get_groups()
          groups <- groups$group
          groups_ms1 <- lapply(groups, function(x, ms1) {
            temp <- ms1[[x]]
            temp
          }, ms1 = ms1)

          private$.groups$ms1 <- groups_ms1
          message("\U2713 MS1 data added to feature groups!")
          self$add_settings(settings)

        } else {
          warning("Mass traces were not found for feature groups!")
        }
      }
      invisible(self)
    },

    #' @description
    #' Method to load and average MS2 spectra from feature groups.
    #'
    #' @param settings X.
    #' @param settingsFeatures X.
    #'
    #' @return Invisible.
    #'
    load_groups_ms2 = function(settings = NULL, settingsFeatures = NULL) {
      valid <- TRUE

      if (is.null(settings)) {
        settings <- self$get_settings(call = "load_groups_ms2")[[1]]
      } else if ("load_groups_ms2" %in% names(settings)) {
        settings <- settings[["load_groups_ms2"]]
      }

      if (validate.settings(settings)) {
        if (!"load_groups_ms2" %in% settings$call) {
          warning("Settings call must be 'load_groups_ms2'!")
          valid <- FALSE
        }
      } else {
        warning("Settings content or structure not conform!")
        valid <- FALSE
      }

      if (!valid) {
        invisible(self)
      }

      algorithm <- settings$algorithm
      parameters <- settings$parameters

      if ("streamFind" %in% algorithm) {

        if (requireNamespace("patRoon", quietly = TRUE)) {
          ana_feats <- self$get_features()
          ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
          group_ids <- self$get_groups()[["groups"]]
          hash <- patRoon:::makeHash(ana_feats, group_ids,  parameters)
          ms2 <- patRoon:::loadCacheData("loadgroupsMS2", hash)
          if (!all(ms2$id %in% group_ids)) ms2 <- NULL

        } else {
          hash <- NULL
          ms2 <- NULL
        }

        if (is.null(ms2)) {

          if (!all(self$has_loaded_features_ms2())) {
            self$load_features_ms2(settings = settingsFeatures)
          }

          if (all(self$has_loaded_features_ms2())) {
            ms2 <- self$get_groups_ms2(
              isolationWindow = NULL, mzClustFeatures = NULL,
              minIntensityFeatures = NULL, loadedFeaturesMS2 = TRUE,
              groupBy = "groups",
              loadedGroupsMS2 = FALSE,
              mzClustGroups = parameters$mzClust,
              minIntensityGroups = parameters$minIntensity,
              verbose = parameters$verbose,
              filtered = parameters$filtered,
              runParallel = parameters$runParallel
            )

            if (!is.null(hash)) {
              patRoon:::saveCacheData("loadGroupsMS2", ms2, hash)
            }

          } else {
            warning("Features MS2 are not presensent and could not be loaded!")
          }
        }

        if (nrow(ms2) > 0) {
          ms2 <- split(ms2, ms2$id)
          groups <- self$get_groups()
          groups <- groups$group
          groups_ms2 <- lapply(groups, function(x, ms2) {
            temp <- ms2[[x]]
            temp
          }, ms2 = ms2)

          private$.groups$ms2 <- groups_ms2
          message("\U2713 MS2 data added to feature groups!")
          self$add_settings(settings)

        } else {
          warning("Mass traces were not found for feature groups!")
        }
      }
      invisible(self)
    },

    ## ___ remove -----

    #' @description
    #' Removes headers entries from the `msData` object. Note that the name,
    #' path and date headers cannot be removed.
    #'
    #' @param value A character vector with the names of the headers entries
    #' to be removed.
    #'
    #' @return Invisible.
    #'
    remove_headers = function(value = NULL) {
      if (!is.null(value)) {
        value <- value[!(value %in% c("name", "path", "date"))]

        if (length(value) == 0) {
          warning("Name, path and date headers cannot be removed!")
          value <- NA_character_
        }

        if (value %in% names(private$.headers)) {
          private$.headers[value] <- NULL
          message("\U2713 Removed headers: \n",
            paste(value, collapse = "\n")
          )
        }
      } else {
        to_remove <- names(private$.headers) %in% c("name", "path", "date")
        to_remove <- names(private$.headers)[!to_remove]
        private$.headers[to_remove] <- NULL
        message("\U2713 Removed headers: \n",
          paste(to_remove, collapse = "\n")
        )
      }
      invisible(self)
    },

    #' @description
    #' Removes settings from the `msData` object.
    #'
    #' @param call A character vector with the settings call name. When `call`
    #' is \code{NULL} all settings are removed.
    #'
    #' @return Invisible.
    #'
    remove_settings = function(call = NULL) {
      if (is.null(call)) {
        private$.settings <- NULL
        cat("Removed settings! \n")
      } else {
        all_calls <- names(private$.settings)
        to_remove <- call %in% all_calls
        call <- call[to_remove]
        if (length(call) > 0) {
          private$.settings[all_calls %in% call] <- NULL
          message("\U2713 Removed settings for:\n",
            paste(call, collapse = "\n")
          )
        }
      }
      invisible(self)
    },

    #' @description
    #' Removes analyses from the `msData` object. Note that unique feature
    #' groups from the removed analyses are also removed.
    #'
    #' @param analyses A character vector with the names or indices of the
    #' analyses to be removed.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)

      if (!is.null(analyses)) {
        allNames <- self$get_analysis_names()
        keepAnalyses <- unname(allNames[!(allNames %in% analyses)])
        removeAnalyses <- unname(allNames[allNames %in% analyses])

        analysesLeft <- self$get_analyses(keepAnalyses)

        if (self$has_groups() & length(analysesLeft) > 0) {
          newGroups <- copy(self$get_groups())
          newGroups[, (removeAnalyses) := NULL]
          newFeatures <- lapply(analysesLeft, function(x) x$features)

          out_list <- update_subset_features_and_groups(newGroups, newFeatures)

          analysesLeft <- Map(
            function(x, y) {
              x$features <- y
              x
            },
            analysesLeft, out_list[["features"]]
          )

          newGroups <- out_list[["groups"]]

        } else {
          newGroups <- NULL
        }

        private$.analyses <- analysesLeft

        if (!is.null(newGroups)) {
          self$add_groups(newGroups)
        } else {
          private$.groups <- NULL
        }

        private$.alignment <- private$.alignment[keepAnalyses]

        message("\U2713 Removed analyses:\n", paste(analyses, collapse = "\n"))
      }
      invisible(self)
    },

    #' @description
    #' Remove feature from the `msData` object.
    #'
    #' @param features A data.frame with columns \emph{analysis} and \emph{feature}
    #' representing the analysis name and the id of the features to remove,
    #' respectively.
    #' @param filtered X.
    #'
    #' @return Invisible.
    #'
    remove_features = function(features = NULL, filtered = FALSE) {
      if (is.null(features) & !filtered) {
        private$.groups <- NULL
        private$.alignment <- NULL
        private$.analyses <- lapply(private$.analyses, function(x) {
          x$features <- data.table()
          x
        })
        message("\U2713 Removed all features and feature groups!")
      }

      if (is.data.frame(features) | filtered) {
        org_fts <- self$get_features(filtered = filtered)
        n_org <- nrow(org_fts)

        if (n_org > 0) {
          unique_fts_ids <- paste0(org_fts$analysis, org_fts$feature)

          if (all(c("analysis", "feature") %in% colnames(features))) {
            rem_fts <- paste0(features$analysis, features$feature)
            rem_fts <- !unique_fts_ids %in% rem_fts
            org_fts <- org_fts[rem_fts, ]
          }

          if (filtered) {
            org_fts <- org_fts[!org_fts$filtered, ]
          }

          n_org_new <- nrow(org_fts)
          if (n_org_new < n_org) {

            if (self$has_groups()) {
              keep_groups <- unique(org_fts$group)
              keep_groups <- private$.groups$group %in% keep_groups
              private$.groups <- private$.groups[keep_groups, ]
              keep_groups <- keep_groups[!keep_groups]
              message("\U2713 Removed ", length(keep_groups), " groups!")
            }

            private$.analyses <- lapply(private$.analyses, function(x, org_fts) {
              temp <- org_fts[org_fts$analysis %in% x$name, ]
              temp[["analysis"]] <- NULL
              x$features <- temp
              x
            }, org_fts = org_fts)

            message("\U2713 Removed ", n_org - n_org_new, " features!")
          }
        }
      }
      invisible(self)
    },

    #' @description
    #' Removes loaded MS1 spectra from features in the analyses of
    #' the `msData` object. In practice, the column \emph{ms1} in the features
    #' data.table of each analysis object is removed.
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
          message("\U2713 Removed all MS1 spectra from features!")
        }
      }
      invisible(self)
    },

    #' @description
    #' Removes loaded MS2 spectra from features in the analyses of
    #' the `msData` object. In practice, the column \emph{ms2} in the features
    #' data.table of each analysis object is removed.
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
          message("\U2713 Removed all MS2 spectra from features!")
        }
      }
      invisible(self)
    },

    #' @description
    #' Remove feature groups from the `msData` object.
    #'
    #' @param groups X.
    #' @param filtered X.
    #'
    #' @return Invisible.
    #'
    remove_groups = function(groups = NULL, filtered = FALSE) {
      if (is.null(groups)) {
        private$.groups <- NULL
        private$.alignment <- NULL
        private$.analyses <- lapply(private$.analyses, function(x) {
          x$features[["group"]] <- NULL
          x$features$filter[x$features$filter %in% "grouping"] <- NA_character_
          x$features$filtered[is.na(x$features$filter)] <- FALSE
          x
        })
      }

      if (is.numeric(groups) & self$has_groups()) {
        groups <- self$get_groups()$group[groups]
      }

      if (filtered) {
        filtered_groups <- self$get_groups()
        filtered_groups <- filtered_groups$group[filtered_groups$filtered]
        groups <- c(groups, filtered_groups)
        groups <- unique(groups)
      }

      if (is.character(groups) & length(groups) > 0 & self$has_groups()) {
        n_org_g <- nrow(private$.groups)
        keep_groups <- !private$.groups$group %in% groups

        if (!all(keep_groups)) {
          private$.groups <- private$.groups[keep_groups, ]
          private$.analyses <- lapply(private$.analyses, function(x, groups) {
            x$features$group[x$features$group %in% groups] <- NA
            NA_groups <- is.na(x$features$group)
            x$features$filter[NA_groups & !x$features$filtered] <- "grouping"
            x$features$filtered[NA_groups & !x$features$filtered] <- TRUE
            x$features <- x$features[order(x$features$mz), ]
            x$features <- x$features[order(x$features$rt), ]
            x$features <- x$features[order(x$features$filtered), ]
            x
          }, groups = groups)
        }
        n_g <- nrow(private$.groups)
        message("\U2713 Removed ", n_org_g - n_g, " groups!")
      }
      invisible(self)
    },

    #' @description
    #' Removes loaded MS1 spectra from feature groups in the `msData` object.
    #' In practice, the column \emph{ms1} in the groups data.table is removed.
    #'
    #' @return Invisible.
    #'
    remove_groups_ms1 = function() {
      if (self$has_groups()) {
        if (any(self$has_loaded_groups_ms1())) {
          private$.groups$ms1 <- NULL
          message("\U2713 Removed all MS1 spectra from feature groups!")
        }
      }
      invisible(self)
    },

    #' @description
    #' Removes loaded MS2 spectra from feature groups in the `msData` object.
    #' In practice, the column \emph{ms2} in the groups data.table is removed.
    #'
    #' @return Invisible.
    #'
    remove_groups_ms2 = function() {
      if (self$has_groups()) {
        if (any(self$has_loaded_groups_ms2())) {
          private$.groups$ms2 <- NULL
          message("\U2713 Removed all MS2 spectra from feature groups!")
        }
      }
      invisible(self)
    },

    #' @description
    #' Removes alignment results from the `msData` object.
    #'
    #' @return Invisible.
    #'
    remove_alignment = function() {
      private$.alignment <- NULL
      message("\U2713 Removed alignment!")
      invisible(self)
    },

    ## ___ subset -----

    #' @description
    #' Subsets an `msData` object on analyses.
    #'
    #' @param analyses X.
    #'
    #' @return A new cloned `msData` object with only the analyses as defined
    #' by the `analyses` argument.
    #'
    subset_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)

      if (!is.null(analyses)) {
        allNames <- self$get_analysis_names()
        removeAnalyses <- unname(allNames[!(allNames %in% analyses)])
        keepAnalyses <- unname(allNames[allNames %in% analyses])

        newAnalyses <- self$get_analyses(keepAnalyses)

        if (self$has_groups()) {
          newGroups <- copy(self$get_groups())
          newGroups[, (removeAnalyses) := NULL]
          newFeatures <- lapply(newAnalyses, function(x) x$features)
          out_list <- update_subset_features_and_groups(newGroups, newFeatures)

          newAnalyses <- Map(
            function(x, y) {
              x$features <- y
              x
            },
            newAnalyses, out_list[["features"]]
          )

          newGroups <- out_list[["groups"]]
        } else {
          newGroups <- NULL
        }

        newAlignment <- private$.alignment[keepAnalyses]

        msData$new(
          files = NULL,
          headers = private$.headers,
          settings = private$.settings,
          analyses = newAnalyses,
          groups = newGroups,
          alignment = newAlignment
        )

      } else {
        self$clone()
      }
    },

    #' @description
    #' Subsets an `msData` object on features from analyses.
    #'
    #' @param features A data.frame with columns \emph{analysis} and \emph{feature}
    #' representing the analysis name and the id of the features to keep in the
    #' new `msData` object, respectively.
    #'
    #' @return A new cloned `msData` object with only the features as defined
    #' by the `features` argument.
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
              new_msData <- self$clone()
              new_msData <- new_msData$remove_features(rem_fts)
              return(new_msData)
            }
          }
        }
      }
      self$clone()
    },

    #' @description
    #' Subsets an `msData` object on groups from correspondence of features
    #' across analyses. Note that when sub-setting groups, features that lose
    #' correspondence are not removed but filtered with "grouping" added as
    #' filter category/tag. Filtered features can be removed with the method
    #' `msData$remove_features(filtered = TRUE)`.
    #'
    #' @param groups X.
    #'
    #' @return A new cloned `msData` object with only the groups as defined
    #' by the `groups` argument.
    #'
    subset_groups = function(groups = NULL) {
      if (self$has_groups() & !is.null(groups)) {
        all_groups <- self$get_groups()
        all_groups <- all_groups$group
        groups_rem <- all_groups[!all_groups %in% groups]

        if (length(groups_rem) > 0) {
          new_msData <- self$clone(deep = TRUE)
          new_msData <- new_msData$remove_groups(groups_rem)
          return(new_msData)
        }
      }
      self$clone()
    },

    ## ___ has -----

    #' @description
    #' Method to check of the `msData` object has analyses.
    #'
    #' @return Logical value.
    #'
    has_analyses = function() {
      length(private$.analyses) > 0
    },

    #' @description
    #' Method to check for loaded spectra in given analyses names/indices.
    #'
    #' @param analyses The analyses names/indices to check for loaded spectra.
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
    #' Method to check for loaded chromatograms in given analyses names/indices.
    #'
    #' @param analyses The analyses names/indices to check for loaded
    #' chromatograms.
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
    #' Method to check for loaded features MS1 in given analyses names/indices.
    #'
    #' @param analyses The analyses names/indices to check for loaded
    #' chromatograms.
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
    #' Method to check for loaded features MS2 in given analyses names/indices.
    #'
    #' @param analyses The analyses names/indices to check for loaded
    #' chromatograms.
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
    #' Method to check for loaded feature groups MS1.
    #'
    #' @return Logical value.
    #'
    has_loaded_groups_ms1 = function() {
      has_loaded_ms1 <- FALSE

      if (self$has_groups()) {
        groups <- self$get_groups()
        if ("ms1" %in% colnames(groups)) {
          has_loaded_ms1 <- any(vapply(groups$ms1, is.data.frame, FALSE))
        }
      }
      has_loaded_ms1
    },

    #' @description
    #' Method to check for loaded feature groups MS2.
    #'
    #' @return Logical value.
    #'
    has_loaded_groups_ms2 = function() {
      has_loaded_ms2 <- FALSE

      if (self$has_groups()) {
        groups <- self$get_groups()
        if ("ms2" %in% colnames(groups)) {
          has_loaded_ms2 <- any(vapply(groups$ms2, is.data.frame, FALSE))
        }
      }
      has_loaded_ms2
    },

    #' @description
    #' Method to check if there are processing settings in the `msData` object.
    #'
    #' @param call A string with the name of function call.
    #'
    #' @return Logical value.
    #'
    has_settings = function(call = NULL) {
      if (is.null(call)) {
        length(private$.settings) > 0
      } else {
       length(private$.settings[[call]]) > 0
      }
    },

    #' @description
    #' Method to check if given analyses have features.
    #'
    #' @param analyses The analyses names/indices to check for loaded spectra.
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
    #' Method to check if there is alignment of retention time from grouping
    #' features across analyses.
    #'
    #' @return Logical value.
    #'
    has_alignment = function() {
      !is.null(private$.alignment)
    },

    #' @description
    #' Method to check if there are feature groups from grouping features
    #' across analyses.
    #'
    #' @return Logical value.
    #'
    has_groups = function() {
      !is.null(private$.groups)
    },

    ## ___ plot -----

    #' @description
    #' Plots spectra for given MS analyses.
    #'
    #' @param analyses X.
    #' @param levels Name.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param allTraces Logical, when \code{TRUE} all level 2 data is returned.
    #' When \code{FALSE} and level has 2, only the MS2 traces of MS1 targets
    #' are returned, using the `preMZ` value and the `isolationWindow`.
    #' @param isolationWindow X.
    #' @param minIntensityMS1 X.
    #' @param minIntensityMS2 X.
    #' @param runParallel X.
    #' @param colorBy X.
    #'
    #' @return A 3D interactive plot.
    #'
    plot_spectra = function(analyses = NULL, levels = NULL,
                            mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                            allTraces = TRUE, isolationWindow = 1.3,
                            minIntensityMS1 = 0, minIntensityMS2 = 0,
                            runParallel = FALSE, colorBy = "analyses") {

      spec <- self$get_spectra(
        analyses, levels, mz, rt, ppm, sec, id,
        allTraces = allTraces, isolationWindow,
        minIntensityMS1, minIntensityMS2,
        runParallel
      )

      if (nrow(spec) == 0) {
        message("!! Traces not found for the targets!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        spec$replicate <- self$get_replicate_names()[spec$analysis]
      }

      plot_spectra_interactive(spec, colorBy)
    },

    #' @description
    #' Method to plot extract ion chromatograms (EIC) and \emph{m/z} vs
    #' retention time from the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param plotTargetMark x.
    #' @param targetsMark x.
    #' @param ppmMark x.
    #' @param secMark x.
    #' @param numberRows x.
    #'
    #' @return A data.frame.
    #'
    plot_xic = function(analyses = NULL,
                        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                        runParallel = FALSE, legendNames = NULL,
                        plotTargetMark = TRUE, targetsMark = NULL,
                        ppmMark = 5, secMark = 10, numberRows = 1) {

      xic <- self$get_spectra(
        analyses,
        levels = 1, mz, rt, ppm, sec, id,
        allTraces = TRUE,
        isolationWindow = 1.3,
        minIntensityMS1 = 0,
        minIntensityMS2 = 0,
        runParallel = runParallel
      )

      if (nrow(xic) == 0) {
        message("!! Traces not found for the targets!")
        return(NULL)
      }

      plot_xic_interactive(
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
    #' Method to plot extract ion chromatograms (EIC) from the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_eic = function(analyses = NULL,
                        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                        runParallel = FALSE, legendNames = NULL, title = NULL,
                        colorBy = "targets", interactive = TRUE) {

      eic <- self$get_eic(analyses, mz, rt, ppm, sec, id, runParallel)

      if (nrow(eic) == 0) {
        message("!! Traces not found for the targets!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        eic$replicate <- self$get_replicate_names()[eic$analysis]
      }

      if (!interactive) {
        plot_eic_static(eic, legendNames, colorBy, title)
      } else {
        plot_eic_interactive(eic, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to plot total ion chromatograms (TIC) of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_tic = function(analyses = NULL, title = NULL,
                        colorBy = "analyses", interactive = TRUE) {
      tic <- self$get_tic(analyses)

      tic$id <- "TIC"

      if (nrow(tic) == 0) {
        message("!! TIC not found for the analyses!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        tic$replicate <- self$get_replicate_names()[tic$analysis]
      } else {
        colorBy <- "analyses"
      }

      if (!interactive) {
        plot_eic_static(tic, legendNames, colorBy, title)
      } else {
        plot_eic_interactive(tic, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to plot base peak chromatograms (BPC) of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_bpc = function(analyses = NULL, title = NULL,
                        colorBy = "analyses", interactive = TRUE) {

      bpc <- self$get_bpc(analyses)

      bpc$id <- "BPC"

      if (nrow(bpc) == 0) {
        message("!! BPC not found for the analyses!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        bpc$replicate <- self$get_replicate_names()[bpc$analysis]
      } else {
        colorBy <- "analyses"
      }

      if (!interactive) {
        plot_eic_static(bpc, legendNames, colorBy, title)
      } else {
        plot_bpc_interactive(bpc, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to plot MS2 spectra from the analyses based on targets.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param isolationWindow X.
    #' @param mzClust X.
    #' @param verbose X.
    #' @param minIntensity X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_ms2 = function(analyses = NULL,
                        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                        isolationWindow = 1.3, mzClust = 0.005, verbose = FALSE,
                        minIntensity = 0, runParallel = FALSE,
                        legendNames = NULL, title = NULL,
                        colorBy = "targets", interactive = TRUE) {

      ms2 <- self$get_ms2(
        analyses, mz, rt, ppm, sec, id, isolationWindow,
        mzClust, verbose, minIntensity, runParallel
      )

      if (nrow(ms2) == 0) {
        message("!! MS2 traces not found for the targets!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        ms2$replicate <- self$get_replicate_names()[ms2$analysis]
      }

      if (!interactive) {
        plot_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        plot_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to plot MS1 spectra from the analyses based on targets.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param mzClust X.
    #' @param verbose X.
    #' @param minIntensity X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_ms1 = function(analyses = NULL,
                        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                        mzClust = 0.001, verbose = FALSE,
                        minIntensity = 1000, runParallel = FALSE,
                        legendNames = NULL, title = NULL,
                        colorBy = "targets", interactive = TRUE) {

      ms1 <- self$get_ms1(
        analyses, mz, rt, ppm, sec, id, mzClust,
        verbose, minIntensity, runParallel
      )

      if (nrow(ms1) == 0) {
        message("!! MS1 traces not found for the targets!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        ms1$replicate <- self$get_replicate_names()[ms1$analysis]
      }

      if (!interactive) {
        plot_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        plot_ms1_interactive(ms1, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to plot features from analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param features X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param rtExpand X.
    #' @param mzExpand X.
    #' @param filtered X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return plot.
    #'
    plot_features = function(analyses = NULL, features = NULL, mass = NULL,
                             mz = NULL, rt = NULL, ppm = 20, sec = 60,
                             rtExpand = 120, mzExpand = 0.005,
                             filtered = FALSE, runParallel = FALSE,
                             legendNames = NULL, title = NULL,
                             colorBy = "targets", interactive = TRUE) {

      fts <- self$get_features(analyses, features, mass, mz, rt, ppm, sec, filtered)

      eic <- self$get_features_eic(
        analyses = unique(fts$analysis), features = fts$feature,
        rtExpand = rtExpand, mzExpand = mzExpand, runParallel = runParallel
      )

      if (nrow(eic) == 0) {
        message("!! Traces not found for the targets!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        eic$replicate <- self$get_replicate_names()[eic$analysis]
      }

      if (!interactive) {
        plot_features_static(eic, fts, legendNames, colorBy, title)
      } else {
        plot_features_interactive(eic, fts, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to map retention time and \emph{m/z} of features from analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param features X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param filtered X.
    #' @param xlim X.
    #' @param ylim X.
    #' @param showLegend X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    map_features = function(analyses = NULL, features = NULL, mass = NULL,
                            mz = NULL, rt = NULL, ppm = 20, sec = 60,
                            filtered = FALSE, xlim = 30, ylim = 0.05,
                            showLegend = TRUE, legendNames = NULL, title = NULL,
                            colorBy = "targets", interactive = TRUE) {

      fts <- self$get_features(analyses, features, mass, mz, rt, ppm, sec, filtered)

      if (nrow(fts) == 0) {
        message("!! Features not found for the targets!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        fts$replicate <- self$get_replicate_names()[fts$analysis]
      }

      if (!interactive) {
       map_features_static(
          fts, colorBy, legendNames,
          xlim, ylim, title, showLegend
        )
      } else {
       map_features_interactive(
          fts, colorBy, legendNames,
          xlim, ylim, title
        )
      }
    },

    #' @description
    #' Method to plot MS1 spectra from features in the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param features X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param rtWindow X.
    #' @param mzWindow X.
    #' @param mzClust X.
    #' @param minIntensity X.
    #' @param verbose X.
    #' @param filtered X.
    #' @param loadedMS1 X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_features_ms1 = function(analyses = NULL, features = NULL, mass = NULL,
                                 mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                 rtWindow = c(-2, 2), mzWindow = c(-5, 100),
                                 mzClust = 0.003, minIntensity = 1000,
                                 verbose = FALSE, filtered = FALSE,
                                 loadedMS1 = TRUE, runParallel = FALSE,
                                 legendNames = NULL, title = NULL,
                                 colorBy = "targets", interactive = TRUE) {

      ms1 <- self$get_features_ms1(
        analyses, features, mass, mz, rt, ppm, sec,
        rtWindow, mzWindow, mzClust, minIntensity,
        verbose, filtered, loadedMS1, runParallel
      )

      if (nrow(ms1) == 0) {
        message("!! MS1 traces not found for the targets!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        ms1$replicate <- self$get_replicate_names()[ms1$analysis]
      }

      if (!interactive) {
        plot_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        plot_ms1_interactive(ms1, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to plot MS2 spectra from features in the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #' @param features X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param isolationWindow X.
    #' @param mzClust X.
    #' @param minIntensity X.
    #' @param verbose X.
    #' @param filtered X.
    #' @param loadedMS2 X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_features_ms2 = function(analyses = NULL, features = NULL, mass = NULL,
                                 mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                 isolationWindow = 1.3, mzClust = 0.005,
                                 minIntensity = 0, verbose = FALSE,
                                 filtered = FALSE, loadedMS2 = TRUE,
                                 runParallel = FALSE, legendNames = NULL,
                                 title = NULL, colorBy = "targets",
                                 interactive = TRUE) {

      ms2 <- self$get_features_ms2(
        analyses, features, mass, mz, rt, ppm, sec,
        isolationWindow, mzClust, minIntensity,
        verbose, filtered, loadedMS2, runParallel
      )

      if (nrow(ms2) == 0) {
        message("!! MS2 traces not found for the targets!")
        return(NULL)
      }
      if ("replicates" %in% colorBy) {
        ms2$replicate <- self$get_replicate_names()[ms2$analysis]
      }

      if (!interactive) {
        plot_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        plot_ms2_interactive(ms2, legendNames, colorBy, title)
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
        warning("!! Adjusted retention time not found!")
        return(NULL)
      }

      alignment <- private$.alignment
      colors <- get_colors(names(alignment))

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
    #' Method to plot feature groups EIC.
    #'
    #' @param groups X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param rtExpand X.
    #' @param mzExpand X.
    #' @param filtered X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_groups = function(groups = NULL, mass = NULL,
                           mz = NULL, rt = NULL, ppm = 20, sec = 60,
                           rtExpand = 120, mzExpand = 0.005,
                           filtered = FALSE, runParallel = FALSE,
                           legendNames = NULL, title = NULL,
                           colorBy = "targets", interactive = TRUE) {

      fts <- self$get_features(
        analyses = NULL,
        groups, mass, mz, rt, ppm, sec, filtered
      )

      if (!is.null(legendNames)) {
        if (is.character(legendNames) &
          length(legendNames) == length(unique(fts$group))) {
          leg <- legendNames
          names(leg) <- unique(fts$group)
          fts$group <- leg[fts$group]
        }
      }

      self$plot_features(
        features = fts,
        rtExpand = rtExpand, mzExpand = mzExpand,
        runParallel = runParallel, legendNames = fts$group,
        title = title, colorBy = colorBy, interactive = interactive
      )
    },

    #' @description
    #' Method to plot MS1 spectra from features in the analyses.
    #'
    #' @param groups X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param rtWindow X.
    #' @param mzWindow X.
    #' @param mzClustFeatures X.
    #' @param minIntensityFeatures X.
    #' @param loadedFeaturesMS1 X.
    #' @param mzClustGroups X.
    #' @param minIntensityGroups X.
    #' @param groupBy X.
    #' @param verbose X.
    #' @param filtered X.
    #' @param loadedGroupsMS1 X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_groups_ms1 = function(groups = NULL, mass = NULL,
                               mz = NULL, rt = NULL, ppm = 20, sec = 60,
                               rtWindow = c(-2, 2), mzWindow = c(-5, 90),
                               mzClustFeatures = 0.005,
                               minIntensityFeatures = 1000,
                               loadedFeaturesMS1 = TRUE,
                               mzClustGroups = 0.005,
                               minIntensityGroups = 1000,
                               verbose = FALSE, filtered = FALSE,
                               loadedGroupsMS1 = TRUE, runParallel = FALSE,
                               legendNames = NULL, title = NULL,
                               colorBy = "targets", interactive = TRUE) {

      if ("groups" %in% colorBy | "targets" %in% colorBy) {
        groupBy <- "groups"
      } else {
        groupBy <- "replicates"
      }

      ms1 <- self$get_groups_ms1(
        groups, mass, mz, rt, ppm, sec,
        rtWindow, mzWindow, mzClustFeatures,
        minIntensityFeatures, loadedFeaturesMS1,
        mzClustGroups, minIntensityGroups, verbose,
        filtered, loadedGroupsMS1, runParallel
      )

      if (nrow(ms1) == 0) {
        message("!! MS1 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) colorBy <- "replicates"

      if (!interactive) {
        plot_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        plot_ms1_interactive(ms1, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to plot MS1 spectra from features in the analyses.
    #'
    #' @param groups X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param isolationWindow X.
    #' @param mzClustFeatures X.
    #' @param minIntensityFeatures X.
    #' @param loadedFeaturesMS2 X.
    #' @param mzClustGroups X.
    #' @param minIntensityGroups X.
    #' @param groupBy X.
    #' @param verbose X.
    #' @param filtered X.
    #' @param loadedGroupsMS2 X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    #'
    plot_groups_ms2 = function(groups = NULL, mass = NULL,
                               mz = NULL, rt = NULL, ppm = 20, sec = 60,
                               isolationWindow = 1.3,
                               mzClustFeatures = 0.003,
                               minIntensityFeatures = 100,
                               loadedFeaturesMS2 = TRUE,
                               mzClustGroups = 0.003,
                               minIntensityGroups = 100,
                               verbose = FALSE, filtered = FALSE,
                               loadedGroupsMS2 = TRUE, runParallel = FALSE,
                               legendNames = NULL, title = NULL,
                               colorBy = "targets", interactive = TRUE) {

      if ("groups" %in% colorBy | "targets" %in% colorBy) {
        groupBy <- "groups"
      } else {
        groupBy <- "replicates"
      }

      ms2 <- self$get_groups_ms2(
        groups, mass, mz, rt, ppm, sec,
        isolationWindow, mzClustFeatures,
        minIntensityFeatures, loadedFeaturesMS2,
        mzClustGroups, minIntensityGroups,
        groupBy, verbose, filtered,
        loadedGroupsMS2, runParallel
      )

      if (nrow(ms2) == 0) {
        message("!! MS2 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) colorBy <- "replicates"

      if (!interactive) {
       plot_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        plot_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to give an overview of the EIC, alignment and intensity variance
    #' from features within target feature groups.
    #'
    #' @param analyses X.
    #' @param groups X.
    #' @param mass X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param rtExpand X.
    #' @param mzExpand X.
    #' @param filtered X.
    #' @param runParallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param heights x.
    #'
    #' @return A plot.
    #'
    plot_groups_overview = function(analyses = NULL, groups = NULL,
                                    mass = NULL, mz = NULL, rt = NULL,
                                    ppm = 20, sec = 60,
                                    rtExpand = 120, mzExpand = 0.005,
                                    filtered = FALSE,
                                    runParallel = FALSE,
                                    legendNames = NULL, title = NULL,
                                    heights = c(0.35, 0.5, 0.15)) {

      fgs <- self$get_groups(groups, mass, mz, rt, ppm, sec, filtered,
        onlyIntensities = FALSE, average = FALSE
      )

      fts <- self$get_features(analyses = analyses, features = fgs$group)

      eic <- self$get_features_eic(
        analyses = fts$analysis, features = fts,
        rtExpand = rtExpand, mzExpand = mzExpand,
        filtered = TRUE, runParallel = runParallel
      )

      if (nrow(eic) == 0) {
        message("!! Traces and/or features not found for targets!")
        return(NULL)
      }

      if (!is.null(legendNames)) {
        if (is.character(legendNames) &
          length(legendNames) == length(unique(fgs$group))) {
          leg <- legendNames
          names(leg) <- unique(fts$group)
          leg <- leg[fts$group]
        }
      } else {
        leg <- fts$group
      }

      names(leg) <- paste0(fts$feature, "_", fts$analysis)
      eic$uid <- paste0(eic$id, "_", eic$analysis)
      fts$uid <- paste0(fts$feature, "_", fts$analysis)
      eic$var <- leg[eic$uid]
      fts$var <- leg

      analyses <- private$.check_analyses_argument(analyses)

      plot_groups_overview_aux(fts, eic, heights, analyses)
    },

    ## ___ processing -----

    #' @description Finds features (i.e., chromatographic peaks) from MS data
    #' in an `msData` class object. The function uses the \pkg{patRoon} package
    #' for peak finding, enabling the use of several algorithms (see details).
    #' Note that the settings call name must be "find_features".
    #'
    #' @param settings A list object with call name, algorithm and parameters.
    #' When not given, settings will be searched within the `msData` object.
    #'
    #' @details See the \link[patRoon]{findFeatures} function from the
    #' \pkg{patRoon} package or the
    #' \href{https://rickhelmus.github.io/patRoon/reference/findFeatures.html}{reference guide} for more information. The following algorithms are
    #' available via \pkg{patRoon}: "xcms3", "xcms", "openms", "envipick",
    #' "sirius", "kpic2", "safd". The algorithm in the  settings should be
    #' one of the described. The parameters are given as a list and should
    #' match with algorithm requirements. Certain algorithms also require
    #' defined MS file formats and data in profile mode.
    #'
    #' @return Invisible.
    #'
    #' @references
    #' \insertRef{patroon01}{streamFind}
    #'
    find_features = function(settings = NULL) {
      valid <- TRUE

      if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
        warning("Install package patRoon for finding peaks!")
        valid <- FALSE
      }

      if (is.null(settings)) {
        settings <- self$get_settings(call = "find_features")[[1]]
      } else if ("find_features" %in% names(settings)) {
        settings <- settings[["find_features"]]
      }

      if (validate.settings(settings)) {
        if (!"find_features" %in% settings$call) {
          warning("Settings call must be find_features!")
          valid <- FALSE
        }
      } else {
        warning("Settings content or structure not conform!")
        valid <- FALSE
      }

      if (!valid) {
        invisible(self)
      }

      algorithm <- settings$algorithm
      parameters <- settings$parameters

      anaInfo <- self$get_overview()
      anaInfo <- data.frame(
        "path" = dirname(anaInfo$file),
        "analysis" = anaInfo$analysis,
        "group" = anaInfo$replicate,
        "blank" = anaInfo$blank
      )

      anaInfo$blank[is.na(anaInfo$blank)] <- ""
      anaInfo$algorithm <- algorithm
      ag <- list(analysisInfo = anaInfo, algorithm = algorithm)
      pp_fun <- patRoon::findFeatures
      pat <- do.call(pp_fun, c(ag, parameters, verbose = FALSE))

      features <- build_features_table_from_patRoon(pat, self)

      self$add_settings(settings)

      private$.analyses <- Map(
        function(x, y) {
          x$features <- y
          x
        },
        private$.analyses, features
      )

      message("\U2713 Features added to analyses!")
      invisible(self)
    },

    #' @description Groups and aligns features across analyses in the `msData`
    #' object. The function uses the \pkg{patRoon} package for grouping
    #' features, enabling the use of several algorithms (see details).
    #' Note that the settings call name must be "group_features".
    #'
    #' @param settings A list object with call name, algorithm and parameters.
    #' When not given, settings will be searched within the `msData` object.
    #'
    #' @return Invisible.
    #'
    #' @details See the \link[patRoon]{groupFeatures} function from the
    #' \pkg{patRoon} package or the
    #' \href{https://rickhelmus.github.io/patRoon/reference/groupFeatures.html}{reference guide}
    #' for more information. The following algorithms are
    #' possible: "xcms3", "xcms", "openms" or "kpic2". The algorithm slot in the
    #' settings should be one of the described. The parameters
    #' are given as a list and should match with algorithm requirements.
    #'
    group_features = function(settings = NULL) {
      valid <- TRUE

      if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
        warning("Install package patRoon for finding peaks!")
        valid <- FALSE
      }

      if (is.null(settings)) {
        settings <- self$get_settings(call = "group_features")[[1]]
      } else if ("group_features" %in% names(settings)) {
        settings <- settings[["group_features"]]
      }

      if (validate.settings(settings)) {
        if (!"group_features" %in% settings$call) {
          warning("Settings call must be group_features!")
          valid <- FALSE
        }
      } else {
        warning("Settings content or structure not conform!")
        valid <- FALSE
      }

      pat_features <- self$as_features_patRoon()

      if (length(pat_features) == 0) {
        warning("Features were not found! Run find_features method first!")
        valid <- FALSE
      }

      if (!valid) {
        invisible(self)
      }

      algorithm <- settings$algorithm
      parameters <- settings$parameters

      if (algorithm == "xcms3") {
        parameters$groupParam@sampleGroups <- self$get_replicate_names()
        if ("rtalign" %in% names(parameters)) {
          if (parameters$rtalign) {
            parameters$preGroupParam@sampleGroups <- self$get_replicate_names()
          }
        }
      }

      ag <- list(obj = pat_features, algorithm = algorithm)
      gr_fun <- patRoon::groupFeatures
      pat <- do.call(gr_fun, c(ag, parameters))

      features <- build_features_table_from_patRoon(pat, self)
      out_list <- build_feature_groups_table_from_patRoon(pat, features, self)

      alignment <- extract_time_alignment(pat, self)

      self$add_settings(settings)

      private$.analyses <- Map(
        function(x, y) {
          x$features <- y
          x
        },
        private$.analyses, out_list[["features"]]
      )

      private$.groups <- out_list[["groups"]]

      message("\U2713 Added feature groups from correspondence analysis!")

      if (!is.null(alignment)) {
        private$.alignment <- alignment
        message("\U2713 Added alignment of retention time for each analysis!")
      }
      invisible(self)
    },

    ## ___ as -----

    #' @description
    #' Creates an object with S4 class `features` from the package \pkg{patRoon}
    #' with the features in the analyses of the msData object.
    #'
    #' @return An object with S4 class `features`.
    #'
    as_features_patRoon = function() {
      if (!requireNamespace("patRoon", quietly = TRUE)) {
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

      polarities <- self$get_polarities()
      if (length(unique(polarities)) > 1) anaInfo$set <- polarities

      anaInfo$file <- self$get_file_paths()
      rownames(anaInfo) <- seq_len(nrow(anaInfo))

      features <- lapply(self$get_analyses(), function(x) {
        ft <- copy(x$features)
        if ("filtered" %in% colnames(ft)) ft <- ft[!ft$filtered, ]

        if (nrow(ft) == 0) {
          return(ft)
        }

        setnames(ft, c("feature", "rt", "rtmin", "rtmax"),
          c("ID", "ret", "retmin", "retmax"),
          skip_absent = TRUE
        )

        setcolorder(
          ft,
          c(
            "ID", "mz", "mzmin", "mzmax", "ret", "retmin", "retmax",
            "intensity", "area"
          )
        )

        ft$ID <- as.numeric(gsub(".*_f", "", ft$ID))

        return(ft)
      })

      if (length(unique(polarities)) > 1) {
        features <- lapply(features, function(x) {
          if (nrow(x) == 0) {
            return(x)
          }
          x$mzmin <- x$mass - (x$mz - x$mzmin)
          x$mzmax <- x$mass + (x$mzmax - x$mz)
          x$mz <- x$mass
          x$mass <- NULL
          return(x)
        })
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

    ## checks -----

    #' @description
    #' Checks the correspondence of features within feature groups in
    #' the `msData` object.
    #'
    #' @return \code{TRUE} or \code{FALSE}.
    #'
    check_correspondence = function() {
      valid <- FALSE

      if (all(self$has_features()) & self$has_groups()) {
        valid <- rcpp_ms_feature_groups_correspondence(
          groups = self$get_groups(),
          features = self$get_features(),
          verbose = TRUE
        )
      }
      valid
    },

    ## ___ save -----

    #' @description
    #' Method to save the headers list.
    #'
    #' @param format X.
    #' @param name X.
    #' @param path X.
    #'
    #' @return Saves the headers list as the defined \code{format} in
    #' \code{path} and returns invisible.
    #'
    save_headers = function(format = "json", name = "headers", path = getwd()) {
      if (format %in% "json") {
        js_headers <- toJSON(
          self$get_headers(),
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
        saveRDS(self$get_headers(), file = paste0(path, "/", name, ".rds"))
      }
      invisible(self)
    },

    #' @description
    #' Method to save settings list.
    #'
    #' @param call x:
    #' @param format X.
    #' @param name X.
    #' @param path X.
    #'
    #' @return Saves the settings list as the defined \code{format} in
    #' \code{path} and returns invisible.
    #'
    save_settings = function(call = NULL, format = "json",
                             name = "settings", path = getwd()) {
      js_settings <- self$get_settings(call)

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
    #' Method to save the list of analyses.
    #'
    #' @param analyses X.
    #' @param format X.
    #' @param name X.
    #' @param path X.
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
    #' Method to save the feature groups \code{data.table}.
    #'
    #' @param format X.
    #' @param name X.
    #' @param path X.
    #'
    #' @return Saves the groups \code{data.table} as the defined \code{format}
    #' in the \code{path} and returns invisible.
    #'
    save_groups = function(format = "json", name = "groups", path = getwd()) {
      if (format %in% "json") {
        js_groups <- self$get_groups()

        js_groups <- toJSON(
          js_groups,
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

        write(js_groups, file = paste0(path, "/", name, ".json"))
      }

      if (format %in% "rds") {
        saveRDS(self$get_groups(), file = paste0(path, "/", name, ".rds"))
      }
      invisible(self)
    },

    #' @description
    #' Method to save the private fields (i.e., headers, settings, analyses,
    #' groups and alignment) of the msData object.
    #'
    #' @param format X.
    #' @param name X.
    #' @param path X.
    #'
    #' @return Saves the private fields of the msdata as the defined `format`
    #' in the \code{path} and returns invisible.
    #'
    save = function(format = "json", name = "msData", path = getwd()) {
      list_all <- list()

      headers <- self$get_headers()
      settings <- self$get_settings()
      analyses <- self$get_analyses()
      groups <- self$get_groups()
      alignment <- self$get_alignment()

      if (length(headers) > 0) list_all$headers <- headers
      if (!is.null(settings)) list_all$settings <- settings
      if (!is.null(analyses)) list_all$analyses <- analyses
      if (!is.null(groups)) list_all$groups <- groups
      if (!is.null(alignment)) list_all$alignment <- alignment

      if (format %in% "json") {
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
        saveRDS(list_all, file = paste0(path, "/", name, ".rds"))
      }
      invisible(self)
    },

    ## ___ import -----

    #' @description
    #' Method to import headers to the `msData` object from a \emph{rds} or
    #' \emph{json} file.
    #'
    #' @param file X.
    #' @param list X.
    #'
    #' @return Invisible.
    #'
    import_headers = function(file = NA_character_, list = NULL) {
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
    #' Method to import processing settings to the `msData` object from a
    #' \emph{rds} or \emph{json} file.
    #'
    #' @param file X.
    #'
    #' @return Invisible.
    #'
    import_settings = function(file = NA_character_) {
      if (file.exists(file)) {
        settings <- NULL
        if (file_ext(file) %in% "json") settings <- fromJSON(file)
        if (file_ext(file) %in% "rds") settings <- readRDS(file)
        self$add_settings(settings)

      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },

    #' @description
    #' Method to import analyses to the `msData` object from a \emph{rds} or
    #' \emph{json} file.
    #'
    #' @param file X.
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
        self$add_analyses(analyses)
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },

    #' @description
    #' Method to import feature groups to the `msData` object from a
    #' \emph{rds} or \emph{json} file.
    #'
    #' @param file X.
    #'
    #' @return Invisible.
    #'
    import_groups = function(file = NA_character_) {
      if (file.exists(file)) {
        groups <- NULL
        if (file_ext(file) %in% "json") {
          groups <- fromJSON(file, simplifyDataFrame = FALSE)
        }
        if (file_ext(file) %in% "rds") groups <- readRDS(file)
        self$add_groups(groups)
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },

    ## ___ info -----

    #' @description
    #' Possible processing function calls.
    #'
    #' @return A character vector with ordered possible function calls for data
    #' pre and post-processing.
    #'
    processing_function_calls = function() {
      c(
        "find_features",
        "annotate_features",
        "load_features_ms1",
        "load_features_ms2",
        "load_groups_ms1",
        "load_groups_ms2",
        "group_features",
        "fill_features",
        "filter_features"
      )
    }
  )
)

# _ import msData class -----

#' Function to import an msData class object from a *json* or *rds* file
#'
#' @description Function to import an `msData` class object from a saved *json*
#' or *rds* file.
#'
#' @param file A *json* or *rds* file as obtained by the msData method `save()`.
#'
#' @return An `msData` class object.
#'
#' @export
#'
import_msData <- function(file) {
  if (file.exists(file)) {
    new_ms <- NULL

    if (file_ext(file) %in% "json") {
      js_ms <- fromJSON(file, simplifyDataFrame = FALSE)

      fields_present <- names(js_ms)

      new_ms <- msData$new()

      if ("headers" %in% fields_present) new_ms$add_headers(js_ms[["headers"]])

      if ("settings" %in% fields_present) {
        if (!is.null(js_ms[["settings"]])) {
          new_ms$add_settings(js_ms[["settings"]])
        }
      }

      if ("analyses" %in% fields_present) {
        if (!is.null(js_ms[["analyses"]])) {
          new_ms$add_analyses(js_ms[["analyses"]])
        }
      }

      if ("groups" %in% fields_present) {
        if (!is.null(js_ms[["groups"]]) & length(js_ms[["groups"]]) > 0) {
          new_ms$add_groups(js_ms[["groups"]])
        }
      }

      if ("alignment" %in% fields_present) {
        if (!is.null(js_ms[["alignment"]])) {
          new_ms$add_alignment(js_ms[["alignment"]])
        }
      }

      message("\U2713 msData class object imported from json file!")
    }

    if (file_ext(file) %in% "rds") {
      new_ms <- readRDS(file)

      # TODO validate object
      message("\U2713 msData class object imported from rds file!")
    }

    new_ms

  } else {
    warning("File not found in given path!")
    NULL
  }
}

# _ not-exported functions -----

#' @title build_features_table_from_patRoon
#'
#' @param pat An object with class `features` or `featureGroups` from the
#' package \pkg{patRoon}.
#' @param self An `msData` object. When applied within the R6, the self object.
#'
#' @return A list of with a features \linkS4class{data.table} for each analysis.
#'
#' @noRd
#'
build_features_table_from_patRoon <- function(pat, self) {

  if ("features" %in% is(pat)) {
    anaInfo <- pat@analysisInfo
    isSet <- TRUE %in% grepl("Set", is(pat))
    features <- pat@features
    if ("featuresXCMS3" %in% is(pat)) {
      if (xcms::hasFilledChromPeaks(pat@xdata)) {
        extra <- xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE)
        extra$is_filled <- as.logical(extra$is_filled)
        extra$analysis <- anaInfo$analysis[extra$sample]
        extra <- split(extra, extra$analysis)
      } else {
        extra <- NULL
      }
    } else {
      extra <- NULL
    }
  }

  if ("featureGroups" %in% is(pat)) {
    anaInfo <- pat@analysisInfo
    features <- copy(pat@features@features)
    isSet <- TRUE %in% grepl("Set", is(pat))
    if ("featureGroupsXCMS3" %in% is(pat)) {
      if (xcms::hasFilledChromPeaks(pat@xdata)) {
        extra <- xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE)
        extra$is_filled <- as.logical(extra$is_filled)
        extra$analysis <- anaInfo$analysis[extra$sample]
        extra <- split(extra, extra$analysis)
      } else {
        extra <- NULL
      }
    } else {
      extra <- NULL
    }
  }

  analyses <- names(features)

  features <- lapply(analyses, function(x, extra, features, self, isSet) {
    temp <- features[[x]]

    if (!is.null(extra)) {
      if (temp == nrow(extra[[x]]) & all(temp$mz == extra[[x]]$mz)) {
        temp$is_filled <- extra[[x]]$is_filled
      }
    }

    polarity <- self$get_polarities(x)

    if (polarity %in% "positive") {
      adduct <- "[M+H]+"
      adduct_val <- -1.007276
    }

    if (polarity %in% "negative") {
      adduct <- "[M-H]-"
      adduct_val <- 1.007276
    }

    # required as when is set the mz value is neutralized from patRoon
    if (isSet) {
      temp[temp$adduct %in% "[M-H]-", `:=`(
        mzmin = (temp$mz - 1.007276) - (temp$mz - temp$mzmin),
        mzmax = (temp$mz - 1.007276) + (temp$mzmax - temp$mz),
        mz = temp$mz - 1.007276
      )]
      temp[temp$adduct %in% "[M+H]+", `:=`(
        mzmin = (temp$mz + 1.007276) - (temp$mz - temp$mzmin),
        mzmax = (temp$mz + 1.007276) + (temp$mzmax - temp$mz),
        mz = temp$mz + 1.007276
      )]
    }

    if (!"adduct" %in% colnames(temp)) temp$adduct <- adduct
    if (!"mass" %in% colnames(temp)) temp$mass <- temp$mz + adduct_val
    if (!"is_filled" %in% colnames(temp)) {
      temp$is_filled <- FALSE
    } else {
      temp$is_filled <- as.logical(temp$is_filled)
    }
    if (!"filtered" %in% colnames(temp)) temp$filtered <- FALSE
    if (!"filter" %in% colnames(temp)) temp$filter <- NA_character_

    setnames(temp,
      c("ID", "ret", "retmin", "retmax"),
      c("feature", "rt", "rtmin", "rtmax"),
      skip_absent = TRUE
    )

    # when grouping features are removed from grouping conditions in patRoon
    # therefore, old features are retained and tagged with filter "grouping"
    temp_org <- self$get_features(x)
    if (nrow(temp_org) > 0) {
      temp_org$analysis <- NULL
      if (nrow(temp_org) != nrow(temp)) {
        temp_org_not_grouped <- temp_org[!temp_org$index %in% temp$feature, ]
        temp_list <- list(temp, temp_org_not_grouped)
        temp <- rbindlist(temp_list, fill = TRUE)
      }
    }

    if ("group" %in% colnames(temp)) {
      temp$filter[is.na(temp$group)] <- "grouping"
      temp$filtered[is.na(temp$group)] <- TRUE
    }

    dppm <- round((temp$mzmax - temp$mzmin) / temp$mzmin * 1E6, 0)
    drt <- round(temp$rtmax - temp$rtmin, 0)

    temp <- temp[order(temp$mz), ]
    temp <- temp[order(temp$rt), ]
    temp <- temp[order(temp$filtered), ]
    temp$index <- seq_len(nrow(temp))

    setcolorder(
      temp,
      c(
        "feature", "index", "rt", "mz", "intensity", "area",
        "rtmin", "rtmax", "mzmin", "mzmax", "adduct", "mass",
        "is_filled", "filtered", "filter"
      )
    )

    temp$feature <- paste0(
      "mz",
      round(temp$mz, digits = 3),
      "_d",
      dppm,
      "_rt",
      round(temp$rt, digits = 0),
      "_t",
      drt,
      "_f",
      temp$index
    )

    temp$rt <- round(temp$rt, 3)
    temp$rtmin <- round(temp$rtmin, 3)
    temp$rtmax <- round(temp$rtmax, 3)

    temp$mz <- round(temp$mz, 8)
    temp$mzmin <- round(temp$mzmin, 8)
    temp$mzmax <- round(temp$mzmax, 8)

    return(temp)
  }, extra = extra, features = features, self = self, isSet = isSet)

  names(features) <- analyses

  features
}

#' build_feature_groups_table_from_patRoon
#'
#' @param pat An object with class `featureGroups` from the package \pkg{patRoon}.
#' @param features X.
#' @param self An `msData` object. When applied within the R6, the self object.
#'
#' @return A \linkS4class{data.table} with the feature groups.
#'
#' @noRd
#'
build_feature_groups_table_from_patRoon <- function(pat, features, self) {

  fgroups <- patRoon::as.data.table(pat, average = FALSE)
  setnames(fgroups, "ret", "rt")

  fts <- copy(features)
  fts <- rbindlist(fts, idcol = "analysis")

  index <- lapply(fgroups$group, function(g, fts) {
    which(fts$group == g)
  }, fts = fts)

  fgroups$rt <- vapply(index, function(x) {
    return(round(mean(fts$rt[x]), 3))
  }, 0)

  fgroups$drt <- vapply(index, function(x) {
    round(max(fts$rtmax[x]) - min(fts$rtmin[x]), 0)
  }, 0)

  fgroups$dppm <- vapply(index, function(x) {
    max_ppm <- max((fts$mzmax[x] - fts$mz[x]) / fts$mz[x] * 1E6)
    min_ppm <- min((fts$mzmin[x] - fts$mz[x]) / fts$mz[x] * 1E6)
    round(max_ppm - min_ppm, 0)
  }, 0)

  fgroups$index <- as.numeric(sub(".*_", "", fgroups$group))

  if ("is_filled" %in% colnames(fts)) {
    fgroups$hasFilled <- vapply(
      index,
      function(x) {
        TRUE %in% fts$is_filled[x]
      }, FALSE
    )
  } else {
    fgroups$hasFilled <- FALSE
  }

  if (!"filtered" %in% colnames(fgroups)) {
    fgroups$filtered <- FALSE
    fgroups$filter <- NA_character_
  }

  if (TRUE %in% grepl("Set", is(pat))) {
    setnames(fgroups, "mz", "mass", skip_absent = TRUE)
    fgroups$neutralMass <- NULL

    fgroups$mass <- vapply(index, function(x) {
      round(mean(fts$mass[x]), 8)
    }, 0)

    new_id <- paste0(
      "m",
      round(fgroups$mass, 3),
      "_d",
      fgroups$dppm,
      "_rt",
      round(fgroups$rt, 0),
      "_t",
      round(fgroups$drt, 0),
      "_g",
      fgroups$index
    )
  } else {
    fgroups$mz <- vapply(index, function(x) {
      round(mean(fts$mz[x]), 8)
    }, 0)

    adduct <- unique(fts$adduct)
    fgroups$adduct <- adduct
    if (adduct %in% "[M+H]+") fgroups$mass <- fgroups$mz - 1.007276
    if (adduct %in% "[M-H]-") fgroups$mass <- fgroups$mz + 1.007276

    new_id <- paste0(
      "mz",
      round(fgroups$mz, digits = 3),
      "_d",
      fgroups$dppm,
      "_rt",
      round(fgroups$rt, digits = 0),
      "_t",
      fgroups$drt,
      "_g",
      fgroups$index
    )
  }

  names(new_id) <- fgroups$group
  fgroups$group <- new_id

  features_new_id <- lapply(features, function(x, new_id) {
    x$group <- new_id[x$group]
    return(x)
  }, new_id = new_id)

 list("features" = features_new_id, "groups" = fgroups)
}

update_subset_features_and_groups <- function(newGroups, newFeatures) {
  fts <- rbindlist(newFeatures, idcol = "analysis")

  fgs_remaining <- unique(unlist(fts$group))
  if (!is.null(fgs_remaining)) {
    newGroups <- newGroups[newGroups$group %in% fgs_remaining, ]
  }

  index <- lapply(newGroups$group, function(g, fts) {
    which(fts$group == g)
  }, fts = fts)

  newGroups$rt <- vapply(index, function(x) {
    round(mean(fts$rt[x]), digits = 3)
  }, 0)

  newGroups$mass <- vapply(index, function(x) {
    round(mean(fts$mass[x]), digits = 8)
  }, 0)

  newGroups$adduct <- vapply(index, function(x) {
    paste(unique(fts$adduct[x]), collapse = "; ")
  }, NA_character_)

  newGroups$drt <- vapply(index, function(x) {
    round(max(fts$rtmax[x]) - min(fts$rtmin[x]), 0)
  }, 0)

  newGroups$dppm <- vapply(index, function(x) {
    max_ppm <- max((fts$mzmax[x] - fts$mz[x]) / fts$mz[x] * 1E6)
    min_ppm <- min((fts$mzmin[x] - fts$mz[x]) / fts$mz[x] * 1E6)
   round(max_ppm - min_ppm, 0)
  }, 0)

  newGroups$index <- seq_len(length(newGroups$group))

  if ("is_filled" %in% colnames(fts)) {
    newGroups$hasFilled <- vapply(index, function(x) {
      TRUE %in% fts$is_filled[x]
    }, FALSE)
  } else {
    newGroups$hasFilled <- FALSE
  }

  if (!"filtered" %in% colnames(newGroups)) {
    newGroups$filtered <- FALSE
    newGroups$filter <- NA_character_
  }

  if (!"mz" %in% colnames(newGroups)) {
    new_id <- paste0(
      "m",
      round(newGroups$mass, digits = 3),
      "_d",
      newGroups$dppm,
      "_rt",
      round(newGroups$rt, digits = 0),
      "_t",
      newGroups$drt,
      "_g",
      newGroups$index
    )
  } else {
    newGroups$mz <- vapply(index, function(x) {
      round(mean(fts$mz[x]), digits = 8)
    }, 0)

    new_id <- paste0(
      "mz",
      round(newGroups$mz, digits = 3),
      "_d",
      newGroups$dppm,
      "_rt",
      round(newGroups$rt, digits = 0),
      "_t",
      newGroups$drt,
      "_g",
      newGroups$index
    )
  }

  names(new_id) <- newGroups$group
  newGroups$group <- new_id

  newFeatures_new_id <- lapply(newFeatures, function(x, new_id) {
    x$group <- new_id[x$group]
    x
  }, new_id = new_id)

  list("features" = newFeatures_new_id, "groups" = newGroups)
}

#' extract_time_alignment
#'
#' @description Function to extract adjusted retention time information from
#' alignment results when using `xcms3` as algorithm for grouping and retention
#' time alignment.
#'
#' @param pat An object with class `featureGroups` from \pkg{patRoon}.
#' @param self An `msData` object. When applied within the R6, the self object.
#'
#' @noRd
#'
extract_time_alignment <- function(pat, self) {
  if ("featureGroupsXCMS3" %in% is(pat)) {

    if (xcms::hasAdjustedRtime(pat@xdata)) {
      rtAdj <- xcms::adjustedRtime(pat@xdata)
      pkAdj <- xcms::processHistory(pat@xdata,
        type = "Retention time correction"
      )[[1]]
      pkAdj <- pkAdj@param

      addAdjPoints <- FALSE
      if ("PeakGroupsParam" %in% is(pkAdj)) {
        addAdjPoints <- TRUE
        pkAdj <- xcms::peakGroupsMatrix(pkAdj)
      }

      # hasSpectra = all(self$has_loaded_spectra())
      hasSpectra <- FALSE

      if (!hasSpectra) {
        rtOrg <- lapply(self$get_file_paths(), function(x) {
          file_link <- mzR::openMSfile(x, backend = "pwiz")
          sH <- suppressWarnings(mzR::header(file_link))
          suppressWarnings(mzR::close(file_link))
          sH$retentionTime
        })
      }

      alignment <- lapply(self$get_analysis_names(),
        function(ana, rtOrg, rtAdj, addAdjPoints, pkAdj, all_ana) {
          ana_idx <- which(all_ana %in% ana)
          n_ana <- length(all_ana)

          rts <- names(rtAdj)
          ana_idx_string <- paste0(
            "F",
            paste(rep("0", nchar(n_ana) - nchar(ana_idx)), collapse = ""),
            ana_idx
          )
          rts <- grepl(ana_idx_string, rts)
          rts <- rtAdj[rts]

          temp <- data.frame(
            "rt_original" = rtOrg[[ana]],
            "rt_adjusted" = rts
          )

          temp$adjustment <- temp$rt_original - temp$rt_adjusted

          if (addAdjPoints) {
            adjPoints <- unique(pkAdj[, ana_idx])
            adjPoints <- adjPoints[adjPoints %in% temp$rt_original]
            temp$adjPoints[temp$rt_original %in% adjPoints] <- adjPoints
          }
          row.names(temp) <- seq_len(nrow(temp))
          temp
        },
        rtOrg = rtOrg,
        rtAdj = rtAdj,
        addAdjPoints = addAdjPoints,
        pkAdj = pkAdj,
        all_ana = self$get_analysis_names()
      )

      return(alignment)
    }
  }
  NULL
}
