
#' R6 Class Representing an MS Set of Analysis Files
#'
#' @description
#' A description of the R6 class.
#'
#' @details
#' The details.
#'
#' @export
R6MS = R6::R6Class("R6MS",

  # private fields -----
  private = list(

    ## .header -----
    .header = list(
      name = NA_character_,
      author =  NA_character_,
      # user = Sys.info("user"),
      # system_info = Sys.info("sysname"),
      # computer = Sys.info("nodename"),
      description = NA_character_,
      path = getwd(),
      date = Sys.time()
    ),

    ## .settings -----
    .settings = list(),

    ## .analyses -----
    .analyses = list(),

    ## .groups -----
    .groups = NULL,

    ## .alignment -----
    .alignment = NULL

  ),

  # public fields/methods -----

  public = list(

    ## system -----

    #' @description
    #' Create a new MS set of analyses.
    #'
    #' @param files A character vector with the full file path of MS analyses
    #' or a data.frame with the columns file, replicate and blank with the
    #' full file path, the replicate group name (string) and the associated
    #' blank replicate group (string).
    #'  @param run_parallel Logical, set to \code{TRUE} for processing the data
    #' in parallel.
    #' @param .header A list with administrative information for the header.
    #' @param .settings A list with settings.
    #' @param .analyses A list with MS analyses information.
    #' @param .groups A data.frame with `groups` representing corresponding
    #' features across MS analyses.
    #' @param .alignment X.
    #'
    #'
    #' @return A new `r6MS` object.
    initialize = function(files = NULL,
                          run_parallel = FALSE,
                          .header = NULL,
                          .settings = NULL,
                          .analyses = NULL,
                          .groups = NULL,
                          .alignment = NULL) {

      if (is.null(.analyses) & !is.null(files)) {

        if (is.data.frame(files)) {
          if ("file" %in% colnames(files)) {

            if ("replicate" %in% colnames(files))
              replicates = as.character(files$replicate)
                else replicates = rep(NA_character_, nrow(files))

            if ("blank" %in% colnames(files))
              blanks = as.character(files$blank)
                else blanks = NULL

            files = files$file

          } else files = ""
        } else {
          replicates = rep(NA_character_, length(files))
          blanks = NULL
        }

        possible_ms_file_formats = ".mzML|.mzXML"

        valid_files = vapply(files, FUN.VALUE = FALSE,
          function(x, possible_ms_file_formats) {
            if (!file.exists(x)) return(FALSE)
            if (FALSE %in% grepl(possible_ms_file_formats, x)) return(FALSE)
            return(TRUE)
        }, possible_ms_file_formats = possible_ms_file_formats)

        if (!all(valid_files)) {
          warning("File/s not valid!")
          return(NULL)
        }

        if (run_parallel & length(files) > 1) {
          workers = parallel::detectCores() - 1
          if (length(files) < workers) workers = length(files)
          par_type = "PSOCK"
          if (parallelly::supportsMulticore()) par_type = "FORK"
          cl = parallel::makeCluster(workers, type = par_type)
          doParallel::registerDoParallel(cl)
          #on.exit(parallel::stopCluster(cl))
        } else {
          registerDoSEQ()
        }

        analyses = foreach(i = files, .packages = "mzR") %dopar% {

          file_link = mzR::openMSfile(i, backend = "pwiz")
          sH = suppressWarnings(mzR::header(file_link))
          cH = suppressWarnings(mzR::chromatogramHeader(file_link))
          instrument = mzR::instrumentInfo(file_link)
          run = suppressWarnings(mzR::runInfo(file_link))

          polarities = NULL
          if (1 %in% sH$polarity) polarities = c(polarities, "positive")
          if (0 %in% sH$polarity) polarities = c(polarities, "negative")
          if (nrow(cH) > 0 & ("polarity" %in% colnames(cH))) {
            if (1 %in% cH$polarity) polarities = c(polarities, "positive")
            if (0 %in% cH$polarity) polarities = c(polarities, "negative")
          }
          if (is.null(polarities)) polarities = NA_character_

          spectra_number = run$scanCount
          spectra_mode = NA_character_
          if (TRUE %in% sH$centroided) spectra_mode = "centroid"
          if (FALSE %in% sH$centroided) spectra_mode = "profile"

          ion_mobility = FALSE
          if (!all(is.na(sH$ionMobilityDriftTime))) ion_mobility = TRUE

          chromatograms_number = 0
          if (grepl(".mzML", i))
            chromatograms_number = mzR::nChrom(file_link)

          if (spectra_number == 0 & chromatograms_number > 0) {

            if (TRUE %in% grepl("SRM", cH$chromatogramId)) data_type = "SRM"

            tic = cH[cH$chromatogramId %in% "TIC", ]
            if (nrow(tic) > 0) {
              tic = mzR::chromatograms(file_link, tic$chromatogramIndex)
              colnames(tic) = c("rt", "intensity")
              if (max(tic$rt) < 60) tic$rt = tic$rt * 60
            } else tic = data.frame("rt" = numeric(), "intensity" = numeric())

            bpc = cH[cH$chromatogramId %in% "BPC", ]
            if (nrow(bpc) > 0) {
              bpc = mzR::chromatograms(file_link, bpc$chromatogramIndex)
              if (!"mz" %in% colnames(bpc)) {
                bpc$mz = NA
                colnames(bpc) = c("rt", "intensity", "mz")
              } else colnames(bpc) = c("rt", "mz", "intensity")

              if (max(bpc$rt) < 60) bpc$rt = bpc$rt * 60
            } else bpc = data.frame("rt" = numeric(),
                                    "mz" = numeric() ,
                                    "intensity" = numeric())

          } else if (spectra_number > 0) {

            if (2 %in% run$msLevels) data_type = "MS/MS"
              else data_type = "MS"

            if (max(sH$retentionTime) < 60)
              sH$retentionTime = sH$retentionTime * 60

            sH_ms1 = sH[sH$msLevel == 1, ]

            tic = data.frame(
              "rt" = sH_ms1$retentionTime,
              "intensity" = sH_ms1$totIonCurrent)

            bpc = data.frame(
              "rt" = sH_ms1$retentionTime,
              "mz" = sH_ms1$basePeakMZ,
              "intensity" = sH_ms1$basePeakIntensity
            )

          } else data_type = NA_character_

          if (is.infinite(run$lowMz)) run$lowMz = NA_real_
          if (is.infinite(run$highMz)) run$highMz = NA_real_
          if (is.infinite(run$dStartTime)) run$dStartTime = min(tic$rt)
          if (is.infinite(run$dEndTime)) run$dEndTime = max(tic$rt)
          if (data_type %in% "SRM") run$msLevels = NA_integer_

          analysis = list(
            "name" = gsub(".mzML|.mzXML", "", basename(i)),
            "replicate" = NA_character_,
            "blank" = NA_character_,
            "file" = i,
            "type" = data_type,
            "instrument" = instrument,
            "time_stamp" = run$startTimeStamp,
            "spectra_number" = as.integer(spectra_number),
            "spectra_mode" = spectra_mode,
            "spectra_levels" = as.integer(run$msLevels),
            "mz_low" = as.numeric(run$lowMz),
            "mz_high" = as.numeric(run$highMz),
            "rt_start" = as.numeric(run$dStartTime),
            "rt_end" = as.numeric(run$dEndTime),
            "polarity" = polarities,
            "chromatograms_number" = as.integer(chromatograms_number),
            "ion_mobility" = ion_mobility,
            "tic" = tic,
            "bpc" = bpc,
            "spectra" = data.frame(),
            "chromatograms" = data.frame(),
            "features" = data.frame(),
            "metadata" = list()
          )

          suppressWarnings(mzR::close(file_link))
          return(analysis)
        }

        if (run_parallel) parallel::stopCluster(cl)

        if (all(is.na(replicates))) {
          replicates = vapply(analyses, function(x) x$name, "")
          replicates = gsub( "-", "_", replicates)
          replicates = sub("_[^_]+$", "", replicates)
        }

        analyses = Map(function(x, y) { x$replicate = y; x },
                       analyses, replicates)

        if (!is.null(blanks) & length(blanks) == length(analyses)) {
          if (all(blanks %in% replicates)) {
            analyses = Map(function(x, y) { x$blank = y; x },
                           analyses, blanks)
          }
        }

        names(analyses) = vapply(analyses, function(x) x$name, "")
        analyses = analyses[order(names(analyses))]
        private$.analyses = analyses

      } else if (!is.null(.analyses)) {

        valid_analyses = vapply(.analyses, validate_list_ms_analysis, FALSE)

        if (all(valid_analyses)) {

          names(.analyses) = vapply(.analyses, function(x) x$name, "")
          .analyses = .analyses[order(names(.analyses))]
          private$.analyses = .analyses

        } else {
          warning("No valid files or analyses were
                given to create the R6MS!")
        }
      } else {
        warning("No valid files or analyses were
                given to create the R6MS!")
      }

      private$.groups = .groups

      private$.alignment = .alignment

      if (!is.null(.header) & is.list(.header)) {

        if (is.character(.header$name) & length(.header$name) == 1)
          private$.header$name = .header$name

        if (is.character(.header$author) & length(.header$author) == 1)
          private$.header$author = .header$author

        if (is.character(.header$description) & length(.header$description) == 1)
          private$.header$description = .header$description

        if (is.character(.header$path) & length(.header$path) == 1)
          if (dir.exists(.header$path)) private$.header$path = .header$path

        if (all(grepl("POSIXct|POSIXt", class(.header$date))) &
            length(.header$date) == 1)
              private$.header$date = .header$date

      }
    },

    #' @description
    #' Prints a summary of the `R6MS` object in the console.
    #' @return Console text.
    print = function() {
      cat(
        "  Class         ", paste(is(self), collapse = "; "), "\n",
        "  Name         ", private$.header$name, "\n",
        "  Date          ", as.character(private$.header$date), "\n",
        sep = ""
      )
      if (length(private$.analyses) > 0) {
        tb = self$get_overview()
        tb$file = NULL

        tb$traces = vapply(private$.analyses, function(x) x$spectra_number, 0)
        tb$features = vapply(private$.analyses, function(x) nrow(x$features), 0)

        if (!is.null(private$.groups)) {
          tb$groups = apply(
            private$.groups[, self$get_analysis_names(), with = FALSE],
            2, function(x) length(x[x > 0])
          )
        } else {
          tb$groups = 0
        }

        print(tb)

      } else {
        cat("     n.a.", "\n", sep = "")
      }
    },

    ## get -----

    #' @description
    #' Method to get the header of the `R6MS` object.
    #'
    #' @param value A character vector with the name of the entry.
    #' Possible values are name, author, description, path and date.
    #'
    #' @return The header list as defined by `value`.
    get_header = function(value) {
      if (missing(value)) return(private$.header)
      else return(private$.header[value])
    },

    #' @description
    #' Method to get specific analyses from the `R6MS` object.
    #'
    #' @param value A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return The list of analyses defined by `value`.
    get_analyses = function(value) {
      if (missing(value)) return(private$.analyses)
        else return(private$.analyses[value])
    },

    #' @description
    #' Method to get the number of analysis in the `R6MS` object.
    #'
    #' @return An integer value.
    get_number_analyses = function() {
      return(length(private$.analyses))
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
        df = data.frame(
          "type" = vapply(private$.analyses, function(x) x$type, ""),
          "analysis" = vapply(private$.analyses, function(x) x$name, ""),
          "replicate" = vapply(private$.analyses, function(x) x$replicate, ""),
          "blank" = vapply(private$.analyses, function(x) x$blank, ""),
          "polarity" = vapply(private$.analyses,function(x) {
            paste(x$polarity, collapse = "; ")
          }, ""),
          "file" = vapply(private$.analyses, function(x) x$file, "")
        )

        row.names(df) = seq_len(nrow(df))

        return(df)
      } else return(data.frame())
    },

    #' @description
    #' Method to get the analysis names.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_analysis_names = function(analyses = NULL) {
      ana = vapply(private$.analyses, function(x) x$name, "")
      names(ana) = vapply(private$.analyses, function(x) x$name, "")
      if (!is.null(analyses)) return(ana[analyses])
      return(ana)
    },

    #' @description
    #' Method to get the analysis replicate names.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_replicate_names = function(analyses = NULL) {
      rpl = vapply(private$.analyses, function(x) x$replicate, "")
      names(rpl) = vapply(private$.analyses, function(x) x$name, "")
      if (!is.null(analyses)) return(rpl[analyses])
      return(rpl)
    },

    #' @description
    #' Method to get the analysis blank replicate names.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_blank_names = function(analyses = NULL) {
      blk = vapply(private$.analyses, function(x) x$blank, "")
      names(blk) = vapply(private$.analyses, function(x) x$name, "")
      if (!is.null(analyses)) return(blk[analyses])
      return(blk)
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_polarities = function(analyses = NULL) {
      pol = vapply(private$.analyses,function(x) {
        paste(x$polarity, collapse = "; ")
      }, "")
      names(pol) = vapply(private$.analyses, function(x) x$name, "")
      if (!missing(analyses)) return(pol[analyses])
      return(pol)
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_file_paths = function(analyses = NULL) {
      fls = vapply(private$.analyses, function(x) x$file, "")
      names(fls) = vapply(private$.analyses, function(x) x$name, "")
      if (!is.null(analyses)) return(fls[analyses])
      return(fls)
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_mz_high = function(analyses = NULL) {
      value = vapply(private$.analyses, function(x) x$mz_high, 0)
      names(value) = vapply(private$.analyses, function(x) x$name, "")
      if (!is.null(analyses)) return(value[analyses])
      return(value)
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_rt_end = function(analyses = NULL) {
      value = vapply(private$.analyses, function(x) x$rt_end, 0)
      names(value) = vapply(private$.analyses, function(x) x$name, "")
      if (!is.null(analyses)) return(value[analyses])
      return(value)
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
    #' are returned, using the `preMZ` value and the `isolationWindow`.
    #' @param isolationWindow X.
    #' @param minIntensityMS1 X.
    #' @param minIntensityMS2 X.
    #' @param run_parallel X.
    #'
    #' @return A data.frame with spectra for each analyses and
    #' targets when defined.
    #'
    get_spectra = function(analyses = NULL,
                           levels = NULL,
                           mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                           allTraces = TRUE, isolationWindow = 1.3,
                           minIntensityMS1 = 0, minIntensityMS2 = 0,
                           run_parallel = FALSE) {

      trim = function(v, a, b)  return(
          rowSums(mapply(function(a, b) v >= a & v <= b, a = a, b = b)) > 0)

      analyses = self$check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.frame())

      targets = make_targets(mz, rt, ppm, sec, id)

      with_targets = !(nrow(targets) == 1 &
        TRUE %in% (targets$mzmax == 0) &
        TRUE %in% (targets$rtmax == 0))

      if (TRUE %in% (targets$mzmax == 0)) {
        targets$mzmax[targets$mzmax == 0] = max(self$get_mz_high(analyses))
      }

      if (TRUE %in% (targets$rtmax == 0)) {
        targets$rtmax[targets$rtmax == 0] = max(self$get_rt_end(analyses))
      }

      if (with_targets) {

        trim_targets = function(traces, targets, preMZr) {

          trim = function(v, a, b)  return(
            rowSums(mapply(function(a, b) v >= a & v <= b, a = a, b = b)) > 0)

          tg_list = lapply(seq_len(nrow(targets)),

            function(z, traces, targets, trim, preMZr) {

              tg = traces

              cutRt = trim(tg$rt, targets$rtmin[z], targets$rtmax[z])
              tg = tg[cutRt, ]

              if (!is.null(preMZr)) {

                 cutMZ = trim(tg$mz, targets$mzmin[z], targets$mzmax[z])
                 tg = tg[tg$level == 2 | (tg$level == 1 & cutMZ), ]

                 cutPreMZ = trim(tg$preMZ, preMZr$mzmin[z], preMZr$mzmax[z])
                 tg = tg[tg$level == 1 | (tg$level == 2 & cutPreMZ), ]

              } else {

                 cutMZ = trim(tg$mz, targets$mzmin[z], targets$mzmax[z])
                 tg = tg[cutMZ, ]

              }

              if (nrow(tg) > 0) tg$id = targets$id[z] else tg$id = character()

              return(tg)

            }, traces = traces, preMZr = preMZr, targets = targets, trim = trim)

          tg_df = do.call("rbind", tg_list)

          return(tg_df)
        }
      }

      if (!allTraces) {
        preMZr = targets[, c("mzmin", "mzmax")]
        preMZr$mzmin = preMZr$mzmin - (isolationWindow/2)
        preMZr$mzmax = preMZr$mzmax + (isolationWindow/2)
        if (nrow(preMZr) == 1 & TRUE %in% (targets$mzmax == 0)) preMZr = NULL
      } else preMZr = NULL

      has_spectra = self$has_loaded_spectra(analyses)

      if (all(has_spectra)) {

        spec_list = lapply(private$.analyses[analyses],
          function(x, levels, with_targets, targets, preMZr) {

            temp = x$spectra

            if (!is.null(levels)) temp = temp[temp$level %in% levels, ]

            if (with_targets) {
              if ("analysis" %in% colnames(targets)) {
                tp_tar = targets[targets$analysis %in% x$name, ]
                if (nrow(tp_tar) > 0) {
                  temp = trim_targets(temp, tp_tar, preMZr)
                } else sH = data.frame()
              } else temp = trim_targets(temp, targets, preMZr)
            }

            return(temp)
        },
        levels = levels,
        with_targets = with_targets,
        targets = targets,
        preMZr = preMZr)

        names(spec_list) = analyses

        spec = rbindlist(spec_list, idcol = "analysis", fill = TRUE)

        spec = spec[!(spec$intensity <= minIntensityMS1 & spec$level == 1), ]
        spec = spec[!(spec$intensity <= minIntensityMS2 & spec$level == 2), ]

        return(spec)
      }

      files = unname(self$get_file_paths(analyses))

      if (all(!is.na(files))) {

        if (run_parallel & length(files) > 1) {
          workers = parallel::detectCores() - 1
          if (length(files) < workers) workers = length(files)
          par_type = "PSOCK"
          if (parallelly::supportsMulticore()) par_type = "FORK"
          cl = parallel::makeCluster(workers, type = par_type)
          doParallel::registerDoParallel(cl)
          #on.exit(parallel::stopCluster(cl))
        } else {
          registerDoSEQ()
        }

        spec_list = foreach(i = files, .packages = "mzR") %dopar% {

          file_link = mzR::openMSfile(i, backend = "pwiz")

          sH = mzR::header(file_link)

          if (nrow(sH) > 0) {

            if (max(sH$retentionTime) < 60) {
              sH$retentionTime = sH$retentionTime * 60
            }

            if (!is.null(levels)) sH = sH[sH$msLevel %in% levels, ]

            if (with_targets) {
              if ("analysis" %in% colnames(targets)) {
                ana_name = gsub(".mzML|.mzXML", "", basename(i))
                tp_tar = targets[targets$analysis %in% ana_name, ]
                if (nrow(tp_tar) > 0) {
                  sH = sH[trim(sH$retentionTime, tp_tar$rtmin, tp_tar$rtmax), ]
                } else sH = data.frame()
              } else {
                sH = sH[trim(sH$retentionTime, targets$rtmin, targets$rtmax), ]
              }
            }

            if(!is.null(preMZr)) {
              preMZ_check = trim(sH$precursorMZ, preMZr$mzmin, preMZr$mzmax)
              sH = sH[(preMZ_check %in% TRUE) | is.na(preMZ_check), ]
            }

            if (nrow(sH) > 0) {

              scans = mzR::peaks(file_link, scans = sH$seqNum)

              mat_idx = rep(sH$seqNum, sapply(scans, nrow))
              scans = as.data.frame(do.call(rbind, scans))
              scans$index = mat_idx

              if (TRUE %in% (unique(sH$msLevel) == 2)) {
                sH_b = data.frame(
                  "index" = sH$seqNum,
                  "scan" = sH$acquisitionNum,
                  "level" = sH$msLevel,
                  "ce" = sH$collisionEnergy,
                  "preScan" = sH$precursorScanNum,
                  "preMZ" = sH$precursorMZ,
                  "rt" = sH$retentionTime)
              } else {
                sH_b = data.frame(
                  "index" = sH$seqNum,
                  "scan" = sH$acquisitionNum,
                  "level" = sH$msLevel,
                  "rt" = sH$retentionTime)
              }

              if (!all(is.na(sH$ionMobilityDriftTime))) {
                rt_unique = unique(sH_b$rt)
                frame_numbers = seq_len(length(rt_unique))
                if ("preMZ" %in% colnames(sH_b)) sH_b$preMZ = NA_real_
                sH_b$frame = factor(sH_b$rt,
                  levels = rt_unique, labels = frame_numbers)
                sH_b$driftTime = sH$ionMobilityDriftTime
              }

              sH = merge(sH_b, scans, by = "index")

              if (with_targets) {
                if ("analysis" %in% colnames(targets)) {
                  ana_name = gsub(".mzML|.mzXML", "", basename(i))
                  tp_tar = targets[targets$analysis %in% ana_name, ]
                  if (nrow(tp_tar) > 0) {
                    sH = trim_targets(sH, tp_tar, preMZr)
                  } else sH = data.frame()
                } else sH = trim_targets(sH, targets, preMZr)
              }

              if (exists("file_link")) suppressWarnings(mzR::close(file_link))

              return(sH)

            } else return(data.frame())

          } else return(data.frame())
        }

        if (run_parallel) parallel::stopCluster(cl)
        # unregister_dopar()

        names(spec_list) = analyses

        spec = rbindlist(spec_list, idcol = "analysis", fill = TRUE)

        spec = spec[!(spec$intensity <= minIntensityMS1 & spec$level == 1), ]
        spec = spec[!(spec$intensity <= minIntensityMS2 & spec$level == 2), ]

        return(spec)

      } else {
        warning("Defined analyses not found!")
        return(list())
      }
    },

    #' @description
    #' Method to get spectra from the MS analyses.
    #'
    #' @param analyses X.
    #' @param minIntensity X.
    #' @param run_parallel X.
    #'
    #' @return A data.frame with spectra.
    get_chromatograms = function(analyses = NULL, minIntensity = 0,
                                 run_parallel = FALSE) {

      analyses = self$check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.frame())

      files = unname(self$get_file_paths(analyses))

      if (all(!is.na(files))) {

        if (run_parallel & length(files) > 1) {
          workers = parallel::detectCores() - 1
          if (length(files) < workers) workers = length(files)
          par_type = "PSOCK"
          if (parallelly::supportsMulticore()) par_type = "FORK"
          cl = parallel::makeCluster(workers, type = par_type)
          doParallel::registerDoParallel(cl)
          #on.exit(parallel::stopCluster(cl))
        } else {
          registerDoSEQ()
        }

        chrom_list = foreach(i = files, .packages = "mzR") %dopar% {

          file_link = mzR::openMSfile(i, backend = "pwiz")

          cH = suppressWarnings(mzR::chromatogramHeader(file_link))

          if (nrow(cH) > 0) {

            cH$polarity = as.character(cH$polarity)
            cH[cH$polarity == 1, "polarity"] = "positive"
            cH[cH$polarity == 0, "polarity"] = "negative"
            cH[cH$polarity == -1, "polarity"] = NA_character_

            chroms = mzR::chromatograms(file_link, cH$chromatogramIndex)

            if (!is.data.frame(chroms)) {

              chroms = lapply(cH$chromatogramIndex, function(x, chroms) {
                temp = chroms[[x]]
                temp = as.data.frame(temp)
                colnames(temp) = c("rt", "intensity")
                temp$index = x
                if (max(temp$rt) < 60) temp$rt = temp$rt * 60
                return(temp)
              }, chroms = chroms)

              chroms = do.call("rbind", chroms)

              cH_b = data.frame(
                "index" = cH$chromatogramIndex,
                "id" = cH$chromatogramId,
                "polarity" = cH$polarity,
                "preMZ" = cH$precursorIsolationWindowTargetMZ,
                "mz" = cH$productIsolationWindowTargetMZ
              )

              chrom_data = merge(cH_b, chroms, by = "index")

            } else {

              colnames(chroms) = c("rt", "intensity")
              if (max(chroms$rt) < 60) chroms$rt = chroms$rt * 60

              chrom_data = data.frame(
                "index" = cH$chromatogramIndex,
                "id" = cH$chromatogramId,
                "polarity" = cH$polarity,
                "preMZ" = cH$precursorIsolationWindowTargetMZ,
                "mz" = cH$productIsolationWindowTargetMZ,
                "rt" = chroms$rt,
                "intensity" = chroms$intensity
              )
            }
            if (exists("file_link")) suppressWarnings(mzR::close(file_link))
            return(chrom_data)
          } else return(data.frame())
        }

        if (run_parallel) parallel::stopCluster(cl)

        names(chrom_list) = analyses
        chrom_df = rbindlist(chrom_list, idcol = "analysis", fill = TRUE)
        chrom_df = chrom_df[chrom_df$intensity > minIntensity, ]

        return(chrom_df)
      } else {
        warning("Defined analyses not found!")
        return(list())
      }
    },

    #' @description
    #' Method to get the total ion chromatograms (TIC) from the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_tic = function(analyses = NULL) {
      analyses = self$check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.frame())
      tic = lapply(private$.analyses[analyses], function(x) x$tic)
      tic = rbindlist(tic, idcol = "analysis", fill = TRUE)
      return(tic)
    },

    #' @description
    #' Method to get the base peak chromatograms (BPC) from the analyses.
    #'
    #' @param analyses A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_bpc = function(analyses = NULL) {
      analyses = self$check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.frame())
      bpc = lapply(private$.analyses[analyses], function(x) x$bpc)
      bpc = rbindlist(bpc, idcol = "analysis", fill = TRUE)
      return(bpc)
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
    #' @param run_parallel X.
    #'
    #' @return A data.frame.
    get_eic = function(analyses = NULL,
                        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                        run_parallel = FALSE) {

      eic = self$get_spectra(
        analyses, levels = 1, mz, rt, ppm, sec, id, allTraces = TRUE,
        isolationWindow = 1.3, minIntensityMS1 = 0, minIntensityMS2 = 0,
        run_parallel = run_parallel
      )

      if (nrow(eic) > 0) {
        eic = as.data.table(eic)
        if (!"id" %in% colnames(eic)) eic$id = NA_character_
        eic = eic[, `:=`(intensity = sum(intensity)),
          by = c("analysis", "id", "rt")][]
        eic = eic[, c("analysis", "id", "rt", "intensity"), with = FALSE]
        eic = unique(eic)
      }

      return(eic)
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
    #' @param run_parallel X.
    #'
    #' @return A data.frame.
    get_ms1 = function(analyses = NULL,
                       mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                       mzClust = 0.003, verbose = FALSE,
                       minIntensity = 1000, run_parallel = FALSE) {

      ms1 = self$get_spectra(
        analyses = analyses, levels = 1,
        mz = mz, rt = rt, ppm = ppm, sec = sec, id = id, allTraces = TRUE,
        minIntensityMS1 = minIntensity, minIntensityMS2 = 0,
        run_parallel = run_parallel
      )

      if (nrow(ms1) == 0) return(ms1)

      if (!"id" %in% colnames(ms1)) {
        ms1$id = paste(
          round(min(ms1$mz), digits = 4),
          "-",
          round(max(ms1$mz), digits = 4),
          "/",
          round(max(ms1$rt), digits = 0),
          "-",
          round(min(ms1$rt), digits = 0),
          sep = ""
        )
      }

      ms1$unique_id = paste0(ms1$analysis, "_", ms1$id)
      ms1_list = rcpp_ms_cluster_spectra(ms1, mzClust, verbose)
      ms1_df = rbindlist(ms1_list, fill = TRUE)

      ms1_df = ms1_df[order(ms1_df$mz), ]
      ms1_df = ms1_df[order(ms1_df$id), ]
      ms1_df = ms1_df[order(ms1_df$analysis), ]

      return(ms1_df)
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
    #' @param run_parallel X.
    #'
    #' @return A data.frame.
    get_ms2 = function(analyses = NULL,
                       mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                       isolationWindow = 1.3, mzClust = 0.005, verbose = TRUE,
                       minIntensity = 0, run_parallel = FALSE) {

      ms2 = self$get_spectra(
        analyses = analyses, levels = 2,
        mz = mz, rt = rt, ppm = ppm, sec = sec, id = id,
        isolationWindow = isolationWindow, allTraces = FALSE,
        minIntensityMS1 = 0, minIntensityMS2 = minIntensity,
        run_parallel = run_parallel
      )

      if (nrow(ms2) == 0) return(ms2)

      if (!"id" %in% colnames(ms2)) {
        ms2$id = paste(
          round(min(ms2$mz), digits = 4),
          "-",
          round(max(ms2$mz), digits = 4),
          "/",
          round(max(ms2$rt), digits = 0),
          "-",
          round(min(ms2$rt), digits = 0),
          sep = ""
        )
      }

      ms2$unique_id = paste0(ms2$analysis, "_", ms2$id)
      ms2_list = rcpp_ms_cluster_ms2(ms2, mzClust, verbose)
      ms2_df = rbindlist(ms2_list, fill = TRUE)

      ms2_df = ms2_df[order(ms2_df$mz), ]
      ms2_df = ms2_df[order(ms2_df$id), ]
      ms2_df = ms2_df[order(ms2_df$analysis), ]

      return(ms2_df)
    },

    #' @description
    #' Method to get settings from analyses.
    #'
    #' @param call A string with the name of function call.
    #'
    #' @return A data.frame.
    get_settings = function(call = NULL) {
      if(is.null(call)) return(private$.settings)
        else return(private$.settings[[call]])
    },

    #' @description
    #' Method to get features from analyses.
    #'
    #' @return A data.frame.
    get_features = function(analyses = NULL, id = NULL, mass = NULL,
                            mz = NULL, rt = NULL, ppm = 20, sec = 60,
                            filtered = FALSE) {

      analyses = self$check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.frame())
      fts = lapply(private$.analyses[analyses], function(x) x$features)
      fts = rbindlist(fts, idcol = "analysis", fill = TRUE)

      if (!filtered) fts = fts[!fts$filtered, ]

      if (!is.null(id)) {
        target_id = id

        if (is.character(target_id)) {
          if ("group" %in% colnames(fts)) {
            fts = fts[fts$id %in% target_id | fts$group %in% target_id, ]
          } else fts = fts[fts$id %in% target_id, ]
          return(fts)
        }

        if (is.data.frame(target_id)) if ("analysis" %in% colnames(target_id)) {
          sel = rep(FALSE, nrow(fts))
          for (i in seq_len(nrow(target_id))) {
            sel[(fts$id %in% target_id$id[i] &
                 fts$analysis %in% target_id$analysis[i])  |
                 fts$group %in% target_id$id] = TRUE
          }
          fts = fts[sel, ]
          return(fts)
        }

        return(data.frame())
      }

      if (!is.null(mass)) {
        if (is.data.frame(mass)) {
          colnames(mass) = gsub("mass", "mz", colnames(mass))
          colnames(mass) = gsub("neutralMass", "mz", colnames(mass))
        }
        targets = makeTargets(mass, rt, ppm, sec)
        sel = rep(FALSE, nrow(fts))
        for (i in seq_len(nrow(targets))) {
          sel[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
                between(fts$rt, targets$rtmin[i], targets$rtmax[i])] = TRUE
        }
        return(fts[sel])
      }

      if (!is.null(mz)) {
        targets = makeTargets(mz, rt, ppm, sec)
        sel = rep(FALSE, nrow(fts))
        for (i in seq_len(nrow(targets))) {
          sel[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
                between(fts$rt, targets$rtmin[i], targets$rtmax[i])] = TRUE
        }
        return(fts[sel])
      }

      return(fts)
    },

    #' @description
    #' Method to get EIC of features from analyses.
    #'
    #' @return A data.frame/data.table.
    get_features_eic = function(analyses = NULL, id = NULL, mass = NULL,
                                mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                rtExpand = 120, mzExpand = 0.005,
                                filtered = FALSE, run_parallel = FALSE) {

      fts = self$get_features(analyses, id, mass, mz, rt, ppm, sec, filtered)

      if (nrow(fts) == 0) return(data.table())

      fts$rtmin = fts$rtmin - rtExpand
      fts$rtmax = fts$rtmax + rtExpand
      fts$mzmin = fts$mzmin - mzExpand
      fts$mzmax = fts$mzmax + mzExpand

      eic = self$get_eic(analyses, mz = fts, run_parallel = run_parallel)

      return(eic)
    },

    #' @description
    #' Method to get an averaged MS1 spectrum for features in analyses.
    #'
    #' @return A data.frame/data.table.
    get_features_ms1 = function(analyses = NULL, id = NULL, mass = NULL,
                                mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                rtWindow = c(-2, 2), mzWindow = c(-5, 100),
                                mzClust = 0.003, minIntensity = 1000,
                                verbose = TRUE, filtered = FALSE,
                                run_parallel = FALSE) {

      fts = self$get_features(analyses, id, mass, mz, rt, ppm, sec, filtered)

      if (nrow(fts) == 0) return(data.frame())

      if (!is.null(rtWindow) & length(rtWindow) == 2 & is.numeric(rtWindow)) {
        fts$rtmin = fts$rt + rtWindow[1]
        fts$rtmax = fts$rt + rtWindow[2]
      }

      if (!is.null(mzWindow) & length(mzWindow) == 2 & is.numeric(mzWindow)) {
        fts$mzmin = fts$mz + mzWindow[1]
        fts$mzmax = fts$mz + mzWindow[2]
      }

      ms1 = self$get_ms1(analyses = unique(fts$analysis), mz = fts,
                         mzClust = mzClust, minIntensity = minIntensity,
                         verbose = verbose, run_parallel = run_parallel)

      if ("group" %in% colnames(fts)) {
        fgs = fts$group
        names(fgs) = fts$id
        ms1$group = fgs[ms1$id]
      }

      return(ms1)
    },

    #' @description
    #' Method to get an averaged MS2 spectrum for features in analyses.
    #'
    #' @return A data.frame/data.table.
    get_features_ms2 = function(analyses = NULL, id = NULL, mass = NULL,
                                mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                isolationWindow = 1.3, mzClust = 0.003,
                                minIntensity = 0, verbose = TRUE,
                                filtered = FALSE, run_parallel = FALSE) {

      fts = self$get_features(analyses, id, mass, mz, rt, ppm, sec, filtered)

      if (nrow(fts) == 0) return(data.frame())

      ms2 = self$get_ms2(analyses = unique(fts$analysis), mz = fts,
                         isolationWindow = isolationWindow, mzClust = mzClust,
                         minIntensity = minIntensity, verbose = verbose,
                         run_parallel = run_parallel)

      if ("group" %in% colnames(fts)) {
        fgs = fts$group
        names(fgs) = fts$id
        ms2$group = fgs[ms2$id]
      }

      return(ms2)
    },

    #' @description
    #' Method to get alignment.
    #'
    #' @return A data.frame.
    get_alignment = function() {

      return(private$.alignment)

    },

    #' @description
    #' Method to get feature groups from analyses.
    #'
    #' @return A data.frame/data.table.
    get_groups = function(groups = NULL, mass = NULL,
                          mz = NULL, rt = NULL, ppm = 20, sec = 60,
                          filtered = FALSE, onlyIntensities = FALSE,
                          average = FALSE) {

      fgroups = copy(private$.groups)
      if(self$has_feature_groups()) {

        if (!filtered) fgroups = fgroups[!fgroups$filtered, ]

        if (!is.null(groups)) {
          fgroups = fgroups[fgroups$group %in% groups, ]

        } else if (!is.null(mass)) {
          if (is.data.frame(mass)) {
            colnames(mass) <- gsub("mass", "mz", colnames(mass))
            colnames(mass) <- gsub("neutralMass", "mz", colnames(mass))
          }
          targets = makeTargets(mass, rt, ppm, sec)
          sel = rep(FALSE, nrow(fgroups))
          for (i in seq_len(nrow(targets))) {
            sel[between(fgroups$mass,
                        targets$mzmin[i],
                        targets$mzmax[i]) &
                  between(fgroups$rt,
                          targets$rtmin[i],
                          targets$rtmax[i])] = TRUE
          }
          fgroups = fgroups[sel, ]

        } else if (!is.null(mz)) {
          targets <- makeTargets(mz, rt, ppm, sec)
          sel = rep(FALSE, nrow(fgroups))

          if (!"mz" %in% colnames(fgroups)) {
            adduct <- paste(unique(fgroups$adduct), collapse = ",")

            if (grepl("\\[M\\+H\\]\\+", adduct)) {
              for (i in seq_len(nrow(targets))) {
                if (targets$rtmax[i] > 0) {
                  sel[between(fgroups$mass,
                              targets$mzmin[i] - 1.0073,
                              targets$mzmax[i] - 1.0073) &
                        between(fgroups$rt,
                                targets$rtmin[i],
                                targets$rtmax[i])] <- TRUE
                } else sel[between(fgroups$mass,
                                   targets$mzmin[i] - 1.0073,
                                   targets$mzmax[i] - 1.0073)] <- TRUE
              }
            }

            if (grepl("\\[M-H\\]-", adduct)) {
              for (i in seq_len(nrow(targets))) {
                if (targets$rtmax[i] > 0) {
                  sel[between(fgroups$mass,
                              targets$mzmin[i] + 1.0073,
                              targets$mzmax[i] + 1.0073) &
                        between(fgroups$rt,
                                targets$rtmin[i],
                                targets$rtmax[i])] <- TRUE
                } else sel[between(fgroups$mass,
                                   targets$mzmin[i] + 1.0073,
                                   targets$mzmax[i] + 1.0073)] <- TRUE
              }
            }

          } else {
            for (i in seq_len(nrow(targets))) {
              if (targets$rtmax[i] > 0) {
                sel[between(fgroups$mz,
                            targets$mzmin[i],
                            targets$mzmax[i]) &
                      between(fgroups$rt,
                              targets$rtmin[i],
                              targets$rtmax[i])] <- TRUE
              } else sel[between(feats@metadata$mz,
                                 targets$mzmin[i],
                                 targets$mzmax[i])] <- TRUE
            }
          }

          fgroups <- fgroups[sel, ]
        }

        if (onlyIntensities) {
          cols_id_ints = unname(c("group", self$get_analysis_names()))
          fgroups = fgroups[, cols_id_ints, with = FALSE]
        }

        if (average) {
          rpl_ana = self$get_overview()[, c("analysis", "replicate")]
          rpl_ana = split(rpl_ana, rpl_ana$replicate)
          rpl_ana = lapply(rpl_ana, function(x) x$analysis)

          sd_vals <- lapply(rpl_ana, function(x, fgroups) {
            temp <- fgroups[, x, with = FALSE]
            temp <- apply(temp, 1, function(x) sd(x) / mean(x) * 100)
            temp[is.nan(temp)] <- 0
            temp <- round(temp, digits = 0)
            return(temp)
          }, fgroups = fgroups)

          for (r in names(rpl_ana)) {
            ana = rpl_ana[[r]]
            fgroups[[r]] <- apply(fgroups[, ana, with = FALSE], 1, mean)
          }

          to_keep = colnames(fgroups)
          to_keep = to_keep[!to_keep %in% self$get_analysis_names()]
          fgroups = fgroups[, to_keep, with = FALSE]

          names(sd_vals) <- paste0(rpl, "_sd")
          fgroups <- cbind(fgroups, as.data.table(sd_vals))
        }
      }
      if (is.null(fgroups)) fgroups = data.table()
      return(fgroups)
    },

    #' @description
    #' Method to get an averaged MS1 spectrum for feature groups in analyses.
    #'
    #' @return A data.frame/data.table.
    get_groups_ms1 = function(groups = NULL, mass = NULL,
                              mz = NULL, rt = NULL, ppm = 20, sec = 60,
                              rtWindow = c(-2, 2), mzWindow = c(-5, 90),
                              mzClustFeatures = 0.003,
                              minIntensityFeatures = 1000,
                              mzClustGroups = 0.003,
                              minIntensityGroups = 1000,
                              groupBy = "groups",
                              verbose = TRUE, filtered = FALSE,
                              run_parallel = FALSE) {

      fgs = self$get_groups(groups, mass, mz, rt, ppm, sec, filtered,
                            onlyIntensities = FALSE, average = FALSE)

      if (nrow(fgs) == 0) return(data.frame())

      fts = self$get_features(id = fgs$group)

      if (nrow(fts) == 0) return(data.frame())

      if (!is.null(rtWindow) & length(rtWindow) == 2 & is.numeric(rtWindow)) {
        fts$rtmin = fts$rt + rtWindow[1]
        fts$rtmax = fts$rt + rtWindow[2]
      }

      if (!is.null(mzWindow) & length(mzWindow) == 2 & is.numeric(mzWindow)) {
        fts$mzmin = fts$mz + mzWindow[1]
        fts$mzmax = fts$mz + mzWindow[2]
      }

      fts$id = fts$group

      ms1 = self$get_ms1(analyses = unique(fts$analysis), mz = fts,
                         mzClust = mzClustFeatures,
                         minIntensity = minIntensityFeatures,
                         verbose = verbose, run_parallel = run_parallel)

      ms1 = ms1[ms1$intensity > minIntensityGroups, ]

      if (nrow(ms1) == 0) return(data.frame())

      if ("groups" %in% groupBy) {
        ms1$unique_id = ms1$id
        ms1$analysis = NA_character_
      } else {
        rpls = self$get_replicate_names()
        ms1$analysis =  rpls[ms1$analysis]
        ms1$unique_id = paste0(ms1$analysis, "_", ms1$id)
      }

      ms1_list = rcpp_ms_cluster_spectra(ms1, mzClustGroups, verbose)
      ms1_df = rbindlist(ms1_list, fill = TRUE)
      ms1_df = ms1_df[order(ms1_df$mz), ]
      ms1_df = ms1_df[order(ms1_df$id), ]

      if ("groups" %in% groupBy) {
        ms1_df[["analysis"]] = NULL
        setnames(ms1_df, "id", "group")
      } else {
        ms1_df = ms1_df[order(ms1_df$analysis), ]
        setnames(ms1_df, c("analysis", "id"), c("replicate", "group"))
      }

      return(ms1_df)
    },

    #' @description
    #' Method to get an averaged MS2 spectrum for feature groups in analyses.
    #'
    #' @return A data.frame/data.table.
    get_groups_ms2 = function(groups = NULL, mass = NULL,
                              mz = NULL, rt = NULL, ppm = 20, sec = 60,
                              isolationWindow = 1.3,
                              mzClustFeatures = 0.003,
                              minIntensityFeatures = 100,
                              mzClustGroups = 0.003,
                              minIntensityGroups = 100,
                              groupBy = "groups",
                              verbose = TRUE, filtered = FALSE,
                              run_parallel = FALSE) {

      fgs = self$get_groups(groups, mass, mz, rt, ppm, sec, filtered,
                            onlyIntensities = FALSE, average = FALSE)

      if (nrow(fgs) == 0) return(data.frame())

      fts = self$get_features(id = fgs$group)

      if (nrow(fts) == 0) return(data.frame())

      fts$id = fts$group

      ms2 = self$get_ms2(analyses = unique(fts$analysis), mz = fts,
                         isolationWindow = isolationWindow,
                         mzClust = mzClustFeatures,
                         minIntensity = minIntensityFeatures,
                         verbose = verbose,
                         run_parallel = run_parallel)

      ms2 = ms2[ms2$intensity > minIntensityGroups, ]

      if (nrow(ms2) == 0) return(data.frame())

      # cor_list = split(ms2, ms2$id)
      # cor_list = lapply(cor_list, function(x, decimals, method, minIntensity) {
      #   temp = copy(x[, c("analysis", "mz", "intensity")])
      #   temp = temp[temp$intensity >= minIntensity, ]
      #   temp$mz = round(temp$mz, digits = decimals)
      #   temp = temp[
      #     data.table::CJ(analysis = analysis, mz = mz, unique = TRUE),
      #     on = .(analysis, mz)
      #   ]
      #   data.table::setnafill(temp, fill = 0, cols = 'intensity')
      #   temp = temp[, `:=`(intensity = sum(intensity)),
      #               by = c("analysis", "mz")][]
      #   temp = unique(temp)
      #
      #   temp = matrix(temp$intensity,
      #                 nrow = length(unique(temp$mz)),
      #                 ncol = length(unique(temp$analysis)),
      #                 dimnames = list(unique(temp$mz),
      #                                 unique(temp$analysis)))
      #   temp = cor(temp, method = method)
      #   return(temp)
      # }, decimals = 3, minIntensity = 50, method = "pearson")
      # cor_list


      if ("groups" %in% groupBy) {
        ms2$unique_id = ms2$id
        ms2$analysis = NA_character_
      } else {
        rpls = self$get_replicate_names()
        ms2$analysis =  rpls[ms2$analysis]
        ms2$unique_id = paste0(ms2$analysis, "_", ms2$id)
      }

      ms2_list = rcpp_ms_cluster_ms2(ms2, mzClustGroups, verbose)
      ms2_df = rbindlist(ms2_list, fill = TRUE)
      ms2_df = ms2_df[order(ms2_df$mz), ]
      ms2_df = ms2_df[order(ms2_df$id), ]

      if ("groups" %in% groupBy) {
        ms2_df[["analysis"]] = NULL
        setnames(ms2_df, "id", "group")
      } else {
        ms2_df = ms2_df[order(ms2_df$analysis), ]
        setnames(ms2_df, c("analysis", "id"), c("replicate", "group"))
      }

      return(ms2_df)
    },

    ## set -----

    #' @description
    #' Method to set the analysis replicate names. Changes the `R6MS` object.
    #'
    #' @param value A character vector with the analysis replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    set_replicate_names = function(value) {

      if (is.character(value) &
          length(value) == self$get_number_analyses()) {

        private$.analyses = Map(function(x, y) { x$replicate = y; x },
                                private$.analyses, value)

        cat("Replicate names added! \n")

      } else warning("Not done, check the value!")
    },

    #' @description
    #' Method to set the analysis blank replicate names.
    #' Changes the `R6MS` object.
    #'
    #' @param value A character vector with the analysis blank replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    set_blank_names = function(value) {

      if (is.character(value) &
          length(value) == self$get_number_analyses()) {

        if (all(value %in% self$get_replicate_names())) {

          private$.analyses = Map(function(x, y) { x$blank = y; x },
                                  private$.analyses, value)

          cat("Blank names added! \n")

        } else warning("Not done, blank names not among replicate names!")
      } else warning("Not done, check the value!")
    },

    ## add -----

    #' @description
    #' Method to add analyses to the `R6MS` object.
    #'
    #' @param analyses A list of analyses.
    #'
    #' @return Invisible.
    add_analyses = function(analyses) {

      if (missing(analyses)) analyses = NULL

      valid_analyses = vapply(analyses, validate_list_ms_analysis, FALSE)

      valid_analyses

      if (all(valid_analyses) & length(valid_analyses) > 0) {

        old_analyses = private$.analyses
        old_names = vapply(old_analyses, function(x) x$name, "")
        new_names = c(old_names, vapply(analyses, function(x) x$name, ""))

        if (!any(duplicated(c(new_names)))) {

          new_analyses = c(old_analyses, analyses)
          names(new_analyses) = new_names
          new_analyses = new_analyses[order(names(new_analyses))]

          old_size = length(private$.analyses)

          private$.analyses = new_analyses

          if (old_size < length(new_analyses)) cat("New analyses added! \n")
            else warning("Not done, check the conformity of the analyses list!")

        } else  warning("Not done, duplicated analysis names found!")

      } else  warning("Not done, check the conformity of the analyses list!")
    },

    #' @description
    #' Method to add processing settings to the `R6MS` object.
    #'
    #' @param settings X.
    #'
    #' @return Invisible.
    add_settings = function(settings = NULL) {

      if (!"settings" %in% class(settings)) {
        warning("Arguments not correct, settings not added!!")
      } else {
        private$.settings[[settings@call]] = settings
        cat(paste0(settings@call, " processing settings added! \n"))
      }
    },

    #' @description
    #' Method to add features to each analysis in the `R6MS` object.
    #'
    #' @param features X.
    #'
    #' @return Invisible.
    add_features = function(features = NULL) {

      valid = FALSE

      if (is.data.frame(features)) {

        must_have_cols = c("analysis", "id", "mz", "rt", "mzmin", "mzmax",
                           "rtmin", "rtmax", "intensity", "area")

        if (all(must_have_cols %in% colnames(features))) {

          features = features[order(features$analysis),]
          analysis_names = unique(features$analysis)
          org_analysis_names = unname(self$get_analysis_names())

          if (identical(analysis_names, org_analysis_names)) {
            valid = TRUE
          }
        }
      }

      if (valid) {
        features = split(features, features$analysis)
        private$.analyses = Map(function(x, y) { x$features = y; x },
                                private$.analyses, features)
        cat("features added! \n")
      }
    },

    ## load -----

    #' @description
    #' Method to load all spectra from analyses to the `R6MS` object.
    #'
    #' @param run_parallel X.
    #'
    #' @return Invisible.
    load_spectra = function(run_parallel = FALSE) {

      spec = self$get_spectra(
        analyses = NULL, levels = NULL,
        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
        allTraces = TRUE, isolationWindow = 1.3,
        minIntensityMS1 = 0, minIntensityMS2 = 0,
        run_parallel = run_parallel
      )

      split_vector = spec$analysis
      spec$analysis = NULL
      spec_list = split(spec, split_vector)

      if (length(spec_list) == self$get_number_analyses()) {

        private$.analyses = Map(function(x, y) { x$spectra = y; x },
                                private$.analyses, spec_list)

        cat("Spectra loaded to all analyses! \n")

      } else warning("Not done, check the MS file paths and formats!")
    },

    #' @description
    #' Method to load all chromatograms from analyses to the `R6MS` object.
    #'
    #' @param run_parallel X.
    #'
    #' @return Invisible.
    load_chromatograms = function(run_parallel = FALSE) {

      chrom = self$get_chromatograms(analyses = NULL, minIntensity = 0,
        run_parallel = run_parallel
      )

      split_vector = chrom$analysis
      chrom$analysis = NULL
      chrom_list = split(chrom, split_vector)

      if (length(chrom_list) == self$get_number_analyses()) {

        private$.analyses = Map(function(x, y) { x$chromatograms = y; x },
                                private$.analyses, chrom_list)

        cat("Chromatograms loaded to all analyses! \n")

      } else warning("Not done, check the MS file paths and formats!")
    },

    ## processing -----

    #' @description Finds features (i.e., chromatographic peaks) from MS data
    #' in an `R6MS` class object. The function uses the \pkg{patRoon} package
    #' for peak finding, enabling the use of several algorithms (see details).
    #'
    #' @param settings A \linkS4class{settings} object. When not given,
    #' \emph{find_features} \linkS4class{settings} will be searched within
    #' the `R6MS` object.
    #'
    #' @note The \linkS4class{settings} call must be set to "find_features".
    #'
    #' @details See the \link[patRoon]{findFeatures} function from the
    #' \pkg{patRoon} package or the
    #' \href{https://rickhelmus.github.io/patRoon/reference/findFeatures.html}
    #' {reference guide} for more information. The following algorithms are
    #' available via \pkg{patRoon}: "xcms3", "xcms", "openms", "envipick",
    #' "sirius", "kpic2", "safd". The algorithm slot in the
    #' \linkS4class{settings} should be one of the described. The parameters
    #' are given as a list and should match with algorithm requirements.
    #' Certain algorithms also require defined MS file formats and data in
    #' profile mode.
    #'
    #' @return X.
    #'
    #' @seealso \link[patRoon]{findFeatures}
    #'
    #' @references
    #' \insertRef{patroon01}{streamFind}
    #'
    find_features = function(settings = NULL) {

      valid = TRUE

      if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
        warning("Install package patRoon for finding peaks!")
        valid = FALSE
      }

      if (is.null(settings))
        settings = self$get_settings(call = "find_features")

      if (!"settings" %in% class(settings)) {
        warning("find_features settings not found!")
        valid = FALSE
      }

      if (!valid) return()

      algorithm = getAlgorithm(settings)
      parameters = getParameters(settings)

      anaInfo = self$get_overview()
      anaInfo = data.frame(
        "path" = dirname(anaInfo$file),
        "analysis" = anaInfo$analysis,
        "group" = anaInfo$replicate,
        "blank" = anaInfo$blank)

      anaInfo$blank[is.na(anaInfo$blank)] = ""
      anaInfo$algorithm = algorithm
      ag = list(analysisInfo = anaInfo, algorithm = algorithm)
      pp_fun = patRoon::findFeatures
      pat = do.call(pp_fun, c(ag, parameters, verbose = TRUE))

      features = build_features_table_from_patRoon(pat, self)

      self$add_settings(settings)

      private$.analyses = Map(function(x, y) { x$features = y; x },
                              private$.analyses, features)

      cat("Features added to analyses! \n")
    },

    #' @description Groups and aligns features across analyses in the `R6MS`
    #' object. The function uses the \pkg{patRoon} package for grouping
    #' features, enabling the use of several algorithms (see details).
    #'
    #' @param settings A \linkS4class{settings} object. When not given,
    #' \emph{group_features} \linkS4class{settings} will be searched within
    #' the `R6MS` object.
    #'
    #' @note The \linkS4class{settings} call must be set to "group_features".
    #'
    #' @return X.
    #'
    #' @details See the \link[patRoon]{groupFeatures} function from the
    #' \pkg{patRoon} package or the
    #' \href{https://rickhelmus.github.io/patRoon/reference/groupFeatures.html}
    #' {reference guide} for more information. The following algorithms are
    #' possible: "xcms3", "xcms", "openms" or "kpic2". The algorithm slot in the
    #' \linkS4class{settings} should be one of the described. The parameters
    #' are given as a list and should match with algorithm requirements.
    #'
    #' @seealso \code{\link[patRoon]{groupFeatures}}
    #'
    #' @references
    #' \insertRef{patroon01}{streamFind}
    #'
    group_features = function(settings = NULL) {

      valid = TRUE

      if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
        warning("Install package patRoon for finding peaks!")
        valid = FALSE
      }

      if (is.null(settings))
        settings = self$get_settings(call = "group_features")

      if (!"settings" %in% class(settings)) {
        warning("group_features settings not found!")
        valid = FALSE
      }

      pat_features = self$as_features_patRoon()

      if (length(pat_features) == 0) {
        warning("Features were not found! Run find_features method first!")
        valid = FALSE
      }

      if (!valid) return()

      algorithm = getAlgorithm(settings)
      parameters = getParameters(settings)

      if (algorithm == "xcms3") {
        parameters$groupParam@sampleGroups = self$get_replicate_names()
        if ("rtalign" %in% names(parameters)) if (parameters$rtalign) {
          parameters$preGroupParam@sampleGroups = self$get_replicate_names()
        }
      }

      ag = list(obj = pat_features, algorithm = algorithm)
      gr_fun = patRoon::groupFeatures
      pat = do.call(gr_fun, c(ag, parameters))

      features = build_features_table_from_patRoon(pat, self)
      out_list = build_feature_groups_table_from_patRoon(pat, features, self)

      alignment = extract_time_alignment(pat, self)

      self$add_settings(settings)

      private$.analyses = Map(function(x, y) { x$features = y; x },
                              private$.analyses, out_list[["features"]])

      private$.groups = out_list[["fgroups"]]

      cat("Added feature groups from correspondence analysis! \n")

      if (!is.null(alignment)) {
        private$.alignment = alignment
        cat("Added alignment of retention time for each analysis! \n")
      }
    },



    ## has -----

    #' @description
    #' Method to check of the `R6MS` object has analyses.
    #'
    #' @return Invisible.
    has_analyses = function() {
      return(length(private$.analyses) > 0)
    },

    #' @description
    #' Method to check for loaded spectra in given analyses names/indices.
    #'
    #' @param analyses The analyses names/indices to check for loaded spectra.
    #'
    #' @return Invisible.
    has_loaded_spectra = function(analyses = NULL) {

      analyses = self$check_analyses_argument(analyses)
      if (is.null(analyses)) return(FALSE)

      has_spectra = vapply(private$.analyses[analyses],
                           function(x) nrow(x$spectra) > 0, FALSE)

      names(has_spectra) = self$get_analysis_names(analyses)

      return(has_spectra)
    },

    #' @description
    #' Method to check for loaded chromatograms in given analyses names/indices.
    #'
    #' @param analyses The analyses names/indices to check for loaded
    #' chromatograms.
    #'
    #' @return Invisible.
    has_loaded_chromatograms = function(analyses = NULL) {

      analyses = self$check_analyses_argument(analyses)
      if (is.null(analyses)) return(FALSE)

      has_chromatograms = vapply(private$.analyses[analyses],
                                 function(x) nrow(x$chromatograms) > 0, FALSE)

      names(has_chromatograms) = self$get_analysis_names(analyses)

      return(has_chromatograms)
    },

    #' @description
    #' Method to check if there are processing settings in the `R6MS` object.
    #'
    #' @param call A string with the name of function call.
    #'
    #' @return Invisible.
    has_settings = function(call = NULL) {
      if(is.null(call)) return(length(private$.settings) > 0)
      else return(length(private$.settings[[call]]) > 0)
    },

    #' @description
    #' Method to check if given analyses have features.
    #'
    #' @param analyses The analyses names/indices to check for loaded spectra.
    #'
    #' @return Invisible.
    has_features = function(analyses = NULL) {

      analyses = self$check_analyses_argument(analyses)
      if (is.null(analyses)) return(FALSE)

      has_fts = vapply(private$.analyses[analyses],
                           function(x) nrow(x$features) > 0, FALSE)

      names(has_fts) = self$get_analysis_names(analyses)

      return(has_fts)
    },

    #' @description
    #' Method to check if there is alignment of retention time from grouping
    #' features across analyses.
    #'
    #' @return Invisible.
    has_alignment = function() {
      return(!is.null(private$.alignment))
    },

    #' @description
    #' Method to check if there are feature groups from grouping features
    #' across analyses.
    #'
    #' @return Invisible.
    has_feature_groups = function() {
      return(!is.null(private$.groups))
    },

    ## plot -----

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
    #' @param run_parallel X.
    #' @param colorBy X.
    #'
    #' @return A 3D interactive plot.
    plot_spectra = function(analyses = NULL, levels = NULL,
                            mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                            allTraces = FALSE, isolationWindow = 1.3,
                            minIntensityMS1 = 0, minIntensityMS2 = 0,
                            run_parallel = FALSE, colorBy = "analyses") {

      spec = self$get_spectra(analyses, levels, mz, rt, ppm, sec, id,
        allTraces, isolationWindow, minIntensityMS1, minIntensityMS2,
        run_parallel = FALSE)

      if (nrow(spec) == 0) {
        message("Traces not found for the targets!")
        return(NULL)
      }

      if (!"id" %in% colnames(spec)) spec$id = ""

      spec$id = factor(spec$id)
      spec$level = paste("MS", spec$level, sep = "")
      spec$level = factor(spec$level)
      spec$analysis = factor(spec$analysis)

      spec$rtmz = paste(
        spec$id, spec$level,
        spec$mz, spec$rt,
        spec$analysis, sep = "")

      spec_temp = spec
      spec_temp$intensity = 0
      spec = rbind(spec, spec_temp)

      if (colorBy == "levels") {
        spec$var = spec$level
      } else if (colorBy == "targets"){
        spec$var = spec$id
      } else if ("replicates" %in% colorBy) {
        spec$replicate = self$get_replicate_names()[spec$analysis]
        spec$var = spec$replicate
      } else {
        spec$var = spec$analysis
      }

      colors_var = get_colors(unique(spec$var))

      fig = plot_ly(spec, x = ~rt, y = ~mz, z = ~intensity) %>%
        group_by(spec$rtmz) %>%
        add_lines(color = ~var,  colors = colors_var)

      fig = fig %>% layout(scene = list(
        xaxis = list(title = "Retention time / seconds"),
        yaxis = list(title = "<i>m/z</i>"),
        zaxis = list(title = "Intensity / counts")))

      return(fig)
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
    #' @param run_parallel X.
    #' @param legendNames x.
    #' @param plotTargetMark x.
    #' @param targetsMark x.
    #' @param ppmMark x.
    #' @param secMark x.
    #' @param numberRows x.
    #'
    #' @return A data.frame.
    plot_xic = function(analyses = NULL,
                        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                        run_parallel = FALSE, legendNames = NULL,
                        plotTargetMark = TRUE, targetsMark = NULL,
                        ppmMark = 5, secMark = 10, numberRows = 1) {

      xic = self$get_spectra(
        analyses, levels = 1, mz, rt, ppm, sec, id, allTraces = TRUE,
        isolationWindow = 1.3, minIntensityMS1 = 0, minIntensityMS2 = 0,
        run_parallel = run_parallel
      )

      if (nrow(xic) == 0) {
        message("Traces not found for the targets!")
        return(NULL)
      }

      if (!"id" %in% colnames(xic)) xic$id = NA_character_

      ids = unique(xic$id)
      if (is.character(legendNames) & length(legendNames) == length(ids)) {
        names(legendNames) = ids
        xic$id = legendNames[xic$id]
      }

      if (plotTargetMark) {
        plotTargetMark = FALSE
        if (is.data.frame(targetsMark)) {
          if (nrow(targetsMark) == length(ids) &
              "mz" %in% colnames(targetsMark) &
              "rt" %in% colnames(targetsMark)) {

            tgmMZ = as.numeric(targetsMark$mz)
            names(tgmMZ) = ids
            tgmRT = as.numeric(targetsMark$rt)
            names(tgmRT) = ids
            xic$mz_id = tgmMZ[xic$id]
            xic$rt_id = tgmRT[xic$id]

            plotTargetMark = TRUE
          }
        }
      }

      plot = plot_interactive_xic(
        xic,
        plotTargetMark = plotTargetMark,
        ppmMark = ppmMark,
        secMark = secMark,
        numberRows = numberRows
      )

      return(plot)
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
    #' @param run_parallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    plot_eic = function(analyses = NULL,
                        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                        run_parallel = FALSE, legendNames = NULL, title = NULL,
                        colorBy = "targets", interactive = TRUE) {

      eic = self$get_eic(analyses, mz, rt, ppm, sec, id, run_parallel)

      if (nrow(eic) == 0) {
        message("Traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) {
        leg = unique(eic$analysis)
        varkey = eic$analysis
      } else if ("replicates" %in% colorBy) {
        eic$replicate = self$get_replicate_names()[eic$analysis]
        leg = unique(eic$replicate)
        varkey = eic$replicate
      } else if (is.character(legendNames) &
                 length(legendNames) == length(unique(eic$id))) {
        leg = legendNames
        names(leg) = unique(eic$id)
        varkey = leg[eic$id]
      } else {
        leg = unique(eic$id)
        varkey = eic$id
      }

      eic$var = varkey

      if (!interactive) {
        return(plot_static_eic(eic, title))
      } else return(plot_interactive_eic(eic, title, colorBy))
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
    plot_tic = function(analyses = NULL, title = NULL,
                         colorBy = "analyses", interactive = TRUE) {

      tic = self$get_tic(analyses)

      tic$id = "TIC"

      if (nrow(tic) == 0) {
        message("TIC not found for the analyses!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        tic$replicate = self$get_replicate_names()[tic$analysis]
        leg = unique(tic$replicate)
        varkey = tic$replicate
      } else {
        leg = unique(tic$analysis)
        varkey = tic$analysis
      }

      tic$var = varkey

      if (!interactive) return(plot_static_eic(tic, title))
      else return(plot_interactive_eic(tic, title, colorBy))
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
    plot_bpc = function(analyses = NULL, title = NULL,
                         colorBy = "analyses", interactive = TRUE) {

      bpc = self$get_bpc(analyses)

      bpc$id = "BPC"

      if (nrow(bpc) == 0) {
        message("BPC not found for the analyses!")
        return(NULL)
      }

      if ("replicates" %in% colorBy) {
        bpc$replicate = self$get_replicate_names()[bpc$analysis]
        leg = unique(bpc$replicate)
        varkey = bpc$replicate
      } else {
        leg = unique(bpc$analysis)
        varkey = bpc$analysis
      }

      bpc$var = varkey

      if (!interactive) return(plot_static_eic(bpc, title))
      else return(plot_interactive_bpc(bpc, title, colorBy))
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
    #' @param run_parallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    plot_ms2 = function(analyses = NULL,
                        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                        isolationWindow = 1.3, mzClust = 0.005, verbose = TRUE,
                        minIntensity = 0, run_parallel = FALSE,
                        legendNames = NULL, title = NULL,
                        colorBy = "targets", interactive = TRUE) {

      ms2 = self$get_ms2(analyses, mz, rt, ppm, sec, id, isolationWindow,
                         mzClust, verbose, minIntensity, run_parallel)

      if (nrow(ms2) == 0) {
        message("MS2 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) {
        leg = unique(ms2$analysis)
        varkey = ms2$analysis
      } else if ("replicates" %in% colorBy) {
        ms2$replicate = self$get_replicate_names()[ms2$analysis]
        leg = unique(ms2$replicate)
        varkey = ms2$replicate
      } else if (is.character(legendNames) &
                 length(legendNames) == length(unique(ms2$id))) {
        leg = legendNames
        names(leg) = unique(ms2$id)
        varkey = leg[ms2$id]
      } else {
        leg = unique(ms2$id)
        varkey = ms2$id
      }

      ms2$var = varkey

      if (!interactive) return(plot_static_ms2(ms2, title))
      else return(plot_interactive_ms2(ms2, title))
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
    #' @param run_parallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    plot_ms1 = function(analyses = NULL,
                        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                        mzClust = 0.001, verbose = FALSE,
                        minIntensity = 1000, run_parallel = FALSE,
                        legendNames = NULL, title = NULL,
                        colorBy = "targets", interactive = TRUE) {

      ms1 = self$get_ms1(analyses, mz, rt, ppm, sec, id, mzClust,
                         verbose, minIntensity, run_parallel)

      if (nrow(ms1) == 0) {
        message("MS1 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) {
        leg = unique(ms1$analysis)
        varkey = ms1$analysis
      } else if ("replicates" %in% colorBy) {
        ms1$replicate = self$get_replicate_names()[ms1$analysis]
        leg = unique(ms1$replicate)
        varkey = ms1$replicate
      } else if (is.character(legendNames) &
                 length(legendNames) == length(unique(ms1$id))) {
        leg = legendNames
        names(leg) = unique(ms1$id)
        varkey = leg[ms1$id]
      } else {
        leg = unique(ms1$id)
        varkey = ms1$id
      }

      ms1$var = varkey

      if (!interactive) return(plot_static_spectra(ms1, title))
      else return(plot_interactive_spectra(ms1, title))
    },

    #' @description
    #' Method to plot features from analyses.
    #'
    #' @return A data.frame.
    plot_features = function(analyses = NULL, id = NULL, mass = NULL,
                             mz = NULL, rt = NULL, ppm = 20, sec = 60,
                             rtExpand = 120, mzExpand = 0.005,
                             filtered = FALSE, run_parallel = FALSE,
                             legendNames = NULL, title = NULL,
                             colorBy = "targets", interactive = TRUE) {

      fts = self$get_features(analyses, id, mass, mz, rt, ppm, sec, filtered)

      eic = self$get_features_eic(
        analyses = unique(fts$analysis), id = fts,
        rtExpand = rtExpand, mzExpand = mzExpand, run_parallel = run_parallel)

      if (nrow(eic) == 0) {
        message("Traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) {
        leg = unique(eic$analysis)
        varkey = eic$analysis
      } else if ("replicates" %in% colorBy) {
        eic$replicate = self$get_replicate_names()[eic$analysis]
        leg = unique(eic$replicate)
        varkey = eic$replicate
      } else if (is.character(legendNames) &
                 length(legendNames) == length(unique(eic$id))) {
        leg = legendNames
        names(leg) = unique(eic$id)
        varkey = leg[eic$id]
      } else {
        leg = unique(eic$id)
        varkey = eic$id
      }

      eic$var = varkey

      if (!interactive) {
        return(plot_features_static(eic, fts, title))
      } else return(plot_features_interactive(eic, fts, title, colorBy))
    },

    #' @description
    #' Method to map retention time and \emph{m/z} of features from analyses.
    #'
    #' @return A plot.
    map_features = function(analyses = NULL, id = NULL, mass = NULL,
                            mz = NULL, rt = NULL, ppm = 20, sec = 60,
                            filtered = FALSE, xlim = 30, ylim = 0.05,
                            showLegend = TRUE, legendNames = NULL, title = NULL,
                            colorBy = "targets", interactive = TRUE) {

      fts = self$get_features(analyses, id, mass, mz, rt, ppm, sec, filtered)

      if (nrow(fts) == 0) {
        message("Features not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) {
        leg = unique(fts$analysis)
        varkey = fts$analysis
      } else if ("replicates" %in% colorBy) {
        fts$replicate = self$get_replicate_names()[fts$analysis]
        leg = unique(fts$replicate)
        varkey = fts$replicate
      } else if (is.character(legendNames) &
                 length(legendNames) == length(unique(fts$id))) {
        leg = legendNames
        names(leg) = unique(fts$id)
        varkey = leg[fts$id]
      } else {
        leg = unique(fts$id)
        varkey = fts$id
      }

      fts$var = varkey

      if (!interactive) {
        return(map_features_static(fts, xlim, ylim, title, showLegend))
      } else return(map_features_interactive(fts, xlim, ylim, title))
    },

    #' @description
    #' Method to plot MS1 spectra from features in the analyses.
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
    #' @param run_parallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    plot_features_ms1 = function(analyses = NULL, id = NULL, mass = NULL,
                                 mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                 rtWindow = c(-2, 2), mzWindow = c(-5, 100),
                                 mzClust = 0.003, minIntensity = 1000,
                                 verbose = TRUE, filtered = FALSE,
                                 run_parallel = FALSE, legendNames = NULL,
                                 title = NULL, colorBy = "targets",
                                 interactive = TRUE) {

      ms1 = self$get_features_ms1(analyses, id, mass, mz, rt, ppm, sec,
                                  rtWindow, mzWindow, mzClust, minIntensity,
                                  verbose, filtered, run_parallel)

      if (nrow(ms1) == 0) {
        message("MS1 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) {
        leg = unique(ms1$analysis)
        varkey = ms1$analysis
      } else if ("replicates" %in% colorBy) {
        ms1$replicate = self$get_replicate_names()[ms1$analysis]
        leg = unique(ms1$replicate)
        varkey = ms1$replicate
      } else if (is.character(legendNames) &
                 length(legendNames) == length(unique(ms1$id))) {
        leg = legendNames
        names(leg) = unique(ms1$id)
        varkey = leg[ms1$id]
      } else {
        leg = unique(ms1$id)
        varkey = ms1$id
      }

      ms1$var = varkey

      if (!interactive) return(plot_static_spectra(ms1, title))
      else return(plot_interactive_spectra(ms1, title))
    },

    #' @description
    #' Method to plot MS2 spectra from features in the analyses.
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
    #' @param run_parallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    plot_features_ms2 = function(analyses = NULL, id = NULL, mass = NULL,
                                 mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                 isolationWindow = 1.3, mzClust = 0.005,
                                 minIntensity = 0, verbose = TRUE,
                                 filtered = FALSE, run_parallel = FALSE,
                                 legendNames = NULL, title = NULL,
                                 colorBy = "targets", interactive = TRUE) {

      ms2 = self$get_features_ms2(analyses, id, mass, mz, rt, ppm, sec,
                                  isolationWindow, mzClust, minIntensity,
                                  verbose, filtered, run_parallel)

      if (nrow(ms2) == 0) {
        message("MS2 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) {
        leg = unique(ms2$analysis)
        varkey = ms2$analysis
      } else if ("replicates" %in% colorBy) {
        ms2$replicate = self$get_replicate_names()[ms2$analysis]
        leg = unique(ms2$replicate)
        varkey = ms2$replicate
      } else if (is.character(legendNames) &
                 length(legendNames) == length(unique(ms2$id))) {
        leg = legendNames
        names(leg) = unique(ms2$id)
        varkey = leg[ms2$id]
      } else {
        leg = unique(ms2$id)
        varkey = ms2$id
      }

      ms2$var = varkey

      if (!interactive) return(plot_static_ms2(ms2, title))
      else return(plot_interactive_ms2(ms2, title))
    },

    #' @description
    #' Plots the results from the retention time alignment across analyses.
    #'
    #' @return A plot with the retention time alignment differences
    #' for each sample.
    #'
    plot_alignment = function() {

      if (!self$has_alignment()) {
        warning("Adjusted retention time not found!")
        return(NULL)
      }

      alignment = private$.alignment
      colors = get_colors(names(alignment))

      xaxis = list(
        linecolor = toRGB("black"),
        linewidth = 2,
        title = "Retention time / seconds",
        titlefont = list(size = 12, color = "black")
      )
      yaxis = list(
        linecolor = toRGB("black"),
        linewidth = 2,
        title = "RT<sub>Raw</sub> - RT<sub>Adjusted</sub> / seconds",
        titlefont = list(size = 12, color = "black")
      )

      plot = plot_ly()

      for (i in names(alignment)) {

        df = alignment[[i]]

        plot  = plot %>% add_trace(
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

        df_pt = df[!is.na(df$adjPoints), ]

        plot = plot %>% add_trace(
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

      plot = plot %>% layout(
        legend = list(title = list(text = "<b> Analyses: </b>")),
        xaxis = xaxis, yaxis = yaxis)

      return(plot)
    },

    #' @description
    #' Method to plot feature groups EIC.
    #'
    #' @return A plot.
    plot_groups = function(groups = NULL, mass = NULL,
                           mz = NULL, rt = NULL, ppm = 20, sec = 60,
                           rtExpand = 120, mzExpand = 0.005,
                           filtered = FALSE, run_parallel = FALSE,
                           legendNames = NULL, title = NULL,
                           colorBy = "targets", interactive = TRUE) {

      fts = self$get_features(analyses = NULL,
        groups, mass, mz, rt, ppm, sec, filtered)

      if (!is.null(legendNames)) {
        if (is.character(legendNames) &
            length(legendNames) == length(unique(fts$group))) {
          leg = legendNames
          names(leg) = unique(fts$group)
          fts$group = leg[fts$group]
        }
      }

      return(
        self$plot_features(id = fts,
          rtExpand = rtExpand, mzExpand = mzExpand,
          run_parallel = run_parallel, legendNames = fts$group,
          title = title, colorBy = colorBy, interactive = interactive))
    },

    #' @description
    #' Method to plot MS1 spectra from features in the analyses.
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
    #' @param run_parallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    plot_groups_ms1 = function(groups = NULL, mass = NULL,
                               mz = NULL, rt = NULL, ppm = 20, sec = 60,
                               rtWindow = c(-2, 2), mzWindow = c(-5, 90),
                               mzClustFeatures = 0.005,
                               minIntensityFeatures = 1000,
                               mzClustGroups = 0.005,
                               minIntensityGroups = 1000,
                               verbose = TRUE, filtered = FALSE,
                               run_parallel = FALSE, legendNames = NULL,
                               title = NULL, colorBy = "targets",
                               interactive = TRUE) {

      if ("groups" %in% colorBy | "targets" %in% colorBy) {
        groupBy = "groups"
      } else {
        groupBy = "replicates"
      }

      ms1 = self$get_groups_ms1(groups, mass, mz, rt, ppm, sec,
                                rtWindow, mzWindow, mzClustFeatures,
                                minIntensityFeatures, mzClustGroups,
                                minIntensityGroups, verbose,
                                filtered, run_parallel)

      if (nrow(ms1) == 0) {
        message("MS1 traces not found for the targets!")
        return(NULL)
      }

      if ("replicates" %in% colorBy | "analyses" %in% colorBy) {
        leg = unique(ms1$replicate)
        varkey = ms1$replicate
      } else if (is.character(legendNames) &
                 length(legendNames) == length(unique(ms1$group))) {
        leg = legendNames
        names(leg) = unique(ms1$group)
        varkey = leg[ms1$group]
      } else {
        leg = unique(ms1$group)
        varkey = ms1$group
      }

      ms1$var = varkey

      if (!interactive) {
        return(plot_static_spectra(ms1, title))
      } else {
        return(plot_interactive_spectra(ms1, title))
      }
    },

    #' @description
    #' Method to plot MS1 spectra from features in the analyses.
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
    #' @param run_parallel X.
    #' @param legendNames x.
    #' @param title x.
    #' @param colorBy x.
    #' @param interactive x.
    #'
    #' @return A plot.
    plot_groups_ms2 = function(groups = NULL, mass = NULL,
                                       mz = NULL, rt = NULL, ppm = 20, sec = 60,
                                       isolationWindow = 1.3,
                                       mzClustFeatures = 0.003,
                                       minIntensityFeatures = 100,
                                       mzClustGroups = 0.003,
                                       minIntensityGroups = 100,
                                       verbose = TRUE, filtered = FALSE,
                                       run_parallel = FALSE, legendNames = NULL,
                                       title = NULL, colorBy = "targets",
                                       interactive = TRUE) {

      if ("groups" %in% colorBy | "targets" %in% colorBy) {
        groupBy = "groups"
      } else {
        groupBy = "replicates"
      }

      ms2 = self$get_groups_ms2(groups, mass, mz, rt, ppm, sec,
                                isolationWindow, mzClustFeatures,
                                minIntensityFeatures,
                                mzClustGroups, minIntensityGroups,
                                groupBy, verbose, filtered,
                                run_parallel)

      if (nrow(ms2) == 0) {
        message("MS2 traces not found for the targets!")
        return(NULL)
      }

      if ("replicates" %in% colorBy | "analyses" %in% colorBy) {
        leg = unique(ms2$replicate)
        varkey = ms2$replicate
      } else if (is.character(legendNames) &
                 length(legendNames) == length(unique(ms2$group))) {
        leg = legendNames
        names(leg) = unique(ms2$group)
        varkey = leg[ms2$group]
      } else {
        leg = unique(ms2$group)
        varkey = ms2$group
      }

      ms2$var = varkey

      if (!interactive) return(plot_static_ms2(ms2, title))
      else return(plot_interactive_ms2(ms2, title))
    },

    #' @description
    #' Method to give an overview of the EIC, alignment and intensity variance
    #' from features within target feature groups.
    #'
    #' @return A plot.
    plot_groups_overview = function(analyses = NULL, groups = NULL,
                                    mass = NULL, mz = NULL, rt = NULL,
                                    ppm = 20, sec = 60,
                                    rtExpand = 120, mzExpand = 0.005,
                                    filtered = FALSE,
                                    run_parallel = FALSE,
                                    legendNames = NULL, title = NULL,
                                    heights = c(0.35, 0.5, 0.15)) {

      fgs = self$get_groups(groups, mass, mz, rt, ppm, sec, filtered,
                            onlyIntensities = FALSE, average = FALSE)

      fts = self$get_features(analyses = analyses, id = fgs$group)

      eic = self$get_features_eic(analyses = fts$analysis, id = fts,
                                  rtExpand = rtExpand, mzExpand = mzExpand,
                                  filtered = TRUE, run_parallel = run_parallel)

      if (nrow(eic) == 0) {
        message("Traces and/or features not found for targets!")
        return(NULL)
      }

      if (!is.null(legendNames)) {
        if (is.character(legendNames) &
            length(legendNames) == length(unique(fgs$group))) {
          leg = legendNames
          names(leg) = unique(fts$group)
          leg = leg[fts$group]
        }
      } else leg = fts$group

      names(leg) = paste0(fts$id, "_", fts$analysis)
      eic$uid = paste0(eic$id, "_", eic$analysis)
      fts$uid = paste0(fts$id, "_", fts$analysis)
      eic$var = leg[eic$uid]
      fts$var = leg

      analyses = self$check_analyses_argument(analyses)

      return(plot_groups_overview_aux(fts, eic, heights, analyses))
    },

    ## as -----

    #' @description
    #' Creates a \linkS4class{features} object from the \pkg{patRoon} package
    #' with the features in the analyses.
    #'
    #' @return A \linkS4class{features} object.
    #'
    as_features_patRoon = function() {

      requireNamespace("patRoon")

      anaInfo = self$get_overview()
      anaInfo = data.frame(
        "path" = dirname(anaInfo$file),
        "analysis" = anaInfo$analysis,
        "group" = anaInfo$replicate,
        "blank" = anaInfo$blank)
      anaInfo$blank[is.na(anaInfo$blank)] = ""

      polarities = self$get_polarities()
      if (length(unique(polarities)) > 1) anaInfo$set = polarities

      anaInfo$file = self$get_file_paths()
      rownames(anaInfo) = seq_len(nrow(anaInfo))

      features = lapply(self$get_analyses(), function(x) {
        ft = copy(x$features)
        if ("filtered" %in% colnames(ft)) ft = ft[!ft$filtered, ]

        if (nrow(ft) == 0) return(ft)

        setnames(ft, c("id", "rt", "rtmin", "rtmax"),
                     c("ID", "ret", "retmin", "retmax"), skip_absent = TRUE)

        ft = select(ft, ID,
          mz, mzmin, mzmax, ret, retmin, retmax, intensity, area, everything())

        ft$ID = as.numeric(gsub(".*_f", "", ft$ID))

        return(ft)
      })

      if (length(unique(polarities)) > 1) {
        features = lapply(features, function(x) {
          if (nrow(x) == 0) return(x)
          x$mzmin = x$mass - (x$mz - x$mzmin)
          x$mzmax = x$mass + (x$mzmax - x$mz)
          x$mz = x$mass
          x$mass = NULL
          return(x)})
        features_obj = new("featuresSet",
          features = features, analysisInfo = anaInfo,
          algorithm = "openms-set")
      } else {
        features_obj = new("featuresOpenMS",
          features = features, analysisInfo = anaInfo)
      }

      return(features_obj)
    },

    ## subset -----

    #' @description
    #' Creates a new MS set of analyses.
    #' @param i Name.
    #' @param ... Hair color.
    subset_analyses = function(i, ...) {

      i = self$check_analyses_argument(i)

      new_analyses = private$.analyses[i]

      new_groups = private$.groups
      fgs_remaining = lapply(new_analyses, function(x) x$features$group)
      fgs_remaining = unique(unlist(fgs_remaining))
      if (!is.null(fgs_remaining)) {
        new_groups = new_groups[new_groups$group %in% fgs_remaining, ]
      }

      new_alignment = private$.alignment[i]

      return(
        R6MS$new(
          files = NULL,
          .header = private$.header,
          .settings = private$.settings,
          .analyses = new_analyses,
          .groups = new_groups,
          .alignment = new_alignment
        )
      )
    },

    ## checks -----

    #' @description
    #' Check the analyses argument as a character/integer vector to match
    #' analyses names or indices from the `R6MS` object.
    #'
    #' @param analyses X.
    #'
    #' @return A valid character vector with analyses names of `NULL`.
    check_analyses_argument = function(analyses) {

      if (is.null(analyses)) {
        return(self$get_analysis_names())
      } else {
        analyses = self$get_analysis_names(analyses)
        if (!all(analyses %in% self$get_analysis_names())) {
          warning("Defined analyses not found!")
          return(NULL)
        } else {
          return(analyses)
        }
      }
    },

    ## export -----



    ## info -----

    #' @description
    #' Possible processing function calls.
    #'
    #' @return A character vector with ordered possible function calls for data
    #' pre and post-processing.
    processing_function_calls = function() {

      return(c(
        "findFeatures",
        "annotateFeatures",
        "groupFeatures",
        "fillFeatures",
        "filterFeatures"
      ))
    }


  )
)

# auxiliary functions -----

#' validate_info_list_ms_analysis
#'
#' @description
#' Validates the list of information parsed from an MS analysis mzML/mzXML file.
#'
#' @param value A list of information from an MS analysis mzML/mzXML file.
#'
#' @return A logical value of length 1.
#'
#' @export
#'
validate_list_ms_analysis = function(value) {

  valid = FALSE

  if (is.list(value)) {

    valid = TRUE

    if (!is.character(value$name) & length(value$name) != 1) valid = FALSE

    if (!is.character(value$replicate) & length(value$replicate) != 1)
      valid = FALSE

    if (!is.character(value$blank) & length(value$blank) != 1) valid = FALSE

    if (!is.character(value$file) & length(value$file) != 1) valid = FALSE
      else if (!file.exists(value$file))
        warning(paste0(value$file,
          " does not exist! Update file paths with R6MS$update_files() method"))

    if (length(value$type) != 1) valid = FALSE
      else if (!(value$type %in% c("MS", "MS/MS", "SMR"))) valid = FALSE

    if (!is.integer(value$spectra_number) &&
      length(value$spectra_number) != 1)
        valid = FALSE

    if (!is.integer(value$chromatograms_number) &&
      length(value$chromatograms_number) != 1)
        valid = FALSE

    if (!is.character(value$spectra_mode) &
      length(value$spectra_mode) != 1)
        valid = FALSE

    if (!is.integer(value$spectra_levels)) valid = FALSE

    if (!is.numeric(value$mz_low) & length(value$mz_low) != 1) valid = FALSE

    if (!is.numeric(value$mz_high) & length(value$mz_high) != 1) valid = FALSE

    if (!is.numeric(value$rt_start) & length(value$rt_end) != 1) valid = FALSE

    if (!is.character(value$polarity)) valid = FALSE
      else if (FALSE %in%
        (value$polarity %in% c("positive", "negative", NA_character_)))
          valid = FALSE

    if (!is.logical(value$ion_mobility) & length(value$ion_mobility) != 1)
      valid = FALSE

    if(!is.data.frame(value$tic)) valid = FALSE
      else if (FALSE %in% (c("rt", "intensity") %in% colnames(value$tic)))
        valid = FALSE

    if(!is.data.frame(value$bpc)) valid = FALSE
      else if (FALSE %in% (c("rt", "mz", "intensity") %in% colnames(value$bpc)))
        valid = FALSE

    if(!is.data.frame(value$spectra)) valid = FALSE

    if(!is.data.frame(value$chromatograms)) valid = FALSE

    if(!is.data.frame(value$features)) valid = FALSE

    if(!is.list(value$metadata)) valid = FALSE

  }

  return(valid)
}

#' @title make_targets
#'
#' @description Helper function to build \emph{m/z} and retention time
#' target pairs for searching data. Each target is composed of an
#' id and \emph{m/z} (Da) and time (seconds) ranges. When mass is defined
#' without time, the time range return 0 and vice versa.
#'
#' @param mz A vector with target \emph{m/z} values or a two columns
#' \linkS4class{data.table} or data.frame with minimum and maximum
#' \emph{m/z} values. Alternatively, \emph{m/z} and retention time values
#' can be given as one \linkS4class{data.table}/data.frame and the deviations
#' given as \code{ppm} and \code{sec} are used to calculate the ranges.
#' The same also works for min and max values of \emph{m/z} and retention
#' time targets. Note that when mass/time ranges are given, \code{ppm} and
#' \code{sec} are not used.
#' @param rt A vector with target retention time values or
#' a two columns \linkS4class{data.table}/data.frame with minimum
#' and maximum retention time values.
#' @param ppm A numeric vector of length one with the mass deviation, in ppm.
#' @param sec A numeric vector of length one with the time deviation, in seconds.
#' @param id An id vector with target identifiers. When not given is built
#' as a combination of the \emph{m/z} and retention time ranges or values.
#'
#' @return A data.frame with columns: id, mz, rt, mzmin, mzmax, rtmin, rtmax.
#'
#' @export
#'
make_targets = function(mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL) {

  mzrts = data.table(
    id = NA_character_,
    mz = 0,
    rt = 0,
    mzmin = 0,
    mzmax = 0,
    rtmin = 0,
    rtmax = 0
  )

  # when only rt is given
  if (is.null(mz) & !is.null(rt)) {

    # as vector
    if (length(rt) >= 1 & is.vector(rt)) {

      mzrts = data.table(
        id = NA_character_,
        mz = rt,
        rt = 0,
        mzmin = 0,
        mzmax = 0,
        rtmin = 0,
        rtmax = 0
      )
      mzrts[, rtmin := rt - sec]
      mzrts[, rtmax := rt + sec]

      #adds id
      if (!is.null(id) & length(id) == length(rt)) {
        mzrts[, id := id][]
      } else {
        mzrts[, id := paste(rtmin, "-", rtmax, sep = "")][]
      }

      # as table
    } else if (is.data.frame(rt) | is.data.table(rt)) {
      rt = as.data.table(rt)

      if ("rt" %in% colnames(rt) & !"rtmin" %in% colnames(mz)) {
        mzrts = data.table(
          id = NA_character_,
          mz = 0,
          rt = rt,
          mzmin = 0,
          mzmax = 0,
          rtmin = 0,
          rtmax = 0
        )
        mzrts$rtmin = rt$rt - sec
        mzrts$rtmax = rt$rt + sec

      } else if ("rtmin" %in% colnames(rt)) {
        mzrts = data.table(
          id = NA_character_,
          mz = 0,
          rt = apply(rt[, .(rtmin, rtmax)], 1, mean),
          mzmin = 0,
          mzmax = 0,
          rtmin = rt$rtmin,
          rtmax = rt$rtmax
        )

        if ("rt" %in% colnames(rt)) {
          mzrts$rt = mz$rt
        } else {
          mzrts$rt =  apply(rt[, .(rtmin, rtmax)], 1, mean)
        }
      }

      #adds id
      if (length(id) == nrow(mzrts) & !is.null(id)) {
        mzrts$id = id
      } else if ("id" %in% colnames(rt)) {
        mzrts$id = rt$id
      } else {
        mzrts$id = paste(mzrts$rtmin, "-", mzrts$rtmax, sep = "")
      }

      if ("analysis" %in% colnames(rt)) mzrts$analysis = rt$analysis
    }

    #when mz is vector, expects rt as vector as well and ranges are calculated
  } else if (length(mz) >= 1 & is.vector(mz)) {

    mzrts = data.table(
      id = NA_character_,
      mz = mz,
      rt = 0,
      mzmin = mz - ((ppm / 1E6) * mz),
      mzmax = mz + ((ppm / 1E6) * mz),
      rtmin = 0,
      rtmax = 0
    )
    #mzrts$mzmin = mz - ((ppm / 1E6) * mz)
    #mzrts[, mzmax := mz + ((ppm / 1E6) * mz)]

    if (is.vector(rt) & length(rt) == length(mz)) {
      mzrts$rt = rt
      mzrts$rtmin = c(rt - sec)
      mzrts$rtmax = c(rt + sec)
    }

    if (!is.null(id) & length(id) == nrow(mzrts)) {
      mzrts$id = id
    } else {
      mzrts[, id := paste(
        round(mzmin, digits = 4),
        "-",
        round(mzmax, digits = 4),
        "/", rtmin,
        "-", rtmax,
        sep = ""
      )][]
    }

    #when mz is a table, ranges could be already in table
  } else if (is.data.frame(mz) | is.data.table(mz)) {
    mz = as.data.table(mz)

    #when mz is in table but not ranges
    if ("mz" %in% colnames(mz) & !"mzmin" %in% colnames(mz)) {
      mzrts = data.table(
        id = NA_character_,
        mz = mz$mz,
        rt = 0,
        mzmin = 0,
        mzmax = 0,
        rtmin = 0,
        rtmax = 0
      )
      mzrts[, mzmin := mz - ((ppm / 1E6) * mz)]
      mzrts[, mzmax := mz + ((ppm / 1E6) * mz)]

      #when mzmin is in table
    } else if ("mzmin" %in% colnames(mz)) {
      mzrts = data.table(
        id = NA_character_,
        mz = apply(mz[, .(mzmin, mzmax)], 1, mean),
        rt = 0,
        mzmin = mz$mzmin,
        mzmax = mz$mzmax,
        rtmin = 0,
        rtmax = 0
      )
      if ("mz" %in% colnames(mz)) mzrts$mz = mz$mz
    }

    #when rt in also in mz table
    if ("rt" %in% colnames(mz) & !"rtmin" %in% colnames(mz)) {
      mzrts$rt = mz$rt
      mzrts$rtmin = mz$rt - sec
      mzrts$rtmax = mz$rt + sec
    } else if ("rtmin" %in% colnames(mz)) {
      mzrts$rt =  apply(mz[, .(rtmin, rtmax)], 1, mean)
      mzrts$rtmin = mz$rtmin
      mzrts$rtmax = mz$rtmax
      if ("rt" %in% colnames(mz)) mzrts$rt = mz$rt
    }

    #when rt is given as a table is rt argument
    if (is.data.frame(rt) | is.data.table(rt)) {
      rt = as.data.table(rt)

      if ("rt" %in% colnames(rt) &
                    nrow(rt) == nrow(mz) &
                            !"rtmin" %in% colnames(mz)) {
        mzrts$rt = rt$rt
        mzrts$rtmin = rt$rt - sec
        mzrts$rtmax = rt$rt + sec
      } else if ("rtmin" %in% colnames(rt) & nrow(rt) == nrow(mz)) {
        mzrts$rt =  apply(rt[, .(rtmin, rtmax)], 1, mean)
        mzrts$rtmin = rt$rtmin
        mzrts$rtmax = rt$rtmax
        if ("rt" %in% colnames(rt)) mzrts$rt = mz$rt
      }
    }

    #adds id
    if (!is.null(id) & length(id) == nrow(mzrts)) {
      mzrts$id = id
    } else if ("id" %in% colnames(mz)) {
      mzrts$id = mz$id
    } else {
      mzrts[, id := paste(
        round(mzmin, digits = 4),
        "-",
        round(mzmax, digits = 4),
        "/",
        rtmin,
        "-",
        rtmax,
        sep = ""
      )][]
    }

    if ("analysis" %in% colnames(mz)) mzrts$analysis = mz$analysis
  }

  return(mzrts)
}

#' @title get_colors
#'
#' @description Function to produce colors for a character vector.
#'
#' @param obj A character vector to associate with the colors.
#'
#' @return A vector of colors. The vector is named according the \code{obj}.
#'
#' @export
#'
get_colors = function(obj) {

  colors = c(brewer.pal(8, "Greys")[6],
              brewer.pal(8, "Greens")[6],
              brewer.pal(8, "Blues")[6],
              brewer.pal(8, "Oranges")[6],
              brewer.pal(8, "Purples")[6],
              brewer.pal(8, "PuRd")[6],
              brewer.pal(8, "YlOrRd")[6],
              brewer.pal(8, "PuBuGn")[6],
              brewer.pal(8, "GnBu")[6],
              brewer.pal(8, "BuPu")[6],
              brewer.pal(8, "Dark2"))

  Ncol = length(unique(obj))

  if (Ncol > 18) {
    colors = colorRampPalette(colors)(Ncol)
  }

  if (length(unique(obj)) < length(obj)) {
    Vcol = colors[seq_len(Ncol)]
    Ncol = length(obj)
    count = dplyr::count(data.frame(n = seq_len(Ncol), char = obj), char)
    Vcol = rep(Vcol, times = count[, "n"])
    names(Vcol) = obj
  } else {
    Vcol = colors[seq_len(Ncol)]
    names(Vcol) = obj
  }

  return(Vcol)
}

#' @title unregister_dopar
#'
#' @description Function to un-register any `foreach` parallel variables.
#'
#' @export
#'
unregister_dopar = function() {
  env = foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

#' @title correlate_analysis_spectra
#'
#' @description Function to correlate MS spectra from analyses.
#'
#' @param spectra A data.table with columns "analysis", "mz" and "intensity".
#' Optionally, a column named "id" or "group" can be given to split the
#' data.table before correlation analysis by setting the argument
#' \code{splitSpectra} to \code{TRUE}. Note that when both "id" and "group"
#' columns are present "group" is used for splitting the data.table not "id".
#' If a column "replicate" is present and the argument \code{byReplicates}
#' is set to \code{TRUE}, the correlation is performed by replicate analysis
#' groups.
#' @param splitSpectra X.
#' @param byReplicates X.
#' @param decimals X.
#' @param minIntensity X.
#' @param method X.
#'
#' @return X.
#'
#' @export
#'
correlate_analysis_spectra = function(spectra,
                                      splitSpectra = FALSE,
                                      byReplicates = FALSE,
                                      decimals = 2,
                                      minIntensity = 1000,
                                      method = "pearson") {

  if (!is.data.table(spectra)) {
    warning("Spectra must be a data.table!")
    return(data.table())
  }

  if ("replicate" %in% colnames(spectra) & byReplicates) {
    spectra$analysis = spectra$replicate
  } else byReplicates = FALSE

  if (!"id" %in% colnames(spectra)) spectra$id = NA_character_

  if ("group" %in% colnames(spectra)) spectra$id = spectra$group

  if (!all(c("id", "analysis", "mz", "intensity") %in% colnames(spectra))) {
    warning("Spectra data.table does not containg mandatory columns!")
    return(data.table())
  }

  if (splitSpectra) {
    cor_list = split(spectra, spectra$id)
  } else {
    cor_list = list(spectra)
  }

  cor_list = lapply(cor_list, function(x, minIntensity, decimals, method) {

    temp = copy(x[, c("analysis", "mz", "intensity")])

    temp = temp[temp$intensity >= minIntensity, ]

    for (i in unique(temp$analysis)) {
      temp$intensity[temp$analysis %in% i] =
        temp$intensity[temp$analysis %in% i] /
        max(temp$intensity[temp$analysis %in% i])
    }

    temp$mz = round(temp$mz, digits = decimals)

    temp = temp[
      data.table::CJ(analysis = analysis, mz = mz, unique = TRUE),
      on = .(analysis, mz)
    ]

    data.table::setnafill(temp, fill = 0, cols = 'intensity')

    temp = temp[, `:=`(intensity = sum(intensity)),
                by = c("analysis", "mz")][]

    temp = unique(temp)

    temp = matrix(temp$intensity,
                  nrow = length(unique(temp$mz)),
                  ncol = length(unique(temp$analysis)),
                  dimnames = list(unique(temp$mz),
                                  unique(temp$analysis)))

    temp = cor(temp, method = method)

    temp = as.data.table(temp,keep.rownames = "analysis")

    return(temp)

  }, decimals = decimals, minIntensity = minIntensity, method = method)

  id_col = "id"

  if ("group" %in% colnames(spectra)) id_col = "group"

  cor_list = rbindlist(cor_list, idcol = "id")

  if (byReplicates) {
    setnames(cor_list, "analysis", "replicate")
  }

  return(cor_list)
}

export_R6MS = function(ms, name = "ms", format = "json", path = getwd()) {

  # list
  js_header <- toJSON(ms$get_header(),
    force = TRUE, auto_unbox = TRUE, pretty = TRUE)

  # list
  js_settings = toJSON(ms$get_settings(),
    force = TRUE, auto_unbox = TRUE, pretty = TRUE)

  # list
  # js_analyses = toJSON(
  #   ms$get_analyses(),
  #   dataframe = "columns",
  #   # matrix = c("rowmajor", "columnmajor"), # No matrices yet
  #   Date = "ISO8601",
  #   POSIXt = "ISO8601",
  #   factor = "string",
  #   complex = "string",
  #   # raw = "js",
  #   null = "null",
  #   na = "null",
  #   auto_unbox = FALSE,
  #   digits = 4,
  #   pretty = TRUE,
  #   force = TRUE
  # )

  # write(js_analyses, file = paste0(getwd(), "/js_analyses.json"))

  # data.table
  js_groups = ms$get_groups()

  # to list
  js_groups = split(js_groups, js_groups$group)

  # as.list data.table
  js_groups = lapply(js_groups, as.list)

  # js_feature_groups = toJSON(
  #   js_feature_groups,
  #   dataframe = "columns",
  #   null = "null",
  #   na = "null",
  #   auto_unbox = FALSE,
  #   digits = 4,
  #   pretty = TRUE,
  #   force = TRUE
  # )

  # write(js_feature_groups, file = paste0(getwd(), "/js_feature_groups.json"))

  # data.table
  # js_alignment = toJSON(
  #   ms$get_alignment(),
  #   dataframe = "columns",
  #   null = "null",
  #   na = "null",
  #   auto_unbox = FALSE,
  #   digits = 4,
  #   pretty = TRUE,
  #   force = TRUE
  # )

  js_all = list(
    "header" = ms$get_header(),
    "settings" = ms$get_settings(),
    "analyses" = ms$get_analyses(),
    "groups" = js_groups,
    "alignment" = ms$get_alignment()
  )

  js_all = toJSON(
    js_all,
    dataframe = "columns",
    # matrix = c("rowmajor", "columnmajor"), # No matrices
    Date = "ISO8601",
    POSIXt = "ISO8601",
    factor = "string",
    complex = "string",
    # raw = c("base64", "hex", "mongo", "int", "js"), # No raw objects
    null = "null",
    na = "null",
    auto_unbox = FALSE,
    digits = 4,
    pretty = TRUE,
    force = TRUE
  )

  write(js_all, file = paste0(path, "/" ,name, ".", "json")) #format
}


## not-exported functions -----

#' @title build_features_table_from_patRoon
#'
#' @param pat A \linkS4class{features} or \linkS4class{featureGroups} object
#' from the package \pkg{patRoon}.
#' @param self An `R6MS` object. When applied within the R6, the self object.
#'
#' @return A list of with a features \linkS4class{data.table} for each analysis.
#'
build_features_table_from_patRoon = function(pat, self) {

  cat("Building features table for each analysis... ")

  if ("features" %in% is(pat)) {
    anaInfo = pat@analysisInfo
    isSet = TRUE %in% grepl("Set", is(pat))
    features = pat@features
    if ("featuresXCMS3" %in% is(pat)) {
      if (xcms::hasFilledChromPeaks(pat@xdata)) {
        extra = xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE)
        extra$is_filled = as.logical(extra$is_filled)
        extra$analysis = anaInfo$analysis[extra$sample]
        extra = split(extra, extra$analysis)
      } else { extra = NULL }
    } else { extra = NULL }
  }

  if ("featureGroups" %in% is(pat)) {
    anaInfo = pat@analysisInfo
    features = copy(pat@features@features)
    isSet = TRUE %in% grepl("Set", is(pat))
    if ("featureGroupsXCMS3" %in% is(pat)) {
      if (xcms::hasFilledChromPeaks(pat@xdata)) {
        extra = xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE)
        extra$is_filled = as.logical(extra$is_filled)
        extra$analysis = anaInfo$analysis[extra$sample]
        extra = split(extra, extra$analysis)
      } else { extra = NULL }
    } else { extra = NULL }
  }

  features = lapply(names(features), function(x, extra, features, self, isSet) {

    temp = features[[x]]

    if (!is.null(extra)) {
      if (temp == nrow(extra[[x]]) & all(temp$mz == extra[[x]]$mz)) {
        temp$is_filled = extra[[x]]$is_filled
      }
    }

    polarity = self$get_polarities(x)

    if (polarity %in% "positive") {
      adduct = "[M+H]+"
      adduct_val = -1.0073
    }

    if (polarity %in% "negative") {
      adduct = "[M-H]-"
      adduct_val = 1.0073
    }

    if (isSet) {
      temp[adduct %in% "[M-H]-", `:=`(
        mzmin = (mz - 1.0073) - (mz - mzmin),
        mzmax = (mz - 1.0073) + (mzmax - mz),
        mz = mz - 1.0073
      )]
      temp[adduct %in% "[M+H]+", `:=`(
        mzmin = (mz + 1.0073) - (mz - mzmin),
        mzmax = (mz + 1.0073) + (mzmax - mz),
        mz = mz + 1.0073
      )]
    }

    if (!"adduct" %in% colnames(temp)) temp$adduct = adduct
    if (!"mass" %in% colnames(temp)) temp$mass = temp$mz + adduct_val
    if (!"is_filled" %in% colnames(temp)) { temp$is_filled = FALSE
    } else temp$is_filled = as.logical(temp$is_filled)
    if (!"filtered" %in% colnames(temp)) temp$filtered = FALSE
    if (!"filter" %in% colnames(temp)) temp$filter = NA_character_

    setnames(temp, c("ID", "ret", "retmin", "retmax"),
             c("id", "rt", "rtmin", "rtmax"), skip_absent = TRUE)

    temp_org = self$get_features(x)
    if (nrow(temp_org) > 0) {
      temp_org$analysis = NULL
      if (nrow(temp_org) != nrow(temp)) {
        temp_org_not_grouped = temp_org[!temp_org$index %in% temp$id,  ]
        temp_list = list(temp, temp_org_not_grouped)
        temp = rbindlist(temp_list, fill = TRUE)
      }
    }

    if ("group" %in% colnames(temp)) {
      temp$filter[is.na(temp$group)] = "grouping"
      temp$filtered[is.na(temp$group)] = TRUE
    }

    dppm = round((temp$mzmax - temp$mzmin) / temp$mzmin * 1E6, digits = 0)
    drt = round(temp$rtmax - temp$rtmin, digits = 0)

    temp = temp[order(temp$mz), ]
    temp = temp[order(temp$rt), ]
    temp = temp[order(temp$filtered), ]
    temp$index = seq_len(nrow(temp))

    temp = select(
      temp,
      id,
      index,
      mz, rt,
      intensity, area,
      mzmin, mzmax,
      rtmin, rtmax,
      adduct, mass,
      is_filled,
      filtered,
      filter,
      everything()
    )

    temp$id = paste0(
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

    return(temp)
  }, extra = extra, features = features, self = self, isSet = isSet)

  names(features) = self$get_analysis_names()

  cat("Done! \n")

  return(features)
}

#' build_feature_groups_table_from_patRoon
#'
#' @param pat A \linkS4class{featureGroups} object from the
#' package \pkg{patRoon}.
#' @param self An `R6MS` object. When applied within the R6, the self object.
#'
#' @return A \linkS4class{data.table} with the feature groups.
#'
build_feature_groups_table_from_patRoon = function(pat, features, self) {

  cat("Building table with features... ")

  fgroups = patRoon::as.data.table(pat, average = FALSE)
  setnames(fgroups, "ret", "rt")

  fts = features
  fts = rbindlist(fts, idcol = "analysis")

  fts_ana_index = setNames(data.frame(
    matrix(ncol = length(features),
    nrow = 1)), names(features))
  fts_ana_index[1, ] = 0

  index = lapply(fgroups$group, function(g, fts) {
    return(which(fts$group == g))
  }, fts = fts)

  fgroups$dppm = vapply(index, function(x) {
    return(
      round(max((fts$mzmax[x] - fts$mzmin[x]) / fts$mz[x] * 1E6), digits = 1))
  }, 0)

  fgroups$drt = vapply(index, function(x) {
    return(round(max(fts$rtmax[x] - fts$rtmin[x]), digits = 0))}, 0)

  fgroups$features = lapply(index, function(x, fts, fts_ana_index) {
    temp = copy(fts_ana_index)
    temp[1, colnames(temp) %in% fts$analysis[x]] = fts$index[x]
    return(temp)
  }, fts = fts, fts_ana_index = fts_ana_index)

  fgroups$index = as.numeric(sub(".*_", "", fgroups$group))

  if ("is_filled" %in% colnames(fts)) {
    fgroups$hasFilled = vapply(index,
      function(x) { TRUE %in% fts$is_filled[x] } , FALSE)
  } else fgroups$hasFilled = FALSE

  if (!"filtered" %in% colnames(fgroups)) {
    fgroups$filtered = FALSE
    fgroups$filter = NA_character_
  }

  if (TRUE %in% grepl("Set", is(pat))) {
    isSet = TRUE
    annot = pat@annotations
    setnames(fgroups, "mz", "mass", skip_absent = TRUE)
    fgroups$neutralMass = NULL

    new_id = paste0(
      "m",
      round(fgroups$mass, digits = 3),
      "_d",
      fgroups$dppm,
      "_rt",
      round(fgroups$rt, digits = 0),
      "_t",
      fgroups$drt,
      "_g",
      fgroups$index
    )

  } else {

    adduct = unique(fts$adduct)
    fgroups$adduct = adduct
    if (adduct %in% "[M+H]+")fgroups$mass = fgroups$mz - 1.0073
    if (adduct %in% "[M-H]-") fgroups$mass = fgroups$mz + 1.0073

    new_id = paste0(
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

  names(new_id) = fgroups$group
  fgroups$group = new_id

  features_new_id = lapply(features, function(x, new_id) {
    x$group = new_id[x$group]
    return(x)
  }, new_id = new_id)

  cat("Done! \n")
  return(list("features" = features_new_id, "fgroups" = fgroups))
}

#' extract_time_alignment
#'
#' @description Function to extract adjusted retention time information from
#' alignment results when using `xcms3` as algorithm for grouping and retention
#' time alignment.
#'
#' @param pat A \linkS4class{featureGroups} object from \pkg{patRoon}.
#' @param self An `R6MS` object. When applied within the R6, the self object.
#'
extract_time_alignment = function(pat, self) {

  if ("featureGroupsXCMS3" %in% is(pat)) {

    if (xcms::hasAdjustedRtime(pat@xdata)) {

      cat("Gettings adjusted retention time values... ")

      rtAdj = xcms::adjustedRtime(pat@xdata)
      pkAdj = xcms::processHistory(pat@xdata,
        type = "Retention time correction")[[1]]
      pkAdj = pkAdj@param

      addAdjPoints = FALSE
      if ("PeakGroupsParam" %in% is(pkAdj)) {
        addAdjPoints = TRUE
        pkAdj = xcms::peakGroupsMatrix(pkAdj)
      }

      # hasSpectra = all(self$has_loaded_spectra())
      hasSpectra = FALSE

      if (!hasSpectra) {
        rtOrg = lapply(self$get_file_paths(), function(x) {
          file_link = mzR::openMSfile(x, backend = "pwiz")
          sH = suppressWarnings(mzR::header(file_link))
          suppressWarnings(mzR::close(file_link))
          return(sH$retentionTime)
        })
      }

      alignment = lapply(self$get_analysis_names(),
        function(ana, rtOrg, rtAdj, addAdjPoints, pkAdj, all_ana) {

          ana_idx = which(all_ana %in% ana)
          n_ana = length(all_ana)

          rts = names(rtAdj)
          ana_idx_string = paste0(
            "F",
            paste(rep("0", nchar(n_ana) - nchar(ana_idx)), collapse = ""),
            ana_idx
          )
          rts = grepl(ana_idx_string, rts)
          rts = rtAdj[rts]

          temp = data.frame(
            "rt_original" = rtOrg[[ana]],
            "rt_adjusted" = rts)

          temp$adjustment = temp$rt_original - temp$rt_adjusted

          if (addAdjPoints) {
            adjPoints = unique(pkAdj[, ana_idx])
            adjPoints = adjPoints[adjPoints %in% temp$rt_original]
            temp$adjPoints[temp$rt_original %in% adjPoints] = adjPoints
          }

          row.names(temp) = seq_len(nrow(temp))

          return(temp)
        },
        rtOrg = rtOrg,
        rtAdj = rtAdj,
        addAdjPoints = addAdjPoints,
        pkAdj = pkAdj,
        all_ana = self$get_analysis_names())

      cat("Done! \n")

      return(alignment)
    }
  }

  return(NULL)
}

#' plot_groups_overview_aux
#'
#' @description Plots features for each feature group.
#'
#' @param heights A numeric vector of length two to control the height of
#' the first and second plot, respectively.
#'
plot_groups_overview_aux <- function(fts, eic, heights, analyses) {

  leg = unique(eic$var)
  colors = get_colors(leg)
  showleg = rep(TRUE, length(leg))
  names(showleg) = names(leg)

  plot = plot_ly()

  for (g in leg) {

    uid = unique(eic$uid[eic$var == g])

    for (u in uid) {

      df = eic[eic$uid == u, ]
      ft = fts[fts$uid == u, ]

      plot = plot %>% add_trace(df,
        x = df$rt,
        y = df$intensity,
        type = "scatter", mode = "lines",
        line = list(width = 0.5, color = colors[g]),
        connectgaps = TRUE,
        name = g,
        legendgroup = g,
        showlegend = FALSE
      )

      df = df[rt >= ft$rtmin & rt <= ft$rtmax, ]
      df$mz = as.numeric(df$mz)

      plot = plot %>%  add_trace(df,
        x = df$rt,
        y = df$intensity,
        type = "scatter", mode =  "lines+markers",
        fill = "tozeroy", connectgaps = TRUE,
        fillcolor = paste(color = colors[g], 50, sep = ""),
        line = list(width = 0.1, color = colors[g]),
        marker = list(size = 3, color = colors[g]),
        name = g,
        legendgroup = g,
        showlegend = showleg[which(leg %in% g)],
        hoverinfo = "text",
        hoverlabel = list(bgcolor = colors[g]),
        text = paste(
          "</br> name: ", g,
          "</br> group: ", ft$group,
          "</br> feature: ", ft$id,
          "</br> analysis: ", ft$analysis,
          "</br> <i>m/z</i>: ", round(ft$mz, digits = 4),
          "</br> rt: ", round(df$rt, digits = 0),
          "</br> intensity: ", round(df$intensity, digits = 0)
        )
      )
      showleg[which(leg %in% g)] <- FALSE
    }
  }

  plot2 <- plot_ly()

  for (g in leg) {

    ft2 = fts[fts$var == g, ]


    if (!"is_filled" %in% colnames(ft2)) ft2$is_filled = FALSE

    ft_nf = ft2[!ft2$is_filled, ]

    plot2 <- plot2 %>% add_trace(
      x = ft_nf$rt,
      y = ft_nf$analysis,
      type = "scatter",
      mode = "markers",
      marker = list(
        line = list(color = colors[g], width = 3),
        color = "#000000", size = 10
      ),
      error_x = list(
        type = "data",
        symmetric = FALSE,
        arrayminus = ft_nf$rt - ft_nf$rtmin,
        array = ft_nf$rtmax - ft_nf$rt,
        color = colors[g],
        width = 5
      ),
      name = g,
      legendgroup = g,
      showlegend = FALSE,
      hoverinfo = "text",
      hoverlabel = list(bgcolor = colors[g]),
      text = paste(
        "</br> name: ", g,
        "</br> group: ", ft_nf$group,
        "</br> feature: ", ft_nf$id,
        "</br> analysis: ", ft_nf$analysis,
        "</br> intensity: ", round(ft_nf$intensity, digits = 0),
        "</br> width: ", round(ft_nf$rtmax - ft_nf$rtmin, digits = 0),
        "</br> dppm: ", round(((ft_nf$mzmax - ft_nf$mzmin) / ft_nf$mz) *
          1E6, digits = 1),
        "</br> filled: ", ft_nf$is_filled
      )
    )

    ft_f <- ft2[ft2$is_filled, ]

    if (nrow(ft_f) > 0) {
      plot2 <- plot2 %>% add_trace(
        x = ft_f$rt,
        y = ft_f$analysis,
        type = "scatter",
        mode = "markers",
        marker = list(
          line = list(color = colors[g], width = 3),
          color = "#f8f8f8",
          size = 10
        ),
        error_x = list(
          type = "data",
          symmetric = FALSE,
          arrayminus = ft_f$rt - ft_f$rtmin,
          array = ft_f$rtmax - ft_f$rt,
          color = colors[g],
          width = 5
        ),
        name = g,
        legendgroup = g,
        showlegend = FALSE,
        hoverinfo = "text",
        hoverlabel = list(bgcolor = colors[g]),
        text = paste(
          "</br> name: ", g,
          "</br> group: ", ft_f$group,
          "</br> feature: ", ft_f$id,
          "</br> analysis: ", ft_f$analysis,
          "</br> intensity: ", round(ft_f$intensity, digits = 0),
          "</br> width: ", round(ft_f$rtmax - ft_f$rtmin, digits = 0),
          "</br> dppm: ", round(((ft_f$mzmax - ft_f$mzmin) / ft_f$mz) *
            1E6, digits = 1),
          "</br> filled: ", ft_f$is_filled
        )
      )
    }
  }
  plot2 <- hide_colorbar(plot2)

  plot3 <- plot_ly(fts, x = fts$analysis)

  for (g in leg) {

    df_3 = fts[fts$var == g,]

    if (!all(analyses %in% df_3$analysis)) {
      extra = data.frame(
        "analysis" = analyses[!analyses %in% df_3$analysis],
        "var" = g,
        "intensity" = 0
      )
      df_3 = rbind(df_3[, c("analysis", "var", "intensity")], extra)
      df_3 = df_3[order(df_3$analysis), ]
    }

    plot3 = plot3 %>% add_trace(df_3,
      x = df_3$analysis,
      y = df_3$intensity / max(df_3$intensity),
      type = "scatter", mode = "lines",
      line = list(width = 1, color = colors[g]),
      connectgaps = FALSE,
      name = g,
      legendgroup = g,
      showlegend = FALSE
    )
  }

  xaxis <- list(linecolor = toRGB("black"), linewidth = 2,
                title = "Retention time / seconds",
                titlefont = list(size = 12, color = "black"),
                range = c(min(eic$rt), max(eic$rt)),
                autotick = TRUE, ticks = "outside")

  yaxis1 <- list(linecolor = toRGB("black"), linewidth = 2,
                 title = "Intensity / counts",
                 titlefont = list(size = 12, color = "black"))

  yaxis2 <- list(linecolor = toRGB("black"), linewidth = 2,
                 title = "",
                 titlefont = list(size = 12, color = "black"),
                 tick0 = 0, dtick = 1)

  xaxis3 <- list(linecolor = toRGB("black"), linewidth = 2, title = NULL)

  yaxis3 <- list(linecolor = toRGB("black"), linewidth = 2,
                 title = "Normalized intensity",
                 titlefont = list(size = 12, color = "black"),
                 tick0 = 0, range = c(0, 1))

  plotList <- list()

  plot <- plot %>% layout(xaxis = xaxis3, yaxis = yaxis1)
  plotList[["plot"]] <- plot

  plot2 <- plot2 %>% layout(xaxis = xaxis, yaxis = yaxis2)
  plotList[["plot2"]] <- plot2

  plot3 <- plot3 %>% layout(xaxis = xaxis3, yaxis = yaxis3)

  plotf <- subplot(
    plotList,
    nrows = 2,
    titleY = TRUE, titleX = TRUE,
    heights = heights[1:2],
    margin = 0.01,
    shareX = TRUE,
    which_layout = "merge"
  )

  plotf_2 <- subplot(
    list(plotf, plot3),
    nrows = 2,
    titleY = TRUE, titleX = TRUE,
    heights = c(sum(heights[1:2]) , heights[3]),
    margin = 0.01,
    shareX = FALSE,
    which_layout = "merge"
  )

  plotf_2 <- plotf_2 %>% layout(
    legend = list(title = list(text = paste("<b>", "targets", "</b>"))))

  return(plotf_2)
}
