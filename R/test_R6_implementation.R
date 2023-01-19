
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
      name = character(),
      author =  character(),
      # user = Sys.info("user"),
      # system_info = Sys.info("sysname"),
      # computer = Sys.info("nodename"),
      description = character(),
      path = getwd(),
      date = Sys.time()
    ),

    ## .analyses -----
    .analyses = NULL,

    ## .features -----
    .features = NULL

  ),

  # active bindings -----
  active = list(

    #' @field overview The overview data.frame with all the analysis types,
    #' names, replicates, associated blank replicates, polarities and full
    #' file paths.
    overview = function() {

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

      row.names(df) <- seq_len(nrow(df))

      return(df)
    },

    #' @field number_analyses The number of analyses.
    number_analyses = function() { length(private$.analyses) }

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
    #' @param header A list with administrative information.
    #' @param analyses A list with MS analyses information.
    #' @param features A data.frame with features representing corresponding
    #' peaks across MS analyses.
    #' @param run_parallel Logical, set to \code{TRUE} for processing the data
    #' in parallel.
    #'
    #' @return A new `r6MS` object.
    initialize = function(files = NULL,
                          header = NULL,
                          analyses = NULL,
                          features = NULL,
                          run_parallel = FALSE) {

      if (is.null(analyses) & !is.null(files)) {

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

        ex_packages = "mzR"

        if (run_parallel) {
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

        analyses = foreach(i = files, .packages = ex_packages) %dopar% {

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
                bpc$mz <- NA
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
            "settings" = list(),
            "peaks" = data.frame(),
            "metadata" = list()
          )

          suppressWarnings(mzR::close(file_link))
          return(analysis)
        }

        if (run_parallel) parallel::stopCluster(cl)

        if (all(is.na(replicates))) {
          replicates <- vapply(analyses, function(x) x$name, "")
          replicates <- gsub( "-", "_", replicates)
          replicates <- sub("_[^_]+$", "", replicates)
        }

        analyses = Map(function(x, y) { x$replicate <- y; x },
                       analyses, replicates)

        if (!is.null(blanks) & length(blanks) == length(analyses)) {
          if (all(blanks %in% replicates)) {
            analyses = Map(function(x, y) { x$blank <- y; x },
                           analyses, blanks)
          }
        }

        names(analyses) = vapply(analyses, function(x) x$name, "")
        analyses = analyses[order(names(analyses))]
        private$.analyses = analyses

      } else {

        valid_analyses <- vapply(analyses, validate_list_ms_analysis, FALSE)

        if (all(valid_analyses)) {

          names(analyses) = vapply(analyses, function(x) x$name, "")
          analyses = analyses[order(names(analyses))]
          private$.analyses = analyses

        }
      }

      private$.features = features

      if (!is.null(header) & is.list(header)) {

        if (is.character(header$name) & length(header$name) == 1)
          private$.header$name = header$name

        if (is.character(header$author) & length(header$author) == 1)
          private$.header$author = header$author

        if (is.character(header$description) & length(header$description) == 1)
          private$.header$description = header$description

        if (is.character(header$path) & length(header$path) == 1)
          if (dir.exists(header$path)) private$.header$path = header$path

        if (all(grepl("POSIXct|POSIXt", class(header$date))) &
            length(header$date) == 1)
              private$.header$date = header$date

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
        tb <- self$overview
        tb$file = NULL

        tb$traces <- vapply(private$.analyses, function(x) x$spectra_number, 0)
        tb$peaks <- vapply(private$.analyses, function(x) nrow(x$peaks), 0)

        # if (nrow(object@features@metadata) > 0) {
        #   tb$features <- apply(
        #     object@features@intensity[, analysisNames(object), with = FALSE], 2,
        #     function(x) length(x[x > 0])
        #   )
        # } else {
        #   tb$features <- 0
        # }

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
    #' Method to get the analysis names.
    #'
    #' @param value A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_analysis_names = function(value) {
      ana = self$overview$analysis
      names(ana) = self$overview$analysis
      if (!missing(value)) return(ana[value])
      return(ana)
    },

    #' @description
    #' Method to get the analysis replicate names.
    #'
    #' @param value A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_replicate_names = function(value) {
      rpl = self$overview$replicate
      names(rpl) = self$overview$analysis
      if (!missing(value)) return(rpl[value])
      return(rpl)
    },

    #' @description
    #' Method to get the analysis blank replicate names.
    #'
    #' @param value A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_blank_names = function(value) {
      blk = self$overview$blank
      names(blk) = self$overview$analysis
      if (!missing(value)) return(blk[value])
      return(blk)
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param value A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_polarities = function(value) {
      pol = self$overview$polarity
      names(pol) = self$overview$analysis
      if (!missing(value)) return(pol[value])
      return(pol)
    },

    #' @description
    #' Method to get the polarity of the analyses.
    #'
    #' @param value A numeric/character vector with the number/name
    #' of the analyses.
    #'
    #' @return A character vector.
    get_file_paths = function(value) {
      fls = self$overview$file
      names(fls) = self$overview$analysis
      if (!missing(value)) return(fls[value])
      return(fls)
    },

    #' @description
    #' Method to get spectra from the MS analyses.
    #'
    #' @param analyses X.
    #' @param levels Name.
    #' @param rtr Hair color.
    #' @param preMZrange Hair color.
    #' @param minIntensityMS1 Hair color.
    #' @param minIntensityMS2 Hair color.
    #' @param run_parallel Hair color.
    #' @return A data.frame with spectra.
    get_spectra = function(analyses = NULL, levels = NULL, rtr = NULL,
                           preMZrange = NULL, minIntensityMS1 = 0,
                           minIntensityMS2 = 0, run_parallel = FALSE) {

      temp_checkOverlapRanges = function(vals, ranges)  return(
        rowSums(
          mapply(function(a, b) vals >= a & vals <= b,
                 a = ranges$min, b = ranges$max)) > 0
      )

      if (is.null(analyses)) analyses = self$get_analysis_names

      has_spectra = self$has_loaded_spectra(analyses)

      if (all(has_spectra)) {

        spec_list = lapply(private$.analyses[analyses], function(x) {

          spec = x$spectra

          if (!is.null(levels)) spec = spec[spec$level %in% levels, ]

          if (!is.null(rtr))
            spec = spec[temp_checkOverlapRanges(spec$rt, rtr), ]

          if(!is.null(preMZrange)) {
            preMZ_check = temp_checkOverlapRanges(spec$"preMZ", preMZrange)
            spec = spec[(preMZ_check %in% TRUE) | is.na(preMZ_check), ]
          }

          spec = spec[!(spec$intensity <= minIntensityMS1 &
                          spec$level == 1), ]

          spec = spec[!(spec$intensity <= minIntensityMS2 &
                          spec$level == 2), ]

          return(spec)

        })

        return(spec_list)
      }

      files = self$get_file_paths(analyses)

      analyses = names(files)

      if (all(!is.na(files))) {

        ex_packages = c("mzR")

        if (run_parallel) {
          workers = parallel::detectCores() - 1
          if (length(files) < workers) workers = length(files)
          par_type = "PSOCK"
          if (parallelly::supportsMulticore()) par_type = "FORK"
          cl = parallel::makeCluster(workers, type = par_type)
          doParallel::registerDoParallel(cl)
          on.exit(parallel::stopCluster(cl))
        } else {
          registerDoSEQ()
        }

        spec_list = foreach(i = unname(files), .packages = ex_packages) %dopar% {

          file_link = mzR::openMSfile(i, backend = "pwiz")

          sH = mzR::header(file_link)

          if (nrow(sH) > 0) {

            if (max(sH$retentionTime) < 60)
              sH$retentionTime <- sH$retentionTime * 60

            if (!is.null(levels)) sH = sH[sH$msLevel %in% levels, ]

            if (!is.null(rtr))
              sH = sH[temp_checkOverlapRanges(sH$retentionTime, rtr), ]

            if(!is.null(preMZrange)) {
              preMZ_check = temp_checkOverlapRanges(sH$precursorMZ, preMZrange)
              sH = sH[(preMZ_check %in% TRUE) | is.na(preMZ_check), ]
            }

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

            sH_n = merge(sH_b, scans, by = "index")

            sH_n = sH_n[!(sH_n$intensity <= minIntensityMS1 &
                             sH_n$level == 1), ]

            sH_n = sH_n[!(sH_n$intensity <= minIntensityMS2 &
                             sH_n$level == 2), ]

            if (exists("file_link")) suppressWarnings(mzR::close(file_link))

            return(sH_n)

          } else return(data.frame())
        }

        if (run_parallel) parallel::stopCluster(cl)

        names(spec_list) = analyses

        return(spec_list)

      } else {
          warning("Defined analyses not found!")
          return(list())
      }
    },

    #' @description
    #' Method to get spectra from the MS analyses.
    #'
    #' @param analyses X.
    #' @param levels Name.
    #' @param rtr Hair color.
    #' @param preMZrange Hair color.
    #' @param minIntensityMS1 Hair color.
    #' @param minIntensityMS2 Hair color.
    #' @param run_parallel Hair color.
    #' @return A data.frame with spectra.
    get_chromatograms = function(analyses = NULL, minIntensity = 0,
                                 run_parallel = FALSE) {

      if (is.null(analyses)) analyses = self$get_analysis_names

      files = self$get_file_paths(analyses)

      analyses = names(files)

      if (all(!is.na(files))) {

        ex_packages = c("mzR")

        if (run_parallel) {
          workers = parallel::detectCores() - 1
          if (length(files) < workers) workers = length(files)
          par_type = "PSOCK"
          if (parallelly::supportsMulticore()) par_type = "FORK"
          cl = parallel::makeCluster(workers, type = par_type)
          doParallel::registerDoParallel(cl)
          on.exit(parallel::stopCluster(cl))
        } else {
          registerDoSEQ()
        }

        chrom_list = foreach(i = files, .packages = ex_packages) %dopar% {

          file_link = mzR::openMSfile(i, backend = "pwiz")

          cH <- suppressWarnings(mzR::chromatogramHeader(file_link))

          if (nrow(cH) > 0) {

            cH$polarity = as.character(cH$polarity)
            cH[cH$polarity == 1, "polarity"] = "positive"
            cH[cH$polarity == 0, "polarity"] = "negative"
            cH[cH$polarity == -1, "polarity"] = NA_character_

            chroms <- mzR::chromatograms(file_link, cH$chromatogramIndex)

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

              cH_b <- data.frame(
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

            chrom_data = chrom_data[chrom_data$intensity > minIntensity, ]

            if (exists("file_link")) suppressWarnings(mzR::close(file_link))

            return(chrom_data)

          } else return(data.frame())
        }

        if (run_parallel) parallel::stopCluster(cl)

        names(chrom_list) = analyses

        return(chrom_list)

      } else {
        warning("Defined analyses not found!")
        return(list())
      }
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
          length(value) == self$number_analyses) {

        private$.analyses = Map(function(x, y) { x$replicate <- y; x },
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
          length(value) == self$number_analyses) {

        if (all(value %in% self$overview$replicate)) {

          private$.analyses = Map(function(x, y) { x$blank <- y; x },
                                  private$.analyses, value)

          cat("Blank names added! \n")

        } else warning("Not done, blank names not among replicate names!")
      } else warning("Not done, check the value!")
    },

    ## has -----

    #' @description
    #' Method to check for loaded spectra in given analyses names/indices.
    #'
    #' @param analyses The analyses names/indices to check for loaded spectra.
    #'
    #' @return Invisible.
    has_loaded_spectra = function(analyses) {

      has_spectra = vapply(private$.analyses[analyses],
                           function(x) nrow(x$spectra) > 0, FALSE)

      names(has_spectra) = self$get_analysis_names(analyses)

      return(has_spectra)
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

      valid_analyses <- vapply(analyses, validate_list_ms_analysis, FALSE)

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

    ## plot -----

    #' @description
    #' Plots spectra for given MS analyses.
    #'
    #' @param analyses X.
    #' @param levels X.
    #' @param mz X.
    #' @param rt X.
    #' @param ppm X.
    #' @param sec X.
    #' @param id X.
    #' @param onlyMS2fromMZ X.
    #' @param isolationWindow X.
    #' @param colorBy X.
    #' @param run_parallel X.
    #'
    #' @return A 3D interactive plot.
    plot_spectra = function(analyses = NULL, levels = c(1, 2),
                            mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
                            onlyMS2fromMZ = FALSE, isolationWindow = 1.3,
                            colorBy = "analyses", run_parallel = FALSE) {

      targets <- make_targets(mz, rt, ppm, sec, id)

      rtr <- targets[, c("rtmin", "rtmax")]
      colnames(rtr) = c("min", "max")
      if ((nrow(rtr) == 1) & (TRUE %in% (rtr$max == 0))) rtr = NULL

      if (onlyMS2fromMZ) {
        preMZrange <- targets[, c("mzmin", "mzmax")]
        colnames(preMZrange) = c("min", "max")
        preMZrange$min = preMZrange$min - (isolationWindow/2)
        preMZrange$max = preMZrange$max + (isolationWindow/2)
        if ((nrow(preMZrange) == 1) & (TRUE %in% (rtr$max == 0)))
          preMZrange = NULL
      } else {
        preMZrange = NULL
      }

      spec <- self$get_spectra(analyses, levels, rtr, preMZrange)
      spec = rbindlist(spec, idcol = "analysis", fill = TRUE)

      if (nrow(spec) == 0) {
        warning("Mass MS traces not found!")
        return(NULL)
      }

      trim_ranges = function(vals, ranges)  return(
        rowSums(
          mapply(function(a, b) vals >= a & vals <= b,
                 a = ranges[1], b = ranges[2])) > 0
      )

      spec_tr = lapply(seq_len(nrow(targets)),
        function(x, spec, trim_ranges, onlyMS2fromMZ, preMZrange) {

          temp = spec

          temp = temp[
            trim_ranges(temp$rt, c(targets$rtmin[x], targets$rtmax[x])), ]

          if (onlyMS2fromMZ) {

            temp = temp[level == 2 | (level == 1 &
              trim_ranges(temp$mz, c(targets$mzmin[x], targets$mzmax[x]))), ]

            temp = temp[level == 1 | (level == 2 &
              trim_ranges(temp$preMZ, c(preMZrange$min[x], preMZrange$max[x]))), ]

          } else {

            temp = temp[
              trim_ranges(temp$mz, c(targets$mzmin[x], targets$mzmax[x])), ]

          }

          temp$id = targets$id[x]

          temp = temp[, c("id", "analysis", "level", "rt", "mz", "intensity")]

          return(temp)

        },
        spec = spec,
        trim_ranges = trim_ranges,
        onlyMS2fromMZ = onlyMS2fromMZ,
        preMZrange = preMZrange)

      spec_tr <- rbindlist(spec_tr)

      if (nrow(spec_tr) == 0) {
        message("Requested MS traces not found.")
        return(NULL)
      }

      spec_tr$id = factor(spec_tr$id)
      spec_2$level = paste("MS", spec_2$level, sep = "")
      spec_tr$level = factor(spec_tr$level)
      spec_tr$analysis = factor(spec_tr$analysis)

      spec_2 = copy(spec_tr)
      spec_2$rtmz = paste(spec_2$id, spec_2$level,
                          spec_2$mz, spec_2$rt,
                          spec_2$analysis, sep = "_")

      spec_2_temp = copy(spec_2)
      spec_2_temp$intensity = 0
      spec_2 = rbind(spec_2, spec_2_temp)

      if (colorBy == "levels") {
        spec_2$var = spec_2$level
      } else if (colorBy == "targets"){
        spec_2$var = spec_2$id
      } else {
        spec_2$var = spec_2$analysis
      }

      colors_var <- get_colors(unique(spec_2$var))

      fig <- plot_ly(spec_2, x = ~rt, y = ~mz, z = ~intensity) %>%
        group_by(spec_2$rtmz) %>%
        add_lines(color = ~var,  colors = colors_var)

      fig <- fig %>% layout(scene = list(
        xaxis = list(title = "Retention time / seconds"),
        yaxis = list(title = "<i>m/z</i>"),
        zaxis = list(title = "Intensity / counts")))

      return(fig)
    },

    ## subset -----

    #' @description
    #' Create a new MS set of analyses.
    #' @param i Name.
    #' @param ... Hair color.
    subset_analyses = function(i, ...) {

      new_analyses = private$.analyses[i]
      new_features = private$.features

      return(R6MS$new(
        header = private$.header,
        analyses = new_analyses,
        features = new_features)
      )
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

    if(!is.data.frame(value$peaks)) valid = FALSE

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
make_targets <- function(mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL) {

  mzrts <- data.table(
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

      mzrts <- data.table(
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
      rt <- as.data.table(rt)

      if ("rt" %in% colnames(rt) & !"rtmin" %in% colnames(mz)) {
        mzrts <- data.table(
          id = NA_character_,
          mz = rt,
          rt = 0,
          mzmin = 0,
          mzmax = 0,
          rtmin = 0,
          rtmax = 0
        )
        mzrts$rtmin <- rt$rt - sec
        mzrts$rtmax <- rt$rt + sec

      } else if ("rtmin" %in% colnames(rt) & nrow(rt) == nrow(mz)) {
        mzrts <- data.table(
          id = NA_character_,
          mz = apply(rt[, .(rtmin, rtmax)], 1, mean),
          rt = 0,
          mzmin = rt$rtmin,
          mzmax = rt$rtmax,
          rtmin = 0,
          rtmax = 0
        )

        if ("rt" %in% colnames(rt)) {
          mzrts$rt <- mz$rt
        } else {
          mzrts$rt <-  apply(rt[, .(rtmin, rtmax)], 1, mean)
        }
      }

      #adds id
      if (!is.null(id) %in% length(id) == nrow(mzrts)) {
        mzrts$id <- id
      } else if ("id" %in% colnames(rt)) {
        mzrts$id <- rt$id
      } else {
        mzrts[, id := paste(rtmin, "-", rtmax, sep = "")][]
      }
    }

    #when mz is vector, expects rt as vector as well and ranges are calculated
  } else if (length(mz) >= 1 & is.vector(mz)) {

    mzrts <- data.table(
      id = NA_character_,
      mz = mz,
      rt = 0,
      mzmin = mz - ((ppm / 1E6) * mz),
      mzmax = mz + ((ppm / 1E6) * mz),
      rtmin = 0,
      rtmax = 0
    )
    #mzrts$mzmin <- mz - ((ppm / 1E6) * mz)
    #mzrts[, mzmax := mz + ((ppm / 1E6) * mz)]

    if (is.vector(rt) & length(rt) == length(mz)) {
      mzrts$rt <- rt
      mzrts$rtmin <- c(rt - sec)
      mzrts$rtmax <- c(rt + sec)
    }

    if (!is.null(id) & length(id) == nrow(mzrts)) {
      mzrts$id <- id
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
    mz <- as.data.table(mz)

    #when mz is in table but not ranges
    if ("mz" %in% colnames(mz) & !"mzmin" %in% colnames(mz)) {
      mzrts <- data.table(
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
      mzrts <- data.table(
        id = NA_character_,
        mz = apply(mz[, .(mzmin, mzmax)], 1, mean),
        rt = 0,
        mzmin = mz$mzmin,
        mzmax = mz$mzmax,
        rtmin = 0,
        rtmax = 0
      )
      if ("mz" %in% colnames(mz)) mzrts$mz <- mz$mz
    }

    #when rt in also in mz table
    if ("rt" %in% colnames(mz) & !"rtmin" %in% colnames(mz)) {
      mzrts$rt <- mz$rt
      mzrts$rtmin <- mz$rt - sec
      mzrts$rtmax <- mz$rt + sec
    } else if ("rtmin" %in% colnames(mz)) {
      mzrts$rt <-  apply(mz[, .(rtmin, rtmax)], 1, mean)
      mzrts$rtmin <- mz$rtmin
      mzrts$rtmax <- mz$rtmax
      if ("rt" %in% colnames(mz)) mzrts$rt <- mz$rt
    }

    #when rt is given as a table is rt argument
    if (is.data.frame(rt) | is.data.table(rt)) {
      rt <- as.data.table(rt)

      if ("rt" %in% colnames(rt) & nrow(rt) == nrow(mz) & !"rtmin" %in% colnames(mz)) {
        mzrts$rt <- rt$rt
        mzrts$rtmin <- rt$rt - sec
        mzrts$rtmax <- rt$rt + sec
      } else if ("rtmin" %in% colnames(rt) & nrow(rt) == nrow(mz)) {
        mzrts$rt <-  apply(rt[, .(rtmin, rtmax)], 1, mean)
        mzrts$rtmin <- rt$rtmin
        mzrts$rtmax <- rt$rtmax
        if ("rt" %in% colnames(rt)) mzrts$rt <- mz$rt
      }
    }

    #adds id
    if (!is.null(id) & length(id) == nrow(mzrts)) {
      mzrts$id <- id
    } else if ("id" %in% colnames(mz)) {
      mzrts$id <- mz$id
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
get_colors <- function(obj) {

  colors <- c(brewer.pal(8, "Greys")[6],
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

  Ncol <- length(unique(obj))

  if (Ncol > 18) {
    colors <- colorRampPalette(colors)(Ncol)
  }

  if (length(unique(obj)) < length(obj)) {
    Vcol <- colors[seq_len(Ncol)]
    Ncol <- length(obj)
    count <- dplyr::count(data.frame(n = seq_len(Ncol), char = obj), char)
    Vcol <- rep(Vcol, times = count[, "n"])
    names(Vcol) <- obj
  } else {
    Vcol <- colors[seq_len(Ncol)]
    names(Vcol) <- obj
  }

  return(Vcol)
}
