
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

    .analyses = NULL,

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

        analyses = foreach(file = files, .packages = ex_packages) %dopar% {

          file_link = mzR::openMSfile(file, backend = "pwiz")
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
          if (grepl(".mzML", file))
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
              if ("mz" %in% colnames(bpc)) bpc$mz <- NA
              colnames(bpc) = c("rt", "intensity", "mz")
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
            "name" = gsub(".mzML|.mzXML", "", basename(file)),
            "replicate" = NA_character_,
            "blank" = NA_character_,
            "file" = file,
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

      if (is.null(analyses)) analyses = self$get_analysis_names

      files = self$get_file_paths(analyses)

      analyses = names(files)

      if (all(!is.na(files))) {

        ex_packages = c("mzR", "data.table")

        # ex_globals = c("levels", "rtr", "preMZrange",
        #                "minIntensityMS1", "minIntensityMS2",
        #                "get_ms_spectra_from_file_ext")

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

        spec_list = foreach(i = files, .packages = ex_packages) %dopar% {

          file_link <- mzR::openMSfile(i, backend = "pwiz")

          sH <- mzR::header(file_link)

          if (nrow(sH) > 0) {

            if (max(sH$retentionTime) < 60)
              sH$retentionTime <- sH$retentionTime * 60

            if (!is.null(levels)) sH <- sH[sH$msLevel %in% levels, ]

            temp_checkOverlapRanges <- function(vals, ranges) {
              return(
                rowSums(
                  mapply(function(a, b) data.table::between(vals, a, b),
                    ranges$min, ranges$max)) > 0)
            }

            if (!is.null(rtr))
              sH <- sH[temp_checkOverlapRanges(sH$retentionTime, rtr), ]

            if(!is.null(preMZrange))
              sH <- sH[temp_checkOverlapRanges(sH$precursorMZ, preMZrange), ]

            scans <- mzR::peaks(file_link, scans = sH$seqNum)

            mat_idx <- rep(sH$seqNum, sapply(scans, nrow))
            scans <- as.data.frame(do.call(rbind, scans))
            scans$index <- mat_idx

            if (TRUE %in% (unique(sH$msLevel) == 2)) {
              sH_b <- data.frame(
                "index" = sH$seqNum,
                "scan" = sH$acquisitionNum,
                "level" = sH$msLevel,
                "ce" = sH$collisionEnergy,
                "preScan" = sH$precursorScanNum,
                "preMZ" = sH$precursorMZ,
                "rt" = sH$retentionTime)
            } else {
              sH_b <- data.frame(
                "index" = sH$seqNum,
                "scan" = sH$acquisitionNum,
                "level" = sH$msLevel,
                "rt" = sH$retentionTime)
            }

            if (!all(is.na(sH$ionMobilityDriftTime))) {
              rt_unique <- unique(sH_b$rt)
              frame_numbers <- seq_len(length(rt_unique))
              if ("preMZ" %in% colnames(sH_b)) sH_b$preMZ <- NA_real_
              sH_b$frame <- factor(sH_b$rt,
                                   levels = rt_unique, labels = frame_numbers)
              sH_b$driftTime <- sH$ionMobilityDriftTime
            }

            sH_n <- merge(sH_b, scans, by = "index")

            sH_n <- sH_n[!(sH_n$intensity <= minIntensityMS1 &
                             sH_n$level == 1), ]

            sH_n <- sH_n[!(sH_n$intensity <= minIntensityMS2 &
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

# Auxiliary functions -----

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

#' make_overview_dataframe
#'
#' @description
#' Makes an overview `data.frame` of a given list of MS analyses.
#'
#' @param analyses A list of analyses.
#'
#' @return An overview `data.frame` of the analyses in the given list with
#' columns: type, name, replicate, blank, polarity and file.
#'
#' @export
make_overview_dataframe = function(analyses, validateAnalysis = TRUE) {

  if (validateAnalysis)
    valid_analyses <- vapply(analyses, validate_list_ms_analysis, FALSE)
      else valid_analyses = TRUE

  if (all(valid_analyses)) {

    overview = data.frame(
      "type" = vapply(analyses, function(x) x$type, ""),
      "analysis" = vapply(analyses, function(x) x$name, ""),
      "replicate" = vapply(analyses, function(x) x$replicate, ""),
      "blank" = vapply(analyses, function(x) x$blank, ""),
      "polarity" = vapply(analyses,function(x) {
        paste(x$polarity, collapse = "; ")
      }, ""),
      "file" = vapply(analyses, function(x) x$file, "")
    )

    row.names(overview) <- seq_len(nrow(overview))

    return(overview)

  } else {
    warning("Not done, check the conformity of the analyses list!")
    return(NULL)
  }
}
