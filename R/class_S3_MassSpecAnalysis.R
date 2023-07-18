#' **MassSpecAnalysis** S3 class constructor, methods and functions
#'
#' @description
#' Creates a MassSpecAnalysis S3 class object.
#'
#' @param name *mzML* or *mzXML* file name without extension.
#' @param replicate Character with length one, representing the analysis
#' replicate group name.
#' @param blank Character with length one, representing the associated blank
#' replicate group name.
#' @param file *mzML* or *mzXML* full file path (with extension).
#' @param format Character with length one. One of *mzML* or *mzXML*.
#' @param type Character with length one. Possible values as "MS" for only MS1
#' spectral data, "MS/MS" for tandem spectral data (i.e., MS1 and MS2) or "SRM"
#' for selected reaction monitoring data (i.e., no spectra only chromatograms).
#' @param instrument List with metadata from the instrument used to acquire the
#' data (content is highly vendor dependent).
#' @param software X.
#' @param time_stamp Character with length one, representing the start time and
#' date of the data acquisition.
#' @param spectra_number Integer with the number of spectra in the file.
#' @param spectra_mode Character with length one. Possible values are *centroid*
#' , *profile* or `NA` for centroided data, data in profile mode or mode not
#' defined or absence spectra, respectively.
#' @param spectra_levels Integer vector with the number of levels in the
#' spectra (i.e., 1 or 1 and 2).
#' @param mz_low Numeric length one with the lowest \emph{m/z} value detected
#' in the spectra, in Da.
#' @param mz_high Numeric length one with the highest \emph{m/z} value detected
#' in the spectra, in Da.
#' @param rt_start Numeric length one with the run start time, in seconds.
#' @param rt_end Numeric length one with the run end time, in seconds.
#' @param polarity Character length one. Possible values are *positive*,
#' *negative* or *both* (the latter refers to polarity switching acquisition).
#' @param chromatograms_number Integer with the number of chromatograms in the
#' file.
#' @param has_ion_mobility Logical length one for presence or absence of drift
#' separation from ion mobility.
#' @param run data.table run information for each spectrum.
#' @param spectra data.table with the raw spectra (only present if loaded).
#' @param chromatograms data.table with the raw chromatograms (only present
#' if loaded).
#' @param features data.table with the features from data processing.
#' @param metadata List with flexible storage for experimental metadata
#' (e.g., concentration, location, etc.).
#'
#' @return An MassSpecAnalysis S3 class object.
#'
#' @export
#'
MassSpecAnalysis <- function(name = NA_character_,
                             replicate = NA_character_,
                             blank = NA_character_,
                             file = NA_character_,
                             format = NA_character_,
                             type = NA_character_,
                             time_stamp = NA_character_,
                             spectra_number = NA_integer_,
                             spectra_mode = NA_character_,
                             spectra_levels = NA_integer_,
                             mz_low = NA_real_,
                             mz_high = NA_real_,
                             rt_start = NA_real_,
                             rt_end = NA_real_,
                             polarity = NA_character_,
                             has_ion_mobility = FALSE,
                             chromatograms_number = NA_integer_,
                             instrument = data.table(),
                             software = data.table(),
                             run = data.table(),
                             spectra = data.table(),
                             chromatograms = data.table(),
                             features = data.table(),
                             metadata = list()) {

  if (is.data.frame(features)) {
    if ("ms1" %in% colnames(features)) {
      features$ms1 <- lapply(features$ms1, as.data.table)
    }

    if ("ms2" %in% colnames(features)) {
      features$ms2 <- lapply(features$ms2, as.data.table)
    }
  }

  if (is.list(features)) {
    if ("ms1" %in% names(features)) {
      features$ms1 <- lapply(features$ms1, as.data.table)
    }

    if ("ms2" %in% names(features)) {
      features$ms2 <- lapply(features$ms2, as.data.table)
    }
  }

  x <- list(
    "name" = name,
    "replicate" = replicate,
    "blank" = blank,
    "file" = file,
    "format" = format,
    "type" = type,
    "instrument" = instrument,
    "time_stamp" = time_stamp,
    "spectra_number" = spectra_number,
    "spectra_mode" = spectra_mode,
    "spectra_levels" = spectra_levels,
    "mz_low" = mz_low,
    "mz_high" = mz_high,
    "rt_start" = rt_start,
    "rt_end" = rt_end,
    "polarity" = polarity,
    "chromatograms_number" = chromatograms_number,
    "has_ion_mobility" = has_ion_mobility,
    "run" = run,
    "spectra" = spectra,
    "chromatograms" = chromatograms,
    "features" = features,
    "metadata" = metadata
  )

  x$name <- as.character(x$name)
  x$replicate <- as.character(x$replicate)
  x$blank <- as.character(x$blank)
  if (is.na(x$blank)) x$blank <- NA_character_
  x$file <- as.character(x$file)
  x$format <- as.character(x$format)
  x$type <- as.character(x$type)
  x$time_stamp <- as.character(x$time_stamp)
  x$spectra_number <- as.integer(x$spectra_number)
  x$spectra_mode <- as.character(x$spectra_mode)
  x$spectra_levels <- as.integer(x$spectra_levels)
  x$mz_low <- as.numeric(x$mz_low)
  x$mz_high <- as.numeric(x$mz_high)
  x$rt_start <- as.numeric(x$rt_start)
  x$rt_end <- as.numeric(x$rt_end)
  x$polarity <- as.character(x$polarity)
  x$chromatograms_number <- as.integer(x$chromatograms_number)
  x$has_ion_mobility <- as.logical(x$has_ion_mobility)
  x$run <- as.data.table(x$run)
  x$spectra <- as.data.table(x$spectra)
  x$chromatograms <- as.data.table(x$chromatograms)
  x$features <- as.data.table(x$features)

  if (validate.MassSpecAnalysis(x)) {
    structure(x, class = "MassSpecAnalysis")
  } else {
    NULL
  }
}

#' @describeIn MassSpecAnalysis
#' Validates a MassSpecAnalysis S3 class object, returning a logical value of
#' length one.
#'
#' @param x A MassSpecAnalysis S3 class object.
#'
#' @export
#'
validate.MassSpecAnalysis <- function(x = NULL) {
  valid <- FALSE
  name <- NA_character_

  if (is.list(x)) {
    valid <- TRUE

    if (length(x$name) != 1 & !is.character(x$name)) {
      warning("Analysis name not conform!")
      valid <- FALSE
    } else {
      name <- x$name
    }

    if (length(x$replicate) != 1 &
        !is.character(x$replicate)) {
      warning("Analysis replicate name not conform!")
      valid <- FALSE
    }

    if (length(x$blank) != 1 & !is.character(x$blank)) {
      warning("Analysis blank name not conform!")
      valid <- FALSE
    }

    if (length(x$format) != 1) {
      warning("Analysis format not conform!")
      valid <- FALSE
    } else if (!(x$format %in% c("mzML", "mzXML"))) {
      warning("Analysis format must be 'mzML' ot 'mzXML'!")
      valid <- FALSE
    }

    if (length(x$file) != 1 & !is.character(x$file)) {
      warning("Analysis file path entry not conform!")
      valid <- FALSE
    } else if (!file.exists(x$file)) {
      warning(paste0(x$file, " does not exist!"))
      valid <- FALSE
    }

    if (length(x$type) != 1) {
      warning("Analysis type entry not conform!")
      valid <- FALSE
    } else if (!(x$type %in% c("MS", "MS/MS", "SRM"))) {
      warning("Analysis type must be 'MS', 'MS/MS' or 'SRM'!")
      valid <- FALSE
    }

    if (!is.integer(x$spectra_number) &&
        length(x$spectra_number) != 1) {
      warning("Analysis spectra_numebr entry not conform!")
      valid <- FALSE
    }

    if (!is.integer(x$chromatograms_number) &&
        length(x$chromatograms_number) != 1) {
      warning("Analysis chromatograms_number entry not conform!")
      valid <- FALSE
    }

    if (!is.character(x$spectra_mode) &
        length(x$spectra_mode) != 1) {
      warning("Analysis spectra_mode entry not conform!")
      valid <- FALSE
    }

    if (!is.integer(x$spectra_levels)) {
      warning("Analysis spectra_levels entry not conform!")
      valid <- FALSE
    }

    if (length(x$mz_low) != 1 & !is.numeric(x$mz_low)) {
      warning("Analysis mz_low entry not conform!")
      valid <- FALSE
    }

    if (length(x$mz_high) != 1 & !is.numeric(x$mz_high)) {
      warning("Analysis mz_high entry not conform!")
      valid <- FALSE
    }

    if (length(x$rt_start) != 1 & !is.numeric(x$rt_start)) {
      warning("Analysis rt_start entry not conform!")
      valid <- FALSE
    }

    if (length(x$rt_end) != 1 & !is.numeric(x$rt_end)) {
      warning("Analysis rt_end entry not conform!")
      valid <- FALSE
    }

    if (!is.character(x$polarity)) {
      warning("Analysis polarity entry not conform!")
      valid <- FALSE
    } else if (FALSE %in%
        (x$polarity %in% c("positive", "negative", NA_character_))) {
      warning("Analysis polarity entry not conform!")
      valid <- FALSE
    }

    if (length(x$has_ion_mobility) != 1 &
        !is.logical(x$has_ion_mobility)) {
      warning("Analysis ion_mobility entry not conform!")
      valid <- FALSE
    }

    must_have_run_cols <- c(
      "index", "scan", "traces", "level", "rt",
      "bpc_mz", "bpc_intensity", "tic_intensity",
      "pre_scan", "pre_mz", "pre_ce"
    )

    if (!is.data.frame(x$run)) {
      warning("Analysis run entry not conform!")
      valid <- FALSE
    } else if (FALSE %in%
        (must_have_run_cols %in% colnames(x$run))) {
      warning("Analysis run data.table must have columns index, scan, traces, level, rt, bpc_mz, bpc_intensity, tic_intensity, pre_scan, pre_mz and pre_ce!")
      valid <- FALSE
    }

    if (!is.data.frame(x$spectra)) {
      warning("Analysis spectra entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(x$chromatograms)) {
      warning("Analysis chromatograms entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(x$features)) {
      warning("Analysis features entry not conform!")
      valid <- FALSE
    }

    if (!is.list(x$metadata)) {
      warning("Analysis netadata entry not conform!")
      valid <- FALSE
    }
  }

  if (!valid) warning("Issue/s found with analysis ", x$name, "!")

  valid
}

#' @describeIn MassSpecAnalysis
#' Converts a MassSpecAnalysis S3 class object to a JSON string.
#'
#' @export
asJSON.MassSpecAnalysis <- function(x) {
  toJSON(
    x,
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
}

#' @describeIn MassSpecAnalysis
#' Converts the argument value in a MassSpecAnalysis S3 class object.
#'
#' @param value A list to be checked and/or converted to MassSpecAnalysis S3 class.
#'
#' @export
as.MassSpecAnalysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(MassSpecAnalysis, value)
}

#' @describeIn MassSpecAnalysis
#' Parses information from *mzML* or *mzXML* file/s and returns a list with
#' MassSpecAnalysis S3 class object/s. On error, returns \code{NULL}.
#'
#' @param files A character vector with *mzML* or *mzXML* full file path/s.
#' Alternatively, a data.frame with the column/s file, replicate and blank
#' with the full file path/s, the replicate group name/s (string) and the
#' associated blank replicate group name (string).
#' @template arg-runParallel
#'
#' @export
parse.MassSpecAnalysis <- function(files = NULL, runParallel = FALSE) {

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
        blanks <- NULL
      }

      files <- files$file
    } else {
      files <- ""
    }

  } else {
    replicates <- rep(NA_character_, length(files))
    blanks <- NULL
  }

  possible_ms_file_formats <- ".mzML|.mzXML"

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

  cached_analyses <- FALSE

  if (caches_data()) {
    hash <- patRoon::makeHash(files)
    analyses <- patRoon::loadCacheData("parsed_ms_analyses", hash)

    if (!is.null(analyses)) {
      if (all(vapply(analyses, validate.MassSpecAnalysis, FALSE))) {
        cached_analyses <- TRUE
      } else {
        analyses <- NULL
      }
    }

  } else {
    hash <- NULL
    analyses <- NULL
  }

  if (runParallel & length(files) > 1 & !cached_analyses) {
    workers <- parallel::detectCores() - 1
    if (length(files) < workers) workers <- length(files)
    par_type <- "PSOCK"
    if (parallelly::supportsMulticore()) par_type <- "FORK"
    cl <- parallel::makeCluster(workers, type = par_type)
    doParallel::registerDoParallel(cl)
  } else {
    registerDoSEQ()
  }

  if (!cached_analyses) {
    message("\U2699 Parsing ", length(files),  " MS file/s...",
      appendLF = FALSE
    )

    i <- NULL

    vars <- c("rcpp_parse_ms_analysis")

    analyses <- foreach(i = files,
      .packages = "streamFind",
      .export = vars
    ) %dopar% { rcpp_parse_ms_analysis(i) }

    class_analyses <- vapply(analyses, class, "")

    if (!all(class_analyses %in% "MassSpecAnalysis")) return(NULL)

    message(" Done!")

    if (!is.null(hash)) {
      patRoon::saveCacheData("parsed_ms_analyses", analyses, hash)
      message("\U1f5ab Parsed MS files/s cached!")
    }

  } else {
    message("\U2139 Analyses loaded from cache!")
  }

  if (runParallel) parallel::stopCluster(cl)

  if (!is.null(analyses)) {
    if (all(is.na(replicates))) {
      replicates <- vapply(analyses, function(x) x$name, "")
      replicates <- gsub("-", "_", replicates)
      replicates <- sub("_[^_]+$", "", replicates)
    }

    analyses <- Map(
      function(x, y) {
        x$replicate <- y
        x
      },
      analyses, replicates
    )

    if (!is.null(blanks) & length(blanks) == length(analyses)) {
      if (all(blanks %in% replicates)) {
        analyses <- Map(
          function(x, y) {
            x$blank <- y
            x
          },
          analyses, blanks
        )
      }
    }

    names(analyses) <- vapply(analyses, function(x) x$name, "")
    analyses <- analyses[order(names(analyses))]
  }

  analyses
}
