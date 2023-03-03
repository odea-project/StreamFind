
#' validate_ms_analysis
#'
#' @description
#' Validates an msAnalysis S3 class object.
#'
#' @param x An msAnalysis S3 class object.
#'
#' @return Logical of length one.
#'
#' @export
#'
validate_ms_analysis <- function(x = NULL) {
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

    if (length(x$replicate) != 1 & !is.character(x$replicate)) {
      warning("Analysis replicate name not conform!")
      valid <- FALSE
    }

    if (length(x$blank) != 1 & !is.character(x$blank)) {
      warning("Analysis blank name not conform!")
      valid <- FALSE
    }

    if (length(x$file) != 1 & !is.character(x$file)) {
      warning("Analysis file path entry not conform!")
      valid <- FALSE
    } else if (!file.exists(x$file)) {
      warning(paste0(
        x$file,
        " does not exist! Update file paths with msData$update_files() method"
      ))
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

    if (length(x$ion_mobility) != 1 & !is.logical(x$ion_mobility)) {
      warning("Analysis ion_mobility entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(x$tic)) {
      warning("Analysis tic entry not conform!")
      valid <- FALSE
    } else if (FALSE %in% (c("rt", "intensity") %in% colnames(x$tic))) {
      warning("Analysis tic data.table must have columns rt and intensity!")
      valid <- FALSE
    }

    if (!is.data.frame(x$bpc)) {
      warning("Analysis bpc entry not conform!")
      valid <- FALSE
    } else if (FALSE %in%
               (c("rt", "mz", "intensity") %in% colnames(x$bpc))) {
      warning("Analysis bpc data.table must have columns mz, rt and intensity!")
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

#' msAnalysis S3 Class Constructor
#'
#' @description
#' Creates an msAnalysis S3 class object.
#'
#' @param name X.
#' @param replicate X.
#' @param blank X.
#' @param file X.
#' @param type X.
#' @param instrument X.
#' @param time_stamp X.
#' @param spectra_number X.
#' @param spectra_mode X.
#' @param spectra_levels X.
#' @param mz_low X.
#' @param mz_high X.
#' @param rt_start X.
#' @param rt_end X.
#' @param polarity X.
#' @param chromatograms_number X.
#' @param ion_mobility X.
#' @param tic X.
#' @param bpc X.
#' @param spectra X.
#' @param chromatograms X.
#' @param features X.
#' @param metadata X.
#'
#' @return An msAnalysis S3 class object.
#'
#' @export
#'
msAnalysis <- function(name = NA_character_,
                       replicate = NA_character_,
                       blank = NA_character_,
                       file = NA_character_,
                       type = NA_character_,
                       instrument = list(),
                       time_stamp = NA_character_,
                       spectra_number = NA_integer_,
                       spectra_mode = NA_character_,
                       spectra_levels = NA_integer_,
                       mz_low = NA_real_,
                       mz_high = NA_real_,
                       rt_start = NA_real_,
                       rt_end = NA_real_,
                       polarity = NA_character_,
                       chromatograms_number = NA_integer_,
                       ion_mobility = NA,
                       tic = data.table(),
                       bpc = data.table(),
                       spectra = data.table(),
                       chromatograms = data.table(),
                       features = data.table(),
                       metadata = list()) {

  x <- list(
    "name" = name,
    "replicate" = replicate,
    "blank" = blank,
    "file" = file,
    "type" = type,
    "instrument" = instrument,
    "time_stamp" = time_stamp,
    "spectra_number" = as.integer(spectra_number),
    "spectra_mode" = spectra_mode,
    "spectra_levels" = spectra_levels,
    "mz_low" = as.numeric(mz_low),
    "mz_high" = as.numeric(mz_high),
    "rt_start" = as.numeric(rt_start),
    "rt_end" = as.numeric(rt_end),
    "polarity" = polarity,
    "chromatograms_number" = as.integer(chromatograms_number),
    "ion_mobility" = ion_mobility,
    "tic" = tic,
    "bpc" = bpc,
    "spectra" = spectra,
    "chromatograms" = chromatograms,
    "features" = features,
    "metadata" = metadata
  )

  if (validate_ms_analysis(x)) {
    structure(x, class = "msAnalysis")
  } else {
    NULL
  }
}

#' @rdname msAnalysis
#' @export
as.msAnalysis <- function(analysis) {
  if (length(analysis) == 1) analysis <- analysis[[1]]
  do.call(msAnalysis, analysis)
}
