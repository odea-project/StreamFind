# MARK: MassSpecMethod_LoadSpectra
#' @title MassSpecMethod_LoadSpectra_native S3 Class
#' @description Loads spectra from mass spectrometry data using time and m/z dimensions. Ion mobility dimensions are also supported.
#' @param levels A numeric vector with the levels to be used.
#' @param mzmin A numeric vector with the minimum m/z values to be used.
#' @param mzmax A numeric vector with the maximum m/z values to be used.
#' @param rtmin A numeric vector with the minimum retention time values to be used.
#' @param rtmax A numeric vector with the maximum retention time values to be used.
#' @param mobilitymin A numeric vector with the minimum mobility values to be used.
#' @param mobilitymax A numeric vector with the maximum mobility values to be used.
#' @param minIntensity A numeric value with the minimum intensity to be used.
#' @return A `MassSpecMethod_LoadSpectra_native` object.
#' @export
#'
MassSpecMethod_LoadSpectra_native <- function(
  levels = 1,
  mzmin = 0,
  mzmax = 0,
  rtmin = 0,
  rtmax = 0,
  mobilitymin = 0,
  mobilitymax = 0,
  minIntensity = 0
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "LoadSpectra",
    required = NA_character_,
    algorithm = "native",
    input_class = NA_character_,
    output_class = "MassSpecResults_Spectra",
    parameters = list(
      levels = levels,
      mzmin = mzmin,
      mzmax = mzmax,
      rtmin = rtmin,
      rtmax = rtmax,
      mobilitymin = mobilitymin,
      mobilitymax = mobilitymax,
      minIntensity = minIntensity
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
}

#' @describeIn MassSpecMethod_LoadSpectra_native Validator for the MassSpecMethod_LoadSpectra_native object, returning NULL if valid.
#' @param x A `MassSpecMethod_LoadSpectra_native` object.
#' @export
#'
validate_object.MassSpecMethod_LoadSpectra_native <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "LoadSpectra")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$levels)
  checkmate::assert_numeric(x$parameters$mzmin, len = 1)
  checkmate::assert_numeric(x$parameters$mzmax, len = 1)
  checkmate::assert_numeric(x$parameters$rtmin, len = 1)
  checkmate::assert_numeric(x$parameters$rtmax, len = 1)
  checkmate::assert_numeric(x$parameters$mobilitymin, len = 1)
  checkmate::assert_numeric(x$parameters$mobilitymax, len = 1)
  checkmate::assert_numeric(x$parameters$minIntensity, len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_LoadSpectra_native <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (sum(vapply(engine$Analyses$analyses, function(z) z$spectra_number, 0)) == 0) {
    warning("There are no spectra! Not done.")
    return(FALSE)
  }
  parameters <- x$parameters
  ranges <- data.frame(
    mzmin = parameters$mzmin,
    mzmax = parameters$mzmax,
    rtmin = parameters$rtmin,
    rtmax = parameters$rtmax,
    mobilitymin = parameters$mobilitymin,
    mobilitymax = parameters$mobilitymax
  )
  tryCatch(
    {
      engine$Analyses <- load_spectra(
        engine$Analyses,
        levels = parameters$levels,
        mz = ranges,
        minIntensityMS1 = parameters$minIntensity,
        minIntensityMS2 = parameters$minIntensity
      )
      message(paste0("\U2713 ", "Spectra loaded!"))
      TRUE
    },
    error = function(e) {
      warning("Error loading spectra! Not done.")
      return(FALSE)
    }
  )
}

#' @title MassSpecMethod_LoadSpectra_chrompeaks S3 Class
#' @description Loads spectra based on retention time dimensions of chromatographic peaks.
#' @param levels A numeric vector with the levels to be used.
#' @param mzmin A numeric vector with the minimum m/z values to be used.
#' @param mzmax A numeric vector with the maximum m/z values to be used.
#' @param minIntensity A numeric value with the minimum intensity to be used.
#' @return A `MassSpecMethod_LoadSpectra_chrompeaks` object.
#' @export
#'
MassSpecMethod_LoadSpectra_chrompeaks <- function(
  levels = 1,
  mzmin = 0,
  mzmax = 0,
  minIntensity = 0
) {
  x <- ProcessingStep(
    data_type = "MassSpec",
    method = "LoadSpectra",
    required = "FindChromPeaks",
    algorithm = "chrompeaks",
    input_class = NA_character_,
    output_class = "MassSpecResults_Spectra",
    parameters = list(
      levels = levels,
      mzmin = mzmin,
      mzmax = mzmax,
      minIntensity = minIntensity
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
}

#' @describeIn MassSpecMethod_LoadSpectra_chrompeaks Validator for the MassSpecMethod_LoadSpectra_chrompeaks object, returning NULL if valid.
#' @param x A `MassSpecMethod_LoadSpectra_chrompeaks` object.
#' @export
#'
validate_object.MassSpecMethod_LoadSpectra_chrompeaks <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "LoadSpectra")
  checkmate::assert_choice(x$algorithm, "chrompeaks")
  checkmate::assert_numeric(x$parameters$levels)
  checkmate::assert_numeric(x$parameters$mzmin, len = 1)
  checkmate::assert_numeric(x$parameters$mzmax, len = 1)
  checkmate::assert_numeric(x$parameters$minIntensity, len = 1)
  NULL
}


#' @export
#' @noRd
run.MassSpecMethod_LoadSpectra_chrompeaks <- function(
  x,
  engine = NULL
) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (sum(vapply(engine$Analyses, function(z) z$spectra_number, 0)) == 0) {
    warning("There are no spectra! Not done.")
    return(FALSE)
  }

  if (is.null(engine$ResultsList$MassSpecResults_Chromatograms)) {
    warning("No MassSpecResults_Chromatograms available! Not done.")
    return(FALSE)
  }

  browser()
  browser()
  browser()

  if (!engine$Chromatograms$has_peaks) {
    warning("No chromatograms peaks available! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters

  peaks <- engine$Chromatograms$peaks

  peaks <- data.table::rbindlist(peaks, idcol = "analysis", fill = TRUE)

  peaks$mzmin <- parameters$mzmin

  peaks$mzmax <- parameters$mzmax

  peaks$id <- peaks$peak

  tryCatch(
    {
      engine$Analyses <- load_spectra(
        engine$Analyses,
        levels = parameters$levels,
        mz = peaks,
        minIntensityMS1 = parameters$minIntensity,
        minIntensityMS2 = parameters$minIntensity
      )
      message(paste0("\U2713 ", "Spectra loaded!"))
      TRUE
    },
    error = function(e) {
      warning("Error loading spectra! Not done.")
      return(FALSE)
    }
  )
}
