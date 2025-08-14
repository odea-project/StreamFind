#' @title MassSpecMethod_LoadChromatograms_native Class
#'
#' @description Loads chromatograms from mass spectrometry analyses.
#'
#' @param chromatograms A numeric or character vector with the chromatogram indices or names to be
#' used, respectively.
#' @param rtmin A numeric vector with the minimum retention time values to be used.
#' @param rtmax A numeric vector with the maximum retention time values to be used.
#' @param minIntensity A numeric value with the minimum intensity to be used.
#'
#' @return A `MassSpecMethod_LoadChromatograms_native` object.
#'
#' @export
#'
MassSpecMethod_LoadChromatograms_native <- function(
  chromatograms = 1,
  rtmin = 0,
  rtmax = 0,
  minIntensity = 0
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "LoadChromatograms",
    required = NA_character_,
    algorithm = "native",
    parameters = list(
      chromatograms = chromatograms,
      rtmin = rtmin,
      rtmax = rtmax,
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
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_LoadChromatograms_native object!")
  }
}

#' @describeIn MassSpecMethod_LoadChromatograms_native Validate the MassSpecMethod_LoadChromatograms_native object, returning NULL if valid.
#' @param x A MassSpecMethod_LoadChromatograms_native object.
#' @export
#'
validate_object.MassSpecMethod_LoadChromatograms_native <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "LoadChromatograms")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_true(
    is.numeric(x$parameters$chromatograms) ||
      is.character(x$parameters$chromatograms)
  )
  checkmate::assert_numeric(x$parameters$rtmin, len = 1)
  checkmate::assert_numeric(x$parameters$rtmax, len = 1)
  checkmate::assert_numeric(x$parameters$minIntensity, len = 1)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_LoadChromatograms_native <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (sum(vapply(engine$Analyses$analyses, function(z) z$chromatograms_number, 0)) == 0) {
    warning("There are no chromatograms! Not done.")
    return(FALSE)
  }
  parameters <- x$parameters
  analyses <- engine$Analyses
  analyses <- load_chromatograms(
    analyses,
    chromatograms = parameters$chromatograms,
    rtmin = parameters$rtmin,
    rtmax = parameters$rtmax,
    minIntensity = parameters$minIntensity
  )
  if (!is.null(analyses$results[["MassSpecResults_Chromatograms"]])) {
    engine$Analyses <- analyses
    message(paste0("\U2713 ", "Chromatograms loaded!"))
    TRUE
  } else {
    warning("Failed to load chromatograms!")
    FALSE
  }
}
