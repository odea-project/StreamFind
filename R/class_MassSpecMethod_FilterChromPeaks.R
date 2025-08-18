#' @title MassSpecMethod_FilterChromPeaks_native Class
#'
#' @description Settings for filtering chromatographic peaks.
#'
#' @param minIntensity Numeric (length 1) with the minimum intensity to keep a chromatographic peak.
#' @param retentionTimeRange Numeric (length 2) with the retention time range to keep
#' chromatographic peaks. The first element is the minimum retention time and the second element
#' is the maximum retention time, in seconds.
#'
#' @return A MassSpecMethod_FilterChromPeaks_native object.
#'
#' @export
#'
MassSpecMethod_FilterChromPeaks_native <- function(
  minIntensity = 0,
  retentionTimeRange = c(NA_real_, NA_real_)
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FilterChromPeaks",
    algorithm = "native",
    parameters = list(
      minIntensity = minIntensity,
      retentionTimeRange = retentionTimeRange
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
    stop("Invalid MassSpecMethod_FilterChromPeaks_native object!")
  }
}

#' @export
#' @noRd
#'
validate_object.MassSpecMethod_FilterChromPeaks_native <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "FilterChromPeaks")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_number(x$parameters$minIntensity)
  checkmate::assert_numeric(x$parameters$retentionTimeRange, len = 2)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_FilterChromPeaks_native <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["MassSpecResults_Chromatograms"]])) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  chrom_obj <- engine$Results[["MassSpecResults_Chromatograms"]]
  if (length(chrom_obj$peaks) == 0) {
    warning("No chromatograms peaks available! Not done.")
    return(FALSE)
  }
  minIntensity <- x$parameters$minIntensity
  retentionTimeRange <- x$parameters$retentionTimeRange
  chrom_peaks <- chrom_obj$peaks
  chrom_peaks <- lapply(
    chrom_peaks,
    function(z, minIntensity, retentionTimeRange) {
      z <- z[z$intensity >= minIntensity, ]

      if (!is.na(retentionTimeRange[1])) {
        z <- z[z$rt >= retentionTimeRange[1], ]
      }

      if (!is.na(retentionTimeRange[2])) {
        z <- z[z$rt <= retentionTimeRange[2], ]
      }

      z
    },
    minIntensity = minIntensity,
    retentionTimeRange = retentionTimeRange
  )
  chrom_obj$peaks <- chrom_peaks
  engine$Results <- chrom_obj
  message(paste0("\U2713 ", "Chromatographic peaks filtered!"))
  TRUE
}
