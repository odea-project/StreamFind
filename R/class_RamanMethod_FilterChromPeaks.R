#' **RamanMethod_FilterChromPeaks_native**
#'
#' @description Settings for filtering chromatographic peaks.
#' 
#' @param minIntensity Numeric (length 1) with the minimum intensity to keep a chromatographic peak.
#' @param minSignalNoiseRatio Numeric (length 1) with the minimum signal-to-noise ratio to keep a
#' chromatographic peak.
#' @param rtRange Numeric (length 2) with the retention time range to keep chromatographic peaks.
#'
#' @return A RamanMethod_FilterChromPeaks_native object.
#'
#' @export
#'
RamanMethod_FilterChromPeaks_native <- S7::new_class(
  "RamanMethod_FilterChromPeaks_native",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  constructor = function(minIntensity = 0,
                         minSignalNoiseRatio = 0,
                         rtRange = c(0, 0)) {
    S7::new_object(
      ProcessingStep(
        data_type = "Raman",
        method = "FilterChromPeaks",
        algorithm = "native",
        parameters = list(
          minIntensity = minIntensity,
          minSignalNoiseRatio = minSignalNoiseRatio,
          rtRange = rtRange
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "Raman")
    checkmate::assert_choice(self@method, "FilterChromPeaks")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_number(self@parameters$minIntensity)
    checkmate::assert_number(self@parameters$minSignalNoiseRatio)
    checkmate::assert_numeric(self@parameters$rtRange, len = 2)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanMethod_FilterChromPeaks_native) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$Analyses$has_spectra) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  if (!engine$Spectra$has_chrom_peaks) {
    warning("No chromatographic peaks found! Not done.")
    return(FALSE)
  }
  
  minIntensity <- x$parameters$minIntensity
  minSN <- x$parameters$minSignalNoiseRatio
  rtr <- sort(x$parameters$rtRange)
  
  chrom_peaks <- engine$Spectra$chrom_peaks
  
  chrom_peaks <- lapply(chrom_peaks, function(z, minIntensity, minSN) {
    z <- z[z$intensity >= minIntensity, ]
    z <- z[z$sn >= minSN, ]
    if (rtr[2] > 0) {
      z <- z[z$rt >= rtr[1] & z$rt <= rtr[2], ]
    }
    z
  }, minIntensity = minIntensity, minSN = minSN)
  
  engine$Spectra$chrom_peaks <- chrom_peaks
  message(paste0("\U2713 ", "Chromatographic peaks filtered!"))
  TRUE
}
