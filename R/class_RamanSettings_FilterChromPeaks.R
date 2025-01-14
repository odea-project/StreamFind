#' **RamanSettings_FilterChromPeaks_native**
#'
#' @description Settings for filtering chromatographic peaks.
#' 
#' @param minIntensity Numeric (length 1) with the minimum intensity to keep a chromatographic peak.
#'
#' @return A RamanSettings_FilterChromPeaks_native object.
#'
#' @export
#'
RamanSettings_FilterChromPeaks_native <- S7::new_class(
  "RamanSettings_FilterChromPeaks_native",
  parent = ProcessingSettings,
  package = "StreamFind",
  constructor = function(minIntensity = 0) {
    S7::new_object(
      ProcessingSettings(
        engine = "Raman",
        method = "FilterChromPeaks",
        algorithm = "native",
        parameters = list(
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
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "Raman")
    checkmate::assert_choice(self@method, "FilterChromPeaks")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_number(self@parameters$minIntensity)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_FilterChromPeaks_native) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  if (!engine$spectra$has_chrom_peaks) {
    warning("No chromatographic peaks found! Not done.")
    return(FALSE)
  }
  
  minIntensity <- x$parameters$minIntensity
  
  chrom_peaks <- engine$spectra$chrom_peaks
  
  chrom_peaks <- lapply(chrom_peaks, function(z, minIntensity) {
    z <- z[z$intensity >= minIntensity, ]
    z
  }, minIntensity = minIntensity)
  
  engine$spectra$chrom_peaks <- chrom_peaks
  message(paste0("\U2713 ", "Chromatographic peaks filtered!"))
  TRUE
}
