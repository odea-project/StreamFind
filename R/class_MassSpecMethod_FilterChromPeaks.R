#' **MassSpecMethod_FilterChromPeaks_native**
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
MassSpecMethod_FilterChromPeaks_native <- S7::new_class(
  "MassSpecMethod_FilterChromPeaks_native",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(minIntensity = 0,
                         retentionTimeRange = c(NA_real_, NA_real_)) {
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
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
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "FilterChromPeaks")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_number(self@parameters$minIntensity)
    checkmate::assert_numeric(self@parameters$retentionTimeRange, len = 2)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_FilterChromPeaks_native) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_chromatograms()) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  
  if (!engine$chromatograms$has_peaks) {
    warning("No chromatographic peaks found! Not done.")
    return(FALSE)
  }
  
  minIntensity <- x$parameters$minIntensity
  retentionTimeRange <- x$parameters$retentionTimeRange
  
  chrom_peaks <- engine$chromatograms$peaks
  
  chrom_peaks <- lapply(chrom_peaks, function(z, minIntensity, retentionTimeRange) {
    z <- z[z$intensity >= minIntensity, ]
    
    if (!is.na(retentionTimeRange[1])) {
      z <- z[z$rt >= retentionTimeRange[1], ]
    }
    
    if (!is.na(retentionTimeRange[2])) {
      z <- z[z$rt <= retentionTimeRange[2], ]
    }
    
    z
    
  }, minIntensity = minIntensity, retentionTimeRange = retentionTimeRange)
  
  engine$chromatograms$peaks <- chrom_peaks
  message(paste0("\U2713 ", "Chromatographic peaks filtered!"))
  TRUE
}
