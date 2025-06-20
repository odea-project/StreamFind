#' MassSpecMethod_IntegrateChromatograms_pracma S7 class
#'
#' @description Integrates chromatograms using the function `findpeaks` from the package
#' \pkg{pracma} with natively 
#' added peak exclusion and evaluation steps.
#' 
#' @param merge Logical (length 1) indicating if the nearby peaks should be merged.
#' @param closeByThreshold Numeric (length 1) with the maximum distance between peaks to be merged.
#' @param minPeakHeight Numeric (length 1) with the minimum peak height to be considered.
#' @param minPeakDistance Numeric (length 1) with the minimum distance between peaks.
#' @param minPeakWidth Numeric (length 1) with the minimum peak width.
#' @param maxPeakWidth Numeric (length 1) with the maximum peak width.
#' @param minSN Numeric (length 1) with the minimum signal-to-noise ratio.
#'
#' @return A MassSpecMethod_IntegrateChromatograms_pracma object.
#'
#' @export
#'
MassSpecMethod_IntegrateChromatograms_pracma <- S7::new_class(
  name = "MassSpecMethod_IntegrateChromatograms_pracma",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(merge = TRUE,
                         closeByThreshold = 45,
                         minPeakHeight = 0,
                         minPeakDistance = 10,
                         minPeakWidth = 5,
                         maxPeakWidth = 120,
                         minSN = 10) {
    
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "IntegrateChromatograms",
        required = "LoadChromatograms",
        algorithm = "pracma",
        parameters = list(
          merge = merge,
          closeByThreshold = closeByThreshold,
          minPeakHeight = minPeakHeight,
          minPeakDistance = minPeakDistance,
          minPeakWidth = minPeakWidth,
          maxPeakWidth = maxPeakWidth,
          minSN = minSN
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
    checkmate::assert_choice(self@method, "IntegrateChromatograms")
    checkmate::assert_choice(self@algorithm, "pracma")
    checkmate::assert_logical(self@parameters$merge, max.len = 1)
    checkmate::assert_number(self@parameters$closeByThreshold)
    checkmate::assert_number(self@parameters$minPeakHeight)
    checkmate::assert_number(self@parameters$minPeakDistance)
    checkmate::assert_number(self@parameters$minPeakWidth)
    checkmate::assert_number(self@parameters$maxPeakWidth)
    checkmate::assert_number(self@parameters$minSN)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_IntegrateChromatograms_pracma) <- function(x, engine = NULL) {
  
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
  
  parameters <- x$parameters
  
  chroms <- engine$Chromatograms$chromatograms
  
  chrom_peaks <- lapply(chroms, function(s) {
    
    if (nrow(s) == 0) return(data.table())
    
    s <- split(s, s$id)
    
    s <- lapply(s, function(z) {
      
      pks <- .find_peaks(
        z, "rt",
        parameters$merge,
        parameters$closeByThreshold,
        parameters$minPeakHeight,
        parameters$minPeakDistance,
        parameters$maxPeakWidth,
        parameters$minPeakWidth,
        parameters$minSN
      )
      
      if (nrow(pks) == 0) return(data.table::data.table())
      
      setnames(pks, c("xVal", "min", "max"), c("rt", "rtmin", "rtmax"))
      
      pks$id <- unique(z$id)
      
      pks$polarity <- unique(z$polarity)
      
      pks$pre_ce <- unique(z$pre_ce)
      
      pks$pre_mz <- unique(z$pre_mz)
      
      pks$pro_mz <- unique(z$pro_mz)
      
      pks$index <- unique(z$index)
      
      pks$peak <- paste0("C", pks$index ,"_P", pks$peak, "_RT", round(pks$rt, 0))
      
      setcolorder(pks, c("index", "id", "peak", "polarity", "pre_ce", "pre_mz", "pro_mz"))
      
      pks
    })
    
    all_pks <- rbindlist(s, fill = TRUE)
    
    all_pks
  })
  
  names(chrom_peaks) <- names(chroms)
  
  engine$Chromatograms$peaks <- chrom_peaks
  message(paste0("\U2713 ", "Chromatograms integrated!"))
  TRUE
}
