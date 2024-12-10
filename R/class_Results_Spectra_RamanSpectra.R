#' @export
#' @noRd
RamanSpectra <- S7::new_class(
  "RamanSpectra",
  package = "StreamFind",
  parent = Spectra,
  
  properties = list(
    
    # MARK: has_chromatograms
    ## __has_chromatograms -----
    has_chromatograms = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self$spectra) > 0) {
          return("rt" %in% colnames(self$spectra[[1]]))
        }
      }
    ),
    
    # MARK: chrom_peaks
    ## __chrom_peaks -----
    chrom_peaks = S7::new_property(S7::class_list, default = list()),
    
    # MARK: has_chrom_peaks
    ## __has_chrom_peaks -----
    has_chrom_peaks = S7::new_property(S7::class_logical, getter = function(self) length(self@chrom_peaks) > 0)
  ),
  
  constructor = function(spectra = list(),
                         is_averaged = FALSE,
                         peaks = list(),
                         chrom_peaks = list()) {
    S7::new_object(
      Spectra(), 
      name = "Spectra",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      spectra = spectra,
      is_averaged = is_averaged,
      peaks = peaks,
      chrom_peaks = chrom_peaks
    )
  },
  
  validator = function(self) {
    if (length(self@chrom_peaks) > 0) {
      for (chrom in self@chrom_peaks) {
        checkmate::assert_data_frame(chrom)
      }
    }
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, RamanSpectra) <- function(x) {
  if (length(x@spectra) > 0) {
    cat("Number spectra: ", length(x@spectra), "\n")
    cat("Averaged: ", x@is_averaged, "\n")
    if (x@has_peaks) {
      cat("Number peaks: ", vapply(x@peaks, nrow, 0), "\n")
    } else {
      cat("Number peaks: ", 0, "\n")
    }
    if (x@has_chrom_peaks) {
      cat("Number chrom peaks: ", vapply(x@chrom_peaks, nrow, 0), "\n")
    } else {
      cat("Number chrom peaks: ", 0, "\n")
    }
  } else {
    cat("Number spectra: ", 0, "\n")
  }
}
