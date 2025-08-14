#' @title RamanResults_Spectra Class
#' @description The `RamanResults_Spectra` class is used to store Raman spectra results, including peak information and whether the spectra are averaged.
#' @param spectra A list of spectra data, where each element is a `data.table` representing a Raman spectrum.
#' @param is_averaged A logical value indicating whether the spectra are averaged.
#' @param peaks A list of peaks, where each element is a `data.table` representing the peaks for a corresponding spectrum.
#' @param chrom_peaks A list where each element is a `data.table` representing the chromatographic peaks found in each spectrum, when a time dimension is present.
#' @export
#' @seealso [StreamFind::Results] for the parent class.
#' 
RamanResults_Spectra <- function(
  spectra = list(),
  is_averaged = FALSE,
  peaks = list(),
  chrom_peaks = list()
) {
  x <- structure(
    list(
      type = "Raman",
      name = "RamanResults_Spectra",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      spectra = spectra,
      is_averaged = is_averaged,
      peaks = peaks,
      chrom_peaks = chrom_peaks
    ),
    class = c("RamanResults_Spectra", "Results")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid RamanResults_Spectra object!")
  }
}

#' @describeIn RamanResults_Spectra Validate the RamanResults_Spectra object, returns NULL if valid.
#' @param x A `RamanResults_Spectra` object.
#' @export
#'
validate_object.RamanResults_Spectra <- function(x) {
  checkmate::assert_true(identical(class(x) , c("RamanResults_Spectra", "Results")))
  if (length(x$peaks) > 0) {
    checkmate::assert_true(length(x$spectra) == length(x$peaks))
    for (peak in x$peaks) {
      checkmate::assert_data_frame(peak)
    }
  }
  if (length(x$chrom_peaks) > 0) {
    checkmate::assert_true(length(x$spectra) == length(x$chrom_peaks))
    for (chrom in x$chrom_peaks) {
      checkmate::assert_data_frame(chrom)
    }
  }
  NextMethod()
  NULL
}

#' @describeIn RamanResults_Spectra Show the RamanResults_Spectra object.
#' @param x A `RamanResults_Spectra` object.
#' @export
#'
show.RamanResults_Spectra <- function(x) {
  if (length(x$spectra) > 0) {
    cat("Number spectra: ", length(x$spectra), "\n")
    cat("Averaged: ", x$is_averaged, "\n")
    if (x$has_peaks) {
      cat("Number peaks: ", vapply(x$peaks, nrow, 0), "\n")
    } else {
      cat("Number peaks: ", 0, "\n")
    }
    if (x$has_chrom_peaks) {
      cat("Number chrom peaks: ", vapply(x$chrom_peaks, nrow, 0), "\n")
    } else {
      cat("Number chrom peaks: ", 0, "\n")
    }
  } else {
    cat("Number spectra: ", 0, "\n")
  }
}

#' @describeIn RamanResults_Spectra Subset the `RamanResults_Spectra` object by analyses. The argument `i` can be a character vector with the names of the analyses or a numeric vector with the indices of the analyses to keep.
#' @param x A `RamanResults_Spectra` object.
#' @template arg-i
#' @export
#' 
`[.RamanResults_Spectra` <- function(x, i) {
  x$spectra <- x$spectra[i]
  if (length(x$peaks) > 0) x$peaks <- x$peaks[i]
  if (length(x$chrom_peaks) > 0) x$chrom_peaks <- x$chrom_peaks[i]
  x
}

#' @describeIn RamanResults_Spectra Extract a single analysis from the `RamanResults_Spectra` object. The argument `i` can be a character vector with the name of the analysis or a numeric vector with the index of the analysis to extract.
#' @param x A `RamanResults_Spectra` object.
#' @export
#' 
`[[.RamanResults_Spectra` <- function(x, i) {
  x$spectra <- x$spectra[i]
  if (length(x$peaks) > 0) x$peaks <- x$peaks[i]
  if (length(x$chrom_peaks) > 0) x$chrom_peaks <- x$chrom_peaks[i]
  x
}
