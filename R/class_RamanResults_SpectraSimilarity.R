#' @title RamanResults_SpectraSimilarity Class
#' @description The `RamanResults_SpectraSimilarity` class is used to store results of similarity analysis between Raman spectra.
#' @param data A list of data tables, where each table contains the results of a similarity analysis for a Raman spectrum.
#' @export
#' 
RamanResults_SpectraSimilarity <- function(data = list()) {
  x <- structure(
    list(
      type = "Raman",
      name = "RamanResults_SpectraSimilarity",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      data = data
    ),
    class = c("RamanResults_SpectraSimilarity", "Results")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid RamanResults_SpectraSimilarity object.")
  }
}

#' @describeIn RamanResults_SpectraSimilarity Validate the RamanResults_SpectraSimilarity object, returns NULL if valid.
#' @param x A `RamanResults_SpectraSimilarity` object.
#' @export
#' 
validate_object.RamanResults_SpectraSimilarity = function(x) {
  checkmate::assert_true(identical(class(x), c("RamanResults_SpectraSimilarity", "Results")))
  checkmate::assert_true(x$name == "RamanResults_SpectraSimilarity")
  checkmate::assert_true(x$software == "StreamFind")
  checkmate::assert_list(x$data)
  if (length(x$data) > 0) {
    lapply(x$data, checkmate::assert_data_table)
  }
  NextMethod()
  NULL
}

#' @describeIn RamanResults_SpectraSimilarity Show the RamanResults_SpectraSimilarity object.
#' @param x A `RamanResults_SpectraSimilarity` object.
#' @export
#' 
show.RamanResults_SpectraSimilarity <- function(x) {
  data.table::rbindlist(x$data, idcol = "analysis")[, 1:3]
}
