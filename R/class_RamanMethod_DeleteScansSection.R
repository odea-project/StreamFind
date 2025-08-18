#' @title RamanMethod_DeleteScansSection_native Class
#'
#' @description Deletes a section of the spectra based on a minimum and maximum scan number. Note
#' that both values are should be positive and represent the scan unit (e.g., retention time) and
#' not the scan number.
#'
#' @param min Numeric vector (length 1) with the minimum scan value to delete.
#' @param max Numeric vector (length 1) with the maximum scan value to delete.
#'
#' @return A RamanMethod_DeleteScansSection_native object.
#'
#' @export
#'
RamanMethod_DeleteScansSection_native <- function(min = 0, max = 0) {
  x <- ProcessingStep(
    type = "Raman",
    method = "DeleteScansSection",
    required = NA_character_,
    algorithm = "native",
    parameters = list(
      min = min,
      max = max
    ),
    number_permitted = Inf,
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
    stop("Invalid RamanMethod_DeleteScansSection_native object!")
  }
}

#' @export
#' @noRd
validate_object.RamanMethod_DeleteScansSection_native <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "DeleteScansSection")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$min, len = 1)
  checkmate::assert_numeric(x$parameters$max, len = 1)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_DeleteScansSection_native <- function(x, engine = NULL) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["RamanResults_Spectra"]])) {
    engine$Results <- RamanResults_Spectra(
      lapply(engine$Analyses$analyses, function(a) a$spectra)
    )
  }
  spec_obj <- engine$Results[["RamanResults_Spectra"]]
  rtmin <- x$parameters$min
  rtmax <- x$parameters$max
  spec_list <- spec_obj$spectra
  if (rtmax > 0) {
    rtrange <- c(rtmin, rtmax)
    rtrange <- sort(rtrange)
    spec_list <- lapply(spec_list, function(z) {
      if (nrow(z) > 0 && "rt" %in% colnames(z)) {
        sel <- (z$rt >= rtrange[1]) & (z$rt <= rtrange[2])
        if (length(sel) > 0) z <- z[!sel, ]
      } else {
        warning("No retention time column available for deleting scans!")
      }
      z
    })
  }
  spec_obj$Spectra <- spec_list
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Spectra section deleted!"))
  invisible(TRUE)
}
