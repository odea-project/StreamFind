#' @title RamanMethod_DeleteSpectraSection_native Class
#'
#' @description Deletes a section of the spectra between *shift* minimum and maximum values.
#'
#' @param min Numeric vector (length 1) with the minimum shift value to delete.
#' @param max Numeric vector (length 1) with the maximum shift value to delete.
#'
#' @return A RamanMethod_DeleteSpectraSection_native object.
#'
#' @export
#'
RamanMethod_DeleteSpectraSection_native <- function(min = NULL, max = NULL) {
  x <- ProcessingStep(
    type = "Raman",
    method = "DeleteSpectraSection",
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
    stop("Invalid RamanMethod_DeleteSpectraSection_native object!")
  }
}

#' @describeIn RamanMethod_DeleteSpectraSection_native Validate the RamanMethod_DeleteSpectraSection_native object, returning NULL if valid.
#' @param x A RamanMethod_DeleteSpectraSection_native object.
#' @export
#'
validate_object.RamanMethod_DeleteSpectraSection_native <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "DeleteSpectraSection")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$min, len = 1)
  checkmate::assert_numeric(x$parameters$max, len = 1)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_DeleteSpectraSection_native <- function(x, engine = NULL) {
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
  shiftmin <- x$parameters$min
  shiftmax <- x$parameters$max
  spec_list <- spec_obj$spectra
  if (!(is.null(shiftmin) && is.null(shiftmax))) {
    shiftrange <- c(shiftmin, shiftmax)
    shiftrange <- sort(shiftrange)
    spec_list <- lapply(spec_list, function(z) {
      if (nrow(z) > 0 && "shift" %in% colnames(z)) {
        sel <- (z$shift >= shiftrange[1]) & (z$shift <= shiftrange[2])
        if (length(sel) > 0) z <- z[!sel, ]
      } else {
        warning("No shift column in spectra for deleting section!")
      }
      z
    })
  }
  spec_obj$spectra <- spec_list
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Spectra section deleted!"))
  invisible(TRUE)
}
