#' @title RamanMethod_ScaleTime_native Class
#'
#' @description Scales the time variable of the spectra using the multiplier `value`.
#'
#' @param value Numeric (length 1) with the multiplier to scale the time variable.
#'
#' @return A RamanMethod_ScaleTime_native object.
#'
#' @export
#'
RamanMethod_ScaleTime_native <- function(value = 0) {
  x <- ProcessingStep(
    type = "Raman",
    method = "ScaleTime",
    required = NA_character_,
    algorithm = "native",
    parameters = list(
      value = as.numeric(value)
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
    stop("Invalid RamanMethod_ScaleTime_native object!")
  }
}

#' @export
#' @noRd
validate_object.RamanMethod_ScaleTime_native <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "ScaleTime")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$value)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_ScaleTime_native <- function(x, engine = NULL) {
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
  value <- x$parameters$value
  spec_list <- spec_obj$spectra
  if (value > 0) {
    spec_list <- lapply(spec_list, function(z) {
      if (nrow(z) > 0 && "rt" %in% colnames(z)) {
        z$rt <- z$rt * value
      } else {
        warning("No time column available for scaling!")
      }
      z
    })
  }
  spec_obj$spectra <- spec_list
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Time variable scaled!"))
  invisible(TRUE)
}
