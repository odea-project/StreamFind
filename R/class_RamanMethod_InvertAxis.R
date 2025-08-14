#' @title RamanMethod_InvertAxis_intensity Class
#'
#' @description Inverts the intensity axis of the spectra.
#'
#' @return A RamanMethod_InvertAxis_intensity object.
#'
#' @export
#'
RamanMethod_InvertAxis_intensity <- function() {
  x <- ProcessingStep(
    type = "Raman",
    method = "InvertAxis",
    required = NA_character_,
    algorithm = "intensity",
    parameters = list(),
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
    stop("Invalid RamanMethod_InvertAxis_intensity object!")
  }
}

#' @describeIn RamanMethod_InvertAxis_intensity Validate the RamanMethod_InvertAxis_intensity object, returning NULL if valid.
#' @param x A RamanMethod_InvertAxis_intensity object.
#' @export
#'
validate_object.RamanMethod_InvertAxis_intensity = function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "InvertAxis")
  checkmate::assert_choice(x$algorithm, "intensity")
  NextMethod()
  NULL
}


#' @export
#' @noRd
run.RamanMethod_InvertAxis_intensity <- function(x, engine = NULL) {
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
  spec_list <- spec_obj$spectra
  spec_list <- lapply(spec_list, function(z) {
    z$intensity <- rev(z$intensity)
    z
  })
  spec_obj$spectra <- spec_list
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Spectra section deleted!"))
  invisible(TRUE)
}
