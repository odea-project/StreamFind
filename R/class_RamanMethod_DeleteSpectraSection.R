#' **RamanMethod_DeleteSpectraSection_native**
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
RamanMethod_DeleteSpectraSection_native <- S7::new_class(
  "RamanMethod_DeleteSpectraSection_native",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(min = NULL, max = NULL) {
    S7::new_object(ProcessingStep(
      engine = "Raman",
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
    ))
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "Raman")
    checkmate::assert_choice(self@method, "DeleteSpectraSection")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_numeric(self@parameters$min, len = 1)
    checkmate::assert_numeric(self@parameters$max, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanMethod_DeleteSpectraSection_native) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$Analyses$has_spectra) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  shiftmin <- x$parameters$min
  shiftmax <- x$parameters$max
  
  spec_list <- engine$Spectra$spectra
  
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
  
  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra section deleted!"))
  invisible(TRUE)
}
