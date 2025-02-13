#' **RamanMethod_InvertAxis_intensity**
#'
#' @description Inverts the intensity axis of the spectra.
#'
#' @return A RamanMethod_InvertAxis_intensity object.
#'
#' @export
#'
RamanMethod_InvertAxis_intensity <- S7::new_class(
  "RamanMethod_InvertAxis_intensity",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function() {
    S7::new_object(ProcessingStep(
      engine = "Raman",
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
    ))
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "Raman")
    checkmate::assert_choice(self@method, "InvertAxis")
    checkmate::assert_choice(self@algorithm, "intensity")
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanMethod_InvertAxis_intensity) <- function(x, engine = NULL) {
  
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
  
  spec_list <- engine$Spectra$spectra
  
  spec_list <- lapply(spec_list, function(z) {
    z$intensity <- rev(z$intensity)
    z
  })
  
  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra section deleted!"))
  invisible(TRUE)
}
