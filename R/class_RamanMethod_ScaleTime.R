#' **RamanMethod_ScaleTime_native**
#'
#' @description Scales the time variable of the spectra using the multiplier `value`.
#' 
#' @param value Numeric (length 1) with the multiplier to scale the time variable.
#'
#' @return A RamanMethod_ScaleTime_native object.
#'
#' @export
#'
RamanMethod_ScaleTime_native <- S7::new_class(
  "RamanMethod_ScaleTime_native",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(value = 0) {
    S7::new_object(ProcessingStep(
      engine = "Raman",
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
    ))
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "Raman")
    checkmate::assert_choice(self@method, "ScaleTime")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_numeric(self@parameters$value)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanMethod_ScaleTime_native) <- function(x, engine = NULL) {
  
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
  
  value <- x$parameters$value
  
  spec_list <- engine$Spectra$spectra
  
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
  
  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Time variable scaled!"))
  invisible(TRUE)
}
