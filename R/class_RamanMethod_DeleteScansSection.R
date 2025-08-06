#' **RamanMethod_DeleteScansSection_native**
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
RamanMethod_DeleteScansSection_native <- S7::new_class(
  "RamanMethod_DeleteScansSection_native",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  
  constructor = function(min = 0, max = 0) {
    S7::new_object(ProcessingStep(
      data_type = "Raman",
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
    ))
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "Raman")
    checkmate::assert_choice(self@method, "DeleteScansSection")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_numeric(self@parameters$min, len = 1)
    checkmate::assert_numeric(self@parameters$max, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanMethod_DeleteScansSection_native) <- function(x, engine = NULL) {
  
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
  
  rtmin <- x$parameters$min
  rtmax <- x$parameters$max
  
  spec_list <- engine$Spectra$spectra
  
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
  
  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra section deleted!"))
  invisible(TRUE)
}
