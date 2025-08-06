#' **RamanMethod_SubtractScansSection_native**
#'
#' @description Subtracts the spectra of a section of the scans to all other spectra.
#' 
#' @param sectionWindow Numeric (length 2) with the window for the sectioning.
#'
#' @return A RamanMethod_SubtractScansSection_native object.
#'
#' @export
#'
RamanMethod_SubtractScansSection_native <- S7::new_class(
  name = "RamanMethod_SubtractScansSection_native",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  constructor = function(sectionWindow = c(10, 200)) {
    S7::new_object(
      ProcessingStep(
        data_type = "Raman",
        method = "SubtractScansSection",
        algorithm = "native",
        parameters = list(
          sectionWindow = sectionWindow
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "Raman")
    checkmate::assert_choice(self@method, "SubtractScansSection")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_numeric(self@parameters$sectionWindow, len = 2)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanMethod_SubtractScansSection_native) <- function(x, engine = NULL) {
  
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
  
  sectionWindow = x$parameters$sectionWindow
  spec_list <- engine$Spectra$spectra
  
  intensity <- NULL
  . <- NULL
  
  spec_cut <- lapply(spec_list, function(z) {
    
    if (nrow(z) == 0) return(data.table::data.table())
    if (!"rt" %in% colnames(z)) {
      warning("No 'rt' column in the spectra for finding scans session!")
      return(data.table::data.table())
    }
    
    res <- data.table::copy(z)
    
    if (length(sectionWindow) == 2 && is.numeric(sectionWindow)) {
      sectionWindow <- sort(sectionWindow)
      cutSec <- res[res$rt >= sectionWindow[1] & res$rt <= sectionWindow[2], ]
      if (nrow(cutSec) > 0) {
        cutSec <- cutSec[, .(intensity = mean(intensity)), by = "shift"]
        res <- res[res$rt < sectionWindow[1] | res$rt > sectionWindow[2], ]
        res_list <- split(res, res$rt)
        res_list <- lapply(res_list, function(z, cutSec) {
          z$intensity <- z$intensity - cutSec$intensity
          z
        }, cutSec = cutSec)
        res <- data.table::rbindlist(res_list)
      }
    }
    
    res
  })
  
  engine$Spectra$spectra <- spec_cut
  message(paste0("\U2713 ", "Scans section subtracted!"))
  TRUE
}
