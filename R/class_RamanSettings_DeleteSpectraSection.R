
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_DeleteSpectraSection_StreamFind**
#'
#' @description Deletes a section of the spectra between *shift* and/or *rt* minimum and maximum values.
#' 
#' @param shiftmin Numeric vector (length 1) with the minimum shift value to delete.
#' @param shiftmax Numeric vector (length 1) with the maximum shift value to delete.
#' @param rtmin Numeric vector (length 1) with the minimum retention time value to delete.
#' @param rtmax Numeric vector (length 1) with the maximum retention time value to delete.
#'
#' @return A RamanSettings_DeleteSpectraSection_StreamFind object.
#'
#' @export
#'
RamanSettings_DeleteSpectraSection_StreamFind <- S7::new_class("RamanSettings_DeleteSpectraSection_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(shiftmin = NULL,
                         shiftmax = NULL,
                         rtmin = NULL,
                         rtmax = NULL) {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "DeleteSpectraSection",
      algorithm = "StreamFind",
      parameters = list(
        shiftmin = shiftmin,
        shiftmax = shiftmax,
        rtmin = rtmin,
        rtmax = rtmax
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
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_numeric(self@parameters$shiftmin, len = 1, null.ok = TRUE)
    checkmate::assert_numeric(self@parameters$shiftmax, len = 1, null.ok = TRUE)
    checkmate::assert_numeric(self@parameters$rtmin, len = 1, null.ok = TRUE)
    checkmate::assert_numeric(self@parameters$rtmax, len = 1, null.ok = TRUE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_DeleteSpectraSection_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  shiftmin <- x$parameters$shiftmin
  shiftmax <- x$parameters$shiftmax
  rtmin <- x$parameters$rtmin
  rtmax <- x$parameters$rtmax
  
  spec_list <- engine$spectra$spectra
  
  if (!(is.null(shiftmin) && is.null(shiftmax))) {
    shiftrange <- c(shiftmin, shiftmax)
    
    spec_list <- lapply(spec_list, function(z) {
      
      if (nrow(z) > 0 && "shift" %in% colnames(z)) {
        sel <- (z$shift >= shiftrange[1]) & (z$shift <= shiftrange[2])
        if (length(sel) > 0) z <- z[!sel, ]
      }
      
      z
    })
  }
  
  if (!(is.null(rtmin) && is.null(rtmax))) {
    rtrange <- c(rtmin, rtmax)
    
    spec_list <- lapply(spec_list, function(z) {
      
      if (nrow(z) > 0 && "rt" %in% colnames(z)) {
        sel <- (z$rt >= rtrange[1]) & (z$rt <= rtrange[2])
        if (length(sel) > 0) z <- z[!sel, ]
      }
      
      z
    })
  }
  
  engine$spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra section deleted!"))
  invisible(TRUE)
}
