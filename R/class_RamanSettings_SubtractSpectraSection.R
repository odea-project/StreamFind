
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_SubtractSpectraSection_StreamFind**
#'
#' @description Subtracts a section of the spectra based on a variable (i.e. column name).
#' 
#' @param sectionVal Character (length 1) with the variable to be used for sectioning.
#' @param sectionWindow Numeric (length 2) with the window for the sectioning.
#'
#' @return A RamanSettings_SubtractSpectraSection_StreamFind object.
#'
#' @export
#'
RamanSettings_SubtractSpectraSection_StreamFind <- S7::new_class("RamanSettings_SubtractSpectraSection_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(sectionVal = "rt", sectionWindow = c(10, 200)) {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "SubtractSpectraSection",
      algorithm = "StreamFind",
      parameters = list(
        sectionVal = sectionVal,
        sectionWindow = sectionWindow
      ),
      number_permitted = 1,
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
    checkmate::assert_choice(self@method, "SubtractSpectraSection")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_character(self@parameters$sectionVal, min.len = 1)
    checkmate::assert_numeric(self@parameters$sectionWindow, len = 2)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_SubtractSpectraSection_StreamFind) <- function(x, engine = NULL) {
  
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
  
  sectionVal = x$parameters$sectionVal
  sectionWindow = x$parameters$sectionWindow
  
  spec_list <- engine$spectra$spectra
  
  intensity <- NULL
  . <- NULL
  
  spec_cut <- lapply(spec_list, function(z) {
    
    if (nrow(z) == 0) return(data.table::data.table())
    
    res <- data.table::copy(z)
    
    if (!is.null(sectionVal) && !is.null(sectionWindow)) {
      
      if (sectionVal %in% colnames(z)) {
        
        if (length(sectionWindow) == 2 && is.numeric(sectionWindow)) {
          
          sectionWindow <- sort(sectionWindow)
          
          cutSec <- res[res[[sectionVal]] >= sectionWindow[1] & res[[sectionVal]] <= sectionWindow[2], ]
          
          if (nrow(cutSec) > 0) {
            
            cutSec <- cutSec[, .(intensity = mean(intensity)), by = "shift"]
            
            res <- res[res[[sectionVal]] < sectionWindow[1] | res[[sectionVal]] > sectionWindow[2], ]
            
            res_list <- split(res, res$rt)
            
            res_list <- lapply(res_list, function(z, cutSec) {
              z$intensity <- z$intensity - cutSec$intensity
              z
            }, cutSec = cutSec)
            
            res <- data.table::rbindlist(res_list)
          }
        }
      }
    }
    
    res
  })
  
  engine$spectra$spectra <- spec_cut
  message(paste0("\U2713 ", "Spectra section subtracted!"))
  TRUE
}
