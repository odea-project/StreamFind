
#' @noRd
.process.RamanSettings_SubtractSpectraSection_StreamFind <- function(settings, self, private) {
  
  intensity <- NULL
  
  if (self$has_spectra()) {
    spec_list <- self$spectra
    
  } else {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  sectionVal = settings$parameters$sectionVal
  
  sectionWindow = settings$parameters$sectionWindow
  
  cached_analyses <- FALSE
  
  if (.caches_data()) {
    hash <- patRoon::makeHash(spec_list, settings)
    spec_cut <- patRoon::loadCacheData("subtract_spectra_section", hash)
    
    if (!is.null(spec_cut)) {
      check <- identical(names(spec_cut), names(spec_list))
      
      if (all(check)) {
        cached_analyses <- TRUE
        
      } else {
        spec_cut <- NULL
      }
    }
  } else {
    hash <- NULL
    spec_cut <- NULL
  }
  
  if (is.null(spec_cut)) {
    
    . <- NULL
    
    spec_cut <- lapply(spec_list, function(x) {
      
      if (nrow(x) == 0) return(data.table())
      
      res <- copy(x)
      
      if (!is.null(sectionVal) && !is.null(sectionWindow)) {
        
        if (sectionVal %in% colnames(x)) {
          
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
              
              res <- rbindlist(res_list)
            }
          }
        }
      }
      
      res
    })
    
    if (!is.null(hash)) {
      patRoon::saveCacheData("subtract_spectra_section", spec_cut, hash)
      message("\U1f5ab Subtrated spectra section cached!")
    }
  }
  
  self$spectra <- spec_cut
  
  message(paste0("\U2713 ", "Spectra section subtracted!"))
  
  TRUE
}
