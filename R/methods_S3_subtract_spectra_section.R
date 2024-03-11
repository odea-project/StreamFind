
#' @title .s3_subtract_spectra_section.Settings_subtract_spectra_section_StreamFind
#'
#' @description Subtracts a spectra section based on defined dimensions.
#'
#' @noRd
#'
.s3_subtract_spectra_section.Settings_subtract_spectra_section_StreamFind <- function(settings, self, private) {
  
  intensity <- NULL
  
  if (self$has_averaged_spectra()) {
    spec_list <- self$averaged_spectra
    
  } else if (self$has_spectra()) {
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
              
              if (nrow(res) > 0) {
                
                for (i in unique(res$rt)) res$intensity[res$rt == i] <- res$intensity[res$rt == i] - cutSec$intensity
                
              }
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
  
  if (self$has_averaged_spectra()) {
    private$.results$spectra$data <- Map(
      function(x, y) {
        x$average <- y
        x
      },#
      private$.results$spectra$data, spec_cut
    )
    
  } else {
    
    spec_list <- Map(function(x, y) list("spectra" = x, "average" = y), spec_list, spec_cut)
    
    self$add_results(
      list("spectra" = list(
        "data" = spec_list,
        "software" = "StreamFind",
        "version" = as.character(packageVersion("StreamFind"))
      ))
    )
  }
  
  message(paste0("\U2713 ", "Spectra section subtracted!"))
  
  TRUE
}
