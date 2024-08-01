
#' @noRd
.process.RamanSettings_DeleteSpectraSection_StreamFind <- function(settings, self, private) {
  
  section <- settings$parameters$section
  
  if (length(section) == 0) {
    warning("Sections not found! Not done.")
    return(FALSE)
  }
  
  if (self$has_spectra()) {
    spec_list <- self$spectra
    
  } else {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_del <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      sel <- logical()
      
      for (i in names(section)) {
        if (i %in% colnames(x)) {
          section[[i]] <- sort(section[[i]])
          
          if (length(sel) == 0) {
            sel <- (x[[i]] >= section[[i]][1]) & (x[[i]] <= section[[i]][2])
            
          } else {
            sel <- sel & (x[[i]] >= section[[i]][1]) & (x[[i]] <= section[[i]][2])
          }
        }
      }
      
      if (length(sel) > 0) x <- x[!sel, ]
    }
    
    x
  })
  
  self$spectra <- spec_del
  
  message(paste0("\U2713 ", "Spectra section deleted!"))
  
  TRUE
}
