
#' @title .s3_delete_spectra_section.Settings_delete_spectra_section_StreamFind
#'
#' @description Deletes a spectra section based on a list of defined dimensions.
#'
#' @noRd
#'
.s3_delete_spectra_section.Settings_delete_spectra_section_StreamFind <- function(settings, self, private) {
  
  section <- settings$parameters$section
  
  if (length(section) == 0) {
    warning("Sections not found! Not done.")
    return(FALSE)
  }
  
  if (self$has_averaged_spectra()) {
    spec_list <- self$averaged_spectra
    
  } else if (self$has_spectra()) {
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
  
  if (self$has_averaged_spectra()) {
    
    private$.results$spectra$data <- Map(
      function(x, y) {
        x$average <- y
        x
      },#
      private$.results$spectra$data, spec_del
    )
    
  } else {
    spec_list <- Map(function(x, y) list("spectra" = x, "average" = y), spec_list, spec_del)
    
    self$add_results(
      list("spectra" = list(
        "data" = spec_list,
        "software" = "StreamFind",
        "version" = as.character(packageVersion("StreamFind"))
      ))
    )
  }
  
  message(paste0("\U2713 ", "Spectra section deleted!"))
  
  TRUE
}
