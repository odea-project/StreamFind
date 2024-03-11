
#' @title .s3_normalize_spectra.Settings_normalize_spectra_StreamFind
#'
#' @description Deletes a spectra section based on a list of defined dimensions.
#'
#' @noRd
#'
.s3_normalize_spectra.Settings_normalize_spectra_StreamFind <- function(settings, self, private) {
  
  if (!(self$has_averaged_spectra() || self$has_spectra())) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  private$.results$spectra$data <- lapply(private$.results$spectra$data, function(x) {
    
    if ("average" %in% names(x)) {
      
      if (nrow(x$average) > 0) {
        
        if ("rt" %in% colnames(x$average)) {
          temp_x <- split(x$average, x$average$rt)
          
          temp_x <- lapply(temp_x, function(z) {
            max_int <- max(z$intensity)
            z$intensity <- z$intensity / max_int
            z
          })
          
          x$average <- rbindlist(temp_x)
          
        } else {
          max_int <- max(x$average$intensity)
          x$average$intensity <- x$average$intensity / max_int
        }
      }
      
    } else (
      
      if (nrow(x$spectra) > 0) {
        
        if (nrow(x$average) > 0) {
          
          if ("rt" %in% colnames(x$spectra)) {
            temp_x <- split(x$spectra, x$spectra$rt)
            
            temp_x <- lapply(temp_x, function(z) {
              max_int <- max(z$intensity)
              z$intensity <- z$intensity / max_int
              z
            })
            
            x$spectra <- rbindlist(temp_x)
            
          } else {
            max_int <- max(x$average$intensity)
            x$average$intensity <- x$average$intensity / max_int
          }
        }
      }
    )
    
    x
  })
  
  message(paste0("\U2713 ", "Spectra normalized!"))
  
  TRUE
}
