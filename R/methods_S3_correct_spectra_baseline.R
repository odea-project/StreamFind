
#' @title .baseline_correction
#' 
#' @description Corrects the baseline from a vector using the baseline package.
#' 
#' @noRd
#' 
.baseline_correction <- function(vec, method, opts) {
  
  mat <- matrix(as.numeric(vec), nrow = 1)
  
  mat <- do.call("baseline", c(list(spectra = mat, method = method), opts))
  
  i_baseline <- as.numeric(mat@baseline)
  
  i_corrected <- as.numeric(mat@corrected)
  
  # i_baseline[i_baseline > vec] <- vec[i_baseline > vec]
  # 
  # i_baseline[i_baseline < 0] <- vec[i_baseline < 0]
  # 
  # i_corrected <- vec - i_baseline
  # 
  # i_corrected[i_corrected < 0] <- 0
  
  list("mat" = mat, "baseline" = i_baseline, "corrected" = i_corrected)
}

#' @title .s3_correct_spectra_baseline.Settings_correct_spectra_baseline_StreamFind
#'
#' @description Corrects baseline from spectra.
#'
#' @noRd
#'
.s3_correct_spectra_baseline.Settings_correct_spectra_baseline_StreamFind <- function(settings, self, private) {
  
  baseline_method <- settings$parameters$method
  
  baseline_args <- settings$parameters$args
  
  if (!(self$has_averaged_spectra() || self$has_spectra())) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  private$.results$spectra$data <- lapply(private$.results$spectra$data, function(x, baseline_method, baseline_args) {
    
    if ("average" %in% names(x)) {
      
      if (nrow(x$average) > 0) {
        
        if ("rt" %in% colnames(x$average)) {
          temp_x <- split(x$average, x$average$rt)
          
          temp_x <- lapply(temp_x, function(z) {
            baseline_data <- .baseline_correction(z$intensity, baseline_method, baseline_args)
            z$baseline <- baseline_data$baseline
            z$raw <- z$intensity
            z$intensity <- baseline_data$corrected
            z
          })
          
          x$average <- rbindlist(temp_x)
          
        } else {
          baseline_data <- .baseline_correction(x$average$intensity, baseline_method, baseline_args)
          x$average$baseline <- baseline_data$baseline
          x$average$raw <- x$average$intensity
          x$average$intensity <- baseline_data$corrected
        }
      }
      
    } else (
      
      if (nrow(x$spectra) > 0) {
        
        if (nrow(x$average) > 0) {
          
          if ("rt" %in% colnames(x$spectra)) {
            temp_x <- split(x$spectra, x$spectra$rt)
            
            temp_x <- lapply(temp_x, function(z) {
              baseline_data <- .baseline_correction(z$intensity, baseline_method, baseline_args)
              z$baseline <- baseline_data$baseline
              z$raw <- z$intensity
              z$intensity <- baseline_data$corrected
              z
            })
            
            x$spectra <- rbindlist(temp_x)
            
          } else {
            baseline_data <- .baseline_correction(x$spectra$intensity, baseline_method, baseline_args)
            x$spectra$baseline <- baseline_data$baseline
            x$spectra$raw <- x$spectra$intensity
            x$spectra$intensity <- baseline_data$corrected
          }
        }
      }
    )
    
    x
  }, baseline_method = baseline_method, baseline_args = baseline_args)
  
  message(paste0("\U2713 ", "Spectra beseline corrected!"))
  
  TRUE
}
