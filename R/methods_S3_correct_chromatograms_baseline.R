
#' @title .s3_correct_chromatograms_baseline.Settings_correct_chromatograms_baseline_airpls
#'
#' @description Corrects baseline from chromatograms using airPLS.
#'
#' @noRd
#'
.s3_correct_chromatograms_baseline.Settings_correct_chromatograms_baseline_airpls <- function(settings, self, private) {
  
  lambda = settings$parameters$lambda
  
  differences = settings$parameters$differences
  
  itermax = settings$parameters$itermax
  
  if (!self$has_chromatograms()) {
    warning("Chromatograms not found! Not done.")
    return(FALSE)
  }
  
  chrom_list <- self$chromatograms
  
  chrom_list <- lapply(chrom_list, function(x, lambda, differences, itermax) {
    
    if (nrow(x) > 0) {
      
      if ("id" %in% colnames(x)) {
        temp_x <- split(x, x$id)
        
        temp_x <- lapply(temp_x, function(z) {
          baseline_data <- .airPLS_by_zmzhang(z$intensity, lambda, differences, itermax)
          z$baseline <- baseline_data
          z$raw <- z$intensity
          baseline_data[baseline_data > z$intensity] <- z$intensity[baseline_data > z$intensity]
          z$intensity <- z$intensity - baseline_data
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        baseline_data <- .airPLS_by_zmzhang(x$intensity, lambda, differences, itermax)
        x$baseline <- baseline_data
        x$raw <- x$intensity
        baseline_data[baseline_data > x$intensity] <- x$intensity[baseline_data > x$intensity]
        x$intensity <- x$intensity - baseline_data
      }
    }
    
    x
    
  }, lambda = lambda, differences = differences, itermax = itermax)
  
  self$chromatograms <- chrom_list
  
  message(paste0("\U2713 ", "Chromatograms beseline corrected!"))
  
  TRUE
}

#' @title .s3_correct_chromatograms_baseline.Settings_correct_chromatograms_baseline_baseline
#'
#' @description Corrects baseline from spectra.
#'
#' @noRd
#'
.s3_correct_chromatograms_baseline.Settings_correct_chromatograms_baseline_baseline <- function(settings, self, private) {
  
  if (!requireNamespace("baseline", quietly = TRUE)) {
    warning("Package baseline not found but required! Not done.")
    return(FALSE)
  }
  
  baseline_method <- settings$parameters$method
  
  baseline_args <- settings$parameters$args
  
  if (!self$has_chromatograms()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  chrom_list <- self$chromatograms
  
  chrom_list <- lapply(chrom_list, function(x, baseline_method, baseline_args) {
    
    if (nrow(x) > 0) {
      
      if ("id" %in% colnames(x)) {
        temp_x <- split(x, x$id)
        
        temp_x <- lapply(temp_x, function(z) {
          baseline_data <- .baseline_correction(z$intensity, baseline_method, baseline_args)
          z$baseline <- baseline_data$baseline
          z$raw <- z$intensity
          z$intensity <- baseline_data$corrected
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        baseline_data <- .baseline_correction(x$intensity, baseline_method, baseline_args)
        x$baseline <- baseline_data$baseline
        x$raw <- x$intensity
        x$intensity <- baseline_data$corrected
      }
    }
    
    x
    
  }, baseline_method = baseline_method, baseline_args = baseline_args)
  
  self$chromatograms <- chrom_list
  
  message(paste0("\U2713 ", "Chromatograms beseline corrected!"))
  
  TRUE
}
