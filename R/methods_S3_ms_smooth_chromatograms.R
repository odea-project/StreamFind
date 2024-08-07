
#' @noRd
.process.MassSpecSettings_SmoothChromatograms_savgol <- function(settings, self, private) {
  
  if (!requireNamespace("pracma", quietly = TRUE)) {
    warning("Package pracma not found but required! Not done.")
    return(FALSE)
  }
  
  fl <- settings$parameters$fl
  
  forder <- settings$parameters$forder
  
  dorder <- settings$parameters$dorder
  
  if (!self$has_chromatograms()) {
    warning("Chromatograms not found! Not done.")
    return(FALSE)
  }
  
  chrom_list <- self$chromatograms
  
  chrom_list <- lapply(chrom_list, function(x, fl, forder, dorder) {
    
    if (nrow(x) > 0) {
      
      if ("id" %in% colnames(x)) {
        temp_x <- split(x, x$id)
        
        temp_x <- lapply(temp_x, function(z) {
          z$intensity <- pracma::savgol(z$intensity, fl = fl, forder = forder, dorder = dorder)
          z
        })
        
        x <- data.table::rbindlist(temp_x)
        
      } else {
        x$intensity <- pracma::savgol(x$intensity, fl = fl, forder = forder, dorder = dorder)
      }
    }
    
    x
    
  }, fl = fl, forder = forder, dorder = dorder)
  
  self$chromatograms <- chrom_list
  
  message(paste0("\U2713 ", "Chromatograms smoothed!"))
  
  TRUE
}



#' @noRd
.process.MassSpecSettings_SmoothChromatograms_movingaverage <- function(settings, self, private) {
  
  windowSize <- settings$parameters$windowSize
  
  if (!self$has_chromatograms()) {
    warning("Chromatograms not found! Not done.")
    return(FALSE)
  }
  
  chrom_list <- self$chromatograms
  
  chrom_list <- lapply(chrom_list, function(x, windowSize) {
    
    if (nrow(x) > 0) {
      
      if ("id" %in% colnames(x)) {
        temp_x <- split(x, x$id)
        
        temp_x <- lapply(temp_x, function(z) {
          z$intensity <- .moving_average(z$intensity, windowSize = windowSize)
          z
        })
        
        x <- data.table::rbindlist(temp_x)
        
      } else {
        x$intensity <- .moving_average(x$intensity, windowSize = windowSize)
      }
    }
    
    x
    
  }, windowSize = windowSize)
  
  self$chromatograms <- chrom_list
  
  message(paste0("\U2713 ", "Chromatograms smoothed!"))
  
  TRUE
}
