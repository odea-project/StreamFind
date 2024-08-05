
#' @noRd
.process.MassSpecSettings_NormalizeSpectra_minmax <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if (("rt" %in% colnames(x)) && ("shift" %in% colnames(x))) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          max_int <- max(z$intensity)
          min_int <- min(z$intensity)
          z$intensity <- (z$intensity - min_int) / (max_int - min_int + abs(min_int))
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        max_int <- max(x$intensity)
        min_int <- min(x$intensity)
        x$intensity <- (x$intensity - min_int) / (max_int - min_int)
      }
    }

    x
    
  })
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra normalized!"))
  
  TRUE
}



#' @noRd
.process.MassSpecSettings_NormalizeSpectra_snv <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  liftTozero <- settings$parameters$liftTozero
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if (("rt" %in% colnames(x)) && ("shift" %in% colnames(x))) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          mean_int <- mean(z$intensity)
          sd_int <- sd(z$intensity)
          z$intensity <- (z$intensity - mean_int) / sd_int
          if (liftTozero) z$intensity <- z$intensity + abs(min(z$intensity))
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        mean_int <- mean(x$intensity)
        sd_int <- sd(x$intensity)
        x$intensity <- (x$intensity - mean_int) / sd_int
        if (liftTozero) x$intensity <- x$intensity + abs(min(x$intensity))
      }
    }
    
    x
    
  })
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra normalized!"))
  
  TRUE
}



#' @noRd
.process.MassSpecSettings_NormalizeSpectra_scale <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }

  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if (("rt" %in% colnames(x)) && ("shift" %in% colnames(x))) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          sd_int <- sd(z$intensity)
          z$intensity <- z$intensity / sd_int
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        sd_int <- sd(x$intensity)
        x$intensity <- x$intensity / sd_int
      }
    }
    
    x
    
  })
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra normalized!"))
  
  TRUE
}



#' @noRd
.process.MassSpecSettings_NormalizeSpectra_blockweight <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if (("rt" %in% colnames(x)) && ("shift" %in% colnames(x))) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          z$intensity <- z$intensity / sqrt(length(z$intensity))
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        x$intensity <- x$intensity / sqrt(length(x$intensity))
      }
    }
    
    x
    
  })
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra normalized!"))
  
  TRUE
}



#' @noRd
.process.MassSpecSettings_NormalizeSpectra_meancenter <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if (("rt" %in% colnames(x)) && ("shift" %in% colnames(x))) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          mean_int <- mean(z$intensity)
          z$intensity <- z$intensity - mean_int
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        mean_int <- mean(x$intensity)
        x$intensity <- x$intensity / mean_int
      }
    }
    
    x
    
  })
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra normalized!"))
  
  TRUE
}
