
#' @title .s3_normalize_spectra.Settings_normalize_spectra_StreamFind
#'
#' @description Deletes a spectra section based on a list of defined dimensions.
#'
#' @noRd
#'
.s3_normalize_spectra.Settings_normalize_spectra_StreamFind <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  liftTozero <- settings$parameters$liftTozero
  
  xName <- settings$parameters$xName
  
  xVal <- settings$parameters$xVal
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if ("rt" %in% colnames(x)) {
        
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          
          if (liftTozero) z$intensity <- z$intensity + abs(min(z$intensity))
          
          max_int <- max(z$intensity)
          z$intensity <- z$intensity / max_int
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        
        if (liftTozero) x$intensity <- x$intensity + abs(min(x$intensity))
        
        max_int <- max(x$intensity)
        x$intensity <- x$intensity / max_int
      }
    }
    
    x
    
  })
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra normalized!"))
  
  TRUE
}

#' @title .s3_normalize_spectra.Settings_normalize_spectra_minmax
#'
#' @description Normalizes intensity based on mix and max.
#'
#' @noRd
#'
.s3_normalize_spectra.Settings_normalize_spectra_minmax <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if ("rt" %in% colnames(x)) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          max_int <- max(z$intensity)
          min_int <- min(z$intensity)
          z$intensity <- (z$intensity - min_int) / (max_int - min_int)
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

#' @title .s3_normalize_spectra.Settings_normalize_spectra_snv
#'
  #' @description Normalizes intensity based on standard normal variate (SNV) transformation.
#'
#' @noRd
#'
.s3_normalize_spectra.Settings_normalize_spectra_snv <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  liftTozero <- settings$parameters$liftTozero
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if ("rt" %in% colnames(x)) {
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

#' @title .s3_normalize_spectra.Settings_normalize_spectra_scale
#'
#' @description Scales spectra based on the standard deviation.
#'
#' @noRd
#'
.s3_normalize_spectra.Settings_normalize_spectra_scale <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }

  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if ("rt" %in% colnames(x)) {
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

#' @title .s3_normalize_spectra.Settings_normalize_spectra_blockweight
#'
#' @description Scales spectra based on the standard deviation.
#'
#' @noRd
#'
.s3_normalize_spectra.Settings_normalize_spectra_blockweight <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if ("rt" %in% colnames(x)) {
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

#' @title .s3_normalize_spectra.Settings_normalize_spectra_meancenter
#'
#' @description Performs a mean center normalzation to spectra.
#'
#' @noRd
#'
.s3_normalize_spectra.Settings_normalize_spectra_meancenter <- function(settings, self, private) {
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x) {
    
    if (nrow(x) > 0) {
      
      if ("rt" %in% colnames(x)) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          mean_int <- mean(z$intensity)
          z$intensity <- z$intensity / mean_int
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
