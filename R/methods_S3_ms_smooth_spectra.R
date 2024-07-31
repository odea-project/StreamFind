
#' @title .moving_average
#' 
#' @description Smooths a vector using a moving average window. Optionally, a time window can be given to also limit the
#' window time.
#' 
#' @noRd
#' 
.moving_average <- function(vec, windowSize,  timeVec = NULL, timeWindow = NULL) {
  
  output <- numeric(length(vec))
  
  use_time <- FALSE
  
  if (is.numeric(timeVec) && length(timeVec) == length(vec) && is.numeric(timeWindow)) {
    use_time <- TRUE
  }
  
  all_idx <- seq_len(length(vec))
  
  for (z in seq_len(length(vec))) {
    
    if (use_time) {
      idx <- which(abs(timeVec - timeVec[z]) <= timeWindow)
      
    } else {
      idx <- all_idx
    }
    
    left_window <- idx >= max(min(idx), z - windowSize) & idx < z
    
    left_window <- idx[left_window]
    
    left_size <- length(left_window)
    
    if (left_size > 0) left_window = vec[left_window]
    
    right_window <- idx <= min(max(idx), z + windowSize) & idx > z
    
    right_window <- idx[right_window]
    
    right_size <- length(right_window)
    
    if (right_size > 0) right_window <- vec[right_window]
    
    if (left_size == 0) {
      output[z] <- vec[z]
      next
    }
    
    if (right_size == 0) {
      output[z] <- vec[z]
      next
    }
    
    if (left_size < right_size) left_window <- c(left_window, rep(0, right_size - left_size))
    
    if (right_size < left_size) right_window <- c(right_window, rep(0, left_size - right_size))
    
    output[z] <- mean(c(left_window, vec[z], right_window))
  }
  
  output
}

#' @title .s3_SmoothSpectra.MassSpecSettings_SmoothSpectra_movingaverage
#'
#' @description Smooths spectra using a moving average approach.
#'
#' @noRd
#'
.s3_SmoothSpectra.MassSpecSettings_SmoothSpectra_movingaverage <- function(settings, self, private) {
  
  windowSize <- settings$parameters$windowSize
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x, windowSize) {
    
    if (nrow(x) > 0) {
      
      if ("rt" %in% colnames(x)) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          z$intensity <- .moving_average(z$intensity, windowSize = windowSize)
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        x$intensity <- .moving_average(x$intensity, windowSize = windowSize)
      }
    }

    x
    
  }, windowSize = windowSize)
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra smoothed!"))
  
  TRUE
}

#' @title .s3_SmoothSpectra.MassSpecSettings_SmoothSpectra_savgol
#'
#' @description Smooths of spectra based on Savitzky and Golay from pracma package.
#'
#' @noRd
#'
.s3_SmoothSpectra.MassSpecSettings_SmoothSpectra_savgol <- function(settings, self, private) {
  
  if (!requireNamespace("pracma", quietly = TRUE)) {
    warning("Package pracma not found but required! Not done.")
    return(FALSE)
  }
  
  fl <- settings$parameters$fl
  
  forder <- settings$parameters$forder
  
  dorder <- settings$parameters$dorder
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x, fl, forder, dorder) {
    
    if (nrow(x) > 0) {

      if ("rt" %in% colnames(x)) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          z$intensity <- pracma::savgol(z$intensity, fl = fl, forder = forder, dorder = dorder)
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        x$intensity <- pracma::savgol(x$intensity, fl = fl, forder = forder, dorder = dorder)
      }
    }
    
    x
    
  }, fl = fl, forder = forder, dorder = dorder)
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra smoothed!"))
  
  TRUE
}
