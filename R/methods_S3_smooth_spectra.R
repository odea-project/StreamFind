
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

#' @title .s3_smooth_spectra.Settings_smooth_spectra_StreamFind
#'
#' @description Smooths of spectra.
#'
#' @noRd
#'
.s3_smooth_spectra.Settings_smooth_spectra_StreamFind <- function(settings, self, private) {
  
  windowSize <- settings$parameters$windowSize
  
  if (!(self$has_averaged_spectra() || self$has_spectra())) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  private$.results$spectra$data <- lapply(private$.results$spectra$data, function(x, windowSize) {
    
    if ("average" %in% names(x)) {
      
      if (nrow(x$average) > 0) {
        
        if ("rt" %in% colnames(x$average)) {
          temp_x <- split(x$average, x$average$rt)
          
          temp_x <- lapply(temp_x, function(z) {
            z$intensity <- .moving_average(z$intensity, windowSize = windowSize)
            z
          })
          
          x$average <- rbindlist(temp_x)
          
        } else {
          x$average$intensity <- .moving_average(x$average$intensity, windowSize = windowSize)
        }
      }
      
    } else (
      
      if (nrow(x$spectra) > 0) {
        
        if (nrow(x$average) > 0) {
          
          if ("rt" %in% colnames(x$spectra)) {
            temp_x <- split(x$spectra, x$spectra$rt)
            
            temp_x <- lapply(temp_x, function(z) {
              z$intensity <- .moving_average(z$intensity, windowSize = windowSize)
              z
            })
            
            x$spectra <- rbindlist(temp_x)
            
          } else {
            x$spectra$intensity <- .moving_average(x$spectra$intensity, windowSize = windowSize)
          }
        }
      }
    )
    
    x
  }, windowSize = windowSize)
  
  message(paste0("\U2713 ", "Spectra smoothed!"))
  
  TRUE
}

#' @title .s3_smooth_spectra.Settings_smooth_spectra_savgol
#'
#' @description Smooths of spectra based on Savitzky and Golay from pracma package.
#'
#' @noRd
#'
.s3_smooth_spectra.Settings_smooth_spectra_savgol <- function(settings, self, private) {
  
  fl <- settings$parameters$fl
  forder <- settings$parameters$forder
  dorder <- settings$parameters$dorder
  
  if (!(self$has_averaged_spectra() || self$has_spectra())) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  private$.results$spectra$data <- lapply(private$.results$spectra$data, function(x, fl, forder, dorder) {
    
    if ("average" %in% names(x)) {
      
      if (nrow(x$average) > 0) {
        
        if ("rt" %in% colnames(x$average)) {
          temp_x <- split(x$average, x$average$rt)
          
          temp_x <- lapply(temp_x, function(z) {
            z$intensity <- pracma::savgol(z$intensity, fl = fl, forder = forder, dorder = dorder)
            z
          })
          
          x$average <- rbindlist(temp_x)
          
        } else {
          x$average$intensity <- pracma::savgol(x$average$intensity, fl = fl, forder = forder, dorder = dorder)
        }
      }
      
    } else (
      
      if (nrow(x$spectra) > 0) {
        
        if (nrow(x$average) > 0) {
          
          if ("rt" %in% colnames(x$spectra)) {
            temp_x <- split(x$spectra, x$spectra$rt)
            
            temp_x <- lapply(temp_x, function(z) {
              z$intensity <- pracma::savgol(z$intensity, fl = fl, forder = forder, dorder = dorder)
              z
            })
            
            x$spectra <- rbindlist(temp_x)
            
          } else {
            x$spectra$intensity <- pracma::savgol(x$spectra$intensity, fl = fl, forder = forder, dorder = dorder)
          }
        }
      }
    )
    
    x
  }, fl = fl, forder = forder, dorder = dorder)
  
  message(paste0("\U2713 ", "Spectra smoothed!"))
  
  TRUE
}
