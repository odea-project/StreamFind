
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
  
  i_baseline[i_baseline > vec] <- vec[i_baseline > vec]
  
  i_baseline[i_baseline < 0] <- vec[i_baseline < 0]

  i_corrected <- vec - i_baseline
  
  i_corrected[i_corrected < 0] <- 0
  
  list("mat" = mat, "baseline" = i_baseline, "corrected" = i_corrected)
}



#' @title .find_peaks
#' 
#' @description Finds peaks in a given vector using the `findpeaks` function from `pracma` and an additional attempt 
#' to merge peaks.
#' 
#' @noRd
#' 
.find_peaks <- function(data, xVar,
                        merge = TRUE,
                        closeByThreshold = 45,
                        valeyThreshold = 0.5,
                        minPeakHeight,
                        minPeakDistance,
                        maxPeakWidth,
                        minPeakWidth) {
  
  plotLevel <- 1
  
  vec <- data$intensity
  
  xVec <- data[[xVar]]
  
  prac_pks <- pracma::findpeaks(
    x = vec,
    nups = 1,
    ndowns = 1,
    zero = "0",
    peakpat = NULL,
    minpeakheight = minPeakHeight, # Minimum peak height
    minpeakdistance = minPeakDistance, # Minimum peak separation
    threshold = 0, # Minimum height difference
    npeaks = 0, # Maximum number of peaks
    sortstr = FALSE # Peak sorting
  )
  
  if (is.null(prac_pks)) {
    return(data.table())
    
  } else {
    pks <- data.table(
      "idx" = as.integer(prac_pks[, 2]),
      "xVal" = xVec[prac_pks[, 2]],
      "min" = xVec[as.integer(prac_pks[, 3]) - 1],
      "max" = xVec[as.integer(prac_pks[, 4]) + 1],
      "intensity" = vec[as.integer(prac_pks[, 2])]
    )
  }
  
  if (plotLevel > 3) {
    plot(xVec, vec, type = "l",main = "Found peaks")
    points(pks$xVal, pks$intensity, pch = 16, col = "red", cex = 1.5)
    text(x = pks$xVal, y = pks$intensity, adj = c(-0.4, 0.25),
      labels = pks$index, cex = 0.6, col = "darkred", font = NULL, srt = 90
    )
  }
  
  setorder(pks, "xVal")
  
  pks$index <- seq_len(nrow(pks))
  
  if (merge) {
    
    pks$merged <- FALSE
    
    next_pk <- FALSE
    
    pk_pos <- 1
    
    for (i in seq_len(nrow(pks) - 1)) {
      
      if (next_pk) pk_pos <- i
      
      # if (i > 7) browser()
      
      pk_diff <- abs(pks$min[i + 1] - pks$max[i])
      
      pk_closeby <- pk_diff <= closeByThreshold
      
      if (pk_closeby) {
        
        pk_i <- vec[xVec == pks$xVal[i]]
        
        pk_next <- vec[xVec == pks$xVal[i + 1]]
        
        if (pk_i < pk_next) {
          
          pk_i <- vec[xVec == pks$max[i]]
          
          ints_between <- vec[xVec > pks$max[i] & xVec < pks$xVal[i + 1]]
          
          if (all(ints_between < pk_next)) {
            
            if (!all(ints_between > pk_i)) {
              
              if (length(ints_between[ints_between < pk_i]) / length(ints_between) < 0.5) {
                do_merge <- TRUE 
                
              } else {
                do_merge <- FALSE
              }
              
            } else {
              do_merge <- TRUE
            }
            
          } else {
            do_merge <- FALSE
          }
          
        } else if (pk_i > pk_next) {
          
          pk_next <- vec[xVec == pks$min[i + 1]]
          
          ints_between <- vec[xVec > pks$xVal[i] & xVec < pks$min[i + 1]]
          
          if (all(ints_between < pk_i)) {
            
            if (!all(ints_between > pk_next)) {
              
              if (length(ints_between[ints_between < pk_next]) / length(ints_between) < 0.5) {
                do_merge <- TRUE
                
              } else {
                do_merge <- FALSE
              }
              
            } else {
              do_merge <- TRUE
            }
            
          } else {
            do_merge <- FALSE
          }
          
        } else if (pk_i == pk_next) {
          do_merge <- TRUE
          
        } else {
          do_merge <- FALSE
        }
        
        if (do_merge) {
          pks$max[pk_pos] <- pks$max[i + 1]
          t_i <- pks$intensity[pk_pos]
          pks$intensity[pk_pos] <- max(pks$intensity[pk_pos], pks$intensity[i + 1])
          if (t_i != pks$intensity[pk_pos]) pks$xVal[pk_pos] <- pks$xVal[i + 1]
          pks$merged[i + 1] <- TRUE
          next_pk <- FALSE
          
        } else {
          next_pk <- TRUE
        }
        
      } else {
        next_pk <- TRUE
      }
    }
    
    if (plotLevel > 3) {
      plot(xVec, vec, type = "l",main = "Found peaks")
      points(pks$xVal, pks$intensity, pch = 16, col = "red", cex = 1.5)
      points(pks$xVal[pks$merged], pks$intensity[pks$merged], pch = 16, col = "darkgreen", cex = 1.5)
      text(x = pks$xVal, y = pks$intensity, adj = c(-0.4, 0.25),
        labels = pks$index, cex = 0.6, col = "darkred", font = NULL, srt = 90
      )
    }
    
    pks <- pks[!pks$merged]
    
    pks[["merged"]] <- NULL
  }

  pks$width <- pks$max - pks$min
  
  pks <- pks[pks$width >= minPeakWidth & pks$width <= maxPeakWidth, ]
  
  if (nrow(pks) == 0) return(data.table())
  
  pks$index <- seq_len(nrow(pks))
  
  if ("smoothed" %in% colnames(data)) {
    i_vec <- data$smoothed
    
  } else if ("raw" %in% colnames(data)) {
    i_vec <- data$raw
    
  } else {
    i_vec <- data$intensity
  }
  
  pks$intensity <- vapply(pks$index, function(i) {
    quarter_pk <- (pks$max[i] - pks$min[i]) / 4
    max(i_vec[xVec > (pks$xVal[i] - quarter_pk) & xVec < (pks$xVal[i] + quarter_pk)])
  }, 0)
  
  pks$xVal <- vapply(pks$index, function(i) {
    quarter_pk <- (pks$max[i] - pks$min[i]) / 4
    xVec[xVec > (pks$xVal[i] - quarter_pk) & xVec < (pks$xVal[i] + quarter_pk) & i_vec == pks$intensity[i]]
  }, 0)
  
  pks$idx <- vapply(pks$index, function(i) which(xVec %in% pks$xVal[i]), 0)
  
  .trapezoidal_integration <- function(x, y) sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  
  .integrate_peak_area <- function(xVec, i_vec, baseline, peak_start, peak_end) {
    peak_mask <- (xVec >= peak_start) & (xVec <= peak_end)
    peak_xVec <- xVec[peak_mask]
    if (!is.null(baseline)) {
      peak_intensity <- i_vec[peak_mask] - baseline[peak_mask]
    }
    peak_intensity[peak_intensity < 0] <- 0
    return(.trapezoidal_integration(peak_xVec, peak_intensity))
  }
  
  if ("baseline" %in% colnames(data)) {
    i_baseline <- data$baseline
    
  } else {
    i_baseline <- NULL
  }
  
  integrated_areas <- vapply(pks$index, function(i) {
    .integrate_peak_area(xVec, i_vec, i_baseline, pks$min[i], pks$max[i])
  }, 0)
  
  pks$area <- integrated_areas
  
  if (plotLevel > 0) {
    
    if ("smoothed" %in% colnames(data)) i_vec <- data$smoothed
    
    plot(xVec, i_vec, type = "l", col = "black",ylim = c(0, max(i_vec) * 1.1) )
    
    if ("baseline" %in% colnames(data)) lines(xVec, as.numeric(i_baseline), col = "darkred", lwd = 1, lty = 2)
    
    if (!"baseline" %in% colnames(data)) i_baseline <- rep(0, length(i_vec))
    
    colors <- .get_colors(pks$index)
    
    for (i in pks$index) {
      
      sel <- xVec > pks$min[i] & xVec < pks$max[i]
      
      lines(xVec[sel], i_vec[sel], pch = 16, col = colors[i], cex = 1.5)
      
      lines(xVec[sel], i_baseline[sel], pch = 16, col = colors[i], cex = 1.5)
      
      polygon(
        c(xVec[sel], rev(xVec[sel])),
        c(i_vec[sel], rev(i_baseline[sel])),
        col = colors[i],
        border = NA
      )
      
      text(
        x = pks$xVal[i], y = i_vec[pks$idx[i]], adj = c(0.5, -1),
        labels = pks$index[i],
        vfont = NULL, cex = 0.8, col = "black", font = NULL, srt = 0
      )
    }
  }
  
  pks
}
