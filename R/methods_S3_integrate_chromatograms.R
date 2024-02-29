
#' @title .s3_integrate_chromatograms.Settings_integrate_chromatograms_StreamFind
#'
#' @description Integrates chromatograms using a native smoothing and the baseline and pracma R packages.
#'
#' @noRd
#'
.s3_integrate_chromatograms.Settings_integrate_chromatograms_StreamFind <- function(settings, self) {
  
  message("Integrating chromatograms...", appendLF = TRUE)
  
  if (!requireNamespace("pracma", quietly = TRUE)) {
    warning("Package pracma not found but required! Not done.")
    return(FALSE)
  }
  
  if (!validate(settings)) return(FALSE)
  
  parameters <- settings$parameters
  
  if (parameters$baseline && !requireNamespace("baseline", quietly = TRUE)) {
    warning("Package baseline not found but required for baseline correction! Not done.")
    return(FALSE)
  }
  
  chroms <- self$get_chromatograms(chromatograms = parameters$chromatograms)
  
  if (nrow(chroms) == 0) {
    warning("Chromatograms not found! Not done.")
    return(FALSE)
  }
  
  chroms <- split(chroms, chroms$analysis)
  
  plotLevel <- 0
  
  xlab <- "Retention time / seconds"
  
  ylab <- "Intensity / counts"
  
  chrom_peaks <- lapply(chroms, function(s) {
    
    if (nrow(s) == 0) {
      return(
        list(
          "peaks" = data.table(),
          "chromatograms" = data.table()
        )
      )
    }
    
    s <- split(s, s$id)
    
    s <- lapply(s, function(x) {
      
      i_raw <- x$intensity
      
      i_smoothed <- NULL
      
      i_corrected <- NULL
      
      i_baseline <- NULL
      
      i_vec <- i_raw
      
      if (plotLevel > 3) {
        plot(x$rt, i_raw, type = 'p',
          xlab = xlab,
          ylab = ylab,
          main = "Raw intensity profile"
        )
      }
      
      if (parameters$smoothing) {
        
        .moving_average <- function(i_vec, t_vec, windowSize, timeWindow) {
          
          output <- numeric(length(i_vec))
          
          for (z in seq_len(length(i_vec))) {
            
            # if (z > 30) browser()
            
            idx <- which(abs(t_vec - t_vec[z]) <= timeWindow)
            
            # i_vec[idx]
            
            left_window <- idx >= max(min(idx), z - windowSize) & idx < z
            
            left_window <- idx[left_window]
            
            left_size <- length(left_window)
            
            if (left_size > 0) left_window = i_vec[left_window]
            
            right_window <- idx <= min(max(idx), z + windowSize) & idx > z
            
            right_window <- idx[right_window]
            
            right_size <- length(right_window)
              
            if (right_size > 0) right_window <- i_vec[right_window]
            
            if (left_size == 0) {
              output[z] <- i_vec[z]
              next
            }

            if (right_size == 0) {
              output[z] <- i_vec[z]
              next
            }
            
            if (left_size < right_size) left_window <- c(left_window, rep(0, right_size - left_size))
              
            if (right_size < left_size) right_window <- c(right_window, rep(0, left_size - right_size))

            output[z] <- mean(c(left_window, i_vec[z], right_window))
          }
          
          output
        }
        
        i_smoothed <- .moving_average(i_vec, x$rt, parameters$windowSize, 3)
        
        if (plotLevel > 3)  {
          plot(x$rt, i_smoothed, type = 'l',
            xlab = xlab,
            ylab = ylab,
            main = "Smoothed intensity profile"
          )
        }
        
        i_vec <- i_smoothed
      }
      
      if (parameters$baseline) {
        
        mat <- matrix(as.numeric(i_vec), nrow = 1, dimnames = list(1, x$rt))
        
        mat <- do.call("baseline", c(list(spectra = mat, method = parameters$baseline_method), parameters$baseline_args))
        
        i_baseline <- as.numeric(mat@baseline)
        
        i_baseline[i_baseline > i_vec] <- i_vec[i_baseline > i_vec]
        
        i_baseline[i_baseline < 0] <- i_vec[i_baseline < 0]
        
        if (plotLevel > 3) {
          plot(x$rt, i_vec, type = "l",
            xlab = xlab,
            ylab = ylab,
            main = "Baseline correction"
          )

          lines(x$rt, as.numeric(i_baseline), col = "darkred")
        }
        
        # i_corrected <- as.numeric(mat@corrected)
        i_corrected <- i_vec - i_baseline
        
        i_corrected[i_corrected < 0] <- 0
        
        if (plotLevel > 3)  {
          plot(x$rt, i_corrected, col = "darkgreen", type = 'l',
            xlab = xlab,
            ylab = ylab,
            main = "Corrected intensity profile"
          )
        }
        
        i_vec <- i_corrected
      }
      
      # threshold <- 1000
      # window_size <- 10
      # threshold_factor <- 3
      
      # # .find_main_peaks <- function(data, window_size, threshold) {
      # #   peaks <- numeric(length(data))
      # #   
      # #   for (i in 1:(length(data) - window_size)) {
      # #     window_data <- data[i:(i + window_size - 1)]
      # #     
      # #     if (max(window_data) - min(window_data) > threshold) {
      # #       peaks[i] <- 1
      # #     }
      # #   }
      # #   
      # #   return(peaks)
      # # }
      # # 
      # # # Find main peaks using the sliding window approach
      # # main_peaks <- .find_main_peaks(i_vec, 10, 1000)
      # 
      # .estimate_baseline <- function(data, window_size) {
      #   stats::filter(data, rep(1 / window_size, window_size), sides = 2)
      # }
      # 
      # bl <- .estimate_baseline(i_raw, 20)
      # 
      # plot(bl)
      # 
      # threshold <- mean(bl, na.rm = T) + threshold_factor * sd(bl, na.rm = T)
      # 
      # above_threshold <- i_vec > threshold
      # peaks_indices <- which(diff(above_threshold) == 1) + 1
      # if (above_threshold[1]) {
      #   peaks_indices <- c(1, peaks_indices)
      # }
      # 
      # # Get peak heights and widths
      # peak_heights <- numeric(length(peaks_indices))
      # peak_widths <- numeric(length(peaks_indices))
      # 
      # for (i in seq_along(peaks_indices)) {
      #   start_index <- ifelse(i == 1, 1, peaks_indices[i - 1])
      #   end_index <- peaks_indices[i]
      #   peak_segment <- i_vec[start_index:end_index]
      #   peak_heights[i] <- max(peak_segment)
      #   peak_widths[i] <- length(peak_segment)
      # }
      # 
      # browser()
      # 
      # # Combine results into a data.table
      # peak_data <- data.table(
      #   Peak_Index = seq_along(peaks_indices),
      #   Peak_Height = peak_heights,
      #   Peak_Width = peak_widths
      # )
      # 
      # 
      # 
      # prac_pks <- pracma::findpeaks(
      #   x = i_vec,
      #   nups = 3,
      #   ndowns = 3,
      #   zero = "0",
      #   peakpat = NULL,
      #   minpeakheight = parameters$minPeakHeight, # Minimum peak height
      #   minpeakdistance = parameters$minPeakDistance, # Minimum peak separation
      #   threshold = 0, # Minimum height difference
      #   npeaks = 0, # Maximum number of peaks
      #   sortstr = FALSE # Peak sorting
      # )
      
      prac_pks <- pracma::findpeaks(
        x = i_vec,
        nups = 1,
        ndowns = 1,
        zero = "0",
        peakpat = NULL,
        minpeakheight = parameters$minPeakHeight, # Minimum peak height
        minpeakdistance = parameters$minPeakDistance, # Minimum peak separation
        threshold = 0, # Minimum height difference
        npeaks = 0, # Maximum number of peaks
        sortstr = FALSE # Peak sorting
      )
      
      if (is.null(prac_pks)) {
        warning("Peaks not found in chromatogram!")
        
        return(
          list(
            "peaks" = data.table(),
            "chromatograms" = data.table(
              "analysis" = unique(x$analysis),
              "id" = unique(x$id),
              "rt" = x$rt,
              "raw" = i_raw,
              "smoothed" = i_smoothed,
              "baseline" = i_baseline,
              "intensity" = i_vec
            )
          )
        )
        
      } else {
        pks <- data.table(
          "rt" = x$rt[as.integer(prac_pks[, 2])],
          "rtmin" = x$rt[as.integer(prac_pks[, 3]) + 1],
          "rtmax" = x$rt[as.integer(prac_pks[, 4]) - 1],
          "intensity" = x$intensity[as.integer(prac_pks[, 2])]
        )
      }
      
      setorder(pks, rt)

      pks$index <- seq_len(nrow(pks))
      
      if (plotLevel > 3) {
        plot(
          x$rt, x$intensity,
          type = "l",
          xlab = xlab,
          ylab = ylab,
          main = "Found chromatographic peaks"
        )
        
        points(pks$rt, pks$intensity, pch = 16, col = "red", cex = 1.5)
      }
      
      .merge_peaks <- function(peak_table) {

        peak_table$merged <- FALSE

        next_pk <- FALSE

        pk_pos <- 1

        for (i in seq_len(nrow(peak_table) - 1)) {

          if (next_pk) pk_pos <- i
          
          pk_closeby <- abs(peak_table$rtmin[i + 1] - peak_table$rtmax[i]) <= 1

          if (pk_closeby) {

            pk_i <- i_vec[x$rt == peak_table$rt[i]]

            #pk_i_l_before <- i_vec[x$rt == peak_table$rtmin[i]]

            pk_i_r_before <- i_vec[x$rt == peak_table$rtmax[i]]

            pk_i_l_next <- i_vec[x$rt == peak_table$rtmin[i + 1]]

            pk_i_r_next <- i_vec[x$rt == peak_table$rtmax[i + 1]]

            intensity_diff <- pk_i_r_before / pk_i_l_next

            pk_int_conect <- intensity_diff <=  1 + 0.2 && intensity_diff >=  1 - 0.2

            if (pk_int_conect) {
              
              not_equal_sides <- (pk_i - pk_i_l_next) / (pk_i - pk_i_r_next)
              
              not_equal_sides <- not_equal_sides <=  0.1 || not_equal_sides >=  10
              
              if (not_equal_sides) {
                peak_table$rtmax[pk_pos] <- peak_table$rtmax[i + 1]
                peak_table$intensity[pk_pos] <- max(peak_table$intensity[pk_pos], peak_table$intensity[i + 1])
                peak_table$merged[i + 1] <- TRUE

              } else {
                next_pk <- TRUE
              }
            } else {
              next_pk <- TRUE
            }
          } else {
            next_pk <- TRUE
          }
        }
       
        merged_peaks <- peak_table[!peak_table$merged]

        merged_peaks
      }
      
      pks <- .merge_peaks(pks)
      
      pks$merged <- NULL
      
      pks$width <- pks$rtmax - pks$rtmin
      
      pks <- pks[pks$width >= parameters$minPeakWidth & pks$width <= parameters$maxPeakWidth, ]
      
      pks$index <- seq_len(nrow(pks))
      
      .trapezoidal_integration <- function(x, y) sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
      
      .integrate_peak_area <- function(rt, intensity, baseline, peak_start, peak_end) {
        peak_mask <- (rt >= peak_start) & (rt <= peak_end)
        peak_rt <- rt[peak_mask]
        if (!is.null(baseline)) {
          peak_intensity <- intensity[peak_mask] - baseline[peak_mask]
        }
        peak_intensity[peak_intensity < 0] <- 0
        return(.trapezoidal_integration(peak_rt, peak_intensity))
      }
      
      integrated_areas <- vapply(pks$index, function(i) {
        .integrate_peak_area(x$rt, x$intensity, i_baseline, pks$rtmin[i], pks$rtmax[i])
      }, 0)
      
      pks$area <- integrated_areas
      
      pks$intensity <- vapply(pks$index, function(i) {
        quarter_pk <- (pks$rtmax[i] - pks$rtmin[i]) / 4
        max(x$intensity[x$rt > (pks$rt[i] - quarter_pk) & x$rt < (pks$rt[i] + quarter_pk)])
      }, 0)
      
      pks$rt <- vapply(pks$index, function(i) {
        quarter_pk <- (pks$rtmax[i] - pks$rtmin[i]) / 4
        x$rt[x$rt > (pks$rt[i] - quarter_pk) & x$rt < (pks$rt[i] + quarter_pk) & x$intensity == pks$intensity[i]]
      }, 0)
      
      if (plotLevel > 3) {
        plot(
          x$rt, x$intensity,
          type = "l",
          xlab = xlab,
          ylab = ylab,
          main = "Found chromatographic peaks"
        )
        
        for (i in seq_len(nrow(pks))) {
          
          points(pks$rt[i], pks$intensity[i], pch = 16, col = "red", cex = 1.5)
          
          rect(
            xleft = pks$rtmin[i],
            xright = pks$rtmax[i],
            ybottom = 0,
            ytop = pks$intensity[i],
            border = "red"
          )
          
          text(
            x = pks$rt[i], y = pks$intensity[i], adj = c(0.5, -1),
            labels = pks$index[i],
            vfont = NULL, cex = 0.8, col = "black", font = NULL, srt = 0
          )
        }
      }
      
      pks$sn <- sapply(pks$index, function(i) {
        base = which(x$rt >= pks$rtmin[i] & x$rt <= pks$rtmax[i])
        base = c(min(base) - 2, base, max(base) + 2)
        base <- x$intensity[base]
        base <- base[base > 0]
        base <- min(base)
        round(pks$intensity[i] / base, digits = 1)
      })
      
      if (is.numeric(parameters$minSN)) pks <- pks[pks$sn >= parameters$minSN[1], ]
      
      if (nrow(pks) == 0) {
        warning("Peaks not found in chromatogram!")
        
        return(
          list(
            "peaks" = data.table(),
            "chromatograms" = data.table(
              "analysis" = unique(x$analysis),
              "id" = unique(x$id),
              "rt" = x$rt,
              "raw" = i_raw,
              "smoothed" = i_smoothed,
              "baseline" = i_baseline,
              "intensity" = i_vec
            )
          )
        )
      }

      if (nrow(pks) == 0) {
        warning("Peaks not found in chromatogram!")
        
        return(
          list(
            "peaks" = data.table(),
            "chromatograms" = data.table(
              "analysis" = unique(x$analysis),
              "id" = unique(x$id),
              "rt" = x$rt,
              "raw" = i_raw,
              "smoothed" = i_smoothed,
              "baseline" = i_baseline,
              "intensity" = i_vec
            )
          )
        )
      }
      
      pks$index <- seq_len(nrow(pks))
      
      if (plotLevel > 3) {
        plot(
          x$rt, i_corrected,
          type = "l",
          xlab = xlab,
          ylab = ylab,
          col = "black",
          main = paste0("Integrated ", unique(x$id) ,"\nchromatogram from ", unique(x$analysis)),
          ylim = c(0, max(i_corrected) * 1.1)
        )
        
        base_zero <- rep(0, length(i_corrected))
        
        colors <- .get_colors(pks$index)
        
        for (i in pks$index) {
          
          sel <- x$rt > pks$rtmin[i] & x$rt < pks$rtmax[i]
          
          polygon(
            c(x$rt[sel], rev(x$rt[sel])),
            c(i_corrected[sel], rev(base_zero[sel])),
            col = colors[i],
            border = NA
          )
          
          text(
            x = pks$rt[i], y = i_corrected[pks$intensity[i] == i_raw & sel], adj = c(0.5, -1),
            labels = pks$index[i],
            vfont = NULL, cex = 0.8, col = "black", font = NULL, srt = 0
          )
        }
      }
      
      if (plotLevel > 0) {
        plot(
          x$rt, x$intensity,
          type = "l",
          xlab = xlab,
          ylab = ylab,
          col = "black",
          main = paste0("Integrated ", unique(x$id) ,"\nchromatogram from ", unique(x$analysis)),
          ylim = c(0, max(x$intensity) * 1.1)
        )
        
        if (parameters$baseline) lines(x$rt, as.numeric(i_baseline), col = "darkred", lwd = 1, lty = 2)
        
        if (!parameters$baseline) i_baseline <- rep(0, length(i_raw))
        
        colors <- .get_colors(pks$index)
        
        for (i in pks$index) {
          
          sel <- x$rt > pks$rtmin[i] & x$rt < pks$rtmax[i]
          
          lines(x$rt[sel], i_raw[sel], pch = 16, col = colors[i], cex = 1.5)
          
          lines(x$rt[sel], i_baseline[sel], pch = 16, col = colors[i], cex = 1.5)
          
          polygon(
            c(x$rt[sel], rev(x$rt[sel])),
            c(i_raw[sel], rev(i_baseline[sel])),
            col = colors[i],
            border = NA
          )
          
          text(
            x = pks$rt[i], y = pks$intensity[i], adj = c(0.5, -1),
            labels = pks$index[i],
            vfont = NULL, cex = 0.8, col = "black", font = NULL, srt = 0
          )
        }
      }
      
      pks$analysis <- unique(x$analysis)
      
      pks$id <- unique(x$id)
      
      pks$polarity <- unique(x$polarity)
      
      pks$pre_ce <- unique(x$pre_ce)
      
      pks$pre_mz <- unique(x$pre_mz)
      
      pks$pro_mz <- unique(x$pro_mz)
      
      setnames(pks, "index", "peak")
      
      pks$index <- unique(x$index)
      
      setcolorder(pks, c("analysis", "id", "index", "peak", "polarity", "pre_ce", "pre_mz", "pro_mz"))
      
      list(
        "peaks" = pks,
        "chromatograms" = data.table(
          "analysis" = unique(x$analysis),
          "id" = unique(x$id),
          "rt" = x$rt,
          "raw" = i_raw,
          "smoothed" = i_smoothed,
          "baseline" = i_baseline,
          "intensity" = i_vec
        )
      )
    })
    
    pks_list <- lapply(s, function(x) x$peaks)
    
    chr_list <- lapply(s, function(x) x$chromatograms)

    list(
      "peaks" = rbindlist(pks_list, fill = TRUE),
      "chromatograms" = rbindlist(chr_list, fill = TRUE)
    )
  })
  
  names(chrom_peaks) <- names(chroms)
  
  self$add_modules_data(
    list("chrom_peaks" = list(
      "data" = chrom_peaks,
      "software" = "StreamFind",
      "version" = as.character(packageVersion("StreamFind"))
    ))
  )
  
  TRUE
}
