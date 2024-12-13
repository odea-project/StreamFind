
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_FindChromPeaks_LocalMaxima**
#'
#' @description Finds peak maxima in the chromatographic dimension of multiple Raman spectra.
#' 
#' @param minWidth Numeric (length 1) with the minimum width of a peak.
#' @param maxWidth Numeric (length 1) with the maximum width of a peak.
#' @param minHeight Numeric (length 1) with the minimum height of a peak.
#'
#' @return A RamanSettings_FindChromPeaks_LocalMaxima object.
#'
#' @export
#'
RamanSettings_FindChromPeaks_LocalMaxima <- S7::new_class(
  "RamanSettings_FindChromPeaks_LocalMaxima",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(minWidth = 0, maxWidth = 0, minHeight = 0) {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "FindChromPeaks",
      required = NA_character_,
      algorithm = "LocalMaxima",
      parameters = list(
        minWidth = as.numeric(minWidth),
        maxWidth = as.numeric(maxWidth),
        minHeight = as.numeric(minHeight)
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "StreamFind",
      developer = "Ricardo Cunha",
      contact = "cunha@iuta.de",
      link = "https://odea-project.github.io/StreamFind",
      doi = NA_character_
    ))
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "Raman")
    checkmate::assert_choice(self@method, "FindChromPeaks")
    checkmate::assert_choice(self@algorithm, "LocalMaxima")
    checkmate::assert_number(self@parameters$minWidth)
    checkmate::assert_number(self@parameters$maxWidth)
    checkmate::assert_number(self@parameters$minHeight)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_FindChromPeaks_LocalMaxima) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No Spectra results object available! Not done.")
    return(FALSE)
  }
  
  if (!engine$spectra$has_chromatograms) {
    warning("No chromatograms available! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  spectra <- engine$spectra$spectra

  chrom_peaks <- lapply(spectra, function(s) {
    
    if (nrow(s) == 0) return(data.table::data.table())
    
    s <- s[, .(intensity = sum(intensity)), by = "rt"]
      
    find_relevant_peaks <- function(x, y, min_width, max_width, min_height) {
      
      peaks <- numeric(0)
      peaks_left <- numeric(0)
      peaks_right <- numeric(0)
      widths <- numeric(0)
      heights_left <- numeric(0)
      heights_right <- numeric(0)
      peak_bases <- numeric(0)
      
      is_peak <- function(i, y) {
        y[i] > y[i - 1] && y[i] > y[i + 1]
      }
      
      i <- 2  
      while (i < length(y) - 1) {
        
        if (is_peak(i, y)) {
          
          left <- i
          while (left > 1 &&
                 y[left - 1] < y[i] &&
                 x[i] - x[left] <= max_width/1.5) left <- left - 1
          
          right <- i
          while (right < length(y) &&
                 y[right + 1] < y[i] &&
                 x[right] - x[i] <= max_width/1.5) right <- right + 1
          
          
          peak_width <- x[right] - x[left]
          peak_left <- x[left]
          peak_right <- x[right]
          peak_height_right <- y[i] - y[right]
          peak_height_left <- y[i] - y[left]
          
          if (peak_height_right >= min_height &&
              peak_height_left >= min_height &&
              peak_width >= min_width) {
            
            which_max <- which.max(y[left:right]) + left - 1
            
            peaks <- c(peaks, which_max)
            peaks_left <- c(peaks_left, peak_left)
            peaks_right <- c(peaks_right, peak_right)
            widths <- c(widths, peak_width)
            heights_left <- c(heights_left, peak_height_left)
            heights_right <- c(heights_right, peak_height_right)
            peak_bases <- c(peak_bases, min(left, right))
            i <- right
          }
        }
        i <- i + 1
      }
      
      return(data.table::data.table(
        peak = seq_along(peaks),
        rt = x[peaks],
        rtmin = peaks_left,
        rtmax = peaks_right,
        intensity = y[peaks],
        width = widths,
        height_left = heights_left,
        height_right = heights_right,
        area = NA_real_,
        sn = y[peaks] / y[peak_bases]
      ))
    }
    
    peak_results <- find_relevant_peaks(
      s$rt, s$intensity,
      min_width = parameters$minWidth,
      max_width = parameters$maxWidth,
      min_height = parameters$minHeight
    )

    if (FALSE) {
      plot(s$rt, s$intensity, type = "l", main = "Chromatogram with Relevant Peaks")
      points(peak_results$rt, peak_results$intensity, col = "red", pch = 19)
      points(peak_results$rtmin, peak_results$intensity, col = "green", pch = 19)
      points(peak_results$rtmax, peak_results$intensity, col = "green", pch = 19)
    }
    
    return(peak_results)
  })
  
  names(chrom_peaks) <- names(spectra)
  
  engine$spectra$chrom_peaks <- chrom_peaks
  message(paste0("\U2713 ", "Chromatograms peaks found and added!"))
  TRUE
}
