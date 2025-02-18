#' **MassSpecMethod_FindSpectraMaxima_native**
#'
#' @description Finds maximum peaks in continuous spectra.
#' 
#' @param minWidth Numeric (length 1) with the minimum width of a peak.
#' @param maxWidth Numeric (length 1) with the maximum width of a peak.
#' @param minHeight Numeric (length 1) with the minimum height of a peak.
#'
#' @return A MassSpecMethod_FindSpectraMaxima_native object.
#'
#' @export
#'
MassSpecMethod_FindSpectraMaxima_native <- S7::new_class(
  name = "MassSpecMethod_FindSpectraMaxima_native",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(minWidth = 0, maxWidth = 0, minHeight = 0) {
    
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "FindSpectraMaxima",
        required = "LoadSpectra",
        algorithm = "native",
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
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "FindSpectraMaxima")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_number(self@parameters$minWidth)
    checkmate::assert_number(self@parameters$maxWidth)
    checkmate::assert_number(self@parameters$minHeight)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_FindSpectraMaxima_native) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_spectra()) {
    warning("No Spectra results object available! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  spectra <- engine$Spectra$spectra
  
  spectra_peaks <- lapply(spectra, function(s) {
    
    if (nrow(s) == 0) return(data.table::data.table())
    
    s <- split(s, s$id)
    
    s <- lapply(s, function(z) {
      
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
          id = unique(z$id),
          peak = paste0("S", unique(z$id), "_P", seq_along(peaks), "_M", round(x[peaks], 0)),
          mass = x[peaks],
          min = peaks_left,
          max = peaks_right,
          intensity = y[peaks],
          width = widths,
          height_left = heights_left,
          height_right = heights_right,
          area = NA_real_,
          sn = y[peaks] / y[peak_bases]
        ))
      }
      
      peak_results <- find_relevant_peaks(
        z$mass, z$intensity,
        min_width = parameters$minWidth,
        max_width = parameters$maxWidth,
        min_height = parameters$minHeight
      )

      if (FALSE) {
        plot(z$mass, z$intensity, type = "l", main = "Chromatogram with Relevant Peaks")
        points(peak_results$peak, peak_results$peak_intensity, col = "red", pch = 19)
        points(peak_results$peak_left, peak_results$peak_intensity, col = "green", pch = 19)
        points(peak_results$peak_right, peak_results$peak_intensity, col = "green", pch = 19)
      }
      
      peak_results
    })
    
    all_pks <- data.table::rbindlist(s, fill = TRUE)
    
    all_pks
  })
  
  names(spectra_peaks) <- names(spectra)
  
  engine$Spectra$peaks <- spectra_peaks
  message(paste0("\U2713 ", "Spectra integrated!"))
  TRUE
}
