#' **MassSpecSettings_SmoothChromatograms_movingaverage**
#'
#' @description Smooths chromatograms using the moving average algorithm.
#' 
#' @param windowSize Numeric (length 1) with the window size for the moving average.
#'
#' @return A MassSpecSettings_SmoothChromatograms_movingaverage object.
#'
#' @export
#'
MassSpecSettings_SmoothChromatograms_movingaverage <- S7::new_class(
  name = "MassSpecSettings_SmoothChromatograms_movingaverage",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(windowSize = 5) {
    S7::new_object(
      ProcessingSettings(
        engine = "MassSpec",
        method = "SmoothChromatograms",
        required = "LoadChromatograms",
        algorithm = "movingaverage",
        parameters = list(windowSize = windowSize),
        number_permitted = Inf,
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
    checkmate::assert_choice(self@method, "SmoothChromatograms")
    checkmate::assert_choice(self@algorithm, "movingaverage")
    checkmate::assert_number(self@parameters$windowSize)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_SmoothChromatograms_movingaverage) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_chromatograms()) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  
  windowSize <- x$parameters$windowSize
  
  chrom_obj <- engine$chromatograms
  
  chrom_list <- chrom_obj$chromatograms
  
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
  
  chrom_obj$chromatograms <- chrom_list
  
  engine$chromatograms <- chrom_obj
  
  message(paste0("\U2713 ", "Chromatograms smoothed!"))
  TRUE
}

#' **MassSpecSettings_SmoothChromatograms_savgol**
#'
#' @description Smooths chromatograms using the Savitzky-Golay algorithm from the \pkg{pracma}
#' package.
#' 
#' @param fl Numeric (length 1) with the filter length (for instance fl = 51..151), has to be odd.
#' @param forder Numeric (length 1) with the order of the filter
#' (2 = quadratic filter, 4 = quartic).
#' @param dorder Numeric (length 1) with the order of the derivative
#' (0 = smoothing, 1 = first derivative, etc.).
#'
#' @return A MassSpecSettings_SmoothChromatograms_savgol object.
#'
#' @export
#'
MassSpecSettings_SmoothChromatograms_savgol <- S7::new_class(
  name = "MassSpecSettings_SmoothChromatograms_savgol",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(fl = 11, forder = 4, dorder = 0) {
    S7::new_object(
      ProcessingSettings(
        engine = "MassSpec",
        method = "SmoothChromatograms",
        required = "LoadChromatograms",
        algorithm = "savgol",
        parameters = list(
          fl = fl,
          forder = forder,
          dorder = dorder
        ),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = "pracma",
        developer = "Hans W. Borchers",
        contact = NA_character_,
        link = "https://cran.r-project.org/web/packages/pracma/index.html",
        doi = NA_character_
      )
    )
  },
  
  validator = function(self) {
    
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "SmoothChromatograms")
    checkmate::assert_choice(self@algorithm, "savgol")
    checkmate::assert_number(self@parameters$fl)
    checkmate::assert_number(self@parameters$forder)
    checkmate::assert_number(self@parameters$dorder)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_SmoothChromatograms_savgol) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_chromatograms()) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  
  if (!requireNamespace("pracma", quietly = TRUE)) {
    warning("Package pracma not found but required! Not done.")
    return(FALSE)
  }
  
  fl <- x$parameters$fl
  forder <- x$parameters$forder
  dorder <- x$parameters$dorder
  
  chrom_obj <- engine$chromatograms
  
  chrom_list <- chrom_obj$chromatograms
  
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

  chrom_obj$chromatograms <- chrom_list
  
  engine$chromatograms <- chrom_obj
  
  message(paste0("\U2713 ", "Chromatograms smoothed!"))
  TRUE
}
