
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_DeconvoluteSpectra_StreamFind**
#'
#' @description Deconvolutes the spectral mass-to-charge ratio (\emph{m/z}) to mass (Da) after assignment of charges.
#' 
#' @param clustVal Numeric (length 1) with the clustering value for the charge deconvolution.
#' @param window Optional numeric (length 1) with the window in \emph{m/z} for collecting traces of a given charge.
#'
#' @return A MassSpecSettings_DeconvoluteSpectra_StreamFind object.
#'
#' @export
#'
MassSpecSettings_DeconvoluteSpectra_StreamFind <- S7::new_class("MassSpecSettings_DeconvoluteSpectra_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(clustVal = 0.1, window = 20) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "DeconvoluteSpectra",
      algorithm = "StreamFind",
      parameters = list(
        clustVal = as.numeric(clustVal),
        window = as.numeric(window)
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
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "DeconvoluteSpectra")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_number(self@parameters$clustVal)
    checkmate::assert_number(self@parameters$window)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_DeconvoluteSpectra_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra_charges()) {
    warning("No spectra charge results object available! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$spectra$spectra
  
  charges <- engine$spectra$charges
  
  parameters <- x$parameters
  clustVal <- parameters$clustVal
  windowVal <- parameters$window
  
  deconvoluted <- Map(function(z, y) {
    
    if (nrow(z) == 0) return(data.table())
    
    if (nrow(y) == 0) return(data.table())
    
    profiles <- lapply(seq_len(nrow(y)), function(j) {
      
      if (is.na(windowVal) || length(windowVal) == 0) {
        
        if (j == nrow(y)) {
          window <- (y$mz[j] - y$mz[j - 1]) / 2
          
        } else {
          
          if (y$z[j] - y$z[j + 1] > 1) {
            window <- (y$mz[j] - y$mz[j - 1]) / 2
            
          } else {
            window <- (y$mz[j + 1] - y$mz[j]) / 2
          }
        }
        
      } else {
        window <- windowVal
      }
      
      sel <- z$mz >= (y$mz[j] - window) & z$mz <= (y$mz[j] + window)
      
      prfl <- z[sel, ]
      
      prfl$mz <- y$z[j] * (prfl$mz - 1.007276)
      
      prfl
    })
    
    profiles <- profiles[vapply(profiles, function(j) nrow(j) > 0, FALSE)]
    
    max_int <- vapply(profiles, function(j) max(j$intensity), 0)

    idx <- order(max_int, decreasing = TRUE)[1:min(5, length(max_int))]

    profiles <- profiles[idx]
    
    profiles_dt <- rbindlist(profiles)
    
    profiles_dt$unique_id <- "var"
    
    profiles_dt$analysis <- ""
    
    av_profile <- rcpp_ms_cluster_spectra(profiles_dt, mzClust = clustVal, presence = 0.1)[[1]]
    
    av_profile <- as.data.table(av_profile)
    
    av_profile$analysis <- NULL
    
    setnames(av_profile, "mz", "mass")
    
    if (FALSE) plot(av_profile$mass, av_profile$intensity, type = 'l', main = paste0("Merged spectra"))
    
    av_profile
    
  }, spec_list, charges)
  
  names(deconvoluted) <- names(spec_list)
  spectra <- engine$spectra
  spectra$is_neutralized <- TRUE
  spectra$spectra <- deconvoluted
  engine$spectra <- spectra
  message(paste0("\U2713 ", "Spectra deconvoluted!"))
  TRUE
}
