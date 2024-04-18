
#' @title .s3_ms_deconvolute_spectra.Settings_deconvolute_spectra_StreamFind
#'
#' @description Deconvolutes spectra according to calculated charges from multi-charged compounds.
#'
#' @noRd
#'
.s3_ms_deconvolute_spectra.Settings_deconvolute_spectra_StreamFind <- function(settings, self, private) {
  
  parameters <- settings$parameters
  
  clustVal <- parameters$clustVal
  
  windowVal <- parameters$window
  
  if (!self$has_spectra_charges()) {
    warning("Spectra charges not found for deconvolution! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  charges <- self$spectra_charges
  
  deconvoluted <- Map(function(x, y) {
    
    if (nrow(x) == 0) return(data.table())
    
    if (nrow(y) == 0) return(data.table())
    
    profiles <- lapply(seq_len(nrow(y)), function(j) {
      
      if (is.null(windowVal)) {
        
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
      
      sel <- x$mz >= (y$mz[j] - window) & x$mz <= (y$mz[j] + window)
      
      prfl <- x[sel, ]
      
      prfl$mz <- prfl$mz * y$z[j]
      
      prfl
    })
    
    
    profiles <- profiles[vapply(profiles, function(j) nrow(j) > 0, FALSE)]
    
    profiles_dt <- rbindlist(profiles)
    
    profiles_dt$unique_id <- "var"
    
    av_profile <- rcpp_ms_cluster_spectra(profiles_dt, mzClust = clustVal, presence = 0)[[1]]
    
    av_profile <- as.data.table(av_profile)
    
    setnames(av_profile, "mz", "mass")
    
    if (FALSE) plot(av_profile$mass, av_profile$intensity, type = 'l', main = paste0("Merged spectra"))
    
    av_profile
    
  }, spec_list, charges)
  
  names(deconvoluted) <- names(spec_list)
  
  self$spectra <- deconvoluted
  
  message(paste0("\U2713 ", "Spectra deconvoluted!"))
  
  TRUE
}
