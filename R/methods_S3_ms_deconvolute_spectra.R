
#' @title .s3_ms_deconvolute_spectra.Settings_deconvolute_spectra_StreamFind
#'
#' @description Deconvolutes spectra according to calculated charges from multi-charged compounds.
#'
#' @noRd
#'
.s3_ms_deconvolute_spectra.Settings_deconvolute_spectra_StreamFind <- function(settings, self, private) {
  
  parameters <- settings$parameters
  
  clustVal <- parameters$clustVal
  
  if (!self$has_spectra_charges()) {
    warning("Spectra charges not found for deconvolution! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  charges <- self$spectra_charges
  
  deconvoluted <- Map(function(x, y) {
    
    y2 <- y[!y$outlier, ]
    
    profiles <- lapply(seq_len(nrow(y2)), function(j) {
      
      if (j == nrow(y2)) {
        window <- (y2$mz[j] - y2$mz[j - 1]) / 2
        
      } else {
        
        if (y2$z[j] - y2$z[j + 1] > 1) {
          window <- (y2$mz[j] - y2$mz[j - 1]) / 2
          
        } else {
          window <- (y2$mz[j + 1] - y2$mz[j]) / 2
        }
      }
      
      sel <- x$mz >= (y2$mz[j] - window) & x$mz <= (y2$mz[j] + window)
      
      prfl <- x[sel, ]
      
      prfl$mz <- prfl$mz * y2$z[j]
      
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
  
  self$deconvoluted_spectra <- deconvoluted
  
  message(paste0("\U2713 ", "Spectra deconvoluted!"))
  
  TRUE
}
