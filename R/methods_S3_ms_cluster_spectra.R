
#' @title .s3_ClusterSpectra.MassSpecSettings_ClusterSpectra_StreamFind
#'
#' @description Averaging of spectra.
#'
#' @noRd
#'
.s3_ClusterSpectra.MassSpecSettings_ClusterSpectra_StreamFind <- function(settings, self, private) {
  
  val <- settings$parameters$val
  
  clustVal <- settings$parameters$clustVal
  
  presence <- settings$parameters$presence
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  if (!all(vapply(spec_list, function(x) val %in% colnames(x), FALSE))) {
    warning("Val not found in spectra data.tables! Not done.")
    return(FALSE)
  }
  
  spec_list <- lapply(spec_list, function(x, val, clustVal, presence) {
    
    if (nrow(x) > 0) {
      
      x$mz <- x[[val]]
      
      x$unique_id <- x$analysis
      
      res <- rcpp_ms_cluster_spectra(x, clustVal, presence, FALSE)
      
      res <- data.table::rbindlist(res, fill = TRUE)
      
      res <- res[order(res$mz), ]
      
      res <- res[order(res$id), ]
      
      res <- res[order(res$analysis), ]
      
      data.table::setnames(res, "mz", val)
      
      res
      
    } else {
      x
    }
    
  }, val = val, clustVal = clustVal, presence = presence)
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra clustered!"))
  
  TRUE
}
