
#' .S3_ms_centroid_spectra.Settings_centroid_spectra_qCentroids
#'
#' @description Centroids profile spectra.
#'
#' @noRd
#'
.s3_ms_centroid_spectra.Settings_centroid_spectra_qCentroids <- function(settings, self) {

  message("Centroiding spectra with qCentroids...", appendLF = TRUE)

  if (!any(self$has_loaded_spectra())) self$load_spectra()

  zero_spectra <- all(self$get_spectra_number() == 0)
  
  if (zero_spectra) {
    warning("All analyses must have spectra! Analyses not centroided.")
    return(FALSE)
  }
  
  all_profile <- all(self$get_spectra_mode() %in% "profile")
  
  if (!all_profile) {
    warning("Spectra must be in profile mode for centroiding! Analyses not centroided.")
    return(FALSE)
  }
  
  run_list <- lapply(self$get_analyses(), function(x) x$run)
  
  spectra_list <- lapply(self$get_analyses(), function(x) x$spectra)
  
  parameters <- settings$parameters
  
  if (parameters$runParallel & length(spectra_list) > 1) {
    workers <- parallel::detectCores() - 1
    if (length(spectra_list) < workers) workers <- length(spectra_list)
    par_type <- "PSOCK"
    if (parallelly::supportsMulticore()) par_type <- "FORK"
    cl <- parallel::makeCluster(workers, type = par_type)
    doParallel::registerDoParallel(cl)
    
  } else {
    registerDoSEQ()
  }
  
  if (TRUE) {
    message("\U2699 Centroiding spectra for ",
            length(self$get_analyses()),
            " analyses using qCentroids...",
            appendLF = FALSE
    )
    
    i <- NULL
    
    vars <- c("rcpp_centroid_spectra_qCentroids")
    
    centroided_spectra_list <- foreach(i = spectra_list,
      .packages = "StreamFind",
      .export = vars
    ) %dopar% {
      do.call("rcpp_centroid_spectra_qCentroids", list("spectra" = i))
    }
    
    names(centroided_spectra_list) <- self$get_analysis_names()
    
    self$add_spectra(centroided_spectra_list, replace = TRUE)
    
    message(" Done!")
  }

  return(FALSE)
}
