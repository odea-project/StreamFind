
#' @title .s3_merge_spectra_time_series.Settings_merge_spectra_time_series_StreamFind
#'
#' @description Averaging of spectra.
#'
#' @noRd
#'
.s3_merge_spectra_time_series.Settings_merge_spectra_time_series_StreamFind <- function(settings, self, private) {
  
  preCut <- settings$parameters$preCut
  
  rpls <- self$get_replicate_names()
  
  urpls <- unique(rpls)
  
  unified <- lapply(urpls, function(x) {
    
    anas <- names(rpls)[rpls %in% x]
    
    anasl <- self$get_analyses(anas)
    
    cached_merged_analysis <- FALSE
    
    merged_analysis <- NULL
    
    if (.caches_data()) {
      hash <- patRoon::makeHash(x, anas, anasl)
      
      merged_analysis <- patRoon::loadCacheData("merged_raman_analysis", hash)
      
      if (!is.null(merged_analysis)) {
        message("\U2139 Merged Raman analysis loaded from cache!")
        cached_merged_analysis <- TRUE
      }
      
    } else {
      hash <- NULL
      merged_analysis <- NULL
    }
    
    if (is.null(merged_analysis) & !cached_merged_analysis) {
      
      rtvec <- vapply(anasl, function(z) as.numeric(z$metadata$`Accumulate Cycle Time (secs)`), NA_real_)
      
      rtvec <- cumsum(unname(rtvec))
      
      spectral <- lapply(anasl, function(z) z$spectra)
      
      spectral <- spectral[-(1:preCut)]
      
      names(spectral) <- as.character(rtvec[-(1:preCut)])
      
      spectra <- rbindlist(spectral, idcol = "rt")
      
      spectra$rt <- as.numeric(spectra$rt)
      
      setcolorder(spectra, c("rt"))
      
      message("\U2699 Writting unified analysis file..." , appendLF = FALSE)
      
      ana_name <- x
      
      ana_dir <- dirname(anasl[[1]]$file)
      
      ana_ext <- file_ext(anasl[[1]]$file)
      
      new_file <- paste0(ana_dir, "/", ana_name, ".", ana_ext)
      
      ana_metadata <- anasl[[1]]$metadata
      
      if (file.exists(new_file)) file.remove(new_file)
      
      rcpp_write_asc_file(file = new_file, ana_metadata, as.matrix(spectra))
      
      merged_analysis <- list(
        "name" = ana_name,
        "replicate" = ana_name,
        "blank" = NA_character_,
        "file" = new_file,
        "metadata" = ana_metadata,
        "spectra" = spectra
      )
      
      message(" Done!")
      
      if (!is.null(hash)) {
        patRoon::saveCacheData("merged_raman_analysis", merged_analysis, hash)
        message("\U1f5ab Merged Raman analysis cached!")
      }
      
    } else {
      
      if (!file.exists(merged_analysis$file)) {
        message("\U2699 Writting unified analysis file..." , appendLF = FALSE)
        
        rcpp_write_asc_file(file = merged_analysis$file, merged_analysis$metadata, as.matrix(merged_analysis$spectra))
        
        message(" Done!")
      }
    }
    
    merged_analysis
  })
  
  names(unified) <- urpls
  
  unified <- private$.validate_list_analyses(unified, childClass = "RamanAnalysis")
  
  if (!is.null(unified)) {
    
    if (all(vapply(unified, function(x) is(x), NA_character_) %in% "RamanAnalysis")) {
      
      to_remove <- self$get_analysis_names()[self$get_replicate_names() %in% names(unified)]
      
      suppressMessages(self$remove_analyses(to_remove))
      
      self$add_analyses(unified)
      
      TRUE
      
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}
