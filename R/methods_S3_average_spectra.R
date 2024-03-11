
#' @title .s3_average_spectra.Settings_average_spectra_StreamFind
#'
#' @description Averaging of spectra.
#'
#' @noRd
#'
.s3_average_spectra.Settings_average_spectra_StreamFind <- function(settings, self, private) {
  
  spec <- self$spectra
  
  spec <- rbindlist(spec, fill = TRUE)
  
  if (nrow(spec) == 0) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  if ("analysis" %in% colnames(spec)) {
    
    rpl <- self$get_replicate_names()
    
    spec$replicate <- rpl[spec$analysis]
    
    spec_list <- split(spec, spec$replicate)
    
    av_list <- lapply(spec_list, function(x) {
      intensity <- NULL
      res <- copy(x)
      
      res[["analysis"]] <- NULL
      
      groupCols <- "shift"
      
      if ("rt" %in% colnames(x)) groupCols <- c("rt", groupCols)
      
      res <- res[, intensity := mean(intensity), by = groupCols]
      
      res <- unique(res)
      
      setcolorder(res, c("replicate"))
      
      res
      
      list("spectra" = x, "average" = res)
      
    })
    
    if (self$has_averaged_spectra()) {
      private$.results$spectra$data <- av_list
      
    } else {
      self$add_results(
        list("spectra" = list(
          "data" = av_list,
          "software" = "StreamFind",
          "version" = as.character(packageVersion("StreamFind"))
        ))
      )
    }
    
    message(paste0("\U2713 ", "Averaged spectra!"))
    
    TRUE
    
  } else {
    FALSE
  }
}
