
#' @title .s3_AverageSpectra.MassSpecSettings_AverageSpectra_StreamFind
#'
#' @description Averaging of spectra.
#'
#' @noRd
#'
.s3_AverageSpectra.MassSpecSettings_AverageSpectra_StreamFind <- function(settings, self, private) {
  
  spec <- self$spectra
  
  spec <- rbindlist(spec, fill = TRUE)
  
  if (nrow(spec) == 0) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  collapseTime <- settings$parameters$collapseTime
  
  if ("analysis" %in% colnames(spec)) {
    
    . <- NULL
    baseline <- NULL
    
    rpl <- self$get_replicate_names()
    
    spec$replicate <- rpl[spec$analysis]
    
    spec_list <- split(spec, spec$replicate)
    
    av_list <- lapply(spec_list, function(x) {
      
      intensity <- NULL
      
      rt = NULL
      
      res <- copy(x)
      
      res[["analysis"]] <- NULL
      
      groupCols <- "replicate"
      
      if ("shift" %in% colnames(res)) groupCols <- c("shift", groupCols)
      
      if ("rt" %in% colnames(x) && !collapseTime) groupCols <- c("rt", groupCols)
      
      if ("mz" %in% colnames(x)) groupCols <- c("mz", groupCols)
      
      if ("mass" %in% colnames(x)) groupCols <- c("mass", groupCols)
      
      if ("bins" %in% colnames(x)) groupCols <- c("bins", groupCols)
      
      if (collapseTime && "rt" %in% colnames(res)) {
        
        if ("raw" %in% colnames(res) && !"baseline" %in% colnames(res)) {
          res <- res[, c("rt", "intensity", "raw") := .(mean(rt), mean(intensity), mean(raw)), by = groupCols]
          
        } else if ("raw" %in% colnames(res) && "baseline" %in% colnames(res)) {
          res <- res[, c("rt", "intensity", "raw", "baseline") := .(mean(rt), mean(intensity), mean(raw), mean(baseline)), by = groupCols]
          
        } else {
          res <- res[, c("rt", "intensity") := .(mean(rt), mean(intensity)), by = groupCols]
        }
        
        res <- res[, c("intensity", "rt") := .(mean(intensity), mean(rt)), by = groupCols]
        
      } else {
        
        if ("raw" %in% colnames(res) && !"baseline" %in% colnames(res)) {
          res <- res[, c("intensity", "raw") := .(mean(rt), mean(intensity), mean(raw)), by = groupCols]
          
        } else if ("raw" %in% colnames(res) && "baseline" %in% colnames(res)) {
          res <- res[, c("intensity", "raw", "baseline") := .(mean(intensity), mean(raw), mean(baseline)), by = groupCols]
          
        } else {
          res <- res[, c("intensity") := .(mean(intensity)), by = groupCols]
        }
      }
      
      res <- unique(res)
      
      setcolorder(res, c("replicate"))
      
      res
      
    })
    
    self$spectra <- av_list
    
    message(paste0("\U2713 ", "Averaged spectra!"))
    
    TRUE
    
  } else {
    FALSE
  }
}
