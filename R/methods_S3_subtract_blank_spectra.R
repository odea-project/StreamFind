#' @title .s3_average_spectra.Settings_subtract_blank_spectra_StreamFind
#'
#' @description Subtracts blank analyses according to blank correspondence.
#'
#' @noRd
#'
.s3_subtract_blank_spectra.Settings_subtract_blank_spectra_StreamFind <- function(settings, self, private) {
  
  if (self$has_averaged_spectra()) {
    spec_list <- self$averaged_spectra
    
  } else if (self$has_spectra()) {
    spec_list <- self$spectra
    
  } else {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  blks <- self$get_blank_names()
  
  names(blks) <- self$get_replicate_names()
  
  spec_blk <- spec_list[names(spec_list) %in% blks]
  
  if (length(spec_blk) == 0) {
    warning("Blank spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_sub <- lapply(spec_list, function(x) {
    
    rp <- unique(x$replicate)
    
    if (rp %in% blks) return(data.table())
    
    blk <- spec_blk[blks[rp]]
    
    if (length(blk) > 1) {
      intensity <- NULL
      blk <- rbindlist(blk)
      blk[["analysis"]] <- NULL
      blk[["replicate"]] <- NULL
      blk <- blk[, intensity := mean(intensity), by = c("shift")][]
      blk <- blk$intensity
      
    } else {
      blk <- blk[[1]]$intensity
    }
    
    x$blank <- blk
    x$intensity <- x$intensity - blk
    
    if ("analysis" %in% colnames(x) && "replicate" %in% colnames(x)) {
      if (unique(x$analysis) == unique(x$replicate)) {
        x[["analysis"]] <- NULL
      }
    }
    
    x <- unique(x)
    
    x
  })
  
  if (self$has_averaged_spectra()) {
    private$.results$spectra$data <- Map(
      function(x, y) {
        x$average <- y
        x
      },
      private$.results$spectra$data, spec_sub
    )
    
    message(paste0("\U2713 ", "Blank spectra subtracted in averaged spectra!"))
    
    TRUE
    
  } else if (self$has_spectra()) {
    private$.results$spectra$data <- Map(
      function(x, y) {
        x$spectra <- y
        x
      },
      private$.results$spectra$data, spec_sub
    )
    
    message(paste0("\U2713 ", "Blank spectra subtracted in spectra!"))
    
    TRUE
  }
}
