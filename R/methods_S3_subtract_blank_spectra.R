#' @title .s3_average_spectra.Settings_subtract_blank_spectra_StreamFind
#'
#' @description Subtracts blank analyses according to blank correspondence.
#'
#' @noRd
#'
.s3_subtract_blank_spectra.Settings_subtract_blank_spectra_StreamFind <- function(settings, self, private) {
  
  ntozero <- settings$parameters$negativeToZero
  
  if (self$has_spectra()) {
    spec_list <- self$spectra
    
  } else {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  blks <- self$get_blank_names()
  
  names(blks) <- self$get_replicate_names()
  
  blk_anas <- self$get_replicate_names()
  blk_anas <- blk_anas[blk_anas %in% blks]
  
  spec_blk <- spec_list[names(spec_list) %in% c(blks, names(blk_anas))]
  
  if (length(spec_blk) == length(unique(blks))) {
    names(spec_blk) <- unique(blks)
    
  } else if (length(spec_blk) == length(blk_anas)) {
    names(spec_blk) <- blk_anas
    
  } else {
    warning("Blank spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_sub <- lapply(spec_list, function(x) {
    
    if (nrow(x) == 0) return(x)
    
    if (!"replicate" %in% colnames(x)) {
      rp <- self$get_replicate_names()[x$analysis[1]]
      
    } else {
      rp <- unique(x$replicate)
    }
    
    if (rp %in% blks) return(data.table())
    
    blk <- spec_blk[names(spec_blk) %in% blks[rp]]
    
    if (length(blk) > 1) {
      intensity <- NULL
      blk <- rbindlist(blk)
      blk[["analysis"]] <- NULL
      blk[["replicate"]] <- NULL
      blk[["polarity"]] <- NULL
      blk[["level"]] <- NULL
      blk[["pre_mz"]] <- NULL
      blk[["pre_ce"]] <- NULL
      
      merge_vals <- character()
      if ("shift" %in% colnames(blk)) merge_vals <- c(merge_vals, "shift")
      if ("rt" %in% colnames(blk)) merge_vals <- c(merge_vals, "rt")
      if ("mz" %in% colnames(blk)) merge_vals <- c(merge_vals, "mz")
      
      blk <- blk[, intensity := mean(intensity), by = merge_vals]
      
      blk <- unique(blk)
      
      blk <- blk$intensity
      
    } else {
      blk <- blk[[1]]$intensity
    }
    
    if (length(blk) != nrow(x)) {
      warning("Spectra do not have the same dimention! Not done.")
      return(x)
    }
    
    x$blank <- blk
    
    x$intensity <- x$intensity - blk
    
    if (ntozero) x$intensity[x$intensity < 0] <- 0
    
    if ("analysis" %in% colnames(x) && "replicate" %in% colnames(x)) {
      if (unique(x$analysis) == unique(x$replicate)) {
        x[["analysis"]] <- NULL
      }
    }
    
    x <- unique(x)
    
    x
  })
  
  self$spectra <- spec_sub
  
  message(paste0("\U2713 ", "Blank spectra subtracted in spectra!"))
  
  TRUE
}
