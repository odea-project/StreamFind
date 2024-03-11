
#' @title .s3_bin_spectra.Settings_bin_spectra_qBinning
#'
#' @description Bins spectra using the algorithm qBinning.
#'
#' @noRd
#'
.s3_bin_spectra.Settings_bin_spectra_qBinning <- function(settings, self, private) {

  message("Binning spectra with qBinning...", appendLF = TRUE)
  
  if (self$get_number_analyses() == 0) {
    warning("There are no analyses! Add MS analyses as mzML or mzXML files!")
    return(FALSE)
  }

  if (!any(self$has_loaded_spectra())) self$load_spectra()

  if (!any(self$has_loaded_spectra())) {
    warning("Spectra not found in MS analyses.")
    return(FALSE)
  }

  spectra <- self$get_spectra()

  # TODO Max Implementation qBinning method

  # self$add_features_eic(eics, replace = TRUE)

  FALSE
}

#' @title .s3_bin_spectra.Settings_bin_spectra_StreamFind
#'
#' @description Bins spectra.
#'
#' @noRd
#'
.s3_bin_spectra.Settings_bin_spectra_StreamFind <- function(settings, self, private) {
  
  if (self$has_averaged_spectra()) {
    spec_list <- self$averaged_spectra
    
  } else if (self$has_spectra()) {
    spec_list <- self$spectra
    
  } else {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  valSpectrumUnits = settings$parameters$valSpectrumUnits
  
  windowSpectrumUnits = settings$parameters$windowSpectrumUnits
  
  xVals = settings$parameters$xVals #c("rt", "shift")
  
  xWindows = settings$parameters$xWindows
  
  cached_analyses <- FALSE
  
  if (.caches_data()) {
    hash <- patRoon::makeHash(spec_list, settings)
    spec_binned <- patRoon::loadCacheData("bin_spectra", hash)
    
    if (!is.null(spec_binned)) {
      check <- identical(names(spec_binned), names(spec_list))
      
      if (all(check)) {
        cached_analyses <- TRUE
        
      } else {
        spec_binned <- NULL
      }
    }
  } else {
    hash <- NULL
    spec_binned <- NULL
  }
  
  if (is.null(spec_binned)) {
    
    spec_binned <- lapply(spec_list, function(x) {
      
      if (nrow(x) == 0) return(data.table())
      
      if (!is.null(valSpectrumUnits) && !is.null(windowSpectrumUnits)) {
        
        unitVal <- unique(x[[valSpectrumUnits]])
        
        max_i <- length(unitVal) # maximum number of scan
        
        min_i <- 1
        
        unitSections <- seq(min_i, max_i, windowSpectrumUnits)
        
        idx <- seq_len(max_i)
        
        binKey <- rep(NA_integer_, max_i)
        
        for (i in unitSections) binKey[idx >= i] <- i
        
        names(binKey) <- as.character(unitVal)
        
        res <- data.table("intensity" = x$intensity, "bin_key" = binKey[as.character(x[[valSpectrumUnits]])])
        
        if (is.null(xVals)) {
          xVals <- colnames(x)
          xVals <- xVals[!xVals %in% c("analysis", "replicate", "intensity")]
        }
        
        for (i in xVals) res[[i]] <- x[[i]]
        
        valKeys <- xVals[!xVals %in% valSpectrumUnits]
        
        res <- res[, .(x = mean(x), intensity = mean(intensity)), by = c("bin_key", valKeys), env = list(x = valSpectrumUnits)]
        
        res <- unique(res)
        
        setcolorder(res, c(valSpectrumUnits, valKeys, "intensity"))
        
        res$bin_key <- NULL
        
        if ("replicate" %in% colnames(x)) {
          res$replicate <- unique(x$replicate)
          setcolorder(res, c("replicate"))
        }
        
        if ("analysis" %in% colnames(x)) {
          res$analysis <- unique(x$analysis)
          setcolorder(res, c("analysis"))
        }
        
        res
        
      } else {
        
        max_x <- max(x[[xVal]])
        min_x <- min(x[[xVal]])
        
        max_x2 <- max(x[[x2Val]])
        min_x2 <- min(x[[x2Val]])
        
        
        
        
        
        
        
        
        
        
      }
      
      # x_all <- seq(round(min_x, digits = 0), round(max_x , digits = 0), rt_bin_size)
      # x2_all <- seq(round(min_x2, digits = 0), round(max_x2 , digits = 0), mz_bin_size)
      # 
      # bins_number <- length(rts_all) * length(mzs_all)
      # bins_id <- rep(NA_character_, bins_number)
      # mat <- matrix(rep(1, bins_number * 2), nrow = bins_number, ncol = 2)
      # counter <- 0
      # for (i in seq_len(length(rts_all))) {
      #   for (j in seq_len(length(mzs_all))) {
      #     bins_id[counter + j] <- paste0(rts_all[i], "-", mzs_all[j])
      #     mat[counter + j, 1] <- rts_all[i]
      #     mat[counter + j, 2] <- mzs_all[j]
      #   }
      #   counter <- counter + j
      # }
      # dimnames(mat) <- list(bins_id, c("rt", "mz"))
      # as.data.frame(mat)
      
    })
    
    if (!is.null(hash)) {
      patRoon::saveCacheData("bin_spectra", spec_binned, hash)
      message("\U1f5ab Binned spectra cached!")
    }
    
  }
  
  if (self$has_averaged_spectra()) {
    
    private$.results$spectra$data <- Map(
      function(x, y) {
        x$average <- y
        x
      },#
      private$.results$spectra$data, spec_binned
    )
    
  } else {
    
    spec_list <- Map(function(x, y) list("spectra" = x, "average" = y), spec_list, spec_binned)
    
    self$add_results(
      list("spectra" = list(
        "data" = spec_list,
        "software" = "StreamFind",
        "version" = as.character(packageVersion("StreamFind"))
      ))
    )
  }
  
  message(paste0("\U2713 ", "Spectra binned!"))
  
  TRUE
}
