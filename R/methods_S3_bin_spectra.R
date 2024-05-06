
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
  
  if (self$has_spectra()) {
    spec_list <- self$spectra
    
  } else {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  unitsVal = settings$parameters$unitsVal
  
  unitsNumber = settings$parameters$unitsNumber
  
  bins = settings$parameters$bins
  
  refBinAnalysis = settings$parameters$refBinAnalysis
  
  cache <- .load_chache("bin_spectra", spec_list, settings)
  
  if (!is.null(cache$data)) {
    message("\U2139 Spectra loaded from cache!")
    self$spectra <- cache$data
    message(paste0("\U2713 ", "Spectra binned!"))
    return(TRUE)
  }
  
  useRefBins <- FALSE
  ref_bins_seq_list <- NULL
  ref_bin_matrix <- NULL
  ref_bin_key <- NULL
  
  if (!is.null(refBinAnalysis)) {
    if (is.numeric(refBinAnalysis) && length(refBinAnalysis) == 1) {
      refBinAnalysis <- self$get_analysis_names()[refBinAnalysis]
    }
    
    if (!is.character(refBinAnalysis)) {
      warning("Reference analysis not found! Not done.")
      return(FALSE)
    }
    
    refSpec <- spec_list[[refBinAnalysis]]
    
    if (nrow(refSpec) == 0) {
      warning("Reference analysis not found! Not done.")
      return(FALSE)
    }
    
    if (!is.null(bins)) {
      
      if (!all(names(bins) %in% colnames(refSpec))) stop("Names in bins not found in spectra columns")
      
      .make_bin_sequence <- function(vec, bin_size) seq(round(min(vec), digits = 0), round(max(vec) , digits = 0), bin_size)
      
      ref_bins_seq_list <- Map(function(name, val) .make_bin_sequence(refSpec[[name]], val), names(bins), bins)
      
      ref_bin_matrix <- as.data.frame(do.call(expand.grid, ref_bins_seq_list))
      
      colnames(ref_bin_matrix) <- names(bins)
      
      ref_bin_key <- apply(ref_bin_matrix, 1, function(x) paste0(x, collapse = "-"))
      
      useRefBins <- TRUE
    }
  }
    
  spec_binned <- lapply(spec_list, function(x) {
    
    if (nrow(x) == 0) return(data.table())
    
    # performs binning based on number of units, i.e. traces, for a given dimension
    if (!is.null(unitsVal) && !is.null(unitsNumber) && is.null(bins)) {
      
      intensity <- NULL
      
      unitVal <- unique(x[[unitsVal]])
      
      max_i <- length(unitVal) # maximum number of scan
      
      min_i <- 1
      
      unitSections <- seq(min_i, max_i, unitsNumber)
      
      idx <- seq_len(max_i)
      
      binKey <- rep(NA_integer_, max_i)
      
      for (i in unitSections) binKey[idx >= i] <- i
      
      names(binKey) <- as.character(unitVal)
      
      res <- data.table("intensity" = x$intensity, "bin_key" = binKey[as.character(x[[unitsVal]])])
      
      xVals <- colnames(x)
      
      xVals <- xVals[!xVals %in% c("analysis", "replicate", "intensity")]
      
      for (i in xVals) res[[i]] <- x[[i]]
      
      valKeys <- xVals[!xVals %in% unitsVal]
      
      res <- res[, .(x = mean(x), intensity = mean(intensity)), by = c("bin_key", valKeys), env = list(x = unitsVal)]
      
      res <- unique(res)
      
      setcolorder(res, c(unitsVal, valKeys, "intensity"))
      
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
    
    } else if (!is.null(bins)) {
      
      if (useRefBins) {
        bins_seq_list <- ref_bins_seq_list
        bin_matrix <- ref_bin_matrix
        bin_key <- ref_bin_key
      
      } else {
        
        if (!all(names(bins) %in% colnames(x))) stop("Names in bins not fouond in spectra columns!")
        
        .make_bin_sequence <- function(vec, bin_size) seq(round(min(vec), digits = 0), round(max(vec) , digits = 0), bin_size)
        
        bins_seq_list <- Map(function(name, val) .make_bin_sequence(x[[name]], val), names(bins), bins)
        
        bin_matrix <- as.data.frame(do.call(expand.grid, bins_seq_list))
        
        colnames(bin_matrix) <- names(bins)
        
        bin_key <- apply(bin_matrix, 1, function(x) paste0(x, collapse = "-"))
      }
      
      ints <- rcpp_fill_bin_spectra(x, bin_matrix, bins, overlap = 0.1, summaryFunction = "mean")
      
      out <- data.table(
        "analysis" = unique(x$analysis),
        "id" = unique(x$id),
        "polarity" = unique(x$polarity),
        "rt" = bin_matrix$rt,
        "mz" = bin_matrix$mz,
        "mass" = bin_matrix$mass,
        "intensity" = ints,
        "bins" = bin_key
      )
      
      out
      
    } else {
      x
    }
    
  })
    
  if (!is.null(cache$hash)) {
    .save_cache("bin_spectra", spec_binned, cache$hash)
    message("\U1f5ab Binned spectra cached!")
  }
  
  self$spectra <- spec_binned

  message(paste0("\U2713 ", "Spectra binned!"))
  
  TRUE
}
