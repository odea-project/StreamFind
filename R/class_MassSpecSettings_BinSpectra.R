
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_BinSpectra_StreamFind**
#'
#' @description Bins spectral data according to units of a given variable (e.g., 5 retention time values) or based on 
#' bins given as a named list of numeric values, where the names are the bin labels (i.e. the name of the column) and 
#' the values are the bin dimensions (e.g. 5 seconds).
#' 
#' @param unitsVal Character of length one with the column name of the variable to be used for binning.
#' @param unitsNumber Integer of length one with the number of units to be used for binning.
#' @param bins Named list of numeric values with the bin dimensions.
#' @param refBinAnalysis The analysis index to use a reference for creating the bins.
#' 
#' @returns A MassSpecSettings_BinSpectra_StreamFind object.
#'
#' @export 
#'
MassSpecSettings_BinSpectra_StreamFind <- S7::new_class("MassSpecSettings_BinSpectra_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(unitsVal = NULL,
                         unitsNumber = NULL,
                         bins = NULL,
                         refBinAnalysis = NULL) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "BinSpectra",
      algorithm = "StreamFind",
      parameters = list(
        unitsVal = unitsVal,
        unitsNumber = unitsNumber,
        bins = bins,
        refBinAnalysis = refBinAnalysis
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "StreamFind",
      developer = "Ricardo Cunha",
      contact = "cunha@iuta.de",
      link = "https://odea-project.github.io/StreamFind",
      doi = NA_character_
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "BinSpectra"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_character(self@parameters$unitsVal, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$unitsNumber, len = 1, null.ok = TRUE),
      checkmate::test_list(self@parameters$bins, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$refBinAnalysis, len = 1, null.ok = TRUE)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_BinSpectra_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$spectra$spectra
  
  cache <- .load_chache("bin_spectra", spec_list, x)
  
  if (!is.null(cache$data)) {
    message("\U2139 Binned spectra loaded from cache!")
    engine$spectra$spectra <- cache$data
    message(paste0("\U2713 ", "Spectra binned!"))
    return(TRUE)
  }
  
  unitsVal = x$parameters$unitsVal
  unitsNumber = x$parameters$unitsNumber
  bins = x$parameters$bins
  refBinAnalysis = x$parameters$refBinAnalysis
  
  useRefBins <- FALSE
  ref_bins_seq_list <- NULL
  ref_bin_matrix <- NULL
  ref_bin_key <- NULL
  . <- NULL
  
  if (!is.null(refBinAnalysis)) {
    if (is.numeric(refBinAnalysis) && length(refBinAnalysis) == 1) {
      refBinAnalysis <- engine$analyses$names[refBinAnalysis]
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
  
  spec_binned <- lapply(spec_list, function(z) {
    
    if (nrow(z) == 0) return(data.table::data.table())
    
    # performs binning based on number of units, i.e. traces, for a given dimension
    if (!is.null(unitsVal) && !is.null(unitsNumber) && is.null(bins)) {
      
      intensity <- NULL
      
      unitVal <- unique(z[[unitsVal]])
      
      max_i <- length(unitVal) # maximum number of scan
      
      min_i <- 1
      
      unitSections <- seq(min_i, max_i, unitsNumber)
      
      idx <- seq_len(max_i)
      
      binKey <- rep(NA_integer_, max_i)
      
      for (i in unitSections) binKey[idx >= i] <- i
      
      names(binKey) <- as.character(unitVal)
      
      res <- data.table::data.table("intensity" = z$intensity, "bin_key" = binKey[as.character(z[[unitsVal]])])
      
      xVals <- colnames(z)
      
      xVals <- xVals[!xVals %in% c("analysis", "replicate", "intensity")]
      
      for (i in xVals) res[[i]] <- z[[i]]
      
      valKeys <- xVals[!xVals %in% unitsVal]
      
      res[["z"]] <- res[[unitsVal]]
      
      res <- res[, .(z = mean(z), intensity = mean(intensity)), by = c("bin_key", valKeys)]
      
      res[[unitsVal]] <- res[["z"]]
      
      res$z <- NULL
      
      res <- unique(res)
      
      data.table::setcolorder(res, c(unitsVal, valKeys, "intensity"))
      
      res$bin_key <- NULL
      
      if ("replicate" %in% colnames(z)) {
        res$replicate <- unique(z$replicate)
        data.table::setcolorder(res, c("replicate"))
      }
      
      if ("analysis" %in% colnames(z)) {
        res$analysis <- unique(z$analysis)
        data.table::setcolorder(res, c("analysis"))
      }
      
      res
      
    } else if (!is.null(bins)) {
      
      if (useRefBins) {
        bins_seq_list <- ref_bins_seq_list
        bin_matrix <- ref_bin_matrix
        bin_key <- ref_bin_key
        
      } else {
        
        if (!all(names(bins) %in% colnames(z))) stop("Names in bins not fouond in spectra columns!")
        
        .make_bin_sequence <- function(vec, bin_size) seq(round(min(vec), digits = 0), round(max(vec) , digits = 0), bin_size)
        
        bins_seq_list <- Map(function(name, val) .make_bin_sequence(z[[name]], val), names(bins), bins)
        
        bin_matrix <- as.data.frame(do.call(expand.grid, bins_seq_list))
        
        colnames(bin_matrix) <- names(bins)
        
        bin_key <- apply(bin_matrix, 1, function(i) paste0(i, collapse = "-"))
      }
      
      ints <- rcpp_fill_bin_spectra(z, bin_matrix, bins, overlap = 0.1, summaryFunction = "mean")
      
      out <- data.table::data.table(
        "analysis" = unique(z$analysis),
        "id" = unique(z$id),
        "polarity" = unique(z$polarity),
        "rt" = bin_matrix$rt,
        "mz" = bin_matrix$mz,
        "mass" = bin_matrix$mass,
        "intensity" = ints,
        "bins" = bin_key
      )
      
      out
      
    } else {
      z
    }
    
  })
  
  if (!is.null(cache$hash)) {
    .save_cache("bin_spectra", spec_binned, cache$hash)
    message("\U1f5ab Binned spectra cached!")
  }
  
  engine$spectra$spectra <- spec_binned
  message(paste0("\U2713 ", "Spectra binned!"))
  TRUE
}
