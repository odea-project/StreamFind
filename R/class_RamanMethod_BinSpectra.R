#' **RamanMethod_BinSpectra_StreamFind**
#'
#' @description Bins spectral data based on variables.
#' 
#' @param binNames Character with the variable names for building the bins. Possible values are *rt* and *shift*. Note 
#' that the `binNames` must be in the spectra column names.
#' @param binValues Numeric with the bin values for each variable.
#' @param byUnit Logical of length one to bin by unit, meaning that the binning is performed by the number of units not
#' actual values. For instance, if the bin is 10 and `binName` is *rt*, then the binning is performed by 10 seconds not
#' 10 values for each bin. If byUnit is `FALSE`, then the binning is performed by the actual values but only the first 
#' of binNames and binValues is used.
#' @param refBinAnalysis The analysis index to use a reference for creating the bins.
#' 
#' @returns A RamanMethod_BinSpectra_StreamFind object.
#'
#' @export 
#'
RamanMethod_BinSpectra_StreamFind <- S7::new_class("RamanMethod_BinSpectra_StreamFind",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  
  constructor = function(binNames = c("rt"),
                         binValues = c(5),
                         byUnit = FALSE,
                         refBinAnalysis = NULL) {
    
    S7::new_object(ProcessingStep(
      data_type = "Raman",
      method = "BinSpectra",
      required = NA_character_,
      algorithm = "StreamFind",
      parameters = list(
        binNames = binNames,
        binValues = binValues,
        byUnit = byUnit,
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
    checkmate::assert_choice(self@data_type, "Raman")
    checkmate::assert_choice(self@method, "BinSpectra")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_character(self@parameters$binNames)
    checkmate::assert_true(all(self@parameters$binNames %in% c("rt", "shift")))
    checkmate::assert_numeric(self@parameters$binValues)
    checkmate::assert_true(length(self@parameters$binNames) == length(self@parameters$binValues))
    checkmate::assert_logical(self@parameters$byUnit, len = 1)
    checkmate::assert_numeric(self@parameters$refBinAnalysis, len = 1, null.ok = TRUE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanMethod_BinSpectra_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$Analyses$has_spectra) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$Spectra$spectra
  
  cache <- .load_cache_sqlite("bin_spectra", spec_list, x)
  
  if (!is.null(cache$data)) {
    message("\U2139 Binned spectra loaded from cache!")
    engine$Spectra$spectra <- cache$data
    message(paste0("\U2713 ", "Spectra binned!"))
    return(TRUE)
  }
  
  binNames = x$parameters$binNames
  binValues = x$parameters$binValues
  byUnit = x$parameters$byUnit
  refBinAnalysis = x$parameters$refBinAnalysis
  
  useRefBins <- FALSE
  ref_bins_seq_list <- NULL
  ref_bin_matrix <- NULL
  ref_bin_key <- NULL
  . <- NULL
  
  if (!is.null(refBinAnalysis)) {
    if (is.numeric(refBinAnalysis) && length(refBinAnalysis) == 1) {
      refBinAnalysis <- names(engine$Analyses)[refBinAnalysis]
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
    
    if (byUnit && !is.null(binNames)) {
      
      if (!all(names(binNames) %in% colnames(refSpec))) stop("Names in bins not found in spectra columns")
      
      .make_bin_sequence <- function(vec, bin_size) seq(round(min(vec), digits = 0), round(max(vec) , digits = 0), bin_size)
      
      ref_bins_seq_list <- Map(function(name, val) .make_bin_sequence(refSpec[[name]], val), binNames, binValues)
      
      ref_bin_matrix <- as.data.frame(do.call(expand.grid, ref_bins_seq_list))
      
      colnames(ref_bin_matrix) <- binNames
      
      ref_bin_key <- apply(ref_bin_matrix, 1, function(x) paste0(x, collapse = "-"))
      
      if (length(ref_bin_key) > 0) useRefBins <- TRUE
    }
  }
  
  spec_binned <- lapply(spec_list, function(z) {
    
    if (nrow(z) == 0) return(data.table::data.table())
    
    if (!byUnit && binNames[1] %in% colnames(z)) {
      
      intensity <- NULL
      
      unitVal <- unique(z[[binNames[1]]])
      
      max_i <- length(unitVal) # maximum number of scan
      
      min_i <- 1
      
      unitSections <- seq(min_i, max_i, binValues[1])
      
      idx <- seq_len(max_i)
      
      binKey <- rep(NA_integer_, max_i)
      
      for (i in unitSections) binKey[idx >= i] <- i
      
      names(binKey) <- as.character(unitVal)
      
      res <- data.table::data.table("intensity" = z$intensity, "bin_key" = binKey[as.character(z[[binNames[1]]])])
      
      xVals <- colnames(z)
      
      xVals <- xVals[!xVals %in% c("analysis", "replicate", "intensity")]
      
      for (i in xVals) res[[i]] <- z[[i]]
      
      valKeys <- xVals[!xVals %in% binNames[1]]
      
      res[["z"]] <- res[[binNames[1]]]
      
      res <- res[, .(z = mean(z), intensity = mean(intensity)), by = c("bin_key", valKeys)]
      
      res[[binNames[1]]] <- res[["z"]]
      
      res$z <- NULL
      
      res <- unique(res)
      
      data.table::setcolorder(res, c(binNames[1], valKeys, "intensity"))
      
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
      
    } else if (byUnit) {
      
      if (useRefBins) {
        bins_seq_list <- ref_bins_seq_list
        bin_matrix <- ref_bin_matrix
        bin_key <- ref_bin_key
        
      } else {
        
        if (!all(binNames %in% colnames(z))) stop("Names in bins not fouond in spectra columns!")
        
        .make_bin_sequence <- function(vec, bin_size) seq(round(min(vec), digits = 0), round(max(vec) , digits = 0), bin_size)
        
        bins_seq_list <- Map(function(name, val) .make_bin_sequence(z[[name]], val), binNames, binValues)
        
        bin_matrix <- as.data.frame(do.call(expand.grid, bins_seq_list))
        
        colnames(bin_matrix) <- binNames
        
        bin_key <- apply(bin_matrix, 1, function(i) paste0(i, collapse = "-"))
      }
      
      bins <- lapply(seq_along(binNames), function(z) binValues[z])
      names(bins) <- binNames
      
      ints <- rcpp_fill_bin_spectra(z, bin_matrix, bins, overlap = 0.1, summaryFunction = "mean")
      
      browser()
      
      out <- data.table::data.table(
        "analysis" = unique(z$analysis),
        "id" = unique(z$id),
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
    .save_cache_sqlite("bin_spectra", spec_binned, cache$hash)
    message("\U1f5ab Binned spectra cached!")
  }
  
  engine$Spectra$spectra <- spec_binned
  message(paste0("\U2713 ", "Spectra binned!"))
  TRUE
}
