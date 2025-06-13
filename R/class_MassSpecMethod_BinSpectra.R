#' Mass Spectrometry Method for Binning Spectra (StreamFind algorithm)
#'
#' @description Bins spectral data based on variables.
#' 
#' @param binNames Character with the variable names for building the bins. Possible values are
#' *rt*, *mz*, *mass*, *mobility*. Note that the `binNames` must be in the spectra column names.
#' @param binValues Numeric with the bin values for each variable.
#' @param byUnit Logical of length one to bin by unit, meaning that the binning is performed by the
#' number of units not actual values. For instance, if the bin is 10 and `binName` is *rt*, then the
#' binning is performed by 10 seconds not 10 values for each bin. If byUnit is `FALSE`, then the
#' binning is performed by the actual values but only the first of binNames and binValues is used.
#' @param refBinAnalysis The analysis index to use a reference for creating the bins.
#' 
#' @returns A MassSpecMethod_BinSpectra_StreamFind object.
#'
#' @export 
#'
MassSpecMethod_BinSpectra_StreamFind <- S7::new_class(
  name = "MassSpecMethod_BinSpectra_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(binNames = c("rt", "mz"),
                         binValues = c(5, 10),
                         byUnit = TRUE,
                         refBinAnalysis = NA_integer_) {
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "BinSpectra",
        required = "LoadSpectra",
        algorithm = "StreamFind",
        parameters = list(
          binNames = as.character(binNames),
          binValues = as.numeric(binValues),
          byUnit = as.logical(byUnit),
          refBinAnalysis = as.integer(refBinAnalysis)
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "BinSpectra")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_character(self@parameters$binNames)
    checkmate::assert_true(all(self@parameters$binNames %in% c("rt", "mz", "mass", "mobility")))
    checkmate::assert_numeric(self@parameters$binValues)
    checkmate::assert_true(length(self@parameters$binNames) == length(self@parameters$binValues))
    checkmate::assert_logical(self@parameters$byUnit, len = 1)
    checkmate::assert_numeric(self@parameters$refBinAnalysis, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_BinSpectra_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$Analyses$has_results_spectra) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$Spectra$spectra
  
  binNames = x$parameters$binNames
  binValues = x$parameters$binValues
  byUnit = x$parameters$byUnit
  refBinAnalysis = x$parameters$refBinAnalysis
  
  useRefBins <- FALSE
  ref_bins_seq_list <- NULL
  ref_bin_matrix <- NULL
  ref_bin_key <- NULL
  . <- NULL
  
  if (!is.na(refBinAnalysis)) {
    if (is.integer(refBinAnalysis) && length(refBinAnalysis) == 1) {
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
  } else {
    refSpec <- data.table::rbindlist(spec_list)
  }
  
  if (byUnit && !is.null(binNames)) {
    if (!all(names(binNames) %in% colnames(refSpec))) {
      stop("Names in bins not found in spectra columns")
    }
    .make_bin_sequence <- function(vec, bin_size) {
      seq(round(min(vec), digits = 0), round(max(vec) , digits = 0), bin_size)
    }
    ref_bins_seq_list <- Map(function(name, val) {
      .make_bin_sequence(refSpec[[name]], val)
    }, binNames, binValues)
    ref_bin_matrix <- as.data.frame(do.call(expand.grid, ref_bins_seq_list))
    colnames(ref_bin_matrix) <- binNames
    ref_bin_key <- apply(ref_bin_matrix, 1, function(x) paste0(x, collapse = "-"))
    if (length(ref_bin_key) > 0) useRefBins <- TRUE
  }
  
  spec_binned <- lapply(spec_list, function(z,
                                            ref_bin_key,
                                            ref_bin_matrix,
                                            binNames,
                                            binValues,
                                            byUnit) {
    
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
      res <- data.table::data.table(
        "intensity" = z$intensity,
        "bin_key" = binKey[as.character(z[[binNames[1]]])]
      )
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
      bins <- lapply(seq_along(binNames), function(k) binValues[k])
      names(bins) <- binNames
      ints <- rcpp_fill_bin_spectra(z, ref_bin_matrix, bins, overlap = 0.1, summaryFunction = "mean")
      out <- data.table::data.table(
        "analysis" = unique(z$analysis),
        "id" = unique(z$id),
        "polarity" = unique(z$polarity),
        "rt" = ref_bin_matrix$rt,
        "mz" = ref_bin_matrix$mz,
        "mass" = ref_bin_matrix$mass,
        "intensity" = ints,
        "bins" = ref_bin_key
      )
      out
      
    } else {
      z
    }
  },
  ref_bin_key = ref_bin_key,
  ref_bin_matrix = ref_bin_matrix,
  binNames = binNames,
  binValues = binValues,
  byUnit = byUnit
  )
  
  engine$Spectra$spectra <- spec_binned
  message(paste0("\U2713 ", "Spectra binned!"))
  TRUE
}
