#' Raman Method for Binning Time Scans (native algorithm)
#'
#' @description Merges scans by averaging spectra according to a given binning size.
#' 
#' @param mode Character (length 1) with the binning mode. Possible values are `scans` and `time`.
#' When `scans` is selected, the binning is done by the number of scans. When `time` is selected, 
#' the binning is done by time unit (e.g., seconds or minutes).
#' @param value Numeric (length 1) with the binning size.
#' @param global Logical (length 1) to perform a global binning. If `TRUE`, the binning is performed
#' over all analyses. If `FALSE`, the binning is performed for each analysis.
#' @param refAnalysis Integer (length 1) with the reference analysis index to use for binning.
#' 
#' @returns A RamanMethod_BinScans_native object.
#'
#' @export 
#'
RamanMethod_BinScans_native <- S7::new_class(
  name = "RamanMethod_BinScans_native",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  constructor = function(mode = c("unit"),
                         value = 5,
                         global = FALSE,
                         refAnalysis = NA_integer_) {
    S7::new_object(
      ProcessingStep(
        data_type = "Raman",
        method = "BinScans",
        required = NA_character_,
        algorithm = "native",
        parameters = list(
          mode = mode,
          value = value,
          global = global,
          refAnalysis = refAnalysis
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
    checkmate::assert_choice(self@data_type, "Raman")
    checkmate::assert_choice(self@method, "BinScans")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_choice(self@parameters$mode, c("scans", "time"))
    checkmate::assert_numeric(self@parameters$value, max.len = 1)
    checkmate::assert_logical(self@parameters$global, len = 1)
    checkmate::assert_numeric(self@parameters$refAnalysis, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanMethod_BinScans_native) <- function(x, engine = NULL) {
  
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
  
  binning_mode <- x$parameters$mode
  binning_value <- x$parameters$value
  binning_global <- x$parameters$global
  binning_refAnalysis <- x$parameters$refAnalysis
  
  if (binning_global) {
    
    warning("Not yet implemented")
    return(FALSE)
    
    if (is.na(binning_refAnalysis)) {
      binning_refAnalysis <- 1
    }
    if (binning_refAnalysis > length(spec_list)) {
      warning("Reference analysis index is out of range!")
      return(FALSE)
    }
    
    refSpec <- spec_list[[binning_refAnalysis]]
  }
  
  spec_binned <- lapply(spec_list, function(z) {
    
    if (nrow(z) == 0) return(data.table::data.table())
    
    if (!"rt" %in% colnames(z)) {
      warning("No retention time column found in spectra!")
      return(z)
    }
    
    urts <- unique(z$rt)
    ushifts <- unique(z$shift)
    urts_size <- length(urts)
    ushifts_size <- length(ushifts)
    
    .SD <- NULL
    
    if ("scans" %in% binning_mode) {
      bin_key <- cut(seq_len(urts_size), seq(1, urts_size, binning_value), include.lowest = TRUE)
      bin_key <- as.numeric(bin_key)
      bin_key[is.na(bin_key)] <- max(bin_key, na.rm = TRUE)
      bin_key <- rep(bin_key, 1, each = ushifts_size)
      z$bin_key <- bin_key
      z <- z[, lapply(.SD, mean), by = c("bin_key", "shift")]
      z$bin_key <- NULL
    } else {
      bin_key <- cut(urts, seq(min(urts), max(urts), binning_value), include.lowest = TRUE)
      bin_key_right <- as.numeric(gsub(".*,(.*)]", "\\1", bin_key))
      bin_key_left <- as.numeric(gsub("[\\[(](.*),.*", "\\1", bin_key))
      bin_key <- (bin_key_right + bin_key_left) / 2
      bin_key[is.na(bin_key)] <- max(bin_key, na.rm = TRUE)
      bin_key <- rep(bin_key, 1, each = ushifts_size)
      z$rt <- bin_key
      z <- z[, lapply(.SD, mean), by = c("rt", "shift")]
    }
    
    z
  })
  
  engine$Spectra$spectra <- spec_binned
  message(paste0("\U2713 ", "Scans binned!"))
  TRUE
}
