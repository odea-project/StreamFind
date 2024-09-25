
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_SubtractBlankSpectra_StreamFind**
#'
#' @description Subtracts the blank spectra to each analysis according to the blank assignment.
#' 
#' @param negativeToZero Logical (length 1) indicating if negative values should be set to zero.
#'
#' @return A MassSpecSettings_SubtractBlankSpectra_StreamFind object.
#'
#' @export
#'
MassSpecSettings_SubtractBlankSpectra_StreamFind <- S7::new_class("MassSpecSettings_SubtractBlankSpectra_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(negativeToZero = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "SubtractBlankSpectra",
      algorithm = "StreamFind",
      parameters = list(negativeToZero = negativeToZero),
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
      checkmate::test_choice(self@method, "SubtractBlankSpectra"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_logical(self@parameters$negativeToZero, max.len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_SubtractBlankSpectra_StreamFind) <- function(x, engine = NULL) {
  
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
  
  spec_list <- Map(function(i, j) {
    i$analysis <- j
    i
  }, spec_list, names(spec_list))
  
  ntozero <- x$parameters$negativeToZero
  
  blks <- engine$analyses$blanks
  names(blks) <- engine$analyses$replicates
  blk_anas <- engine$analyses$replicates
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
  
  spec_sub <- lapply(spec_list, function(z) {
    
    if (nrow(z) == 0) return(z)
    
    if (!engine$spectra$is_averaged) {
      rp <- engine$analyses$replicates[z$analysis[1]]
      
    } else {
      rp <- z$analysis[1]
    }
    
    if (rp %in% blks) return(data.table::data.table())
    
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
    
    if (length(blk) != nrow(z)) {
      warning("Spectra do not have the same dimention! Not done.")
      return(z)
    }
    
    z$blank <- blk
    z$intensity <- z$intensity - blk
    
    if (ntozero) z$intensity[z$intensity < 0] <- 0
    
    z$analysis <- NULL
    z <- unique(z)
    
    z
  })
  
  engine$spectra$spectra <- spec_sub
  message(paste0("\U2713 ", "Blank spectra subtracted in spectra!"))
  TRUE
}
