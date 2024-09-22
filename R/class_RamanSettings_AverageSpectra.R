
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_AverageSpectra_StreamFind**
#'
#' @description Averages spectra based on analysis replicate groups.
#' 
#' @param collapseTime Logical (length 1). When `TRUE` the spectra are averaged, reducing the time variable.
#'
#' @return A RamanSettings_AverageSpectra_StreamFind object.
#'
#' @export
#'
RamanSettings_AverageSpectra_StreamFind <- S7::new_class("RamanSettings_AverageSpectra_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(collapseTime = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "AverageSpectra",
      algorithm = "StreamFind",
      parameters = list(collapseTime = collapseTime),
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
      checkmate::test_choice(self@engine, "Raman"),
      checkmate::test_choice(self@method, "AverageSpectra"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_logical(self@parameters$collapseTime, max.len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_AverageSpectra_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
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
  
  if (engine$spectra$is_averaged) {
    warning("Spectra are already averaged! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$spectra$spectra
  
  spec <- data.table::rbindlist(spec_list, idcol = "analysis", fill = TRUE)
  
  collapseTime <- x$parameters$collapseTime
  
  if ("analysis" %in% colnames(spec)) {
    . <- NULL
    baseline <- NULL
    
    rpl <- engine$analyses$replicates
    spec$replicate <- rpl[spec$analysis]
    spec$analysis <- NULL
    rpl <- unique(rpl)
    
    spec_list <- split(spec, spec$replicate)
    for (r in rpl) if (!r %in% names(spec_list)) spec_list[[r]] <- data.table::data.table()
    spec_list <- spec_list[rpl]
    
    av_list <- lapply(spec_list, function(z) {
      if (nrow(z) == 0) return(z)
      intensity <- NULL
      rt = NULL
      groupCols <- "replicate"
      
      if ("shift" %in% colnames(z)) groupCols <- c("shift", groupCols)
      
      if ("rt" %in% colnames(z) && !collapseTime) groupCols <- c("rt", groupCols)
      
      if ("mz" %in% colnames(z)) groupCols <- c("mz", groupCols)
      
      if ("mass" %in% colnames(z)) groupCols <- c("mass", groupCols)
      
      if ("bins" %in% colnames(z)) groupCols <- c("bins", groupCols)
      
      if (collapseTime && "rt" %in% colnames(z)) {
        
        if ("raw" %in% colnames(z) && !"baseline" %in% colnames(z)) {
          z <- z[, c("rt", "intensity", "raw") := .(mean(rt), mean(intensity), mean(raw)), by = groupCols]
          
        } else if ("raw" %in% colnames(z) && "baseline" %in% colnames(z)) {
          z <- z[, c("rt", "intensity", "raw", "baseline") := .(mean(rt), mean(intensity), mean(raw), mean(baseline)), by = groupCols]
          
        } else {
          z <- z[, c("rt", "intensity") := .(mean(rt), mean(intensity)), by = groupCols]
        }
        
        z <- z[, c("intensity", "rt") := .(mean(intensity), mean(rt)), by = groupCols]
        
      } else {
        
        if ("raw" %in% colnames(z) && !"baseline" %in% colnames(z)) {
          z <- z[, c("intensity", "raw") := .(mean(rt), mean(intensity), mean(raw)), by = groupCols]
          
        } else if ("raw" %in% colnames(z) && "baseline" %in% colnames(z)) {
          z <- z[, c("intensity", "raw", "baseline") := .(mean(intensity), mean(raw), mean(baseline)), by = groupCols]
          
        } else {
          z <- z[, c("intensity") := .(mean(intensity)), by = groupCols]
        }
      }
      
      z <- unique(z)
      z$replicate <- NULL
      z
    })
    
    spectra <- engine$spectra
    spectra$is_averaged <- TRUE
    spectra$spectra <- av_list
    engine$spectra <- spectra
    message(paste0("\U2713 ", "Averaged spectra!"))
    TRUE
  } else {
    FALSE
  }
}
