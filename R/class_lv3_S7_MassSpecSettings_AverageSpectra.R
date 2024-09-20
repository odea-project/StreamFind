
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_AverageSpectra_StreamFind**
#'
#' @description Averages spectra based on analysis replicate groups.
#' 
#' @param collapseTime Logical (length 1). When `TRUE` the spectra are averaged, reducing the time variable.
#'
#' @return A MassSpecSettings_AverageSpectra_StreamFind object.
#'
#' @export
#'
MassSpecSettings_AverageSpectra_StreamFind <- S7::new_class("MassSpecSettings_AverageSpectra_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(collapseTime = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
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
      checkmate::test_choice(self@engine, "MassSpec"),
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
S7::method(run, MassSpecSettings_AverageSpectra_StreamFind) <- function(x, engine = NULL) {
  
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
  
  spec <- data.table::rbindlist(spec_list, fill = TRUE)
  
  collapseTime <- x$parameters$collapseTime
  
  if ("analysis" %in% colnames(spec)) {
    
    . <- NULL
    
    baseline <- NULL
    
    rpl <- engine$analyses$replicates
    
    spec$replicate <- rpl[spec$analysis]
    
    spec_list <- split(spec, spec$replicate)
    
    av_list <- lapply(spec_list, function(x) {
      
      intensity <- NULL
      
      rt = NULL
      
      res <- data.table::copy(x)
      
      res[["analysis"]] <- NULL
      
      groupCols <- "replicate"
      
      if ("shift" %in% colnames(res)) groupCols <- c("shift", groupCols)
      
      if ("rt" %in% colnames(x) && !collapseTime) groupCols <- c("rt", groupCols)
      
      if ("mz" %in% colnames(x)) groupCols <- c("mz", groupCols)
      
      if ("mass" %in% colnames(x)) groupCols <- c("mass", groupCols)
      
      if ("bins" %in% colnames(x)) groupCols <- c("bins", groupCols)
      
      if (collapseTime && "rt" %in% colnames(res)) {
        
        if ("raw" %in% colnames(res) && !"baseline" %in% colnames(res)) {
          res <- res[, c("rt", "intensity", "raw") := .(mean(rt), mean(intensity), mean(raw)), by = groupCols]
          
        } else if ("raw" %in% colnames(res) && "baseline" %in% colnames(res)) {
          res <- res[, c("rt", "intensity", "raw", "baseline") := .(mean(rt), mean(intensity), mean(raw), mean(baseline)), by = groupCols]
          
        } else {
          res <- res[, c("rt", "intensity") := .(mean(rt), mean(intensity)), by = groupCols]
        }
        
        res <- res[, c("intensity", "rt") := .(mean(intensity), mean(rt)), by = groupCols]
        
      } else {
        
        if ("raw" %in% colnames(res) && !"baseline" %in% colnames(res)) {
          res <- res[, c("intensity", "raw") := .(mean(rt), mean(intensity), mean(raw)), by = groupCols]
          
        } else if ("raw" %in% colnames(res) && "baseline" %in% colnames(res)) {
          res <- res[, c("intensity", "raw", "baseline") := .(mean(intensity), mean(raw), mean(baseline)), by = groupCols]
          
        } else {
          res <- res[, c("intensity") := .(mean(intensity)), by = groupCols]
        }
      }
      
      res <- unique(res)
      
      data.table::setcolorder(res, c("replicate"))
      
      res
      
    })
    
    engine$spectra$spectra <- av_list
    message(paste0("\U2713 ", "Averaged spectra!"))
    TRUE
    
  } else {
    FALSE
  }
}
