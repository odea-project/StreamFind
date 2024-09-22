
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_DeleteSpectraSection_StreamFind**
#'
#' @description Deletes a section of the spectra based on a named list of data ranges for a given variable (i.e. column name).
#' 
#' @param section Named list with the variable to be used for sectioning and the window for the sectioning. The names 
#' should match column names in the data.
#'
#' @return A RamanSettings_DeleteSpectraSection_StreamFind object.
#'
#' @export
#'
RamanSettings_DeleteSpectraSection_StreamFind <- S7::new_class("RamanSettings_DeleteSpectraSection_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(section = list()) {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "DeleteSpectraSection",
      algorithm = "StreamFind",
      parameters = list(section = section),
      number_permitted = Inf,
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
      checkmate::test_choice(self@method, "DeleteSpectraSection"),
      checkmate::test_choice(self@algorithm, "StreamFind")
      # TODO add section checks in validation of RamanSettings_DeleteSpectraSection_StreamFind
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_DeleteSpectraSection_StreamFind) <- function(x, engine = NULL) {
  
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
  
  section <- x$parameters$section
  
  if (length(section) == 0) {
    warning("Sections not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$spectra$spectra
  
  spec_del <- lapply(spec_list, function(z) {
    
    if (nrow(z) > 0) {
      
      sel <- logical()
      
      for (i in names(section)) {
        if (i %in% colnames(z)) {
          section[[i]] <- sort(section[[i]])
          
          if (length(sel) == 0) {
            sel <- (z[[i]] >= section[[i]][1]) & (z[[i]] <= section[[i]][2])
            
          } else {
            sel <- sel & (z[[i]] >= section[[i]][1]) & (z[[i]] <= section[[i]][2])
          }
        }
      }
      
      if (length(sel) > 0) z <- z[!sel, ]
    }
    
    z
  })
  
  engine$spectra$spectra <- spec_del
  message(paste0("\U2713 ", "Spectra section deleted!"))
  TRUE
}
