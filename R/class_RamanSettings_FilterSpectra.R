
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_FilterSpectra_StreamFind**
#'
#' @description Various filter options for Raman spectra.
#' 
#' @param onlyChromPeaksSpectra Logical (length 1) for keeping only spectra with chromatographic peaks.
#'
#' @return A RamanSettings_FilterSpectra_StreamFind object.
#'
#' @export
#'
RamanSettings_FilterSpectra_StreamFind <- S7::new_class(
  "RamanSettings_FilterSpectra_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(onlyChromPeaksSpectra = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "FilterSpectra",
      algorithm = "StreamFind",
      parameters = list(
        onlyChromPeaksSpectra = onlyChromPeaksSpectra
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
    checkmate::assert_choice(self@engine, "Raman")
    checkmate::assert_choice(self@method, "FilterSpectra")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_logical(self@parameters$onlyChromPeaksSpectra, max.len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_FilterSpectra_StreamFind) <- function(x, engine = NULL) {
  
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
  
  if (x$parameters$onlyChromPeaksSpectra) {
    
    if (!engine$spectra$has_chrom_peaks) {
      warning("No chromatographic peaks found! Not done.")
      return(FALSE)
    }
    
    spec_list <- engine$spectra$spectra
    chrom_peaks <- engine$spectra$chrom_peaks
    
    if (!identical(names(chrom_peaks), names(spec_list))) {
      warning("Chromatograms and spectra do not match! Not done.")
      return(FALSE)
    }
    
    sub_spectra <- Map(function(z, y) {
      z$id <- NA_character_
      for (i in seq_len(nrow(y))) {
        z$id[z$rt >= y$rtmin[i] & z$rt <= y$rtmax[i]] <- paste0(y$peak[i], "_", round(y$rt[i], digits = 1))
      }
      z <- z[!is.na(z$id), ]
      z
    }, spec_list, chrom_peaks)
    
    engine$spectra$spectra <- sub_spectra
    message(paste0("\U2713 ", "Only spectra from chromatographic peaks kept!"))
  }
  
  invisible(TRUE)
}
