
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_LoadSpectra**
#'
#' @description .
#' 
#' @param levels A numeric vector with the levels to be used.
#' @param mzmin A numeric vector with the minimum m/z values to be used.
#' @param mzmax A numeric vector with the maximum m/z values to be used.
#' @param rtmin A numeric vector with the minimum retention time values to be used.
#' @param rtmax A numeric vector with the maximum retention time values to be used.
#' @param mobilitymin A numeric vector with the minimum mobility values to be used.
#' @param mobilitymax A numeric vector with the maximum mobility values to be used.
#' @param minIntensity A numeric value with the minimum intensity to be used.
#'
#' @return A `MassSpecSettings_LoadSpectra` object.
#'
#' @export
#'
MassSpecSettings_LoadSpectra <- S7::new_class("MassSpecSettings_LoadSpectra",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(levels = NULL,
                         mzmin = NULL,
                         mzmax = NULL,
                         rtmin = NULL,
                         rtmax = NULL,
                         mobilitymin = NULL,
                         mobilitymax = NULL,
                         minIntensity = 0) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "LoadSpectra",
      algorithm = "StreamFind",
      parameters = list(
        levels = levels,
        mzmin = mzmin,
        mzmax = mzmax,
        rtmin = rtmin,
        rtmax = rtmax,
        mobilitymin = mobilitymin,
        mobilitymax = mobilitymax,
        minIntensity = minIntensity
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
      checkmate::test_choice(self@method, "LoadSpectra"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_numeric(self@parameters$levels),
      checkmate::test_numeric(self@parameters$mzmin, len = 1),
      checkmate::test_numeric(self@parameters$mzmax, len = 1),
      checkmate::test_numeric(self@parameters$rtmin, len = 1),
      checkmate::test_numeric(self@parameters$rtmax, len = 1),
      checkmate::test_numeric(self@parameters$mobilitymin, len = 1),
      checkmate::test_numeric(self@parameters$mobilitymax, len = 1),
      checkmate::test_numeric(self@parameters$minIntensity, len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_LoadSpectra) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!any(engine$get_spectra_number > 0)) {
    warning("There are no spectra! Not done.")
    return(FALSE)
  }
  
  parameters <- x@parameters
  
  ranges <- data.frame(
    mzmin = parameters$mzmin,
    mzmax = parameters$mzmax,
    rtmin = parameters$rtmin,
    rtmax = parameters$rtmax,
    mobilitymin = parameters$mobilitymin,
    mobilitymax = parameters$mobilitymax,
    minIntensity = parameters$minIntensity
  )
  
  spec <- engine$get_spectra(levels = parameters$levels, mz = ranges)
  
  spec <- Spectra(spec, FALSE)
  
  engine$Spectra <- spec

  message(paste0("\U2713 ", "Spectra loaded!"))

  TRUE

}
