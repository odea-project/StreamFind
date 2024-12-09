
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_LoadSpectra_StreamFind**
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
#' @return A `MassSpecSettings_LoadSpectra_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_LoadSpectra_StreamFind <- S7::new_class("MassSpecSettings_LoadSpectra_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(levels = 1,
                         mzmin = 0,
                         mzmax = 0,
                         rtmin = 0,
                         rtmax = 0,
                         mobilitymin = 0,
                         mobilitymax = 0,
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
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "LoadSpectra")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_numeric(self@parameters$levels)
    checkmate::assert_numeric(self@parameters$mzmin, len = 1)
    checkmate::assert_numeric(self@parameters$mzmax, len = 1)
    checkmate::assert_numeric(self@parameters$rtmin, len = 1)
    checkmate::assert_numeric(self@parameters$rtmax, len = 1)
    checkmate::assert_numeric(self@parameters$mobilitymin, len = 1)
    checkmate::assert_numeric(self@parameters$mobilitymax, len = 1)
    checkmate::assert_numeric(self@parameters$minIntensity, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_LoadSpectra_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!any(engine$get_spectra_number() > 0)) {
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
    mobilitymax = parameters$mobilitymax
  )
  
  tryCatch({
    engine$load_spectra(
      levels = parameters$levels,
      mz = ranges,
      minIntensityMS1 = parameters$minIntensity,
      minIntensityMS2 = parameters$minIntensity
    )
    message(paste0("\U2713 ", "Spectra loaded!"))
    TRUE
  }, error = function(e) {
    warning("Error loading spectra! Not done.")
    return(FALSE)
  })
}
