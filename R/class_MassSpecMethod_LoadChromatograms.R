#' MassSpecMethod_LoadChromatograms_native
#'
#' @description Loads chromatograms from mass spectrometry analyses.
#' 
#' @param chromatograms A numeric or character vector with the chromatogram indices or names to be
#' used, respectively.
#' @param rtmin A numeric vector with the minimum retention time values to be used.
#' @param rtmax A numeric vector with the maximum retention time values to be used.
#' @param minIntensity A numeric value with the minimum intensity to be used.
#'
#' @return A `MassSpecMethod_LoadChromatograms_native` object.
#'
#' @export
#'
MassSpecMethod_LoadChromatograms_native <- S7::new_class(
  name = "MassSpecMethod_LoadChromatograms_native",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  
  constructor = function(chromatograms = 1,
                         rtmin = 0,
                         rtmax = 0,
                         minIntensity = 0) {
    
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "LoadChromatograms",
        required = NA_character_,
        algorithm = "native",
        parameters = list(
          chromatograms = chromatograms,
          rtmin = rtmin,
          rtmax = rtmax,
          minIntensity = minIntensity
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
    checkmate::assert_choice(self@method, "LoadChromatograms")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_true(
      is.numeric(self@parameters$chromatograms) || is.character(self@parameters$chromatograms)
    )
    checkmate::assert_numeric(self@parameters$rtmin, len = 1)
    checkmate::assert_numeric(self@parameters$rtmax, len = 1)
    checkmate::assert_numeric(self@parameters$minIntensity, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_LoadChromatograms_native) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!any(engine$get_chromatograms_number() > 0)) {
    warning("There are no chromatograms! Not done.")
    return(FALSE)
  }
  
  parameters <- x@parameters
  
  analyses <- engine$Analyses
  
  analyses <- StreamFind::load_chromatograms(
    analyses,
    chromatograms = parameters$chromatograms,
    rtmin = parameters$rtmin,
    rtmax = parameters$rtmax,
    minIntensity = parameters$minIntensity
  )
  
  if (analyses$has_results_chromatograms) {
    engine$Analyses <- analyses
    message(paste0("\U2713 ", "Chromatograms loaded!"))
    TRUE
  } else {
    FALSE
  }
}
