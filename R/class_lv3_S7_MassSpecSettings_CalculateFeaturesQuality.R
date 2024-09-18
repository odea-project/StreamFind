
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_CalculateFeaturesQuality_StreamFind**
#'
#' @description Settings for calculating quality parameters of features (e.g., signal-to-noise (sn) ratio).
#'
#' @template arg-ms-filtered
#' @template arg-ms-rtExpand 
#' @template arg-ms-mzExpand 
#' @param minTraces Numeric of length 1 with the minimum number traces for calculating feature quality.
#' @param minIntensity Numeric of length 1 with the minimum intensity of spectra traces for calculating feature quality.
#' @param baseCut Numeric of length 1 with the base cut for calculating feature Gaussian fit.
#'
#' @return A `MassSpecSettings_CalculateFeaturesQuality_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_CalculateFeaturesQuality_StreamFind <- S7::new_class("MassSpecSettings_CalculateFeaturesQuality_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(filtered = FALSE, rtExpand = 0, mzExpand = 0, minTracesIntensity = 0, minNumberTraces = 6, baseCut = 0) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "CalculateFeaturesQuality",
      algorithm = "StreamFind",
      parameters = list(
        "filtered" = filtered,
        "rtExpand" = rtExpand,
        "mzExpand" = mzExpand,
        "minTracesIntensity" = minTracesIntensity,
        "minNumberTraces" = minNumberTraces,
        "baseCut" = baseCut
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
      checkmate::test_choice(self@method, "CalculateFeaturesQuality"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_logical(self@parameters$filtered, max.len = 1),
      checkmate::test_number(self@parameters$rtExpand),
      checkmate::test_number(self@parameters$mzExpand),
      checkmate::test_integer(as.integer(self@parameters$minNUmberTraces)),
      checkmate::test_number(self@parameters$minTracesIntensity),
      checkmate::test_number(self@parameters$baseCut)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_CalculateFeaturesQuality_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }
  
  nts <- engine$nts
  
  if (!nts@has_features) {
    warning("NTS object does not have features! Not done.")
    return(FALSE)
  }
  
  feature_list <- nts$feature_list
  
  feature_list <- lapply(feature_list, function(z) {
    if (!"quality" %in% colnames(z)) z$quality <- rep(list(), nrow(z))
    if (!"eic" %in% colnames(z)) z$eic <- rep(list(), nrow(z))
    z
  })
  
  cache <- .load_chache("calculate_quality", feature_list, x)
  
  if (!is.null(cache$data)) {
    feature_list <- cache$data
    tryCatch({
      engine$nts$feature_list <- feature_list
      message("\U2139 Calculated features quality loaded from cache!")
      return(TRUE)
    }, error = function(e) {
      warning(e)
      return(FALSE)
    })
  }
  
  parameters <- x$parameters
  analyses_list <- engine$analyses$analyses
  
  feature_list <- rcpp_ms_calculate_features_quality(
    analyses_list,
    feature_list,
    parameters$filtered,
    parameters$rtExpand,
    parameters$mzExpand,
    parameters$minTracesIntensity,
    parameters$minNumberTraces,
    parameters$baseCut
  )
  
  if (!is.null(cache$hash)) {
    .save_cache("calculate_quality", feature_list, cache$hash)
    message("\U1f5ab Calculated features quality cached!")
  }
  
  tryCatch({
    engine$nts$feature_list <- feature_list
    return(TRUE)
  }, error = function(e) {
    warning(e)
    return(FALSE)
  })
}
