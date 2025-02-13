#' **MassSpecMethod_LoadFeaturesMS1_StreamFind**
#'
#' @description Settings for loading MS1 spectra for features.
#'
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#'
#' @return A `MassSpecMethod_LoadFeaturesMS1_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_LoadFeaturesMS1_StreamFind <- S7::new_class(
  name = "MassSpecMethod_LoadFeaturesMS1_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(rtWindow = c(-2, 2),
                         mzWindow = c(-1, 6),
                         mzClust = 0.005,
                         presence = 0.8,
                         minIntensity = 250,
                         filtered = FALSE) {
    
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "LoadFeaturesMS1",
        required = "FindFeatures",
        algorithm = "StreamFind",
        parameters = list(
          "rtWindow" = rtWindow,
          "mzWindow" = mzWindow,
          "mzClust" = mzClust,
          "presence" = presence,
          "minIntensity" = minIntensity,
          "filtered" = filtered
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
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "LoadFeaturesMS1")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_double(as.numeric(self@parameters$rtWindow), max.len = 2)
    checkmate::assert_double(as.numeric(self@parameters$mzWindow), max.len = 2)
    checkmate::assert_number(self@parameters$mzClust)
    checkmate::assert_number(self@parameters$minIntensity)
    checkmate::assert_logical(self@parameters$filtered, max.len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_LoadFeaturesMS1_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_features()) {
    warning("There are no features! Not done.")
    return(FALSE)
  }
  
  NTS <- engine$NTS
  
  feature_list <- NTS$feature_list
  
  feature_list <- lapply(feature_list, function(z) {
    if (!"ms1" %in% colnames(z)) z$ms1 <- rep(list(), nrow(z))
    z
  })
  
  parameters <- x$parameters
  
  feature_list <- rcpp_ms_load_features_ms1(
    analyses = engine$Analyses$analyses,
    features = feature_list,
    filtered = parameters$filtered,
    rtWindow = parameters$rtWindow,
    mzWindow = parameters$mzWindow,
    minTracesIntensity = parameters$minIntensity,
    mzClust = parameters$mzClust,
    presence = parameters$presence
  )
  
  tryCatch({
    engine$NTS$feature_list <- feature_list
    message("\U2713 MS1 added to features!")
    return(TRUE)
  }, error = function(e) {
    warning(e)
    return(FALSE)
  })
}

#' **MassSpecMethod_LoadFeaturesMS2_StreamFind**
#'
#' @description Settings for loading MS2 spectra for features.
#'
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#'
#' @return A `MassSpecMethod_LoadFeaturesMS2_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_LoadFeaturesMS2_StreamFind <- S7::new_class(
  name = "MassSpecMethod_LoadFeaturesMS2_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(isolationWindow = 1.3,
                         mzClust = 0.005,
                         presence = 0.8,
                         minIntensity = 10,
                         filtered = FALSE) {
   
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "LoadFeaturesMS2",
        required = "FindFeatures",
        algorithm = "StreamFind",
        parameters = list(
          "isolationWindow" = isolationWindow,
          "mzClust" = mzClust,
          "presence" = presence,
          "minIntensity" = minIntensity,
          "filtered" = filtered
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
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "LoadFeaturesMS2")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_number(self@parameters$isolationWindow)
    checkmate::assert_number(self@parameters$mzClust)
    checkmate::assert_number(self@parameters$minIntensity)
    checkmate::assert_logical(self@parameters$filtered, max.len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_LoadFeaturesMS2_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }
  
  NTS <- engine$NTS
  
  feature_list <- NTS$feature_list
  
  feature_list <- lapply(feature_list, function(z) {
    if (!"ms2" %in% colnames(z)) z$ms2 <- rep(list(), nrow(z))
    z
  })
  
  parameters <- x$parameters
  
  feature_list <- rcpp_ms_load_features_ms2(
    analyses = engine$Analyses$analyses,
    features = feature_list,
    filtered = parameters$filtered,
    minTracesIntensity = parameters$minIntensity,
    isolationWindow = parameters$isolationWindow,
    mzClust = parameters$mzClust,
    presence = parameters$presence
  )
  
  tryCatch({
    engine$NTS$feature_list <- feature_list
    message("\U2713 MS2 added to features!")
    return(TRUE)
  }, error = function(e) {
    warning(e)
    return(FALSE)
  })
}

#' **MassSpecMethod_LoadFeaturesEIC_StreamFind**
#'
#' @description Settings for loading spectra EIC for feature groups.
#'
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#' @param minIntensity Numeric of length one with the minimum intensity of traces to extract for
#' EIC.
#'
#' @return A `MassSpecMethod_LoadFeaturesEIC_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_LoadFeaturesEIC_StreamFind <- S7::new_class(
  name = "MassSpecMethod_LoadFeaturesEIC_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(filtered = FALSE, rtExpand = 120, mzExpand = 0, minIntensity = 0) {
   
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "LoadFeaturesEIC",
        required = "FindFeatures",
        algorithm = "StreamFind",
        parameters = list(
          "filtered" = filtered,
          "rtExpand" = rtExpand,
          "mzExpand" = mzExpand,
          "minIntensity" = minIntensity
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
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "LoadFeaturesEIC")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_number(self@parameters$rtExpand)
    checkmate::assert_number(self@parameters$mzExpand)
    checkmate::assert_number(self@parameters$minIntensity)
    checkmate::assert_logical(self@parameters$filtered, max.len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_LoadFeaturesEIC_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }
  
  NTS <- engine$NTS

  if (!NTS@has_features) {
    warning("NTS object does not have features! Not done.")
    return(FALSE)
  }
  
  feature_list <- NTS$feature_list
  
  feature_list <- lapply(feature_list, function(z) {
    if (!"eic" %in% colnames(z)) z$eic <- rep(list(), nrow(z))
    z
  })
    
  parameters <- x$parameters
  
  feature_list <- rcpp_ms_load_features_eic(
    engine$Analyses$analyses,
    feature_list,
    parameters$filtered,
    parameters$rtExpand,
    parameters$mzExpand,
    parameters$minIntensity
  )
  
  tryCatch({
    engine$NTS$feature_list <- feature_list
    message("\U2713 EIC added to features!")
    return(TRUE)
  }, error = function(e) {
    warning(e)
    return(FALSE)
  })
}
