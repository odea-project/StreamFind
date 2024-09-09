
# ______________________________________________________________________________________________________________________
# LoadFeaturesMS1_StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_LoadFeaturesMS1_StreamFind**
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
#' @return A `MassSpecSettings_LoadFeaturesMS1_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_LoadFeaturesMS1_StreamFind <- S7::new_class("MassSpecSettings_LoadFeaturesMS1_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(rtWindow = c(-2, 2),
                         mzWindow = c(-1, 6),
                         mzClust = 0.005,
                         presence = 0.8,
                         minIntensity = 250,
                         filtered = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "LoadFeaturesMS1",
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
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "LoadFeaturesMS1"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_double(as.numeric(self@parameters$rtWindow), max.len = 2),
      checkmate::test_double(as.numeric(self@parameters$mzWindow), max.len = 2),
      checkmate::test_number(self@parameters$mzClust),
      checkmate::test_number(self@parameters$minIntensity),
      checkmate::test_logical(self@parameters$filtered, max.len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_LoadFeaturesMS1_StreamFind) <- function(x, engine = NULL) {
  
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
  
  nts <- engine$nts
  
  parameters <- x$parameters
  
  cache <- .load_chache("load_features_ms1", nts$features, x)
  
  if (!is.null(cache$data)) {
    tryCatch({
      nts <- .add_features_column(nts, "ms1", cache$data)
      engine$nts <- nts
      message("\U2139 Features MS1 spectra loaded from cache!")
      return(TRUE)
    }, error = function(e) {
      warning(e)
      return(FALSE)
    })
  }
  
  ms1 <- engine$get_features_ms1(
    rtWindow = parameters$rtWindow,
    mzWindow = parameters$mzWindow,
    mzClust = parameters$mzClust,
    presence = parameters$presence,
    minIntensity = parameters$minIntensity,
    filtered = parameters$filtered,
    useLoadedData = FALSE
  )
  
  analyses <- engine$get_analysis_names()
  
  features <- nts$feature_list
  
  ms1_col <- lapply(analyses, function(x, ms1, features) {
    
    ana_ms1 <- ms1[ms1$analysis %in% x, ]
    
    fts_all <- features[[x]]$feature
    
    fts_ms1 <- lapply(fts_all, function(z, ana_ms1) {
      
      ft_ms1 <- ana_ms1[ana_ms1$feature %in% z, ]
      
      if (nrow(ft_ms1) > 0) {
        ft_ms1[["group"]] <- NULL
        ft_ms1[["analysis"]] <- NULL
        ft_ms1[["feature"]] <- NULL
        ft_ms1
        
      } else {
        NULL
      }
    }, ana_ms1 = ana_ms1)
    
    fts_ms1
    
  }, ms1 = ms1, features = features)
  
  if (!is.null(cache$hash)) {
    .save_cache("load_features_ms1", ms1_col, cache$hash)
    message("\U1f5ab Features MS1 spectra saved to cache!")
  }
  
  nts <- .add_features_column(nts, "ms1", ms1_col)
  engine$nts <- nts
  message("\U2713 MS1 spectra added to features!")
  TRUE
}

# ______________________________________________________________________________________________________________________
# LoadFeaturesMS2_StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_LoadFeaturesMS2_StreamFind**
#'
#' @description Settings for loading MS2 spectra for features.
#'
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#'
#' @return A `MassSpecSettings_LoadFeaturesMS2_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_LoadFeaturesMS2_StreamFind <- S7::new_class("MassSpecSettings_LoadFeaturesMS2_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(isolationWindow = 1.3,
                         mzClust = 0.005,
                         presence = 0.8,
                         minIntensity = 10,
                         filtered = FALSE) {
   
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "LoadFeaturesMS2",
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
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "LoadFeaturesMS2"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_number(self@parameters$isolationWindow),
      checkmate::test_number(self@parameters$mzClust),
      checkmate::test_number(self@parameters$minIntensity),
      checkmate::test_logical(self@parameters$filtered, max.len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_LoadFeaturesMS2_StreamFind) <- function(x, engine = NULL) {
  
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
  
  parameters <- x$parameters
  
  cache <- .load_chache("load_features_ms2", nts$features, x)
  
  if (!is.null(cache$data)) {
    tryCatch({
      nts <- .add_features_column(nts, "ms2", cache$data)
      engine$nts <- nts
      message("\U2139 Features MS2 spectra loaded from cache!")
      return(TRUE)
    }, error = function(e) {
      warning(e)
      return(FALSE)
    })
  }
  
  ms2 <- engine$get_features_ms2(
    isolationWindow =  parameters$isolationWindow,
    mzClust = parameters$mzClust,
    presence = parameters$presence,
    minIntensity = parameters$minIntensity,
    filtered = parameters$filtered,
    useLoadedData = FALSE
  )
  
  analyses <- engine$get_analysis_names()
  
  features <- nts$feature_list
  
  ms2_col <- lapply(analyses, function(x, ms2, features) {
    
    ana_ms2 <- ms2[ms2$analysis %in% x, ]
    
    fts_all <- features[[x]]$feature
    
    fts_ms2 <- lapply(fts_all, function(z, ana_ms2) {
      
      ft_ms2 <- ana_ms2[ana_ms2$feature %in% z, ]
      
      if (nrow(ft_ms2) > 0) {
        ft_ms2[["group"]] <- NULL
        ft_ms2[["analysis"]] <- NULL
        ft_ms2[["feature"]] <- NULL
        ft_ms2
      } else {
        NULL
      }
    }, ana_ms2 = ana_ms2)
    
    fts_ms2
    
  }, ms2 = ms2, features = features)
  
  if (!is.null(cache$hash)) {
    .save_cache("load_features_ms2", ms2_col, cache$hash)
    message("\U1f5ab Features MS2 spectra saved to cache!")
  }
  
  nts <- .add_features_column(nts, "ms2", ms2_col)
  engine$nts <- nts
  message("\U2713 MS2 spectra added to features!")
  TRUE
}

# ______________________________________________________________________________________________________________________
# LoadFeaturesEIC_StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_LoadFeaturesEIC_StreamFind**
#'
#' @description Settings for loading spectra EIC for feature groups.
#'
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#' @param minTracesIntensity Numeric of length one with the minimum intensity of traces to extract for EIC.
#'
#' @return A `MassSpecSettings_LoadFeaturesEIC_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_LoadFeaturesEIC_StreamFind <- S7::new_class("MassSpecSettings_LoadFeaturesEIC_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(filtered = FALSE, rtExpand = 120, mzExpand = 0, minTracesIntensity = 0) {
   
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "LoadFeaturesEIC",
      algorithm = "StreamFind",
      parameters = list(
        "filtered" = filtered,
        "rtExpand" = rtExpand,
        "mzExpand" = mzExpand,
        "minTracesIntensity" = minTracesIntensity
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
      checkmate::test_choice(self@method, "LoadFeaturesEIC"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_number(self@parameters$rtExpand),
      checkmate::test_number(self@parameters$mzExpand),
      checkmate::test_number(self@parameters$minTracesIntensity),
      checkmate::test_logical(self@parameters$filtered, max.len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_LoadFeaturesEIC_StreamFind) <- function(x, engine = NULL) {
  
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
    if ("eic" %in% colnames(z)) z$eic <- rep(list(), nrow(z))
    z
  })
    
  parameters <- x$parameters
  
  cache <- .load_chache("load_features_eic", feature_list, x)
  
  if (!is.null(cache$data)) {
    feature_list <- cache$data
    tryCatch({
      engine$nts$feature_list <- feature_list
      message("\U2139 Features EIC loaded from cache!")
      return(TRUE)
    }, error = function(e) {
      warning(e)
      return(FALSE)
    })
  }
  
  feature_list <- rcpp_ms_load_features_eic(
    engine$analyses$analyses,
    feature_list,
    parameters$filtered,
    parameters$rtExpand,
    parameters$mzExpand,
    parameters$minTracesIntensity
  )
  
  if (!is.null(cache$hash)) {
    .save_cache("load_features_eic", feature_list, cache$hash)
    message("\U1f5ab Features EIC spectra saved to cache!")
  }
  
  tryCatch({
    engine$nts$feature_list <- feature_list
    message("\U2713 EIC added to features!")
    return(TRUE)
  }, error = function(e) {
    warning(e)
    return(FALSE)
  })
}
