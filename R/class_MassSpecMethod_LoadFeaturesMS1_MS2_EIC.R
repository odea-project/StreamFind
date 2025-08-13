# MARK: MassSpecMethod_LoadFeaturesMS1_StreamFind
#' @title MassSpecMethod_LoadFeaturesMS1_StreamFind S3 Class
#' @description Settings for loading MS1 spectra for features.
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#' @return A `MassSpecMethod_LoadFeaturesMS1_StreamFind` object.
#' @export
#'
MassSpecMethod_LoadFeaturesMS1_StreamFind <- function(
  rtWindow = c(-2, 2),
  mzWindow = c(-1, 6),
  mzClust = 0.005,
  presence = 0.8,
  minIntensity = 250,
  filtered = FALSE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "LoadFeaturesMS1",
    required = "FindFeatures",
    algorithm = "StreamFind",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
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
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_LoadFeaturesMS1_StreamFind object!")
  }
}

#' @describeIn MassSpecMethod_LoadFeaturesMS1_StreamFind Validator for the MassSpecMethod_LoadFeaturesMS1_StreamFind object, returning NULL if valid.
#' @param x A MassSpecMethod_LoadFeaturesMS1_StreamFind object.
#' @export
#'
validate_object.MassSpecMethod_LoadFeaturesMS1_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "LoadFeaturesMS1")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_double(as.numeric(x$parameters$rtWindow), max.len = 2)
  checkmate::assert_double(as.numeric(x$parameters$mzWindow), max.len = 2)
  checkmate::assert_number(x$parameters$mzClust)
  checkmate::assert_number(x$parameters$minIntensity)
  checkmate::assert_logical(x$parameters$filtered, max.len = 1)
  NULL
}


#' @export
#' @noRd
run.MassSpecMethod_LoadFeaturesMS1_StreamFind <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Analyses$results[["MassSpecResults_NonTargetAnalysis"]])) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  if (sum(vapply(nts$features, function(z) nrow(z), 0)) == 0) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters

  feature_list <- rcpp_nts_load_features_ms1(
    nts$info,
    nts$headers,
    nts$features,
    filtered = parameters$filtered,
    rtWindow = parameters$rtWindow,
    mzWindow = parameters$mzWindow,
    minTracesIntensity = parameters$minIntensity,
    mzClust = parameters$mzClust,
    presence = parameters$presence
  )

  tryCatch(
    {
      nts$features <- feature_list
      engine$Results <- nts
      message("\U2713 MS1 added to features!")
      return(TRUE)
    },
    error = function(e) {
      warning(e)
      return(FALSE)
    }
  )
}

# MARK: MassSpecMethod_LoadFeaturesMS2_StreamFind
#' @title MassSpecMethod_LoadFeaturesMS2_StreamFind S3 Class
#' @description Settings for loading MS2 spectra for features.
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#' @return A `MassSpecMethod_LoadFeaturesMS2_StreamFind` object.
#' @export
#'
MassSpecMethod_LoadFeaturesMS2_StreamFind <- function(
  isolationWindow = 1.3,
  mzClust = 0.005,
  presence = 0.8,
  minIntensity = 10,
  filtered = FALSE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "LoadFeaturesMS2",
    required = "FindFeatures",
    algorithm = "StreamFind",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
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
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_LoadFeaturesMS2_StreamFind object!")
  }
}

#' @describeIn MassSpecMethod_LoadFeaturesMS2_StreamFind Validator for the MassSpecMethod_LoadFeaturesMS2_StreamFind object, returning NULL if valid.
#' @param x A `MassSpecMethod_LoadFeaturesMS2_StreamFind` object.
#' @export
#'
validate_object.MassSpecMethod_LoadFeaturesMS2_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "LoadFeaturesMS2")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_number(x$parameters$isolationWindow)
  checkmate::assert_number(x$parameters$mzClust)
  checkmate::assert_number(x$parameters$minIntensity)
  checkmate::assert_logical(x$parameters$filtered, max.len = 1)
  NULL
}


#' @export
#' @noRd
run.MassSpecMethod_LoadFeaturesMS2_StreamFind <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Analyses$results[["MassSpecResults_NonTargetAnalysis"]])) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  if (sum(vapply(nts$features, function(z) nrow(z), 0)) == 0) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters

  feature_list <- rcpp_nts_load_features_ms2(
    nts$info,
    nts$headers,
    nts$features,
    filtered = parameters$filtered,
    minTracesIntensity = parameters$minIntensity,
    isolationWindow = parameters$isolationWindow,
    mzClust = parameters$mzClust,
    presence = parameters$presence
  )

  tryCatch(
    {
      nts$features <- feature_list
      engine$Results <- nts
      message("\U2713 MS2 added to features!")
      return(TRUE)
    },
    error = function(e) {
      warning(e)
      return(FALSE)
    }
  )
}

# MARK: MassSpecMethod_LoadFeaturesEIC_StreamFind
#' @title MassSpecMethod_LoadFeaturesEIC_StreamFind S3 Class
#' @description Settings for loading spectra EIC for feature groups.
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#' @param minIntensity Numeric of length one with the minimum intensity of traces to extract for
#' EIC.
#' @return A `MassSpecMethod_LoadFeaturesEIC_StreamFind` object.
#' @export
#'
MassSpecMethod_LoadFeaturesEIC_StreamFind <- function(
  filtered = FALSE,
  rtExpand = 120,
  mzExpand = 0,
  minIntensity = 0
) {
  x <- ProcessingStep(
    type = "MassSpec",
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
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_LoadFeaturesEIC_StreamFind object!")
  }
}

#' @describeIn MassSpecMethod_LoadFeaturesEIC_StreamFind Validator for the MassSpecMethod_LoadFeaturesEIC_StreamFind object, returning NULL if valid.
#' @param x A `MassSpecMethod_LoadFeaturesEIC_StreamFind` object.
#' @export
#'
validate_object.MassSpecMethod_LoadFeaturesEIC_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "LoadFeaturesEIC")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_number(x$parameters$rtExpand)
  checkmate::assert_number(x$parameters$mzExpand)
  checkmate::assert_number(x$parameters$minIntensity)
  checkmate::assert_logical(x$parameters$filtered, max.len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_LoadFeaturesEIC_StreamFind <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Analyses$results[["MassSpecResults_NonTargetAnalysis"]])) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  if (sum(vapply(nts$features, function(z) nrow(z), 0)) == 0) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters

  feature_list <- rcpp_nts_load_features_eic(
    nts$info,
    nts$headers,
    nts$features,
    parameters$filtered,
    parameters$rtExpand,
    parameters$mzExpand,
    parameters$minIntensity
  )

  tryCatch(
    {
      nts$features <- feature_list
      engine$Results <- nts
      message("\U2713 EIC added to features!")
      return(TRUE)
    },
    error = function(e) {
      warning(e)
      return(FALSE)
    }
  )
}
