#' Mass Spectrometry Method for Annotating Features in MassSpecResults_NonTargetAnalysis (StreamFind algorithm)
#'
#' @description Method for annotation of isotopic and adduct features. The method uses the
#' `maxIsotopes` to define the maximum length of the isotopic chain. The list of candidate features
#' is build with the `rtWindowAlignment` and the maximum mass increment to match the maximum chain
#' length. Then, the mass difference  of the natural isotopes and a given monoisotopic ion (i.e.,
#' feature) are targeted. Each candidate is then evaluated according to the mass error and the
#' expected relative intensity. Adducts are annotated by the mass difference of the monoisotopic
#' ion and the adduct ion. Elements considered for annotation of isotopes are C, H, N, O, S, Cl, Br
#' and Si, which are the most relevent for organic molecules. The adducts considered are Na+, K+,
#' NH4+, CH3OH+, DMSO+, CH3CN+ for positive ionization and Cl-,Br-, -2H+Na, -2H+K, CHO2- and
#' CH3COO- for negative ionization. In-source fragments are not yet annotated in the current
#' version but planned.
#'
#' @param rtWindowAlignment Numeric (length 1) with the proportion of the monoisotopic feature time
#'  window to be used for retrieving isotopic candidates.
#' @param maxIsotopes Numeric (length 1) with the maximum number of isotopic steps.
#' @param maxCharge Numeric (length 1) with the maximum charge that ions can be ionized to find
#' isotopes.
#' @param maxGaps Numeric (length 1) with the maximum of allowed gaps in isotopic chains.
#'
#' @return A `MassSpecMethod_AnnotateFeatures_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_AnnotateFeatures_StreamFind <- function(
  rtWindowAlignment = 0.3,
  maxIsotopes = 8,
  maxCharge = 1,
  maxGaps = 1
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "AnnotateFeatures",
    required = "FindFeatures",
    algorithm = "StreamFind",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      maxIsotopes = as.integer(maxIsotopes),
      maxCharge = as.integer(maxCharge),
      rtWindowAlignment = as.numeric(rtWindowAlignment),
      maxGaps = as.integer(maxGaps)
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
    stop("Invalid MassSpecMethod_AnnotateFeatures_StreamFind object!")
  }
}

#' @describeIn MassSpecMethod_AnnotateFeatures_StreamFind Validate the MassSpecMethod_AnnotateFeatures_StreamFind object, returning NULL if valid.
#' @param x A `MassSpecMethod_AnnotateFeatures_StreamFind` object.
#' @export
#'
validate_object.MassSpecMethod_AnnotateFeatures_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "AnnotateFeatures")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_count(x$parameters$maxIsotopes)
  checkmate::assert_count(x$parameters$maxCharge)
  checkmate::assert_count(x$parameters$maxGaps)
  checkmate::assert_number(x$parameters$rtWindowAlignment)
  NULL
}

#' @export
#' @noRd
#'
run.MassSpecMethod_AnnotateFeatures_StreamFind <- function(x, engine = NULL) {
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

  nts <- engine$ResultsList$MassSpecResults_NonTargetAnalysis

  if (sum(vapply(nts$features, function(z) nrow(z), 0)) == 0) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  feature_list <- nts$features

  parameters <- x$parameters

  tryCatch(
    {
      feature_list <- rcpp_nts_annotate_features(
        feature_list,
        rtWindowAlignment = parameters$rtWindowAlignment,
        maxIsotopes = as.integer(parameters$maxIsotopes),
        maxCharge = as.integer(parameters$maxCharge),
        maxGaps = as.integer(parameters$maxGaps)
      )

      nts$features <- feature_list
      validate_object(nts)
      engine$ResultsList <- nts
      message(paste0("\U2713 ", "Features annotated!"))
      TRUE
    },
    error = function(e) {
      warning("Error during annotation of features!", "\n", e)
      return(FALSE)
    }
  )
}
