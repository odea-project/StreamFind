# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_AnnotateFeatures_StreamFind**
#'
#' @description Settings for annotation of isotopic and adduct features. The method uses the `maxIsotopes` to define the
#' maximum length of the isotopic chain. The list of candidate features is build with the `rtWindowAlignment` and the
#' maximum mass increment to match the maximum chain length. Then, the mass difference  of the natural isotopes and a
#' given monoisotopic ion (i.e., feature) are targeted. Each candidate is then evaluated according to the mass error and
#' the expected relative intensity. Adducts are annotated by the mass difference of the monoisotopic ion and the adduct
#' ion. Elements considered for annotation of isotopes are C, H, N, O, S, Cl, Br and Si, which are the most relevent for
#' organic molecules. The adducts considered are Na+, K+, NH4+, CH3OH+, DMSO+, CH3CN+ for positive ionization and
#' Cl-,Br-, -2H+Na, -2H+K, CHO2- and CH3COO- for negative ionization. In-source fragments are not yet annotated in the
#' current version but planned.
#'
#' @param rtWindowAlignment Numeric (length 1) with the proportion of the monoisotopic feature time window to be used
#' for retrieving isotopic candidates.
#' @param maxIsotopes Numeric (length 1) with the maximum number of isotopic steps.
#' @param maxCharge Numeric (length 1) with the maximum charge that ions can be ionized to find isotopes.
#' @param maxGaps Numeric (length 1) with the maximum of allowed gaps in isotopic chains.
#'
#' @return A `MassSpecSettings_AnnotateFeatures_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_AnnotateFeatures_StreamFind <- S7::new_class("MassSpecSettings_AnnotateFeatures_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  constructor = function(rtWindowAlignment = 0.3,
                         maxIsotopes = 8,
                         maxCharge = 1,
                         maxGaps = 1) {
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "AnnotateFeatures",
      algorithm = "StreamFind",
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
    ))
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "AnnotateFeatures")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_count(self@parameters$maxIsotopes)
    checkmate::assert_count(self@parameters$maxCharge)
    checkmate::assert_count(self@parameters$maxGaps)
    checkmate::assert_number(self@parameters$rtWindowAlignment)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_AnnotateFeatures_StreamFind) <- function(x, engine = NULL) {
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

  if (nts@number_features == 0) {
    warning("NTS object is empty! Not done.")
    return(FALSE)
  }

  feature_list <- nts$feature_list

  cache <- .load_chache("annotate_features", feature_list, x)

  if (!is.null(cache$data)) {
    message("\U2139 Features annotation loaded from cache!")
    nts$feature_list <- cache$data
    engine$nts <- nts
    return(TRUE)
  } else {
    parameters <- x$parameters

    feature_list <- rcpp_ms_annotate_features(
      feature_list,
      rtWindowAlignment = parameters$rtWindowAlignment,
      maxIsotopes = as.integer(parameters$maxIsotopes),
      maxCharge = as.integer(parameters$maxCharge),
      maxGaps = as.integer(parameters$maxGaps)
    )

    if (!is.null(cache$hash)) {
      .save_cache("annotate_features", feature_list, cache$hash)
      message("\U1f5ab Annotated features cached!")
    }

    nts$feature_list <- feature_list
    engine$nts <- nts
    message(paste0("\U2713 ", "Features annotated!"))
    TRUE
  }
}
