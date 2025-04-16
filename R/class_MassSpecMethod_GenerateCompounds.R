#' **MassSpecMethod_GenerateCompounds_metfrag**
#'
#' @description Settings for generating compounds using
#' \href{https://ipb-halle.github.io/MetFrag/}{MetFrag}. The algorithm is used via the function
#' \link[patRoon]{generateCompounds} from the package \pkg{patRoon}. Therefore, it is highly
#' recommended to check the original documentation of the function in \pkg{patRoon} for more
#' details.
#' 
#' @param method Character (length 1) with the method to be used for MetFrag execution: "CL" for MetFragCL and "R" for MetFragR.
#' @param timeout Numeric (length 1) with the maximum time (in seconds) before a MetFrag query for a feature group is stopped.
#' @param timeoutRetries Numeric (length 1) with the maximum number of retries after reaching a timeout before completely
#' skipping the MetFrag query for a feature group.
#' @param errorRetries Numeric (length 1) with the maximum number of retries after an error occurred.
#' @param topMost Numeric (length 1) with the maximum number of top candidates to be returned.
#' @param dbRelMzDev Numeric (length 1) with the relative mass deviation, in ppm, for the database search.
#' @param fragRelMzDev Numeric (length 1) with the relative mass deviation, in ppm, for the fragment search.
#' @param fragAbsMzDev Numeric (length 1) with the absolute mass deviation, in Da, for the fragment search.
#' @param adduct Character (length 1) with the adduct to be used for the MetFrag annotation.
#' @param database Character (length 1) with the database to be used for the MetFrag annotation. Valid values are:
#' "pubchem", "chemspider", "for-ident", "comptox", "pubchemlite", "kegg", "sdf", "psv" and "csv".
#' @param extendedPubChem Extended PubChem database is used for the MetFrag annotation when `database` is "pubchem".
#' Valid values are: FALSE (never use it), TRUE (always use it) or "auto" (default, use if specified scorings demand it).
#' @param chemSpiderToken Character (length 1) with the ChemSpider token to be used for the MetFrag annotation when
#' `database` is "chemspider".
#' @param scoreTypes Character vector with the score types to be used for the MetFrag annotation.
#' @param scoreWeights Numeric vector with the score weights to be used for the MetFrag annotation.
#' @param preProcessingFilters Character vector with the pre-processing filters to be used for the MetFrag annotation.
#' @param postProcessingFilters Character vector with the post-processing filters to be used for the MetFrag annotation.
#' @param maxCandidatesToStop Numeric (length 1) with the maximum number of candidates to be returned before stopping the
#' MetFrag query for a feature group.
#'
#' @details Detailed documentation can be found in \link[patRoon]{generateCompoundsMetFrag} and in
#' the \href{https://ipb-halle.github.io/MetFrag/projects/metfragr/}{MetFrag} documentation.
#'
#' @return A `MassSpecMethod_GenerateCompounds_metfrag` object.
#'
#' @references
#'
#' \insertRef{metfrag01}{StreamFind}
#'
#' \insertRef{metfrag02}{StreamFind}
#'
#' \insertRef{metfrag03}{StreamFind}
#'
#' \insertRef{metfrag04}{StreamFind}
#'
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' @export
#'
MassSpecMethod_GenerateCompounds_metfrag <- S7::new_class(
  name = "MassSpecMethod_GenerateCompounds_metfrag",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(method = "CL",
                         timeout = 300,
                         timeoutRetries = 5,
                         errorRetries = 5,
                         topMost = 5,
                         dbRelMzDev = 8,
                         fragRelMzDev = 10,
                         fragAbsMzDev = 0.005,
                         adduct = character(),
                         database = "comptox",
                         extendedPubChem = FALSE,
                         chemSpiderToken = "",
                         scoreTypes = patRoon::compoundScorings(
                           "metfrag",
                           "comptox",
                           onlyDefault = TRUE
                         )$name,
                         scoreWeights = 1,
                         preProcessingFilters = c("UnconnectedCompoundFilter", "IsotopeFilter"),
                         postProcessingFilters = c("InChIKeyFilter"),
                         maxCandidatesToStop = 100) {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "GenerateCompounds",
        required = c("FindFeatures", "GroupFeatures", "LoadFeaturesMS1", "LoadFeaturesMS2"),
        algorithm = "metfrag",
        parameters = list(
          method = as.character(method),
          timeout = as.numeric(timeout),
          timeoutRetries = as.numeric(timeoutRetries),
          errorRetries = as.numeric(errorRetries),
          topMost = as.numeric(topMost),
          dbRelMzDev = as.numeric(dbRelMzDev),
          fragRelMzDev = as.numeric(fragRelMzDev),
          fragAbsMzDev = as.numeric(fragAbsMzDev),
          adduct = as.character(adduct),
          database = as.character(database),
          extendedPubChem = as.character(extendedPubChem),
          chemSpiderToken = as.character(chemSpiderToken),
          scoreTypes = as.character(scoreTypes),
          scoreWeights = as.numeric(scoreWeights),
          preProcessingFilters = as.character(preProcessingFilters),
          postProcessingFilters = as.character(postProcessingFilters),
          maxCandidatesToStop = as.numeric(maxCandidatesToStop)
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "MetFrag",
        developer = "Christoph Ruttkies and Emma L. Schymanski",
        contact = "cruttkie@ipb-halle.de",
        link = "https://ipb-halle.github.io/MetFrag/",
        doi = "https://doi.org/10.1186/s13321-016-0115-9"
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "GenerateCompounds")
    checkmate::assert_choice(self@algorithm, "metfrag")
    checkmate::assert_choice(self@parameters$method, c("CL", "R"))
    checkmate::assert_number(self@parameters$timeout)
    checkmate::assertCount(self@parameters$timeoutRetries)
    checkmate::assertCount(self@parameters$errorRetries)
    checkmate::assert_number(self@parameters$topMost)
    checkmate::assert_number(self@parameters$dbRelMzDev)
    checkmate::assert_number(self@parameters$fragRelMzDev)
    checkmate::assert_number(self@parameters$fragAbsMzDev)
    checkmate::assert_character(self@parameters$adduct, null.ok = TRUE)
    checkmate::assert_choice(
      self@parameters$database,
      c("pubchem", "chemspider", "for-ident", "comptox", "pubchemlite", "kegg", "sdf", "psv", "csv")
    )
    checkmate::assert_choice(self@parameters$extendedPubChem, c("auto", TRUE, FALSE))
    checkmate::assert_character(self@parameters$chemSpiderToken)
    checkmate::assert_character(self@parameters$scoreTypes, min.len = 1)
    checkmate::assert_numeric(self@parameters$scoreWeights, min.len = 1)
    checkmate::assert_character(self@parameters$preProcessingFilters, min.len = 1)
    checkmate::assert_character(self@parameters$postProcessingFilters, min.len = 1)
    checkmate::assertCount(self@parameters$maxCandidatesToStop)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_GenerateCompounds_metfrag) <- function(x, engine = NULL) {
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
  
  nts <- engine$NTS
  
  if (!nts@has_groups) {
    warning("NTS object does not have feature groups! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  algorithm <- x$algorithm
  
  fg <- get_patRoon_features(
    nts,
    filtered = FALSE,
    featureGroups = TRUE
  )
  
  load_ms1_param <- engine$Workflow[grepl("LoadFeaturesMS1", names(engine$Workflow))][[1]]
  load_ms2_param <- engine$Workflow[grepl("LoadFeaturesMS2", names(engine$Workflow))][[1]]
  mzClust <- min(load_ms1_param$parameters$mzClust, load_ms2_param$parameters$mzClust)
  presence <- min(load_ms1_param$parameters$presence, load_ms2_param$parameters$presence)
  minIntensity <- min(load_ms1_param$parameters$minIntensity, load_ms2_param$parameters$minIntensity)
  
  mspl <- get_patRoon_MSPeakLists(
    nts,
    mzClust = mzclust,
    minIntensity = minIntensity,
    presence = presence,
    top = 100,
    normalized = FALSE
  )
  
  if (length(mspl) == 0) {
    warning("MSPeakLists empty! Use the load_MSPeakLists to load MS1 and MS2 data. Not done.")
    return(FALSE)
  }
  
  fg <- fg[names(mspl@peakLists), ]
  
  parameters <- c(parameters, list(identifiers = NULL, extraOpts = NULL))
  
  if ("featureGroupsSet" %in% is(fg)) {
    parameters$adduct <- NULL
    parameters <- c(parameters, list(
      setThreshold = 0,
      setThresholdAnn = 0,
      setAvgSpecificScores = FALSE
    ))
  } else {
    pol <- unique(unname(engine$get_spectra_polarity()))
    if ("positive" %in% pol) parameters$adduct <- "[M+H]+"
    if ("negative" %in% pol) parameters$adduct <- "[M-H]-"
  }
  
  ag <- list(fGroups = fg, MSPeakLists = mspl, algorithm = algorithm)
  
  pp_fun <- patRoon::generateCompounds
  
  compounds <- do.call(pp_fun, c(ag, parameters))
  
  if (length(compounds) == 0) {
    warning("No compounds generated!")
    return(FALSE)
  }
  
  feature_list <- nts$feature_list
  
  if ("featureGroupsSet" %in% is(fg)) {
    feature_list <- lapply(feature_list, function(z, compounds) {
      pos_set <- compounds@setObjects[["positive"]]
      neg_set <- compounds@setObjects[["negative"]]
      if (nrow(z) > 0) {
        for (g in seq_len(nrow(z))) {
          grp <- z$group[g]
          if (!is.na(grp)) {
            if (z$polarity[g] == 1) {
              temp <- pos_set@groupAnnotations[[grp]]
              if (length(temp) > 0) {
                z$compounds[[g]] <- temp
              }
            } else if (z$polarity[g] == -1) {
              temp <- compounds@setObjects[["negative"]]@groupAnnotations[[grp]]
              if (length(temp) > 0) {
                z$compounds[[g]] <- temp
              }
            }
          }
        }
      }
      z
    }, compounds = compounds)
    
  } else {
    feature_list <- lapply(feature_list, function(z, compounds) {
      if (nrow(z) > 0) {
        for (g in seq_len(nrow(z))) {
          grp <- z$group[g]
          if (!is.na(grp)) {
            temp <- compounds@groupAnnotations[[grp]]
            if (length(temp) > 0) {
              z$compounds[[g]] <- temp
            }
          }
        }
      }
      z
    }, compounds = compounds)
  }
  
  nts@feature_list <- feature_list
  
  engine$NTS <- nts
  
  message(
    paste0("\U2713 ", length(compounds), " compounds generated and added to the feature list!")
  )
  
  TRUE
}
