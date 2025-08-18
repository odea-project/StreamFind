#' MassSpecMethod_GenerateCompounds_metfrag Class
#'
#' @description Settings for generating compounds using \href{https://ipb-halle.github.io/MetFrag/}{MetFrag}. The algorithm is used via the function \link[patRoon]{generateCompounds} from the package \pkg{patRoon}. Therefore, it is highly recommended to check the original documentation of the function in \pkg{patRoon} for more details.
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
#' @param database Character (length 1) with the database to be used for the MetFrag annotation. Valid values are: "pubchem", "chemspider", "for-ident", "comptox", "pubchemlite", "kegg", "sdf", "psv" and "csv".
#' @param extendedPubChem Extended PubChem database is used for the MetFrag annotation when `database` is "pubchem". Valid values are: FALSE (never use it), TRUE (always use it) or "auto" (default, use if specified scorings demand it).
#' @param chemSpiderToken Character (length 1) with the ChemSpider token to be used for the MetFrag annotation when
#' `database` is "chemspider".
#' @param scoreTypes Character vector with the score types to be used for the MetFrag annotation.
#' @param scoreWeights Numeric vector with the score weights to be used for the MetFrag annotation.
#' @param preProcessingFilters Character vector with the pre-processing filters to be used for the MetFrag annotation.
#' @param postProcessingFilters Character vector with the post-processing filters to be used for the MetFrag annotation.
#' @param maxCandidatesToStop Numeric (length 1) with the maximum number of candidates to be returned before stopping the MetFrag query for a feature group.
#' @details Detailed documentation can be found in \link[patRoon]{generateCompoundsMetFrag} and in the \href{https://ipb-halle.github.io/MetFrag/projects/metfragr/}{MetFrag} documentation.
#' @return A `MassSpecMethod_GenerateCompounds_metfrag` object.
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
MassSpecMethod_GenerateCompounds_metfrag <- function(
  method = "CL",
  timeout = 300,
  timeoutRetries = 5,
  errorRetries = 5,
  topMost = 5,
  dbRelMzDev = 8,
  fragRelMzDev = 10,
  fragAbsMzDev = 0.005,
  adduct = character(),
  database = "comptox",
  extendedPubChem = "auto",
  chemSpiderToken = "",
  scoreTypes = patRoon::compoundScorings(
    "metfrag",
    "comptox",
    onlyDefault = TRUE
  )$name,
  scoreWeights = 1,
  preProcessingFilters = c("UnconnectedCompoundFilter", "IsotopeFilter"),
  postProcessingFilters = c("InChIKeyFilter"),
  maxCandidatesToStop = 100
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "GenerateCompounds",
    required = c(
      "FindFeatures",
      "GroupFeatures",
      "LoadFeaturesMS1",
      "LoadFeaturesMS2"
    ),
    algorithm = "metfrag",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
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
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_GenerateCompounds_metfrag object!")
  }
}

#' @export
#' @noRd
validate_object.MassSpecMethod_GenerateCompounds_metfrag <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "GenerateCompounds")
  checkmate::assert_choice(x$algorithm, "metfrag")
  checkmate::assert_choice(x$parameters$method, c("CL", "R"))
  checkmate::assert_number(x$parameters$timeout)
  checkmate::assertCount(x$parameters$timeoutRetries)
  checkmate::assertCount(x$parameters$errorRetries)
  checkmate::assert_number(x$parameters$topMost)
  checkmate::assert_number(x$parameters$dbRelMzDev)
  checkmate::assert_number(x$parameters$fragRelMzDev)
  checkmate::assert_number(x$parameters$fragAbsMzDev)
  checkmate::assert_character(x$parameters$adduct, null.ok = TRUE)
  checkmate::assert_choice(
    x$parameters$database,
    c(
      "pubchem",
      "chemspider",
      "for-ident",
      "comptox",
      "pubchemlite",
      "kegg",
      "sdf",
      "psv",
      "csv"
    )
  )
  checkmate::assert_choice(x$parameters$extendedPubChem, c("auto", TRUE, FALSE))
  checkmate::assert_character(x$parameters$chemSpiderToken)
  checkmate::assert_character(x$parameters$scoreTypes, min.len = 1)
  checkmate::assert_numeric(x$parameters$scoreWeights, min.len = 1)
  checkmate::assert_character(x$parameters$preProcessingFilters, min.len = 1)
  checkmate::assert_character(x$parameters$postProcessingFilters, min.len = 1)
  checkmate::assertCount(x$parameters$maxCandidatesToStop)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_GenerateCompounds_metfrag <- function(x, engine = NULL) {
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
    warning(
      "MassSpecResults_NonTargetAnalysis object does not have features! Not done."
    )
    return(FALSE)
  }

  has_groups <- any(vapply(
    nts$features,
    function(z) any(!(is.na(z$group) | z$group %in% "")),
    FALSE
  ))

  if (!has_groups) {
    warning(
      "MassSpecResults_NonTargetAnalysis object does not have feature groups! Not done."
    )
    return(FALSE)
  }

  parameters <- x$parameters
  algorithm <- x$algorithm

  fg <- get_patRoon_features(
    nts,
    filtered = FALSE,
    featureGroups = TRUE
  )

  load_ms1_param <- engine$Workflow[grepl(
    "LoadFeaturesMS1",
    names(engine$Workflow)
  )][[1]]
  load_ms2_param <- engine$Workflow[grepl(
    "LoadFeaturesMS2",
    names(engine$Workflow)
  )][[1]]
  mzClust <- min(
    load_ms1_param$parameters$mzClust,
    load_ms2_param$parameters$mzClust
  )
  presence <- min(
    load_ms1_param$parameters$presence,
    load_ms2_param$parameters$presence
  )
  minIntensity <- min(
    load_ms1_param$parameters$minIntensity,
    load_ms2_param$parameters$minIntensity
  )

  mspl <- get_patRoon_MSPeakLists(
    nts,
    mzClust = mzClust,
    minIntensity = minIntensity,
    presence = presence,
    top = 100,
    normalized = FALSE
  )

  if (length(mspl) == 0) {
    warning(
      "MSPeakLists empty! Use the load_MSPeakLists to load MS1 and MS2 data. Not done."
    )
    return(FALSE)
  }

  fg <- fg[names(mspl@peakLists), ]

  parameters <- c(parameters, list(identifiers = NULL, extraOpts = NULL))

  if ("featureGroupsSet" %in% is(fg)) {
    parameters$adduct <- NULL
    parameters <- c(
      parameters,
      list(
        setThreshold = 0,
        setThresholdAnn = 0,
        setAvgSpecificScores = FALSE
      )
    )
  } else {
    pol <- unique(vapply(
      nts$headers,
      function(a) {
        paste0(unique(a$polarity), collapse = ", ")
      },
      NA_character_
    ))
    if ("1" %in% pol) {
      parameters$adduct <- "[M+H]+"
    }
    if ("-1" %in% pol) parameters$adduct <- "[M-H]-"
  }
  ag <- list(fGroups = fg, MSPeakLists = mspl, algorithm = algorithm)
  pp_fun <- patRoon::generateCompounds
  compounds <- do.call(pp_fun, c(ag, parameters))

  if (length(compounds) == 0) {
    warning("No compounds generated!")
    return(FALSE)
  }

  feature_list <- nts$features

  if ("featureGroupsSet" %in% is(fg)) {
    feature_list <- lapply(
      feature_list,
      function(z, compounds) {
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
                temp <- compounds@setObjects[["negative"]]@groupAnnotations[[
                  grp
                ]]
                if (length(temp) > 0) {
                  z$compounds[[g]] <- temp
                }
              }
            }
          }
        }
        z
      },
      compounds = compounds
    )
  } else {
    feature_list <- lapply(
      feature_list,
      function(z, compounds) {
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
      },
      compounds = compounds
    )
  }
  nts$features <- feature_list
  engine$Results <- nts
  message(
    paste0(
      "\U2713 ",
      length(compounds),
      " compounds generated and added to the feature list!"
    )
  )
  TRUE
}
