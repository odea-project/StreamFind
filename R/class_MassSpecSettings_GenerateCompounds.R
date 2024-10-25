
# ______________________________________________________________________________________________________________________
# metfrag -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_GenerateCompounds_metfrag**
#'
#' @description Settings for generating compounds using \href{https://ipb-halle.github.io/MetFrag/}{MetFrag}. 
#' The algorithm is used via the function \link[patRoon]{generateCompounds} from the package \pkg{patRoon}. Therefore, 
#' it is highly recommended to check the original documentation of the function in \pkg{patRoon} for more details.
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
#' @details Detailed documentation can be found in \link[patRoon]{generateCompoundsMetFrag}.
#'
#' @return A `MassSpecSettings_GenerateCompounds_metfrag` object.
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
MassSpecSettings_GenerateCompounds_metfrag <- S7::new_class("MassSpecSettings_GenerateCompounds_metfrag",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(method = "CL",
                         timeout = 300,
                         timeoutRetries = 5,
                         errorRetries = 5,
                         topMost = 5,
                         dbRelMzDev = 8,
                         fragRelMzDev = 10,
                         fragAbsMzDev = 0.005,
                         adduct = NULL,
                         database = "comptox",
                         extendedPubChem = "auto",
                         chemSpiderToken = "",
                         scoreTypes = patRoon::compoundScorings("metfrag", "comptox", onlyDefault = TRUE)$name,
                         scoreWeights = 1,
                         preProcessingFilters = c("UnconnectedCompoundFilter", "IsotopeFilter"),
                         postProcessingFilters = c("InChIKeyFilter"),
                         maxCandidatesToStop = 100) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "GenerateCompounds",
      algorithm = "metfrag",
      parameters = list(
        method = method,
        timeout = timeout,
        timeoutRetries = timeoutRetries,
        errorRetries = errorRetries,
        topMost = topMost,
        dbRelMzDev = dbRelMzDev,
        fragRelMzDev = fragRelMzDev,
        fragAbsMzDev = fragAbsMzDev,
        adduct = adduct,
        database = database,
        extendedPubChem = extendedPubChem,
        chemSpiderToken = chemSpiderToken,
        scoreTypes = scoreTypes,
        scoreWeights = scoreWeights,
        preProcessingFilters = preProcessingFilters,
        postProcessingFilters = postProcessingFilters,
        maxCandidatesToStop = maxCandidatesToStop
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "MetFrag",
      developer = "Christoph Ruttkies and Emma L. Schymanski",
      contact = "cruttkie@ipb-halle.de",
      link = "https://ipb-halle.github.io/MetFrag/",
      doi = "https://doi.org/10.1186/s13321-016-0115-9"
    ))
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "GenerateCompounds")
    checkmate::assert_choice(self@algorithm, "metfrag")
    checkmate::assert_choice(self@parameters$method, c("CL", "R"))
    checkmate::assert_number(self@parameters$timeout)
    checkmate::assert_number(self@parameters$timeoutRetries)
    checkmate::assert_number(self@parameters$errorRetries)
    checkmate::assert_number(self@parameters$topMost)
    checkmate::assert_number(self@parameters$dbRelMzDev)
    checkmate::assert_number(self@parameters$fragRelMzDev)
    checkmate::assert_number(self@parameters$fragAbsMzDev)
    checkmate::assert_character(self@parameters$adduct, null.ok = TRUE)
    checkmate::assert_choice(self@parameters$database, c("pubchem", "chemspider", "for-ident", "comptox", "pubchemlite", "kegg", "sdf", "psv", "csv"))
    checkmate::assert_choice(self@parameters$extendedPubChem, c("auto", TRUE, FALSE))
    checkmate::assert_character(self@parameters$chemSpiderToken)
    checkmate::assert_character(self@parameters$scoreTypes, min.len = 1)
    checkmate::assert_numeric(self@parameters$scoreWeights, min.len = 1)
    checkmate::assert_character(self@parameters$preProcessingFilters, min.len = 1)
    checkmate::assert_character(self@parameters$postProcessingFilters, min.len = 1)
    checkmate::assert_number(self@parameters$maxCandidatesToStop)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_GenerateCompounds_metfrag) <- function(x, engine = NULL) {
  
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
  
  if (!nts@has_groups) {
    warning("NTS object does not have feature groups! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters
  
  algorithm <- x$algorithm
  
  fg <- nts$features
  
  mspl <- nts$mspl
  
  if (length(mspl) == 0) {
    warning("MSPeakLists empty! Use the load_MSPeakLists to load MS1 and MS2 data. Not done.")
    return(FALSE)
  }
  
  if ("featureGroupsSet" %in% is(fg)) {
    parameters$adduct <- NULL
    
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
  
  compound_list <- compounds@groupAnnotations
  
  compounds_col <- lapply(names(feature_list), function(x, feature_list, compound_list) {
    
    fts <- feature_list[[x]]
    
    if (nrow(fts) > 0) {
      res <- lapply(fts$group, function(z, compound_list) {
        if (!is.na(z)) return(compound_list[[z]])
        list(NULL)
      }, compound_list = compound_list)
      return(res)
    }
    rep(list(NULL), nrow(feature_list[[x]]))
  }, feature_list = feature_list,  compound_list = compound_list)
  
  nts <- .add_features_column(nts, "compounds", compounds_col)
  
  nts$compounds <- compounds
  
  engine$nts <- nts
  
  message(paste0("\U2713 ", length(compounds), " compounds generated and added!"))
  
  TRUE
}
