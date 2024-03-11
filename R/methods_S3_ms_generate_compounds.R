
#' @title .s3_ms_generate_compounds.Settings_generate_compounds_metfrag
#'
#' @description Generate compounds for feature groups using MetFrag.
#'
#' @noRd
#'
.s3_ms_generate_compounds.Settings_generate_compounds_metfrag <- function(settings, self, private) {
  
  if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }
  
  if (!self$has_groups()) {
    warning("There are no feaure groups! Not done.")
    return(FALSE)
  }
  
  if (!validate(settings)) return(FALSE)
  
  parameters <- settings$parameters
  
  algorithm <- settings$algorithm
  
  fg <- self$featureGroups
  
  if (is.null(fg)) {
    warning("Feature groups not found! Not done.")
    return(FALSE)
  }
  
  mspl <- self$MSPeakLists
  
  if (is.null(mspl)) {
    warning("MSPeakLists not found! Use the load_MSPeakLists to load MS1 and MS2 data. Not done.")
    return(FALSE)
  }
  
  if ("featureGroupsSet" %in% is(fg)) {
    parameters$adduct <- NULL
    
  } else {
    pol <- unique(unname(self$get_polarities()))
    
    if ("positive" %in% pol) parameters$adduct <- "[M+H]+"
    
    if ("negative" %in% pol) parameters$adduct <- "[M-H]-"
  }
  
  ag <- list(fGroups = fg, MSPeakLists = mspl, algorithm = algorithm)
  
  pp_fun <- patRoon::generateCompounds
  
  compounds <- do.call(pp_fun, c(ag, parameters))
  
  # feature_list <- self$feature_list
  
  # compounds_col <- lapply(names(feature_list), function(x, feature_list, compounds) {
  #   
  #   compound <- compounds@featureFormulas[[x]]
  #   
  #   if (!is.null(compound)) {
  #     
  #     fts <- feature_list[[x]]
  #     
  #     if (nrow(fts) > 0) {
  #       res <- lapply(fts$group, function(z, compound) compound[[z]], compound = compound)
  #       return(res)
  #     }
  #   }
  #   
  #   rep(list(NULL), nrow(feature_list[[x]]))
  #   
  # }, feature_list = feature_list,  compounds = compounds)
  
  # self$add_features_column("compounds", compounds_col, feature_list)
  
  self$compounds <- compounds
  
  if ("compounds" %in% is(self$compounds)) {
    
    message(paste0("\U2713 ", length(compounds), " compounds generated and added!"))
    
    TRUE
    
  } else {
    FALSE
  }
}
