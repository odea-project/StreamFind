
#' @title .s3_ms_generate_formulas.Settings_generate_formulas_genform
#'
#' @description Generate formulas for feature groups using GenForm.
#'
#' @noRd
#'
.s3_ms_generate_formulas.Settings_generate_formulas_genform <- function(settings, self, private) {
  
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
  
  pp_fun <- patRoon::generateFormulas
  
  formulas <- do.call(pp_fun, c(ag, parameters))
  
  feature_list <- self$feature_list
  
  formulas_col <- lapply(names(feature_list), function(x, feature_list, formulas) {
    
    formula <- formulas@featureFormulas[[x]]
    
    if (!is.null(formula)) {
      
      fts <- feature_list[[x]]
      
      if (nrow(fts) > 0) {
        res <- lapply(fts$group, function(z, formula) formula[[z]], formula = formula)
        return(res)
      }
    }
    
    rep(list(NULL), nrow(feature_list[[x]]))
    
  }, feature_list = feature_list,  formulas = formulas)
  
  private$.add_features_column("formulas", formulas_col)
  
  self$formulas <- formulas

  if ("formulas" %in% is(self$formulas)) {
    
    message(paste0("\U2713 ", length(formulas), " formulas generated and added!"))
    
    TRUE
    
  } else {
    FALSE
  }
}
