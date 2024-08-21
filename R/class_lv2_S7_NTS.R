#' @export
#' @noRd
NTS <- S7::new_class("NTS", package = "StreamFind", parent = Results,
  
  properties = list(
    
    features = S7::new_property(S7::as_class(methods::getClassDef("features", package = "patRoon"))),
    
    filtered = S7::new_property(S7::class_list, default = list()),
    
    mspl = S7::new_property(S7::as_class(methods::getClassDef("MSPeakLists", package = "patRoon"))),
    
    formulas = S7::new_property(S7::as_class(methods::getClassDef("formulas", package = "patRoon"))),
    
    compounds = S7::new_property(S7::as_class(methods::getClassDef("compounds", package = "patRoon")))
  ),
  
  constructor = function(
    features = new("featuresOpenMS"),
    filtered = list(),
    mspl = new("MSPeakLists", algorithm = NA_character_),
    formulas = new("formulas", algorithm = NA_character_),
    compounds = new("compounds", algorithm = NA_character_)) {
    
    if (!requireNamespace("patRoon", quietly = TRUE)) {
      warning("patRoon package not found! Install it for finding features.")
      return(NULL)
    }
    
    if (!"features" %in% is(features)) {
      warning("the argument features must be of class features or featureGroups from patRoon")
      return(NULL)
    }
    
    S7::new_object(
      Results(), 
      engine = "MassSpecEngine",
      name = "NTS",
      software = "patRoon",
      version = as.character(packageVersion("patRoon")),
      features = features,
      filtered = filtered,
      mspl = mspl,
      formulas = formulas,
      compounds = compounds
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_character(self@engine, len = 1),
      checkmate::test_true(self@engine == "MassSpecEngine"),
      checkmate::test_true(self@name == "NTS"),
      checkmate::test_true(self@software == "patRoon"),
      checkmate::test_character(self@version, len = 1),
      checkmate::test_true("features" %in% is(self@features)),
      checkmate::test_list(self@filtered),
      checkmate::test_true("MSPeakLists" %in% is(self@mspl)),
      checkmate::test_true("formulas" %in% is(self@formulas)),
      checkmate::test_true("compounds" %in% is(self@compounds))
    )
    if (!valid) return(FALSE)
    NULL
  }
)

# Adds an extra column to features.
#
.add_features_column = function(name = NULL, data = NULL) {

  # if (any(self$has_features())) {
  # 
  #   all_features <- self$feature_list
  # 
  #   feature_list <- self$features@features
  # 
  #   feature_list <- Map(function(x, y, z) {
  #     y_to_add <- y[!z$filtered]
  #     if (nrow(x) == length(y_to_add)) x[[name]] <- y_to_add
  #     x
  #   }, feature_list, data, all_features)
  # 
  #   self$feature_list <- feature_list
  # 
  #   filtered_feature_list <- self$filtered_features
  # 
  #   filtered_feature_list <- Map(function(x, y, z) {
  #     y_to_add <- y[z$filtered]
  #     if (length(y_to_add) > 0) if (nrow(x) == length(y_to_add)) x[[name]] <- y_to_add
  #     x
  #   }, filtered_feature_list, data, all_features)
  # 
  #   self$filtered_features <- filtered_feature_list
  # 
  #   TRUE
  # 
  # } else {
  #   FALSE
  # }
}


