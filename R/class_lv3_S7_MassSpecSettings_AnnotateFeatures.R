
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_AnnotateFeatures_StreamFind**
#'
#' @description Settings for annotation of isotopic features. The method uses the `maxIsotopes` to define the maximum 
#' length of the isotopic chain. The list of candidate features is build with the `rtWindowAlignment` and the maximum 
#' mass increment to match the maximum chain length. Then, the mass difference  of the natural isotopes defined by 
#' `elements` and a given monoisotopic ion (i.e., feature) are targeted. Each candidate is then evaluated according to 
#' the mass error and the expected relative intensity range as defined by the `mode`.
#'
#' @param maxIsotopes Numeric (length 1) with the maximum number of isotopic steps.
#' @param elements Character vector with the elements to target the isotopic annotation.
#' Possible elements are C, H, N, O, S, Cl, Br.
#' @param mode Character (length 1) with the type of molecules to be targeted. For now, only "small molecules" are possible.
#' @param maxCharge Numeric (length 1) with the maximum charge that ions can be ionized to find isotopes.
#' @param rtWindowAlignment Numeric (length 1) with the proportion of the monoisotopic feature time window to be used 
#' for retrieving isotopic candidates.
#' @param maxGaps Numeric (length 1) with the maximum of allowed gaps in isotopic chains.
#'
#' @return A MassSpecSettings_AnnotateFeatures_StreamFind class object.
#'
#' @export
#'
MassSpecSettings_AnnotateFeatures_StreamFind <- S7::new_class("MassSpecSettings_AnnotateFeatures_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(maxIsotopes = 5,
                         elements = c("C", "H", "N", "O", "S", "Cl", "Br"),
                         mode = "small molecules",
                         maxCharge = 1,
                         rtWindowAlignment = 0.3,
                         maxGaps = 1) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "AnnotateFeatures",
      algorithm = "StreamFind",
      parameters = list(
        maxIsotopes = maxIsotopes,
        elements = elements,
        mode = mode,
        maxCharge = maxCharge,
        rtWindowAlignment = rtWindowAlignment,
        maxGaps = maxGaps
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
      checkmate::test_choice(self@method, "AnnotateFeatures"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_count(self@parameters$maxIsotopes),
      checkmate::test_count(self@parameters$maxCharge),
      checkmate::test_count(self@parameters$maxGaps),
      checkmate::test_number(self@parameters$rtWindowAlignment),
      checkmate::test_choice(self@parameters$mode, "small molecules"),
      checkmate::test_vector(self@parameters$elements, any.missing = FALSE, min.len = 1),
      vapply(self@parameters$elements, function(i) {
       checkmate::test_choice(i, c("C", "H", "N", "O", "S", "Cl", "Br", "Si", "Ge"))
      }, FALSE)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_AnnotateFeatures_StreamFind) <- function(x, engine = NULL) {
  cat("AnnotateFeatures\n")
  
  # if (!any(self$has_features())) {
  #   warning("Features were not found! Run find_features method first!")
  #   return(FALSE)
  # }
  # 
  # features <- self$feature_list
  # 
  # features <- lapply(features, function(x) {
  #   x$index <- seq_len(nrow(x))
  #   x
  # })
  # 
  # cache <- .load_chache("annotate_features", names(features), features, settings)
  # 
  # if (!is.null(cache$data)) {
  #   message("\U2139 Features annotation loaded from cache!")
  #   isotopes <- cache$data
  #   
  # } else {
  #   
  #   parameters <- settings$parameters
  #   
  #   message("\U2699 Annotating features from ", length(self$get_analyses()), " analyses...", appendLF = FALSE)
  #   
  #   isotopes <- lapply(features, function(x) {
  #     do.call("rcpp_ms_annotation_isotopes", c(list("features" = x), parameters))
  #   })
  #   
  #   names(isotopes) <- names(features)
  #   
  #   message(" Done!")
  #   
  #   if (!is.null(cache$hash)) {
  #     .save_cache("annotate_features", isotopes, cache$hash)
  #     message("\U1f5ab Annotated features cached!")
  #   }
  # }
  # 
  # iso_col <- lapply(names(isotopes), function(x, isotopes, features) {
  #   
  #   temp_i <- isotopes[[x]]$output2
  #   
  #   temp_i_fts <- vapply(temp_i, function(x) x$feature, NA_character_)
  #   
  #   names(temp_i) <- temp_i_fts
  #   
  #   temp_f <- data.table::copy(features[[x]])
  #   
  #   temp_f_fts <- temp_f$feature
  #   
  #   if (!all(temp_i_fts %in% temp_f_fts)) stop("Annotated features do not exist in features!")
  #   
  #   temp_i <- temp_i[temp_f_fts]
  #   
  #   if (!identical(names(temp_i), temp_f_fts)) stop("Annotated features do not match features!")
  #   
  #   temp_i
  #   
  # }, isotopes = isotopes, features = features)
  # 
  # names(iso_col) <- names(features)
  # 
  # if (private$.add_features_column("isotope", iso_col)) {
  #   
  #   message(paste0("\U2713 ", "Features annotated!"))
  #   
  #   TRUE
  #   
  # } else {
  #   FALSE
  # }
}
