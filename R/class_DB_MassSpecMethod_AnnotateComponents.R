# MARK: Native
# Native ------

#' @title DB_MassSpecMethod_AnnotateComponents_native class
#' @description Native StreamFind method to annotate feature components with isotope, adduct, and in-source fragment relationships.
#'
#' This method performs three sequential annotation steps for each feature component:
#'
#' **1. Isotope Annotation:** Identifies isotopologue patterns (13C, 34S, 37Cl, 81Br, 2H, 15N, 18O, 33S)
#' and links isotope features to their monoisotopic precursors. Annotations show the element and mass step
#' (e.g., "isotope MZ229 13C `[M+1]`" indicates a 13C isotope of the monoisotopic ion at m/z 229).
#'
#' **2. Adduct Annotation:** Identifies different ionization adducts (e.g., `[M+Na]+`, `[M+NH4]+`, `[M+K]+` in
#' positive mode; `[M+Cl]-`, `[M+FA-H]-` in negative mode) originating from the same neutral molecule.
#' Annotations reference the `[M+H]+` mass (e.g., "adduct MZ229 `[M+Na]+`" indicates a sodium adduct of
#' the protonated molecule at m/z 229).
#'
#' **3. Fragment Annotation:** Identifies in-source fragments resulting from common neutral losses
#' (H2O, CO2, NH3, CO, CH3, CH2O2) from parent `[M+H]+` ions. Annotations show the parent mass and loss
#' (e.g., "loss MZ229 -H2O" indicates a water loss fragment from the parent ion at m/z 229).
#'
#' Features without successful annotations are assigned default adducts (`[M+H]+` for positive mode,
#' `[M-H]-` for negative mode). The method processes features grouped by retention time and m/z similarity
#' (feature components) to ensure related ions are annotated together.
#'
#' @param maxIsotopes Integer specifying the maximum number of isotopes to consider. Defaults to 5.
#' @param maxCharge Integer specifying the maximum charge state to consider. Defaults to 1.
#' @param maxGaps Integer specifying the maximum number of gaps allowed in isotope patterns. Defaults to 1.
#' @param debugComponent Character specifying component ID to debug (empty string to disable). Defaults to "".
#' @param debugAnalysis Character specifying analysis name to debug (empty string to disable). Defaults to "".
#' @export
#'
DB_MassSpecMethod_AnnotateComponents_native <- function(
  maxIsotopes = 5,
  maxCharge = 1,
  maxGaps = 1,
  debugComponent = "",
  debugAnalysis = ""
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "AnnotateComponents",
    required = "CreateComponents",
    algorithm = "native",
    input_class = "DB_MassSpecResults_NonTargetAnalysis",
    output_class = "DB_MassSpecResults_NonTargetAnalysis",
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_,
    parameters = list(
      maxIsotopes = maxIsotopes,
      maxCharge = maxCharge,
      maxGaps = maxGaps,
      debugComponent = as.character(debugComponent),
      debugAnalysis = as.character(debugAnalysis)
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_AnnotateComponents_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_AnnotateComponents_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "AnnotateComponents")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_int(x$parameters$maxIsotopes, lower = 1, null.ok = FALSE)
  checkmate::assert_int(x$parameters$maxCharge, lower = 1, null.ok = FALSE)
  checkmate::assert_int(x$parameters$maxGaps, lower = 0, null.ok = FALSE)
  checkmate::assert_character(x$parameters$debugComponent, len = 1, null.ok = FALSE)
  checkmate::assert_character(x$parameters$debugAnalysis, len = 1, null.ok = FALSE)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_AnnotateComponents_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecAnalyses" %in% class(engine$Analyses)) {
    warning("Engine does not contain DB_MassSpecAnalyses.")
    return(FALSE)
  }

  analyses <- query_db(engine$Analyses, "SELECT * FROM Analyses")
  if (nrow(analyses) == 0) {
    warning("No analyses found in DB_MassSpecAnalyses.")
    return(FALSE)
  }

  db <- file.path(engine$get_project_path(), "DB_MassSpecResults_NonTargetAnalysis.duckdb")
  if (!file.exists(db)) {
    warning("DB_MassSpecResults_NonTargetAnalysis database not found. Run FindFeatures first.")
    return(FALSE)
  }

  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  headers <- query_db(engine$Analyses, "SELECT * FROM SpectraHeaders")
  features <- DBI::dbReadTable(conn, "Features")

  if (nrow(features) == 0) {
    warning("No features found in DB_MassSpecResults_NonTargetAnalysis. Not done.")
    return(FALSE)
  }

  headers_list <- split(headers, headers$analysis)
  features_list <- split(features, features$analysis)

  parameters <- x$parameters

  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses, parameters, features)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
        fts <- load_cache(cache_manager, hash = hash)
        if (!is.null(fts) && nrow(fts) > 0) {
          message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
          DB_MassSpecResults_NonTargetAnalysis(
            projectPath = engine$get_project_path(),
            features = fts
          )
          return(invisible(TRUE))
        }
      }
  }

  fts <- rcpp_nts_annotate_components(
    info = analyses,
    spectra_headers = headers_list,
    feature_list = features_list,
    maxIsotopes = parameters$maxIsotopes,
    maxCharge = parameters$maxCharge,
    maxGaps = parameters$maxGaps,
    debugComponent = parameters$debugComponent,
    debugAnalysis = parameters$debugAnalysis
  )

  if (is.null(fts) || length(fts) == 0) {
    warning("No components annotated.")
    return(FALSE)
  }

  names(fts) <- analyses$analysis
  fts <- data.table::rbindlist(fts, fill = TRUE, idcol = "analysis")

  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_AnnotateComponents_native"),
      hash = .make_hash(x, analyses, parameters, features),
      description = "Components annotated with DB_AnnotateComponents_native method",
      data = as.data.frame(fts)
    )
    message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  }

  DB_MassSpecResults_NonTargetAnalysis(
    projectPath = engine$get_project_path(),
    features = fts
  )
  invisible(TRUE)
}
