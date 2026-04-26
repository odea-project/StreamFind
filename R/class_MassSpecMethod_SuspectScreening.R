# MARK: Native
# Native ------

#' @title MassSpecMethod_SuspectScreening_native class
#' @description Native StreamFind method for suspect screening in non-target analysis results by matching features against a provided suspect database.
#' @param suspects A data.frame with suspect information. Must contain columns: `name` (character) and either `mass` (neutral monoisotopic mass) or `mz` (expected m/z). Optional columns: `rt` (retention time in seconds), `formula` (molecular formula), `SMILES`, `fragments` or `fragments_mz` (MS2 fragment m/z values, semicolon-separated), `fragments_int` (MS2 fragment intensities, semicolon-separated), `fragments_formula` (fragment formulas, semicolon-separated).
#' @param ppm Numeric. Mass tolerance in parts-per-million for matching suspect mass or *m/z* to features. Default: 5.
#' @param sec Numeric. Retention time tolerance in seconds for matching suspect RT to features. Default: 10.
#' @param ppmMS2 Numeric. Mass tolerance in ppm for MS2 fragment matching. Default: 10.
#' @param mzrMS2 Numeric. Minimum absolute m/z range for MS2 fragment matching (used when ppm range is smaller). Default: 0.008.
#' @param minCosineSimilarity Numeric. Minimum cosine similarity score (0-1) for MS2 spectral matching to upgrade identification level. Default: 0.7.
#' @param minSharedFragments Integer. Minimum number of shared fragments for MS2 matching to upgrade identification level. Default: 3.
#' @param filtered Logical. If TRUE, includes filtered features in the search. Default: TRUE.
#' @export
#'
MassSpecMethod_SuspectScreening_native <- function(
  suspects = NULL,
  ppm = 5,
  sec = 10,
  ppmMS2 = 10,
  mzrMS2 = 0.008,
  minCosineSimilarity = 0.7,
  minSharedFragments = 3,
  filtered = TRUE
) {
  if (is.null(suspects)) {
    suspects <- data.table::data.table(
      name = character(),
      mass = numeric(),
      rt = numeric(),
      formula = character(),
      SMILES = character(),
      InChI = character(),
      InChIKey = character(),
      xLogP = numeric(),
      ms2_positive = character(),
      ms2_negative = character()
    )
  } else {
    suspects <- data.table::as.data.table(suspects)
  }

  x <- ProcessingStep(
    type = "MassSpec",
    method = "SuspectScreening",
    required = c("FindFeatures"),
    algorithm = "native",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
    number_permitted = Inf,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_,
    parameters = list(
      suspects = suspects,
      ppm = as.numeric(ppm),
      sec = as.numeric(sec),
      ppmMS2 = as.numeric(ppmMS2),
      mzrMS2 = as.numeric(mzrMS2),
      minCosineSimilarity = as.numeric(minCosineSimilarity),
      minSharedFragments = as.integer(minSharedFragments),
      filtered = as.logical(filtered)
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for MassSpecMethod_SuspectScreening_native.")
  }
}

#' @export
#' @noRd
validate_object.MassSpecMethod_SuspectScreening_native <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "SuspectScreening")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_data_frame(data.table::as.data.table(x$parameters$suspects))
  suspects <- data.table::as.data.table(x$parameters$suspects)
  checkmate::assert_true("name" %in% colnames(suspects))
  checkmate::assert_true("mass" %in% colnames(suspects))
  checkmate::assert_numeric(x$parameters$ppm, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$sec, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$ppmMS2, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$mzrMS2, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minCosineSimilarity, len = 1, lower = 0, upper = 1)
  checkmate::assert_integerish(x$parameters$minSharedFragments, len = 1, lower = 0)
  checkmate::assert_logical(x$parameters$filtered, len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SuspectScreening_native <- function(x, engine = NULL) {
  if (!"MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  analyses_info <- info(engine$Analyses)
  parameters <- x$parameters

  # Check if cache exists
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses_info, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      suspects_out <- load_cache(cache_manager, hash = hash)
      if (!is.null(suspects_out)) {
        if (nrow(suspects_out) > 0) {
          message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
          # Ensure id_level is integer
          suspects_out$id_level <- as.integer(suspects_out$id_level)
          conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
          on.exit(DBI::dbDisconnect(conn), add = TRUE)
          DBI::dbExecute(conn, "DELETE FROM Suspects")
          DBI::dbWriteTable(conn, "Suspects", suspects_out, append = TRUE)
          message("\U2713 Suspects written to database.")

          return(invisible(TRUE))
        }
      }
    }
  }

  # Run suspect screening using suspect_screening
  suspects_out <- suspect_screening(
    x = engine$NonTargetAnalysis,
    analyses = NULL,
    suspects = parameters$suspects,
    ppm = parameters$ppm,
    sec = parameters$sec,
    ppmMS2 = parameters$ppmMS2,
    mzrMS2 = parameters$mzrMS2,
    minCosineSimilarity = parameters$minCosineSimilarity,
    minSharedFragments = parameters$minSharedFragments,
    filtered = parameters$filtered
  )

  if (is.null(suspects_out) || nrow(suspects_out) == 0) {
    warning("No suspects found.")
    return(FALSE)
  }

  # Remove columns that shouldn't be stored (they can change with other methods)
  columns_to_remove <- c("replicate", "feature_group", "feature_component", "adduct")
  columns_to_keep <- setdiff(colnames(suspects_out), columns_to_remove)
  suspects_out <- suspects_out[, ..columns_to_keep]
  if (!"candidate_rank" %in% colnames(suspects_out)) {
    suspects_out$candidate_rank <- 1L
  }
  col_order <- c(
    "analysis", "feature", "candidate_rank", "name", "polarity",
    "db_mass", "exp_mass", "error_mass",
    "db_rt", "exp_rt", "error_rt",
    "intensity", "area",
    "id_level", "score", "shared_fragments", "cosine_similarity",
    "formula", "SMILES", "InChI", "InChIKey", "xLogP", "database_id",
    "db_ms2_size", "db_ms2_mz", "db_ms2_intensity", "db_ms2_formula",
    "exp_ms2_size", "exp_ms2_mz", "exp_ms2_intensity"
  )
  keep_cols <- intersect(col_order, colnames(suspects_out))
  data.table::setcolorder(suspects_out, keep_cols)

  # Ensure id_level is integer
  suspects_out$id_level <- as.integer(suspects_out$id_level)

  # Cache results
  save_cache(
    cache_manager,
    name = paste0("SuspectScreening_native"),
    hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
    description = "Suspects found with SuspectScreening_native method",
    data = as.data.frame(suspects_out)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")

  # Write to database
  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .validate_MassSpecResults_NonTargetAnalysis_Suspects_db_schema(conn)

  # Clear existing data
  DBI::dbExecute(conn, "DELETE FROM Suspects")

  # Ensure id_level is integer before writing
  suspects_out$id_level <- as.integer(suspects_out$id_level)

  # Write new data
  DBI::dbWriteTable(conn, "Suspects", suspects_out, append = TRUE)

  message("\U2713 Suspects written to database.")
  invisible(TRUE)
}


# MARK: MetFrag
# MetFrag ------

#' @title MassSpecMethod_SuspectScreening_metfrag class
#' @description MetFragCL-based suspect screening using MS1/MS2 feature data and an external candidate database.
#' Requires MetFragCL (JAR or executable) and a working Java runtime (\url{https://www.java.com/en/download/}) when
#' using the JAR; see \url{https://ipb-halle.github.io/MetFrag/projects/metfragcl/} for installation details and
#' database options. For method background and scoring details, see the referenced MetFrag publications.
#' @param metfrag_path Character (length 1). Full path to the MetFragCL executable or JAR file.
#' @param database_type Character (length 1). MetFrag database type (e.g., "LocalCSV", "LocalSDF", "PubChem").
#' @param database_path Character (length 1). Path to the local database file (required for local databases).
#' @param ppm Numeric. Mass tolerance in parts-per-million for database search (MS1 precursor). Default: 5.
#' @param sec Numeric. Retention time tolerance in seconds for post-filtering candidates when RT is available. Default: 10.
#' @param ppmMS2 Numeric. Mass tolerance in ppm for MS2 fragment matching. Default: 10.
#' @param mzrMS2 Numeric. Minimum absolute m/z deviation for MS2 fragments (used when ppm range is smaller). Default: 0.008.
#' @param top_n Integer. Maximum number of candidates to consider per feature (top-ranked kept). Default: 1.
#' @param filtered Logical. If TRUE, includes filtered features in the search. Default: FALSE.
#' @param n_cores Integer. Number of parallel workers for batch execution. Default: 1.
#' @param java_path Character (length 1). Path to the Java executable used to run a MetFrag JAR. Default: "java".
#' @param metfrag_args Character vector. Optional arguments passed to MetFrag; use "{params}" as a placeholder for the
#' parameter file path.
#' @param debug Logical. If TRUE, writes per-feature debug metadata alongside MetFrag inputs. Default: FALSE.
#' @param show_progress Logical. If TRUE, shows a progress bar while processing features. Default: TRUE.
#' @param quiet Logical. If TRUE, suppresses console messages during processing. Default: TRUE.
#' @param extra_params Named list. Extra MetFrag parameters to add or override in the parameter file.
#'
#' @references
#'
#' \insertRef{metfrag01}{StreamFind}
#'
#' \insertRef{metfrag02}{StreamFind}
#'
#' \insertRef{metfrag03}{StreamFind}
#'
#' @export
#'
MassSpecMethod_SuspectScreening_metfrag <- function(
  metfrag_path = NULL,
  database_type = "LocalCSV",
  database_path = NULL,
  ppm = 5,
  sec = 10,
  ppmMS2 = 10,
  mzrMS2 = 0.008,
  top_n = 1,
  filtered = FALSE,
  n_cores = 1,
  java_path = "java",
  metfrag_args = NULL,
  debug = FALSE,
  show_progress = TRUE,
  quiet = TRUE,
  extra_params = list()
) {
  if (is.null(metfrag_path)) {
    stop("Argument 'metfrag_path' is required. Provide the full path to the MetFragCL executable or JAR.")
  }

  x <- ProcessingStep(
    type = "MassSpec",
    method = "SuspectScreening",
    required = c("FindFeatures", "LoadFeaturesMS1", "LoadFeaturesMS2"),
    algorithm = "metfrag",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
    number_permitted = Inf,
    version = as.character(packageVersion("StreamFind")),
    software = "MetFragCL",
    developer = "Christoph Ruttkies and Emma L. Schymanski",
    contact = "cruttkie@ipb-halle.de",
    link = "https://ipb-halle.github.io/MetFrag/projects/metfragcl/",
    doi = "https://doi.org/10.1186/s13321-016-0115-9",
    parameters = list(
      metfrag_path = as.character(metfrag_path),
      database_type = as.character(database_type),
      database_path = if (is.null(database_path)) NULL else as.character(database_path),
      ppm = as.numeric(ppm),
      sec = as.numeric(sec),
      ppmMS2 = as.numeric(ppmMS2),
      mzrMS2 = as.numeric(mzrMS2),
      top_n = as.integer(top_n),
      filtered = as.logical(filtered),
      n_cores = as.integer(n_cores),
      java_path = as.character(java_path),
      metfrag_args = if (is.null(metfrag_args)) NULL else as.character(metfrag_args),
      debug = as.logical(debug),
      show_progress = as.logical(show_progress),
      quiet = as.logical(quiet),
      extra_params = extra_params
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for MassSpecMethod_SuspectScreening_metfrag.")
  }
}

#' @export
#' @noRd
validate_object.MassSpecMethod_SuspectScreening_metfrag <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "SuspectScreening")
  checkmate::assert_choice(x$algorithm, "metfrag")
  checkmate::assert_character(x$parameters$metfrag_path, len = 1)
  checkmate::assert_character(x$parameters$database_type, len = 1)
  checkmate::assert_numeric(x$parameters$ppm, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$sec, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$ppmMS2, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$mzrMS2, len = 1, lower = 0)
  checkmate::assert_integerish(x$parameters$top_n, len = 1, lower = 1)
  checkmate::assert_logical(x$parameters$filtered, len = 1)
  checkmate::assert_integerish(x$parameters$n_cores, len = 1, lower = 1)
  checkmate::assert_character(x$parameters$java_path, len = 1)
  checkmate::assert_logical(x$parameters$debug, len = 1)
  checkmate::assert_logical(x$parameters$show_progress, len = 1)
  checkmate::assert_logical(x$parameters$quiet, len = 1)
  if (!is.null(x$parameters$database_path)) {
    checkmate::assert_character(x$parameters$database_path, len = 1)
  }
  if (!is.null(x$parameters$metfrag_args)) {
    checkmate::assert_character(x$parameters$metfrag_args, min.len = 1)
  }
  if (!is.null(x$parameters$extra_params)) {
    checkmate::assert_list(x$parameters$extra_params)
  }
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SuspectScreening_metfrag <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!"MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  parameters <- x$parameters
  analyses_info <- info(engine$Analyses)

  # Check cache
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses_info, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      suspects_out <- load_cache(cache_manager, hash = hash)
      if (!is.null(suspects_out)) {
        if (nrow(suspects_out) > 0) {
          message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
          suspects_out$id_level <- as.integer(suspects_out$id_level)
          conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
          on.exit(DBI::dbDisconnect(conn), add = TRUE)
          DBI::dbExecute(conn, "DELETE FROM Suspects")
          DBI::dbWriteTable(conn, "Suspects", suspects_out, append = TRUE)
          message("\U2713 Suspects written to database.")
          return(invisible(TRUE))
        }
      }
    }
  }

  if (!file.exists(parameters$metfrag_path)) {
    warning("MetFragCL executable not found: ", parameters$metfrag_path)
    return(FALSE)
  }

  nts <- engine$NonTargetAnalysis
  fts <- query_db(nts, "SELECT * FROM Features")
  if (nrow(fts) == 0) {
    warning("No features available for MetFrag suspect screening! Not done.")
    return(FALSE)
  }

  analyses_db <- query_db(nts$analyses, "SELECT * FROM Analyses")

  feature_list <- lapply(analyses_db$analysis, function(ana) {
    ana_features <- fts[fts$analysis == ana, ]
    if (nrow(ana_features) == 0) return(fts[0, ])
    ana_features
  })
  names(feature_list) <- analyses_db$analysis

  spectra_headers <- query_db(nts$analyses, "SELECT * FROM SpectraHeaders")
  headers_list <- split(spectra_headers, spectra_headers$analysis)

  run_dir <- file.path(getwd(), "log", paste0("metfrag_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  if (parameters$debug) {
    message("\U1f50d MetFrag debug files will be written to: ", run_dir)
  }

  suspects_raw <- rcpp_nts_metfrag_screening(
    info = analyses_db,
    spectra_headers = headers_list,
    feature_list = feature_list,
    metfrag_path = parameters$metfrag_path,
    database_type = parameters$database_type,
    database_path = if (is.null(parameters$database_path)) "" else parameters$database_path,
    analyses = "",
    ppm = parameters$ppm,
    sec = parameters$sec,
    ppmMS2 = parameters$ppmMS2,
    mzrMS2 = parameters$mzrMS2,
    top_n = parameters$top_n,
    filtered = parameters$filtered,
    java_path = parameters$java_path,
    run_dir = run_dir,
    debug = parameters$debug,
    extra_params = parameters$extra_params
  )

  suspects_out <- data.table::rbindlist(suspects_raw, fill = TRUE)
  if (nrow(suspects_out) == 0) {
    warning("No suspects found with MetFrag.")
    return(FALSE)
  }

  if ("score" %in% colnames(suspects_out)) {
    suspects_out <- suspects_out[order(analysis, feature, -score)]
    suspects_out[, candidate_rank := seq_len(.N), by = .(analysis, feature)]
  }

  col_order <- c(
    "analysis", "feature", "candidate_rank", "name", "polarity",
    "db_mass", "exp_mass", "error_mass",
    "db_rt", "exp_rt", "error_rt",
    "intensity", "area",
    "id_level", "score", "shared_fragments", "cosine_similarity",
    "formula", "SMILES", "InChI", "InChIKey", "xLogP", "database_id",
    "db_ms2_size", "db_ms2_mz", "db_ms2_intensity", "db_ms2_formula",
    "exp_ms2_size", "exp_ms2_mz", "exp_ms2_intensity"
  )
  keep_cols <- intersect(col_order, colnames(suspects_out))
  data.table::setcolorder(suspects_out, keep_cols)
  suspects_out$id_level <- as.integer(suspects_out$id_level)

  # Cache results
  save_cache(
    cache_manager,
    name = paste0("SuspectScreening_metfrag"),
    hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
    description = "Suspects found with SuspectScreening_metfrag method",
    data = as.data.frame(suspects_out)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")

  # Write to database
  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .validate_MassSpecResults_NonTargetAnalysis_Suspects_db_schema(conn)
  DBI::dbExecute(conn, "DELETE FROM Suspects")
  DBI::dbWriteTable(conn, "Suspects", suspects_out, append = TRUE)
  message("\U2713 Suspects written to database.")
  invisible(TRUE)
}
