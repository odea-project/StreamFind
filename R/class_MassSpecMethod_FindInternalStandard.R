#' @title MassSpecMethod_FindInternalStandard_native class
#' @description Native StreamFind method for finding internal standards in non-target analysis results by suspect screening against a provided database.
#' @param suspects A data.frame with suspect information. Must contain columns: `name` (character) and either `mass` (neutral monoisotopic mass) or `mz` (expected m/z). Optional columns: `rt` (retention time in seconds), `formula` (molecular formula), `SMILES`, `ms2_positive`/`ms2_negative` (MS2 fragment mz-int pairs separated by `;`), or legacy `fragments`/`fragments_mz` + `fragments_int`.
#' @param ppm Numeric. Mass tolerance in parts-per-million for matching suspect mass or *m/z* to features. Default: 5.
#' @param sec Numeric. Retention time tolerance in seconds for matching suspect RT to features. Default: 10.
#' @param ppmMS2 Numeric. Mass tolerance in ppm for MS2 fragment matching. Default: 10.
#' @param mzrMS2 Numeric. Minimum absolute m/z range for MS2 fragment matching (used when ppm range is smaller). Default: 0.008.
#' @param minCosineSimilarity Numeric. Minimum cosine similarity score (0-1) for MS2 spectral matching to upgrade identification level. Default: 0.7.
#' @param minSharedFragments Integer. Minimum number of shared fragments for MS2 matching to upgrade identification level. Default: 3.
#' @param filtered Logical. If TRUE, includes filtered features in the search. Default: TRUE.
#' @export
#'
MassSpecMethod_FindInternalStandard_native <- function(
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
			fragments_mz = character(),
			fragments_int = character(),
			fragments_formula = character()
		)
	} else {
		suspects <- data.table::as.data.table(suspects)
	}

	x <- ProcessingStep(
		type = "MassSpec",
		method = "FindInternalStandard",
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
		stop("Invalid parameters for MassSpecMethod_FindInternalStandard_native.")
	}
}

#' @export
#' @noRd
validate_object.MassSpecMethod_FindInternalStandard_native <- function(x) {
	checkmate::assert_choice(x$type, "MassSpec")
	checkmate::assert_choice(x$method, "FindInternalStandard")
	checkmate::assert_choice(x$algorithm, "native")
	checkmate::assert_data_frame(data.table::as.data.table(x$parameters$suspects))
	suspects <- data.table::as.data.table(x$parameters$suspects)
	checkmate::assert_true("name" %in% colnames(suspects))
	checkmate::assert_true(any(c("mass", "mz") %in% colnames(suspects)) || nrow(suspects) == 0)
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
run.MassSpecMethod_FindInternalStandard_native <- function(x, engine = NULL) {
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
      internal_standards <- load_cache(cache_manager, hash = hash)
      if (!is.null(internal_standards)) {
        if (nrow(internal_standards) > 0) {
          message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
          # Ensure id_level is integer
          internal_standards$id_level <- as.integer(internal_standards$id_level)
          conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
          on.exit(DBI::dbDisconnect(conn), add = TRUE)
          DBI::dbExecute(conn, "DELETE FROM InternalStandards")
          DBI::dbWriteTable(conn, "InternalStandards", internal_standards, append = TRUE)
          message("\U2713 Internal standards written to database.")
          return(invisible(TRUE))
        }
      }
    }
  }

  # Run suspect screening using suspect_screening
  internal_standards <- suspect_screening(
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

  if (is.null(internal_standards) || nrow(internal_standards) == 0) {
    warning("No internal standards found.")
    return(FALSE)
  }

  # Remove columns that shouldn't be stored (they can change with other methods)
  columns_to_remove <- c("replicate", "feature_group", "feature_component", "adduct")
  columns_to_keep <- setdiff(colnames(internal_standards), columns_to_remove)
  internal_standards <- internal_standards[, ..columns_to_keep]
  if (!"candidate_rank" %in% colnames(internal_standards)) {
    internal_standards$candidate_rank <- 1L
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
  keep_cols <- intersect(col_order, colnames(internal_standards))
  data.table::setcolorder(internal_standards, keep_cols)

  # Ensure id_level is integer
  internal_standards$id_level <- as.integer(internal_standards$id_level)

  # Cache results
  save_cache(
    cache_manager,
    name = paste0("FindInternalStandard_native"),
    hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
    description = "Internal standards found with FindInternalStandard_native method",
    data = as.data.frame(internal_standards)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")

  # Write to database
  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Clear existing data
  DBI::dbExecute(conn, "DELETE FROM InternalStandards")

  # Ensure id_level is integer before writing
  internal_standards$id_level <- as.integer(internal_standards$id_level)

  # Write new data
  DBI::dbWriteTable(conn, "InternalStandards", internal_standards, append = TRUE)

  message("\U2713 Internal standards written to database.")
  invisible(TRUE)
}
