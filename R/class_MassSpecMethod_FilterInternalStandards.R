#' MassSpecMethod_FilterInternalStandards_native Class
#'
#' @description Settings for filtering internal standards in MassSpecResults_NonTargetAnalysis objects based on internal standard properties.
#'
#' @param names Character vector with internal standard names to match (via partial matching). Empty vector means no name filtering.
#' @param minScore Numeric (length 1) with the minimum score.
#' @param maxErrorRT Numeric (length 1) with the maximum absolute retention time error in seconds.
#' @param maxErrorMass Numeric (length 1) with the maximum absolute mass error in ppm.
#' @param idLevels Integer vector with the identification levels to keep (e.g., c(1, 2, 3)). Empty vector means no level filtering.
#' @param minSharedFragments Integer (length 1) with the minimum number of shared fragments.
#' @param minCosineSimilarity Numeric (length 1) with the minimum cosine similarity (0-1).
#'
#' @return A `MassSpecMethod_FilterInternalStandards_native` object.
#'
#' @export
#'
MassSpecMethod_FilterInternalStandards_native <- function(
	names = character(0),
	minScore = NA_real_,
	maxErrorRT = NA_real_,
	maxErrorMass = NA_real_,
	idLevels = integer(0),
	minSharedFragments = 0,
	minCosineSimilarity = NA_real_
) {
	x <- ProcessingStep(
		type = "MassSpec",
		method = "FilterInternalStandards",
		required = "FindInternalStandard",
		algorithm = "native",
		input_class = "MassSpecResults_NonTargetAnalysis",
		output_class = "MassSpecResults_NonTargetAnalysis",
		parameters = list(
			names = as.character(names),
			minScore = as.numeric(minScore),
			maxErrorRT = as.numeric(maxErrorRT),
			maxErrorMass = as.numeric(maxErrorMass),
			idLevels = as.integer(idLevels),
			minSharedFragments = as.integer(minSharedFragments),
			minCosineSimilarity = as.numeric(minCosineSimilarity)
		),
		number_permitted = Inf,
		version = as.character(packageVersion("StreamFind")),
		software = "StreamFind",
		developer = "Ricardo Cunha",
		contact = "cunha@iuta.de",
		link = "https://odea-project.github.io/StreamFind",
		doi = NA_character_
	)
	if (is.null(validate_object(x))) {
		return(x)
	} else {
		stop("Invalid MassSpecMethod_FilterInternalStandards_native object!")
	}
}

#' @export
#' @noRd
#'
validate_object.MassSpecMethod_FilterInternalStandards_native <- function(x) {
	checkmate::assert_choice(x$type, "MassSpec")
	checkmate::assert_choice(x$method, "FilterInternalStandards")
	checkmate::assert_choice(x$algorithm, "native")
	checkmate::assert_character(x$parameters$names)
	checkmate::assert_numeric(x$parameters$minScore, len = 1)
	checkmate::assert_numeric(x$parameters$maxErrorRT, len = 1)
	checkmate::assert_numeric(x$parameters$maxErrorMass, len = 1)
	checkmate::assert_integerish(x$parameters$idLevels)
	checkmate::assert_integerish(x$parameters$minSharedFragments, len = 1)
	checkmate::assert_numeric(x$parameters$minCosineSimilarity, len = 1)
	NULL
}

#' @export
#' @noRd
#'
run.MassSpecMethod_FilterInternalStandards_native <- function(
	x,
	engine = NULL
) {
	if (!is(engine, "MassSpecEngine")) {
		warning("Engine is not a MassSpecEngine object!")
		return(FALSE)
	}

	if (is.null(engine$NonTargetAnalysis)) {
		warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
		return(FALSE)
	}

	nts <- engine$NonTargetAnalysis
	analyses_info <- info(engine$Analyses)
	parameters <- x$parameters

	# Check cache
	cache_manager <- engine$Cache
	if (!is.null(cache_manager)) {
		hash <- .make_hash(x, analyses_info, parameters, engine$Workflow)
		cache_info <- get_cache_info(cache_manager)
		if (nrow(cache_info) > 0) {
			istd <- load_cache(cache_manager, hash = hash)
			if (!is.null(istd) && is.data.frame(istd)) {
				if (nrow(istd) > 0) {
					message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
					# Ensure id_level is integer
					istd$id_level <- as.integer(istd$id_level)
					conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
					on.exit(DBI::dbDisconnect(conn), add = TRUE)
					DBI::dbExecute(conn, "DELETE FROM InternalStandards")
					DBI::dbWriteTable(conn, "InternalStandards", istd, append = TRUE)
					message("\U2713 Internal standards written to database.")
					return(invisible(TRUE))
				}
			}
		}
	}

	# Query all internal standards from database
	istd <- query_db(nts, "SELECT * FROM InternalStandards")
	if (nrow(istd) == 0) {
		warning("No internal standards found in MassSpecResults_NonTargetAnalysis! Not done.")
		return(FALSE)
	}

	# Ensure id_level is integer
	istd$id_level <- as.integer(istd$id_level)

	# Count internal standards before filtering
	n_before <- nrow(istd)

	analyses_db <- query_db(engine$Analyses, "SELECT * FROM Analyses")

	internal_standards_list <- lapply(analyses_db$analysis, function(ana) {
		ana_istd <- istd[istd$analysis == ana, ]
		if (nrow(ana_istd) == 0) {
			return(istd[0, ])
		}
		ana_istd
	})
	names(internal_standards_list) <- analyses_db$analysis

	istd_list <- rcpp_nts_filter_internal_standards(
		info = analyses_db,
		internal_standards_list = internal_standards_list,
		names = parameters$names,
		minScore = parameters$minScore,
		maxErrorRT = parameters$maxErrorRT,
		maxErrorMass = parameters$maxErrorMass,
		idLevels = parameters$idLevels,
		minSharedFragments = parameters$minSharedFragments,
		minCosineSimilarity = parameters$minCosineSimilarity
	)

	if (is.null(istd_list) || length(istd_list) == 0) {
		warning("Internal standard filtering failed.")
		return(FALSE)
	}

	names(istd_list) <- analyses_db$analysis
	istd <- data.table::rbindlist(istd_list, fill = TRUE)

	# Count internal standards after filtering
	n_after <- nrow(istd)
	n_filtered <- n_before - n_after

	message(sprintf("\u2713 FilterInternalStandards complete: %d internal standards filtered, %d remaining", n_filtered, n_after))

	# Save to cache
	if (!is.null(cache_manager)) {
		save_cache(
			cache_manager,
			name = paste0("FilterInternalStandards_native"),
			hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
			description = "Internal standards filtered with FilterInternalStandards_native method",
			data = as.data.frame(istd)
		)
		message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
	}

	# Write to database
	conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
	on.exit(DBI::dbDisconnect(conn), add = TRUE)

	# Clear existing data
	DBI::dbExecute(conn, "DELETE FROM InternalStandards")

	# Ensure id_level is integer before writing
	if (nrow(istd) > 0) {
		istd$id_level <- as.integer(istd$id_level)
	}

	# Write new data
	DBI::dbWriteTable(conn, "InternalStandards", istd, append = TRUE)

	message("\U2713 Internal standards written to database.")
	invisible(TRUE)
}
