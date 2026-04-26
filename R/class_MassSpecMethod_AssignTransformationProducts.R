#' @title MassSpecMethod_AssignTransformationProducts_native class
#' @description Native StreamFind method to link transformation products and parents using an existing Suspects
#' table and assign feature groups based on matched suspect features. Writes the TransformationProducts table.
#' @param transformation_products A data.frame with transformation products and parent entries. Uses `SMILES` to
#' link parents and products via `precursor_SMILES`.
#' @param chromatographic_phase Character (length 1). Chromatographic phase for RT plausibility checks.
#' One of: "reverse_phase", "hilic".
#' @param mzrMS2 Numeric. Absolute m/z tolerance for MS2 fragment matching when computing cosine similarity.
#' @export
#'
MassSpecMethod_AssignTransformationProducts_native <- function(
	transformation_products = NULL,
	chromatographic_phase = c("reverse_phase", "hilic"),
	mzrMS2 = 0.008
) {
	if (is.null(transformation_products)) {
		transformation_products <- data.table::data.table(
			name = character(),
			formula = character(),
			mass = numeric(),
			SMILES = character(),
			InChI = character(),
			InChIKey = character(),
			xLogP = numeric(),
			transformation = character(),
			precursor_name = character(),
			precursor_formula = character(),
			precursor_mass = numeric(),
			precursor_SMILES = character(),
			precursor_InChI = character(),
			precursor_InChIKey = character(),
			precursor_xLogP = numeric(),
			main_precursor_name = character(),
			main_precursor_formula = character(),
			main_precursor_mass = numeric(),
			main_precursor_SMILES = character(),
			main_precursor_InChI = character(),
			main_precursor_InChIKey = character(),
			main_precursor_xLogP = numeric()
		)
	} else {
		transformation_products <- data.table::as.data.table(transformation_products)
	}

	chromatographic_phase <- match.arg(chromatographic_phase)
	x <- ProcessingStep(
		type = "MassSpec",
		method = "AssignTransformationProducts",
		required = c("SuspectScreening"),
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
			transformation_products = transformation_products,
			chromatographic_phase = as.character(chromatographic_phase),
			mzrMS2 = as.numeric(mzrMS2)
		)
	)
	if (is.null(validate_object(x))) {
		x
	} else {
		stop("Invalid parameters for MassSpecMethod_AssignTransformationProducts_native.")
	}
}

#' @export
#' @noRd
validate_object.MassSpecMethod_AssignTransformationProducts_native <- function(x) {
	checkmate::assert_choice(x$type, "MassSpec")
	checkmate::assert_choice(x$method, "AssignTransformationProducts")
	checkmate::assert_choice(x$algorithm, "native")
	checkmate::assert_choice(x$parameters$chromatographic_phase, c("reverse_phase", "hilic"))
	checkmate::assert_number(x$parameters$mzrMS2, lower = 0)
	checkmate::assert_data_frame(data.table::as.data.table(x$parameters$transformation_products))
	tps <- data.table::as.data.table(x$parameters$transformation_products)
	if (nrow(tps) > 0) {
		checkmate::assert_true("name" %in% colnames(tps))
		checkmate::assert_true("SMILES" %in% colnames(tps))
	}
	NULL
}

#' @export
#' @noRd
run.MassSpecMethod_AssignTransformationProducts_native <- function(x, engine = NULL) {
  if (!"MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  parameters    <- x$parameters
  analyses_info <- info(engine$Analyses)
  nts           <- engine$NonTargetAnalysis

  tp <- data.table::as.data.table(parameters$transformation_products)

  # Cache check
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash       <- .make_hash(x, analyses_info, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      cached_tp <- load_cache(cache_manager, hash = hash)
      if (!is.null(cached_tp)) {
        message("\u2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
        conn <- DBI::dbConnect(duckdb::duckdb(), nts$db)
        on.exit(DBI::dbDisconnect(conn), add = TRUE)
        .validate_MassSpecResults_NonTargetAnalysis_TransformationProducts_db_schema(conn)
        DBI::dbExecute(conn, "DELETE FROM TransformationProducts")
        DBI::dbWriteTable(conn, "TransformationProducts", cached_tp, append = TRUE)
        message("\u2713 Transformation products written to database.")
        return(invisible(TRUE))
      }
    }
  }

  suspects <- data.table::as.data.table(get_suspects(nts))

  tp_out <- rcpp_nts_assign_transformation_products(
    suspects                = suspects,
    transformation_products = tp,
    chromatographic_phase   = parameters$chromatographic_phase,
    mzrMS2                  = parameters$mzrMS2
  )
  tp_out <- data.table::as.data.table(tp_out)

  save_cache(
    cache_manager,
    name        = "AssignTransformationProducts_native",
    hash        = .make_hash(x, analyses_info, parameters, engine$Workflow),
    description = "Transformation products assignment results",
    data        = as.data.frame(tp_out)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")

  conn <- DBI::dbConnect(duckdb::duckdb(), nts$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .validate_MassSpecResults_NonTargetAnalysis_TransformationProducts_db_schema(conn)
  DBI::dbExecute(conn, "DELETE FROM TransformationProducts")
  DBI::dbWriteTable(conn, "TransformationProducts", tp_out, append = TRUE)
  message("\u2713 Transformation products written to database.")
  invisible(TRUE)
}
