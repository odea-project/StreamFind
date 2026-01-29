# MARK: AssignTransformationProducts
# Native ------

#' @title DB_MassSpecMethod_AssignTransformationProducts_native class
#' @description Native StreamFind method to link transformation products and parents using an existing Suspects
#' table and assign feature groups based on matched suspect features. Writes the TransformationProducts table.
#' @param transformation_products A data.frame with transformation products and parent entries. Uses `SMILES` to
#' link parents and products via `precursor_SMILES`.
#' @export
#'
DB_MassSpecMethod_AssignTransformationProducts_native <- function(
  transformation_products = NULL
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
      precursor_xLogP = numeric()
    )
  } else {
    transformation_products <- data.table::as.data.table(transformation_products)
  }

  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "AssignTransformationProducts",
    required = c("SuspectScreening"),
    algorithm = "native",
    input_class = "DB_MassSpecResults_NonTargetAnalysis",
    output_class = "DB_MassSpecResults_NonTargetAnalysis",
    number_permitted = Inf,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_,
    parameters = list(
      transformation_products = transformation_products
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_AssignTransformationProducts_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_AssignTransformationProducts_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "AssignTransformationProducts")
  checkmate::assert_choice(x$algorithm, "native")
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
run.DB_MassSpecMethod_AssignTransformationProducts_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  parameters <- x$parameters
  analyses_info <- info(engine$Analyses)

  normalize_tp_columns <- function(tp) {
    col_map <- c(
      "name" = "name",
      "inchi" = "InChI",
      "inchikey" = "InChIKey",
      "smiles" = "SMILES",
      "formula" = "formula",
      "mass" = "mass",
      "neutralmass" = "mass",
      "xlogp" = "xLogP",
      "logp" = "xLogP",
      "alogp" = "xLogP",
      "reaction" = "transformation",
      "precursor_name" = "precursor_name",
      "precursor_inchi" = "precursor_InChI",
      "precursor_inchikey" = "precursor_InChIKey",
      "precursor_mass" = "precursor_mass",
      "precursor_smiles" = "precursor_SMILES",
      "precursor_formula" = "precursor_formula",
      "precursor_xlogp" = "precursor_xLogP",
      "precursor_logp" = "precursor_xLogP",
      "precursor_alogp" = "precursor_xLogP"
    )

    nms <- names(tp)
    for (i in seq_along(nms)) {
      key <- tolower(nms[i])
      if (key %in% names(col_map)) {
        target <- col_map[[key]]
        if (!(target %in% names(tp))) {
          data.table::setnames(tp, nms[i], target)
        }
      }
    }

    required_cols <- c(
      "name", "formula", "mass", "SMILES", "InChI", "InChIKey", "xLogP",
      "transformation",
      "precursor_name", "precursor_formula", "precursor_mass",
      "precursor_SMILES", "precursor_InChI", "precursor_InChIKey",
      "precursor_xLogP"
    )

    for (col in required_cols) {
      if (!col %in% names(tp)) {
        tp[[col]] <- if (col %in% c("mass", "xLogP", "precursor_xLogP", "precursor_mass")) {
          NA_real_
        } else {
          NA_character_
        }
      }
    }

    tp$mass <- suppressWarnings(as.numeric(tp$mass))
    tp$xLogP <- suppressWarnings(as.numeric(tp$xLogP))
    tp$precursor_mass <- suppressWarnings(as.numeric(tp$precursor_mass))
    tp$precursor_xLogP <- suppressWarnings(as.numeric(tp$precursor_xLogP))
    tp$cosine_similarity <- suppressWarnings(as.numeric(tp$cosine_similarity))

    tp
  }

  tp <- normalize_tp_columns(parameters$transformation_products)

  # Cache check
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses_info, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      cached_tp <- load_cache(cache_manager, hash = hash)
      if (!is.null(cached_tp)) {
        message("\u2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
        conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
        on.exit(DBI::dbDisconnect(conn), add = TRUE)
        .validate_DB_MassSpecResults_NonTargetAnalysis_TransformationProducts_db_schema(conn)
        DBI::dbExecute(conn, "DELETE FROM TransformationProducts")
        DBI::dbWriteTable(conn, "TransformationProducts", cached_tp, append = TRUE)
        message("\u2713 Transformation products written to database.")
        return(invisible(TRUE))
      }
    }
  }

  tp$feature_group <- ""
  tp$cosine_similarity <- NA_real_
  if (nrow(tp) > 0) {
    conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    if (!"Suspects" %in% DBI::dbListTables(conn)) {
      warning("No Suspects table found in database! Run SuspectScreening first.")
    } else {
      suspects <- data.table::as.data.table(DBI::dbGetQuery(conn, "SELECT * FROM Suspects"))
      if (nrow(suspects) > 0) {
        .validate_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema(conn)
        keys <- unique(paste(suspects$analysis, suspects$feature, sep = "|"))
        placeholders <- paste(rep("?", length(keys)), collapse = ", ")
        feature_query <- sprintf(
          "SELECT analysis, feature, feature_group FROM Features WHERE (analysis || '|' || feature) IN (%s)",
          placeholders
        )
        fts <- data.table::as.data.table(DBI::dbGetQuery(conn, feature_query, params = as.list(keys)))
        if (nrow(fts) > 0) {
          fts <- merge(fts, suspects[, .(analysis, feature, SMILES)], by = c("analysis", "feature"), all.x = TRUE)
          fts$feature_group[is.na(fts$feature_group)] <- ""
          grp <- fts[feature_group != "" & !is.na(SMILES) & SMILES != "", .(
            feature_group = paste(unique(feature_group), collapse = ";")
          ), by = .(SMILES)]
          tp$feature_group <- grp$feature_group[match(tp$SMILES, grp$SMILES)]
          tp$feature_group[is.na(tp$feature_group)] <- ""
        }
      }
    }
  }

  col_order <- c(
    "name", "formula", "mass", "SMILES", "InChI", "InChIKey", "xLogP",
    "transformation",
    "precursor_name", "precursor_formula", "precursor_mass",
    "precursor_SMILES", "precursor_InChI", "precursor_InChIKey",
    "precursor_xLogP",
    "feature_group", "cosine_similarity"
  )
  data.table::setcolorder(tp, col_order)

  # Cache results
  save_cache(
    cache_manager,
    name = paste0("DB_AssignTransformationProducts_native"),
    hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
    description = "Transformation products assignment results",
    data = as.data.frame(tp)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")

  # Write to database
  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .validate_DB_MassSpecResults_NonTargetAnalysis_TransformationProducts_db_schema(conn)
  DBI::dbExecute(conn, "DELETE FROM TransformationProducts")
  DBI::dbWriteTable(conn, "TransformationProducts", tp, append = TRUE)

  message("\u2713 Transformation products written to database.")
  invisible(TRUE)
}
