#' @title DB_MassSpecMethod_CalculateTransformationProductsSimilarity_native class
#' @description Calculates MS2 spectral similarity between transformation products and their parent compounds
#' stored in DB_MassSpecResults_NonTargetAnalysis. Updates the TransformationProducts table with cosine similarity.
#' @param mzr The m/z tolerance for fragment matching. Default is 0.008.
#' @param minNumberSharedFragments Minimum number of shared fragments required for filtering. Default is 1.
#' @param minCosine Minimum cosine similarity required for filtering. Default is 0.2.
#' @param filter Logical, if TRUE removes transformation products not meeting the thresholds. Default is FALSE.
#' @export
#'
DB_MassSpecMethod_CalculateTransformationProductsSimilarity_native <- function(
  mzr = 0.008,
  minNumberSharedFragments = 1,
  minCosine = 0.2,
  filter = FALSE
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "CalculateTransformationProductsSimilarity",
    required = c("AssignTransformationProducts"),
    algorithm = "native",
    input_class = "DB_MassSpecResults_NonTargetAnalysis",
    output_class = "DB_MassSpecResults_NonTargetAnalysis",
    parameters = list(
      mzr = as.numeric(mzr),
      minNumberSharedFragments = as.integer(minNumberSharedFragments),
      minCosine = as.numeric(minCosine),
      filter = as.logical(filter)
    ),
    number_permitted = 1,
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
    stop("Invalid DB_MassSpecMethod_CalculateTransformationProductsSimilarity_native object!")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_CalculateTransformationProductsSimilarity_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "CalculateTransformationProductsSimilarity")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_number(x$parameters$mzr, lower = 0)
  checkmate::assert_int(x$parameters$minNumberSharedFragments, lower = 0)
  checkmate::assert_number(x$parameters$minCosine, lower = 0, upper = 1)
  checkmate::assert_logical(x$parameters$filter, len = 1)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_CalculateTransformationProductsSimilarity_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  parameters <- x$parameters
  analyses_info <- info(engine$Analyses)

  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .validate_DB_MassSpecResults_NonTargetAnalysis_TransformationProducts_db_schema(conn)

  if (!"TransformationProducts" %in% DBI::dbListTables(conn)) {
    warning("No TransformationProducts table found in database.")
    return(FALSE)
  }

  tp <- data.table::as.data.table(DBI::dbGetQuery(conn, "SELECT * FROM TransformationProducts"))
  if (nrow(tp) == 0) {
    warning("TransformationProducts table is empty! Not done.")
    return(FALSE)
  }

  # Cache check
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses_info, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      cached_tp <- load_cache(cache_manager, hash = hash)
      if (!is.null(cached_tp)) {
        message("\u2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
        DBI::dbExecute(conn, "DELETE FROM TransformationProducts")
        DBI::dbWriteTable(conn, "TransformationProducts", cached_tp, append = TRUE)
        message("\u2713 Transformation products similarity written to database.")
        return(invisible(TRUE))
      }
    }
  }

  if (!"feature_group" %in% names(tp)) {
    warning("TransformationProducts table does not have feature_group column! Not done.")
    return(FALSE)
  }

  split_groups <- function(x) {
    if (is.na(x) || !nzchar(x)) return(character(0))
    parts <- unlist(strsplit(x, ";", fixed = TRUE))
    parts <- trimws(parts)
    parts[parts != ""]
  }

  group_list <- unique(unlist(lapply(tp$feature_group, split_groups)))
  if (length(group_list) == 0) {
    warning("No feature groups found in TransformationProducts table! Not done.")
    return(FALSE)
  }

  placeholders <- paste(rep("?", length(group_list)), collapse = ", ")
  query <- sprintf(
    "SELECT feature_group, ms2_size, ms2_mz, ms2_intensity FROM Features WHERE feature_group IN (%s) AND ms2_size > 0",
    placeholders
  )
  fts <- data.table::as.data.table(DBI::dbGetQuery(conn, query, params = as.list(group_list)))
  if (nrow(fts) == 0) {
    warning("No MS2 spectra found for feature groups! Not done.")
    return(FALSE)
  }

  decode_ms2 <- function(mz_enc, int_enc) {
    if (is.na(mz_enc) || is.na(int_enc) || nchar(mz_enc) == 0 || nchar(int_enc) == 0) {
      return(NULL)
    }
    mz_dec <- rcpp_streamcraft_decode_string(mz_enc)
    int_dec <- rcpp_streamcraft_decode_string(int_enc)
    if (length(mz_dec) == 0 || length(int_dec) == 0 || length(mz_dec) != length(int_dec)) {
      return(NULL)
    }
    data.table::data.table(mz = mz_dec, intensity = int_dec)
  }

  group_specs <- split(fts, fts$feature_group)
  spec_map <- lapply(group_specs, function(df) {
    specs <- lapply(seq_len(nrow(df)), function(i) {
      decode_ms2(df$ms2_mz[i], df$ms2_intensity[i])
    })
    specs[!vapply(specs, is.null, logical(1))]
  })

  get_specs_for_row <- function(row) {
    groups <- split_groups(row$feature_group)
    specs <- list()
    for (g in groups) {
      if (!is.null(spec_map[[g]])) {
        specs <- c(specs, spec_map[[g]])
      }
    }
    specs
  }

  calc_similarity <- function(spec1, spec2, tol) {
    if (nrow(spec1) == 0 || nrow(spec2) == 0) return(list(cos = 0, shared = 0L))
    all_mz <- sort(unique(c(spec1$mz, spec2$mz)))
    int1 <- numeric(length(all_mz))
    int2 <- numeric(length(all_mz))
    for (i in seq_along(all_mz)) {
      mz_val <- all_mz[i]
      match1 <- which(abs(spec1$mz - mz_val) <= tol)
      if (length(match1) > 0) {
        int1[i] <- max(spec1$intensity[match1])
      }
      match2 <- which(abs(spec2$mz - mz_val) <= tol)
      if (length(match2) > 0) {
        int2[i] <- max(spec2$intensity[match2])
      }
    }
    dot_product <- sum(int1 * int2)
    norm1 <- sqrt(sum(int1^2))
    norm2 <- sqrt(sum(int2^2))
    cos_val <- if (norm1 == 0 || norm2 == 0) 0 else dot_product / (norm1 * norm2)

    shared <- 0L
    for (mz_val in spec1$mz) {
      if (any(abs(spec2$mz - mz_val) <= tol)) shared <- shared + 1L
    }

    list(cos = cos_val, shared = shared)
  }

  find_parent_idx <- function(tp_row, parents) {
    if ("precursor_InChIKey" %in% names(tp_row)) {
      val <- tp_row$precursor_InChIKey
      if (!is.na(val) && nzchar(val)) {
        idx <- which(parents$InChIKey == val)
        if (length(idx) > 0) return(idx)
      }
    }
    if ("precursor_InChI" %in% names(tp_row)) {
      val <- tp_row$precursor_InChI
      if (!is.na(val) && nzchar(val)) {
        idx <- which(parents$InChI == val)
        if (length(idx) > 0) return(idx)
      }
    }
    if ("precursor_SMILES" %in% names(tp_row)) {
      val <- tp_row$precursor_SMILES
      if (!is.na(val) && nzchar(val)) {
        idx <- which(parents$SMILES == val)
        if (length(idx) > 0) return(idx)
      }
    }
    if ("precursor_name" %in% names(tp_row)) {
      val <- tp_row$precursor_name
      if (!is.na(val) && nzchar(val)) {
        idx <- which(parents$name == val)
        if (length(idx) > 0) return(idx)
      }
    }
    integer(0)
  }

  if (!"precursor_name" %in% names(tp)) {
    warning("TransformationProducts table does not have precursor_name column! Not done.")
    return(FALSE)
  }

  parents <- tp[is.na(precursor_name) | precursor_name == "", ]
  tp_rows <- tp[!(is.na(precursor_name) | precursor_name == ""), ]

  if (nrow(parents) == 0 || nrow(tp_rows) == 0) {
    warning("No parent or transformation product rows found! Not done.")
    return(FALSE)
  }

  cosine_vals <- rep(0, nrow(tp_rows))
  shared_vals <- rep(0L, nrow(tp_rows))

  for (i in seq_len(nrow(tp_rows))) {
    tp_row <- tp_rows[i, ]
    parent_idx <- find_parent_idx(tp_row, parents)
    if (length(parent_idx) == 0) {
      cosine_vals[i] <- 0
      shared_vals[i] <- 0L
      next
    }
    parent_row <- parents[parent_idx[1], ]

    tp_specs <- get_specs_for_row(tp_row)
    parent_specs <- get_specs_for_row(parent_row)
    if (length(tp_specs) == 0 || length(parent_specs) == 0) {
      cosine_vals[i] <- 0
      shared_vals[i] <- 0L
      next
    }

    max_cos <- 0
    max_shared <- 0L
    for (tp_spec in tp_specs) {
      for (parent_spec in parent_specs) {
        sim <- calc_similarity(tp_spec, parent_spec, parameters$mzr)
        if (sim$cos > max_cos || (sim$cos == max_cos && sim$shared > max_shared)) {
          max_cos <- sim$cos
          max_shared <- sim$shared
        }
      }
    }
    cosine_vals[i] <- max_cos
    shared_vals[i] <- max_shared
  }

  tp_rows$cosine_similarity <- cosine_vals
  tp_rows$number_shared_fragments <- shared_vals

  if (parameters$filter) {
    keep <- rep(TRUE, nrow(tp_rows))
    if (parameters$minNumberSharedFragments > 0) {
      keep <- keep & (tp_rows$number_shared_fragments >= parameters$minNumberSharedFragments)
    }
    if (parameters$minCosine > 0) {
      keep <- keep & (tp_rows$cosine_similarity >= parameters$minCosine)
    }
    tp_rows <- tp_rows[keep, ]
  }

  tp_rows$number_shared_fragments <- NULL

  tp <- data.table::rbindlist(list(parents, tp_rows), fill = TRUE)

  col_order <- c(
    "name", "formula", "mass", "SMILES", "InChI", "InChIKey", "xLogP",
    "transformation",
    "precursor_name", "precursor_formula", "precursor_mass",
    "precursor_SMILES", "precursor_InChI", "precursor_InChIKey",
    "precursor_xLogP",
    "feature_group", "cosine_similarity"
  )
  missing_cols <- setdiff(col_order, names(tp))
  for (col in missing_cols) tp[[col]] <- NA
  if ("precursor_mass" %in% names(tp)) {
    tp$precursor_mass <- suppressWarnings(as.numeric(tp$precursor_mass))
  }
  data.table::setcolorder(tp, col_order)

  # Cache results
  save_cache(
    cache_manager,
    name = paste0("DB_CalculateTransformationProductsSimilarity_native"),
    hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
    description = "Transformation products MS2 similarity",
    data = as.data.frame(tp)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")

  DBI::dbExecute(conn, "DELETE FROM TransformationProducts")
  DBI::dbWriteTable(conn, "TransformationProducts", tp, append = TRUE)
  message("\u2713 Transformation products similarity written to database.")
  invisible(TRUE)
}
