# MARK: Native
# Native ------

#' @title DB_MassSpecMethod_SuspectScreening_native class
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
DB_MassSpecMethod_SuspectScreening_native <- function(
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
    stop("Argument 'suspects' is required. Provide a data.frame with at least columns 'name' and 'mass' or 'mz'.")
  }

  suspects <- data.table::as.data.table(suspects)

  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "SuspectScreening",
    required = c("FindFeatures"),
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
    stop("Invalid parameters for DB_MassSpecMethod_SuspectScreening_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_SuspectScreening_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "SuspectScreening")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_data_frame(data.table::as.data.table(x$parameters$suspects))
  suspects <- data.table::as.data.table(x$parameters$suspects)
  checkmate::assert_true("name" %in% colnames(suspects))
  checkmate::assert_true(any(c("mass", "mz") %in% colnames(suspects)))
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
run.DB_MassSpecMethod_SuspectScreening_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain DB_MassSpecResults_NonTargetAnalysis.")
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
    "formula", "SMILES", "CAS", "XLogP", "database_id",
    "db_ms2_size", "db_ms2_mz", "db_ms2_intensity", "db_ms2_formula",
    "exp_ms2_size", "exp_ms2_mz", "exp_ms2_intensity"
  )
  keep_cols <- intersect(col_order, colnames(suspects_out))
  data.table::setcolorder(suspects_out, keep_cols)

  # Cache results
  save_cache(
    cache_manager,
    name = paste0("DB_SuspectScreening_native"),
    hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
    description = "Suspects found with DB_SuspectScreening_native method",
    data = as.data.frame(suspects_out)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")

  # Write to database
  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .validate_DB_MassSpecResults_NonTargetAnalysis_Suspects_db_schema(conn)

  # Clear existing data
  DBI::dbExecute(conn, "DELETE FROM Suspects")

  # Write new data
  DBI::dbWriteTable(conn, "Suspects", suspects_out, append = TRUE)

  message("\U2713 Suspects written to database.")
  invisible(TRUE)
}


# MARK: MetFrag
# MetFrag ------

#' @title DB_MassSpecMethod_SuspectScreening_metfrag class
#' @description MetFragCL-based suspect screening using MS1/MS2 feature data and an external candidate database.
#' Requires MetFragCL (JAR or executable) and a working Java runtime (\url{https://www.java.com/en/download/}) when
#' using the JAR; see \url{https://ipb-halle.github.io/MetFrag/projects/metfragcl/} for installation details and
#' database options. For method background and scoring details, see the referenced MetFrag publications.
#' @param metfrag_path Character (length 1). Full path to the MetFragCL executable or JAR file.
#' @param database_type Character (length 1). MetFrag database type (e.g., "LocalCSV", "LocalSDF", "PubChem").
#' @param database_path Character (length 1). Path to the local database file (required for local databases).
#' @param ppm Numeric. Mass tolerance in parts-per-million for database search (MS1 precursor). Default: 5.
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
DB_MassSpecMethod_SuspectScreening_metfrag <- function(
  metfrag_path = NULL,
  database_type = "LocalCSV",
  database_path = NULL,
  ppm = 5,
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
    type = "DB_MassSpec",
    method = "SuspectScreening",
    required = c("FindFeatures", "LoadFeaturesMS1", "LoadFeaturesMS2"),
    algorithm = "metfrag",
    input_class = "DB_MassSpecResults_NonTargetAnalysis",
    output_class = "DB_MassSpecResults_NonTargetAnalysis",
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
    stop("Invalid parameters for DB_MassSpecMethod_SuspectScreening_metfrag.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_SuspectScreening_metfrag <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "SuspectScreening")
  checkmate::assert_choice(x$algorithm, "metfrag")
  checkmate::assert_character(x$parameters$metfrag_path, len = 1)
  checkmate::assert_character(x$parameters$database_type, len = 1)
  checkmate::assert_numeric(x$parameters$ppm, len = 1, lower = 0)
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
run.DB_MassSpecMethod_SuspectScreening_metfrag <- function(x, engine = NULL) {
  if (!is(engine, "DB_MassSpecEngine")) {
    warning("Engine is not a DB_MassSpecEngine object!")
    return(FALSE)
  }

  if (is.null(engine$NonTargetAnalysis)) {
    warning("No DB_MassSpecResults_NonTargetAnalysis object available! Not done.")
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
          if (!parameters$quiet) {
            message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
          }
          conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
          on.exit(DBI::dbDisconnect(conn), add = TRUE)
          DBI::dbExecute(conn, "DELETE FROM Suspects")
          DBI::dbWriteTable(conn, "Suspects", suspects_out, append = TRUE)
          if (!parameters$quiet) {
            message("\U2713 Suspects written to database.")
          }
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
  features <- get_features(nts, analyses = NULL, filtered = parameters$filtered)
  if (nrow(features) == 0) {
    warning("No features available for MetFrag suspect screening! Not done.")
    return(FALSE)
  }

  log_dir <- file.path(getwd(), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir <- file.path(log_dir, paste0("metfrag_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  safe_id <- function(...) {
    gsub("[^A-Za-z0-9_\\-\\.]+", "_", paste(..., collapse = "_"))
  }

  decode_peaklist <- function(mz_enc, int_enc) {
    if (is.na(mz_enc) || is.na(int_enc) || nchar(mz_enc) == 0 || nchar(int_enc) == 0) {
      return(data.table::data.table())
    }
    mz_dec <- rcpp_streamcraft_decode_string(mz_enc)
    int_dec <- rcpp_streamcraft_decode_string(int_enc)
    if (length(mz_dec) == 0 || length(int_dec) == 0 || length(mz_dec) != length(int_dec)) {
      return(data.table::data.table())
    }
    data.table::data.table(mz = mz_dec, intensity = int_dec)
  }

  build_params <- function(ft, ms2_path, results_path, sample_name, precursor_mass) {
    is_pos <- isTRUE(as.integer(ft$polarity) > 0)
    params <- list(
      MetFragDatabaseType = parameters$database_type,
      DatabaseSearchRelativeMassDeviation = parameters$ppm,
      PeakListPath = ms2_path,
      FragmentPeakMatchRelativeMassDeviation = parameters$ppmMS2,
      FragmentPeakMatchAbsoluteMassDeviation = parameters$mzrMS2,
      NeutralPrecursorMass = precursor_mass,
      PrecursorIonMode = ft$polarity,
      IsPositiveIonMode = if (is_pos) "True" else "False",
      MetFragScoreTypes = "FragmenterScore",
      MetFragScoreWeights = "1",
      MetFragPreProcessingCandidateFilter = "UnconnectedCompoundFilter,IsotopeFilter",
      MetFragPostProcessingCandidateFilter = "InChIKeyFilter",
      MetFragCandidateWriter = "CSV",
      SampleName = sample_name,
      ResultsPath = results_path,
      MaximumTreeDepth = 2,
      UseSmiles = "True"
    )
    if (!is.null(parameters$database_path)) {
      params$LocalDatabasePath <- parameters$database_path
    }
    if (!is.null(parameters$n_cores) && parameters$n_cores > 1) {
      params$NumberThreads <- parameters$n_cores
    }
    if (!is.null(parameters$extra_params)) {
      params <- modifyList(params, parameters$extra_params)
    }
    params$MetFragCandidateWriter <- "CSV"
    params
  }

  write_params_file <- function(params, file_path) {
    format_value <- function(name, x) {
      if (length(x) > 1) {
        val <- paste(x, collapse = ",")
      } else if (is.logical(x)) {
        val <- ifelse(x, "True", "False")
      } else {
        val <- as.character(x)
      }
      val <- trimws(val)
      is_path <- grepl("(Path|File)$", name)
      if (is_path && nzchar(val)) {
        val <- normalizePath(val, winslash = "/", mustWork = FALSE)
      }
      val
    }
    lines <- vapply(names(params), function(nm) {
      val <- params[[nm]]
      if (is.null(val) || length(val) == 0 || all(is.na(val))) {
        return(NA_character_)
      }
      paste0(nm, " = ", format_value(nm, val))
    }, character(1))
    lines <- lines[!is.na(lines)]
    lines <- lines[nzchar(lines)]
    writeLines(lines, file_path, sep = "\n")
  }

  parse_metfrag_results <- function(results_path, sample_name) {
    candidates <- list.files(
      results_path,
      pattern = paste0("^", sample_name, ".*\\.csv$"),
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(candidates) == 0) {
      return(data.table::data.table())
    }
    path <- candidates[1]
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  }

  resolve_col <- function(tbl, options) {
    if (nrow(tbl) == 0) return(NULL)
    nms <- tolower(colnames(tbl))
    opts <- tolower(options)
    idx <- match(opts, nms, nomatch = 0)
    idx <- idx[idx > 0]
    if (length(idx) == 0) return(NULL)
    colnames(tbl)[idx[1]]
  }

  parse_expl_peaks <- function(expl_peaks, formulas) {
    if (is.null(expl_peaks) || is.na(expl_peaks) || expl_peaks == "") {
      return(list(size = 0L, mz = NA_character_, intensity = NA_character_, formula = NA_character_))
    }
    pairs <- unlist(strsplit(expl_peaks, ";", fixed = TRUE))
    pairs <- pairs[nzchar(pairs)]
    if (length(pairs) == 0) {
      return(list(size = 0L, mz = NA_character_, intensity = NA_character_, formula = NA_character_))
    }
    mz_vals <- numeric()
    int_vals <- numeric()
    for (p in pairs) {
      parts <- unlist(strsplit(p, "_", fixed = TRUE))
      if (length(parts) >= 2) {
        mz_vals <- c(mz_vals, suppressWarnings(as.numeric(parts[1])))
        int_vals <- c(int_vals, suppressWarnings(as.numeric(parts[2])))
      }
    }
    if (length(mz_vals) == 0 || length(int_vals) == 0 || length(mz_vals) != length(int_vals)) {
      return(list(size = 0L, mz = NA_character_, intensity = NA_character_, formula = NA_character_))
    }
    formula_str <- NA_character_
    if (!is.null(formulas) && !is.na(formulas) && formulas != "") {
      formula_pairs <- unlist(strsplit(formulas, ";", fixed = TRUE))
      formula_pairs <- formula_pairs[nzchar(formula_pairs)]
      if (length(formula_pairs) > 0) {
        form_vals <- vapply(
          formula_pairs,
          function(x) {
            parts <- unlist(strsplit(x, ":", fixed = TRUE))
            if (length(parts) >= 2) parts[2] else NA_character_
          },
          character(1)
        )
        form_vals <- form_vals[!is.na(form_vals)]
        if (length(form_vals) > 0) {
          formula_str <- paste(form_vals, collapse = ";")
        }
      }
    }
    list(
      size = as.integer(length(mz_vals)),
      mz = rcpp_streamcraft_encode_vector(mz_vals),
      intensity = rcpp_streamcraft_encode_vector(int_vals),
      formula = formula_str
    )
  }

  calc_cosine_similarity <- function(db_mz, db_int, exp_mz, exp_int, ppmMS2, mzrMS2) {
    if (length(db_mz) == 0 || length(exp_mz) == 0) return(0)
    db_mzr <- db_mz * ppmMS2 / 1E6
    db_mzr[db_mzr < mzrMS2] <- mzrMS2
    db_mzmin <- db_mz - db_mzr
    db_mzmax <- db_mz + db_mzr
    exp_idx <- vapply(seq_along(db_mz), function(i) {
      idx <- which(exp_mz >= db_mzmin[i] & exp_mz <= db_mzmax[i])
      if (length(idx) == 0) return(NA_integer_)
      if (length(idx) > 1) {
        mz_error <- abs(exp_mz[idx] - db_mz[i])
        idx <- idx[which.min(mz_error)]
      }
      as.integer(idx[1])
    }, integer(1))
    sel <- !is.na(exp_idx)
    if (!any(sel)) return(0)
    intensity_db <- db_int[sel]
    intensity_exp <- exp_int[exp_idx[sel]]
    if (all(intensity_db == 0) || all(intensity_exp == 0)) return(0)
    intensity_db <- intensity_db / max(intensity_db)
    intensity_exp <- intensity_exp / max(intensity_exp)
    dot_pro <- sum(intensity_db * intensity_exp)
    mag_db <- sqrt(sum(intensity_db^2))
    mag_exp <- sqrt(sum(intensity_exp^2))
    round(dot_pro / (mag_db * mag_exp), digits = 4)
  }

  run_one <- function(i) {
    ft <- features[i, ]
    ms1 <- decode_peaklist(ft$ms1_mz, ft$ms1_intensity)
    ms2 <- decode_peaklist(ft$ms2_mz, ft$ms2_intensity)

    precursor_mass <- NA_real_
    if (nrow(ms1) > 0) {
      precursor_mz <- ms1$mz[which.max(ms1$intensity)]
      precursor_mass <- precursor_mz - (ft$polarity * 1.007276)
    } else if (!is.na(ft$mass) && ft$mass > 0) {
      precursor_mass <- ft$mass
    } else if (!is.na(ft$mz)) {
      precursor_mass <- ft$mz - (ft$polarity * 1.007276)
    }

    if (is.na(precursor_mass)) {
      return(NULL)
    }

    sid <- safe_id(ft$analysis, ft$feature)
    ms2_path <- file.path(run_dir, paste0("ms2_", sid, ".txt"))
    ms1_path <- file.path(run_dir, paste0("ms1_", sid, ".txt"))
    params_path <- file.path(run_dir, paste0("metfrag_", sid, ".params"))
    log_path <- file.path(run_dir, paste0("metfrag_", sid, ".log"))
    debug_path <- file.path(run_dir, paste0("metfrag_", sid, ".debug.txt"))
    sample_name <- paste0("metfrag_", sid)

    if (nrow(ms2) > 0) {
      utils::write.table(
        ms2[, c("mz", "intensity")],
        file = ms2_path,
        row.names = FALSE,
        col.names = FALSE,
        sep = " ",
        quote = FALSE
      )
    } else {
      file.create(ms2_path)
    }

    if (nrow(ms1) > 0) {
      utils::write.table(
        ms1[, c("mz", "intensity")],
        file = ms1_path,
        row.names = FALSE,
        col.names = FALSE,
        sep = " ",
        quote = FALSE
      )
    } else {
      file.create(ms1_path)
    }

    params <- build_params(ft, ms2_path, run_dir, sample_name, precursor_mass)
    write_params_file(params, params_path)

    run_cmd <- parameters$metfrag_path
    run_args <- NULL
    if (!is.null(parameters$metfrag_args)) {
      run_args <- gsub("\\{params\\}", params_path, parameters$metfrag_args)
    } else if (grepl("\\.jar$", run_cmd, ignore.case = TRUE)) {
      run_args <- c("-jar", run_cmd, params_path)
      run_cmd <- parameters$java_path
    } else {
      run_args <- params_path
    }

    system2(run_cmd, args = run_args, stdout = log_path, stderr = log_path)

    if (isTRUE(parameters$debug)) {
      debug_lines <- c(
        paste0("analysis=", ft$analysis),
        paste0("feature=", ft$feature),
        paste0("polarity=", ft$polarity),
        paste0("precursor_mass=", precursor_mass),
        paste0("ms1_points=", nrow(ms1)),
        paste0("ms2_points=", nrow(ms2)),
        paste0("ms1_file=", ms1_path),
        paste0("ms2_file=", ms2_path),
        paste0("params_file=", params_path),
        paste0("results_path=", run_dir),
        paste0("sample_name=", sample_name),
        paste0("log_file=", log_path)
      )
      writeLines(debug_lines, debug_path, sep = "\r\n")
    }

    res <- parse_metfrag_results(run_dir, sample_name)
    if (nrow(res) == 0) {
      return(NULL)
    }

    name_col <- resolve_col(res, c("name", "compoundname", "compound_name"))
    formula_col <- resolve_col(res, c("formula", "molecularformula"))
    smiles_col <- resolve_col(res, c("smiles", "smile", "canonicalsmiles"))
    cas_col <- resolve_col(res, c("cas", "casrn", "casnumber"))
    id_col <- resolve_col(res, c("identifier", "database_id", "databaseid", "inchikey", "pubchemcid"))
    score_col <- resolve_col(res, c("score", "metfragscore", "totalscore", "finalscore"))
    xlogp_col <- resolve_col(res, c("xlogp", "xlogp3", "logp", "xlogp-3"))
    mass_col <- resolve_col(res, c("neutralmass", "monoisotopicmass", "exactmass"))
    expl_col <- resolve_col(res, c("explpeaks", "explainedpeaks"))
    expl_formula_col <- resolve_col(res, c("formulasofexplpeaks", "explpeakformulas"))

    if (!is.null(score_col)) {
      res <- res[order(-as.numeric(res[[score_col]])), , drop = FALSE]
    }
    res <- res[seq_len(min(nrow(res), parameters$top_n)), , drop = FALSE]

    rows <- vector("list", nrow(res))
    for (row_idx in seq_len(nrow(res))) {
      row <- res[row_idx, , drop = FALSE]
      name_val <- if (!is.null(name_col)) row[[name_col]][1] else NA_character_
      if (is.na(name_val) || name_val == "") {
        name_val <- if (!is.null(id_col)) row[[id_col]][1] else ft$feature
      }

      formula_val <- if (!is.null(formula_col)) row[[formula_col]][1] else NA_character_
      smiles_val <- if (!is.null(smiles_col)) row[[smiles_col]][1] else NA_character_
      cas_val <- if (!is.null(cas_col)) row[[cas_col]][1] else NA_character_
      database_id_val <- if (!is.null(id_col)) row[[id_col]][1] else NA_character_
      score_val <- if (!is.null(score_col)) suppressWarnings(as.numeric(row[[score_col]][1])) else 0
      xlogp_val <- if (!is.null(xlogp_col)) suppressWarnings(as.numeric(row[[xlogp_col]][1])) else NA_real_
      db_mass_val <- if (!is.null(mass_col)) suppressWarnings(as.numeric(row[[mass_col]][1])) else NA_real_

      expl_peaks_val <- if (!is.null(expl_col)) row[[expl_col]][1] else NA_character_
      expl_formula_val <- if (!is.null(expl_formula_col)) row[[expl_formula_col]][1] else NA_character_
      expl_parsed <- parse_expl_peaks(expl_peaks_val, expl_formula_val)

      exp_mass_val <- if (!is.na(precursor_mass)) precursor_mass else ft$mass
      error_mass_val <- if (!is.na(db_mass_val) && !is.na(exp_mass_val)) {
        round(((exp_mass_val - db_mass_val) / exp_mass_val) * 1E6, digits = 1)
      } else {
        NA_real_
      }

      id_level <- if (nrow(ms2) > 0) "2" else "4"
      cosine_similarity <- 0
      if (expl_parsed$size > 0 && nrow(ms2) > 0) {
        db_mz <- rcpp_streamcraft_decode_string(expl_parsed$mz)
        db_int <- rcpp_streamcraft_decode_string(expl_parsed$intensity)
        if (length(db_mz) > 0 && length(db_int) == length(db_mz)) {
          cosine_similarity <- calc_cosine_similarity(
            db_mz,
            db_int,
            ms2$mz,
            ms2$intensity,
            parameters$ppmMS2,
            parameters$mzrMS2
          )
        }
      }

      rows[[row_idx]] <- data.table::data.table(
        analysis = ft$analysis,
        feature = ft$feature,
        candidate_rank = as.integer(row_idx),
        name = name_val,
        polarity = ft$polarity,
        db_mass = db_mass_val,
        exp_mass = exp_mass_val,
        error_mass = error_mass_val,
        db_rt = NA_real_,
        exp_rt = ft$rt,
        error_rt = NA_real_,
        intensity = ft$intensity,
        area = ft$area,
        id_level = id_level,
        score = score_val,
        XLogP = xlogp_val,
        shared_fragments = expl_parsed$size,
        cosine_similarity = cosine_similarity,
        formula = formula_val,
        SMILES = smiles_val,
        CAS = cas_val,
        database_id = database_id_val,
        db_ms2_size = expl_parsed$size,
        db_ms2_mz = expl_parsed$mz,
        db_ms2_intensity = expl_parsed$intensity,
        db_ms2_formula = expl_parsed$formula,
        exp_ms2_size = ft$ms2_size,
        exp_ms2_mz = ft$ms2_mz,
        exp_ms2_intensity = ft$ms2_intensity
      )
    }
    data.table::rbindlist(rows, fill = TRUE)
  }

  idx <- seq_len(nrow(features))
  results <- list()
  pb <- NULL
  if (parameters$show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(idx), style = 3)
    on.exit(close(pb), add = TRUE)
  }

  if (parameters$n_cores > 1) {
    cl <- parallel::makeCluster(parameters$n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, {
      library(data.table)
      library(StreamFind)
    })
    chunk_size <- parameters$n_cores
    chunks <- split(idx, ceiling(seq_along(idx) / chunk_size))
    for (chunk in chunks) {
      res_chunk <- parallel::parLapply(cl, chunk, run_one)
      results <- c(results, res_chunk)
      if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, min(length(results), length(idx)))
      }
    }
  } else {
    for (i in idx) {
      results[[length(results) + 1]] <- run_one(i)
      if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, length(results))
      }
    }
  }

    suspects_out <- data.table::rbindlist(results, fill = TRUE)
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
    "formula", "SMILES", "CAS", "XLogP", "database_id",
    "db_ms2_size", "db_ms2_mz", "db_ms2_intensity", "db_ms2_formula",
    "exp_ms2_size", "exp_ms2_mz", "exp_ms2_intensity"
  )
  keep_cols <- intersect(col_order, colnames(suspects_out))
  data.table::setcolorder(suspects_out, keep_cols)

  # Cache results
  save_cache(
    cache_manager,
    name = paste0("DB_SuspectScreening_metfrag"),
    hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
    description = "Suspects found with DB_SuspectScreening_metfrag method",
    data = as.data.frame(suspects_out)
  )
  if (!parameters$quiet) {
    message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  }

  # Write to database
  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .validate_DB_MassSpecResults_NonTargetAnalysis_Suspects_db_schema(conn)
  DBI::dbExecute(conn, "DELETE FROM Suspects")
  DBI::dbWriteTable(conn, "Suspects", suspects_out, append = TRUE)
  if (!parameters$quiet) {
    message("\U2713 Suspects written to database.")
  }
  invisible(TRUE)
}
