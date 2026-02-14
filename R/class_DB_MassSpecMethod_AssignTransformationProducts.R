# MARK: AssignTransformationProducts
# Native ------

#' @title DB_MassSpecMethod_AssignTransformationProducts_native class
#' @description Native StreamFind method to link transformation products and parents using an existing Suspects
#' table and assign feature groups based on matched suspect features. Writes the TransformationProducts table.
#' @param transformation_products A data.frame with transformation products and parent entries. Uses `SMILES` to
#' link parents and products via `precursor_SMILES`.
#' @param chromatographic_phase Character (length 1). Chromatographic phase for RT plausibility checks.
#' One of: "reverse_phase", "hilic".
#' @param mzrMS2 Numeric. Absolute m/z tolerance for MS2 fragment matching when computing cosine similarity.
#' @export
#'
DB_MassSpecMethod_AssignTransformationProducts_native <- function(
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
      precursor_xLogP = numeric()
    )
  } else {
    transformation_products <- data.table::as.data.table(transformation_products)
  }

  chromatographic_phase <- match.arg(chromatographic_phase)
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
      transformation_products = transformation_products,
      chromatographic_phase = as.character(chromatographic_phase),
      mzrMS2 = as.numeric(mzrMS2)
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
run.DB_MassSpecMethod_AssignTransformationProducts_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  parameters <- x$parameters
  analyses_info <- info(engine$Analyses)
  ensure_tp_columns <- function(tp) {
    required_cols <- c(
      "name", "formula", "mass", "SMILES", "InChI", "InChIKey", "xLogP",
      "transformation",
      "precursor_name", "precursor_formula", "precursor_mass",
      "precursor_SMILES", "precursor_InChI", "precursor_InChIKey",
      "precursor_xLogP"
    )
    missing_cols <- setdiff(required_cols, names(tp))
    if (length(missing_cols) > 0) {
      stop(
        "transformation_products is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    tp$mass <- suppressWarnings(as.numeric(tp$mass))
    tp$xLogP <- suppressWarnings(as.numeric(tp$xLogP))
    tp$precursor_mass <- suppressWarnings(as.numeric(tp$precursor_mass))
    tp$precursor_xLogP <- suppressWarnings(as.numeric(tp$precursor_xLogP))
    tp
  }
  tp <- data.table::as.data.table(parameters$transformation_products)
  tp <- ensure_tp_columns(tp)

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

  if (nrow(tp) > 0) {
    tp$feature_group <- ""
    tp$precursor_feature_group <- ""
    tp$main_precursor_feature_group <- ""
    tp$cosine_similarity <- NA_real_
    tp$main_precursor_cosine_similarity <- NA_real_
    tp$rt_plausibility <- NA_real_
    tp$main_precursor_rt_plausibility <- NA_real_
    nts <- engine$NonTargetAnalysis
    suspects <- data.table::as.data.table(get_suspects(nts))

    if (nrow(suspects) > 0) {
      suspects <- suspects[order(analysis, feature, candidate_rank)]
      fts <- data.table::as.data.table(get_features(nts))
      fts <- fts[, c("analysis", "feature", "feature_group"), with = FALSE]
      if (nrow(fts) == 0) {
        warning("No features found in NonTargetAnalysis for feature_group assignment.")
        return(FALSE)
      }
      suspects <- merge(suspects, fts, by = c("analysis", "feature"), all.x = TRUE)

      fg_map <- suspects[!is.na(SMILES) & SMILES != "" & !is.na(feature_group) & feature_group != "",
        .(feature_group = paste(unique(feature_group), collapse = ";")),
        by = .(SMILES)
      ]

      tp$feature_group <- fg_map$feature_group[match(tp$SMILES, fg_map$SMILES)]

      # Assign main_precursor_feature_group
      tp$main_precursor_feature_group <- fg_map$feature_group[match(tp$main_precursor_SMILES, fg_map$SMILES)]
      tp$main_precursor_feature_group[is.na(tp$main_precursor_feature_group)] <- ""

      tp$precursor_feature_group <- apply(tp, 1, function(z) {
        if (z[["feature_group"]] != "" && !is.na(z[["feature_group"]])) {
          pg <- suspects$feature_group[suspects$SMILES %in% z[["precursor_SMILES"]]]
          if (length(pg) == 0) return("")
          pg <- pg[!is.na(pg) & pg != ""]
          if (length(pg) == 0) return("")
          pg <- unique(pg)
          pg <- paste(pg, collapse = ";")
          pg
        } else {
          ""
        }
      })

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

      calc_similarity <- function(spec1, spec2, tol) {
        if (nrow(spec1) == 0 || nrow(spec2) == 0) return(NA_real_)
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
        if (norm1 == 0 || norm2 == 0) return(0)
        dot_product / (norm1 * norm2)
      }

      score_rt_plausibility <- function(prod_logp, prec_logp, prod_rt, prec_rt, phase) {
        if (is.na(prod_logp) || is.na(prec_logp) || is.na(prod_rt) || is.na(prec_rt)) return(NA_real_)
        logp_diff <- prod_logp - prec_logp
        rt_diff <- prod_rt - prec_rt
        if (phase == "reverse_phase") {
          return(sign(logp_diff) * sign(rt_diff))
        }
        if (phase == "hilic") {
          return(-sign(logp_diff) * sign(rt_diff))
        }
        NA_real_
      }

      eligible <- tp$feature_group != "" & tp$precursor_feature_group != ""

      for (i in which(eligible)) {
        prod_fg <- tp$feature_group[i]
        prod_fg <- unlist(strsplit(prod_fg, ";", fixed = TRUE))
        prec_fg <- tp$precursor_feature_group[i]
        prec_fg <- unlist(strsplit(prec_fg, ";", fixed = TRUE))

        prod_sus <- suspects[feature_group %in% prod_fg & SMILES == tp$SMILES[i]]
        prec_sus <- suspects[feature_group %in% prec_fg & SMILES == tp$precursor_SMILES[i]]

        prod_exp_rt <- prod_sus$exp_rt
        prec_exp_rt <- prec_sus$exp_rt
        prod_logp <- tp$xLogP[i]
        prec_logp <- tp$precursor_xLogP[i]
        if (length(prod_exp_rt) > 0 && length(prec_exp_rt) > 0 &&
              !all(is.na(prod_exp_rt)) && !all(is.na(prec_exp_rt)) &&
              !is.na(prod_logp) && !is.na(prec_logp)) {
          prod_rt_val <- median(prod_exp_rt, na.rm = TRUE)
          prec_rt_val <- median(prec_exp_rt, na.rm = TRUE)
          if (!is.na(prod_rt_val) && !is.na(prec_rt_val)) {
            tp$rt_plausibility[i] <- score_rt_plausibility(
              prod_logp, prec_logp, prod_rt_val, prec_rt_val, parameters$chromatographic_phase
            )
          }
        }

        max_cos <- NA_real_
        has_comparison <- FALSE
        prod_sus_valid <- prod_sus[exp_ms2_size > 0]
        prec_sus_valid <- prec_sus[exp_ms2_size > 0]
        if (nrow(prod_sus_valid) > 0 && nrow(prec_sus_valid) > 0) {
          for (ps_idx in seq_len(nrow(prod_sus_valid))) {
            ps_mz <- prod_sus_valid$exp_ms2_mz[ps_idx]
            ps_int <- prod_sus_valid$exp_ms2_int[ps_idx]
            ps_spec <- decode_ms2(ps_mz, ps_int)
            if (is.null(ps_spec) || nrow(ps_spec) == 0) next
            for (qs_idx in seq_len(nrow(prec_sus_valid))) {
              qs_mz <- prec_sus_valid$exp_ms2_mz[qs_idx]
              qs_int <- prec_sus_valid$exp_ms2_int[qs_idx]
              qs_spec <- decode_ms2(qs_mz, qs_int)
              if (is.null(qs_spec) || nrow(qs_spec) == 0) next
              cos_val <- calc_similarity(ps_spec, qs_spec, parameters$mzrMS2)
              if (!is.na(cos_val)) {
                if (!has_comparison) {
                  max_cos <- cos_val
                  has_comparison <- TRUE
                } else if (cos_val > max_cos) {
                  max_cos <- cos_val
                }
              }
            }
          }
        }
        tp$cosine_similarity[i] <- max_cos

        # Calculate main_precursor metrics
        if (!is.na(tp$main_precursor_SMILES[i]) && tp$main_precursor_SMILES[i] != "") {
          main_prec_fg <- tp$main_precursor_feature_group[i]
          if (!is.na(main_prec_fg) && main_prec_fg != "") {
            main_prec_fg <- unlist(strsplit(main_prec_fg, ";", fixed = TRUE))
            main_prec_sus <- suspects[feature_group %in% main_prec_fg & SMILES == tp$main_precursor_SMILES[i]]

            # Calculate main_precursor_rt_plausibility
            main_prec_exp_rt <- main_prec_sus$exp_rt
            main_prec_logp <- tp$main_precursor_xLogP[i]
            if (length(prod_exp_rt) > 0 && length(main_prec_exp_rt) > 0 &&
                  !all(is.na(prod_exp_rt)) && !all(is.na(main_prec_exp_rt)) &&
                  !is.na(prod_logp) && !is.na(main_prec_logp)) {
              prod_rt_val <- median(prod_exp_rt, na.rm = TRUE)
              main_prec_rt_val <- median(main_prec_exp_rt, na.rm = TRUE)
              if (!is.na(prod_rt_val) && !is.na(main_prec_rt_val)) {
                tp$main_precursor_rt_plausibility[i] <- score_rt_plausibility(
                  prod_logp, main_prec_logp, prod_rt_val, main_prec_rt_val, parameters$chromatographic_phase
                )
              }
            }

            # Calculate main_precursor_cosine_similarity
            max_cos_main <- NA_real_
            has_comparison_main <- FALSE
            main_prec_sus_valid <- main_prec_sus[exp_ms2_size > 0]
            if (nrow(prod_sus_valid) > 0 && nrow(main_prec_sus_valid) > 0) {
              for (ps_idx in seq_len(nrow(prod_sus_valid))) {
                ps_mz <- prod_sus_valid$exp_ms2_mz[ps_idx]
                ps_int <- prod_sus_valid$exp_ms2_int[ps_idx]
                ps_spec <- decode_ms2(ps_mz, ps_int)
                if (is.null(ps_spec) || nrow(ps_spec) == 0) next
                for (ms_idx in seq_len(nrow(main_prec_sus_valid))) {
                  ms_mz <- main_prec_sus_valid$exp_ms2_mz[ms_idx]
                  ms_int <- main_prec_sus_valid$exp_ms2_int[ms_idx]
                  ms_spec <- decode_ms2(ms_mz, ms_int)
                  if (is.null(ms_spec) || nrow(ms_spec) == 0) next
                  cos_val <- calc_similarity(ps_spec, ms_spec, parameters$mzrMS2)
                  if (!is.na(cos_val)) {
                    if (!has_comparison_main) {
                      max_cos_main <- cos_val
                      has_comparison_main <- TRUE
                    } else if (cos_val > max_cos_main) {
                      max_cos_main <- cos_val
                    }
                  }
                }
              }
            }
            tp$main_precursor_cosine_similarity[i] <- max_cos_main
          }
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
    "main_precursor_name", "main_precursor_formula", "main_precursor_mass",
    "main_precursor_SMILES", "main_precursor_InChI", "main_precursor_InChIKey",
    "main_precursor_xLogP",
    "feature_group", "precursor_feature_group", "main_precursor_feature_group",
    "cosine_similarity", "main_precursor_cosine_similarity",
    "rt_plausibility", "main_precursor_rt_plausibility"
  )

  tp <- tp[, ..col_order]

  save_cache(
    cache_manager,
    name = paste0("DB_AssignTransformationProducts_native"),
    hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
    description = "Transformation products assignment results",
    data = as.data.frame(tp)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")

  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .validate_DB_MassSpecResults_NonTargetAnalysis_TransformationProducts_db_schema(conn)
  DBI::dbExecute(conn, "DELETE FROM TransformationProducts")
  DBI::dbWriteTable(conn, "TransformationProducts", tp, append = TRUE)
  message("\u2713 Transformation products written to database.")
  invisible(TRUE)
}
