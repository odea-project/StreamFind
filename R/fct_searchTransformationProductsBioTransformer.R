# MARK: searchTransformationProductsBioTransformer

#' @title Search transformation products with BioTransformer web API
#' @description Submits parent compounds to the BioTransformer web API, polls for completion,
#' and returns a table containing the parents and predicted transformation products.
#' See \url{https://biotransformer.ca/} for details on BioTransformer.
#' @param parents data.frame or data.table with required columns:
#' `name`, `formula`, `mass`, `SMILES`, `InChI`, `InChIKey`, `xLogP`.
#' Optional columns used when present: `rt`, `polarity`, `fragments`, `LogP`.
#' @param biotransformer_option Character (length 1). BioTransformer module to use.
#' One of: "CYP450", "EC-BASED", "PHASEII", "HGUT", "ENVMICRO", "ALLHUMAN",
#' "SUPERBIO", "MULTIBIO", "ABIOTICBIO".
#' @param number_of_steps Integer (>=1). Number of reaction iterations.
#' @param throttle_sec Numeric. Delay (seconds) between POST requests (server limit is 2/min).
#' @param poll_delay Numeric. Delay (seconds) between status polls.
#' @param max_poll Integer. Maximum number of polling attempts per query.
#' @param debug Logical. If TRUE, saves the raw BioTransformer JSON response in the temp
#' directory and prints the path.
#' @return A data.table with columns:
#' `name`, `formula`, `mass`, `rt`, `SMILES`, `InChI`, `InChIKey`, `polarity`, `fragments`,
#' `xLogP`, `transformation`, `precursor_name`, `precursor_formula`, `precursor_mass`,
#' `precursor_SMILES`, `precursor_InChI`, `precursor_InChIKey`, `precursor_xLogP`.
#' The first row(s) correspond to the parent compounds (transformation = "parent").
#' @export
searchTransformationProductsBioTransformer <- function(
  parents,
  biotransformer_option = c(
    "CYP450",
    "EC-BASED",
    "PHASEII",
    "HGUT",
    "ENVMICRO",
    "ALLHUMAN",
    "SUPERBIO",
    "MULTIBIO",
    "ABIOTICBIO"
  ),
  number_of_steps = 1,
  throttle_sec = 31,
  poll_delay = 5,
  max_poll = 60,
  debug = FALSE
) {
  biotransformer_option <- match.arg(biotransformer_option)
  number_of_steps <- as.integer(number_of_steps)
  if (is.na(number_of_steps) || number_of_steps < 1) {
    stop("number_of_steps must be >= 1.")
  }
  parents <- data.table::as.data.table(parents)
  if (!requireNamespace("checkmate", quietly = TRUE)) {
    stop("checkmate is required for input validation.")
  }
  required_parent_cols <- c("name", "formula", "mass", "SMILES", "InChI", "InChIKey", "xLogP")
  checkmate::assert_data_frame(parents)
  checkmate::assert_names(names(parents), must.include = required_parent_cols)
  checkmate::assert_count(number_of_steps, positive = TRUE)
  checkmate::assert_number(throttle_sec, lower = 0)
  checkmate::assert_number(poll_delay, lower = 0)
  checkmate::assert_count(max_poll, positive = TRUE)
  checkmate::assert_flag(debug)

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required for BioTransformer API calls.")
  }

  curl_bin <- Sys.which("curl")
  if (!nzchar(curl_bin)) {
    stop("curl executable not found in PATH; required for BioTransformer API calls.")
  }

  tps_cols <- c(
    "name", "formula", "mass", "SMILES", "InChI", "InChIKey", "xLogP",
    "transformation", "precursor_name", "precursor_formula", "precursor_mass",
    "precursor_SMILES", "precursor_InChI", "precursor_InChIKey", "precursor_xLogP"
  )
  extra_parent_cols <- setdiff(names(parents), tps_cols)
  output_cols <- c(tps_cols, extra_parent_cols)

  make_empty_tps <- function() {
    out <- data.table::data.table(matrix(nrow = 0, ncol = length(tps_cols)))
    data.table::setnames(out, tps_cols)
    out
  }

  safe_scalar <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (is.list(x)) x <- x[[1]]
    if (length(x) == 0) return(NA_character_)
    as.character(x[1])
  }

  calc_formula_from_mol <- function(mol) {
    fobj <- rcdk::get.mol2formula(mol, charge = 0)
    fobj@string
  }

  calc_props_from_smiles <- function(smiles) {
    if (length(smiles) == 0) {
      return(list(
        formula = NA_character_, mass = NA_real_, inchi = NA_character_,
        inchikey = NA_character_, logp = NA_real_, smiles = NA_character_
      ))
    }
    smiles <- as.character(smiles[1])
    if (is.na(smiles) || !nzchar(smiles)) {
      return(list(
        formula = NA_character_, mass = NA_real_, inchi = NA_character_,
        inchikey = NA_character_, logp = NA_real_, smiles = NA_character_
      ))
    }
    if (!requireNamespace("rcdk", quietly = TRUE)) {
      return(list(
        formula = NA_character_, mass = NA_real_, inchi = NA_character_,
        inchikey = NA_character_, logp = NA_real_, smiles = smiles
      ))
    }
    mol <- tryCatch(rcdk::parse.smiles(smiles)[[1]], error = function(e) NULL)
    if (is.null(mol)) {
      return(list(
        formula = NA_character_, mass = NA_real_, inchi = NA_character_,
        inchikey = NA_character_, logp = NA_real_, smiles = smiles
      ))
    }
    canonical_smiles <- tryCatch(
      rcdk::get.smiles(mol, smilesFlavor = "Canonical"),
      error = function(e) tryCatch(rcdk::get.smiles(mol), error = function(e2) NA_character_)
    )
    mass <- tryCatch(as.numeric(rcdk::get.exact.mass(mol)), error = function(e) NA_real_)
    logp <- tryCatch(as.numeric(rcdk::get.xlogp(mol)), error = function(e) NA_real_)
    inchi <- NA_character_
    inchikey <- NA_character_
    if (requireNamespace("rJava", quietly = TRUE)) {
      inchi <- tryCatch(rJava::.jcall("org/guha/rcdk/util/Misc", "S", "getInChi", mol, check = FALSE), error = function(e) NA_character_)
      inchikey <- tryCatch(rJava::.jcall("org/guha/rcdk/util/Misc", "S", "getInChiKey", mol, check = FALSE), error = function(e) NA_character_)
    }
    if (requireNamespace("rcdk", quietly = TRUE)) {
      conv_fn <- tryCatch(getFromNamespace("convert.implicit.to.explicit", "rcdk"), error = function(e) NULL)
      if (is.function(conv_fn)) {
        tryCatch(conv_fn(mol), error = function(e) NULL)
      }
    }
    formula <- tryCatch(calc_formula_from_mol(mol), error = function(e) NA_character_)
    list(formula = formula, mass = mass, inchi = inchi, inchikey = inchikey, logp = logp, smiles = canonical_smiles)
  }

  harmonize_compound_columns <- function(dt, prefix = "") {
    smiles_col <- paste0(prefix, "SMILES")
    inchi_col <- paste0(prefix, "InChI")
    inchikey_col <- paste0(prefix, "InChIKey")
    formula_col <- paste0(prefix, "formula")
    mass_col <- paste0(prefix, "mass")
    logp_col <- if (prefix == "") "xLogP" else "precursor_xLogP"
    needed <- c(smiles_col, inchi_col, inchikey_col, formula_col, mass_col)
    if (!all(needed %in% names(dt))) return(dt)
    for (i in seq_len(nrow(dt))) {
      props <- calc_props_from_smiles(dt[[smiles_col]][i])
      if (!is.na(props$smiles) && nzchar(props$smiles)) dt[[smiles_col]][i] <- props$smiles
      if (!is.na(props$inchi) && nzchar(props$inchi)) dt[[inchi_col]][i] <- props$inchi
      if (!is.na(props$inchikey) && nzchar(props$inchikey)) dt[[inchikey_col]][i] <- props$inchikey
      if (!is.na(props$formula) && nzchar(props$formula)) dt[[formula_col]][i] <- props$formula
      if (!is.na(props$mass)) dt[[mass_col]][i] <- as.numeric(props$mass)
      if (logp_col %in% names(dt) && !is.na(props$logp)) dt[[logp_col]][i] <- as.numeric(props$logp)
    }
    dt
  }

  harmonize_names_by_smiles <- function(dt, name_col, smiles_col, inchikey_col = NULL, parent_map = NULL) {
    if (!all(c(name_col, smiles_col) %in% names(dt))) return(dt)
    if (!is.null(parent_map) && nrow(parent_map) > 0) {
      if (!is.null(inchikey_col) && inchikey_col %in% names(dt) && "InChIKey" %in% names(parent_map)) {
        map_ik <- setNames(parent_map$name, parent_map$InChIKey)
        hit_ik <- !is.na(dt[[inchikey_col]]) & dt[[inchikey_col]] != "" & dt[[inchikey_col]] %in% names(map_ik)
        dt[[name_col]][hit_ik] <- unname(map_ik[dt[[inchikey_col]][hit_ik]])
      }
      map_smiles <- setNames(parent_map$name, parent_map$SMILES)
      hit_smiles <- !is.na(dt[[smiles_col]]) & dt[[smiles_col]] %in% names(map_smiles)
      dt[[name_col]][hit_smiles] <- unname(map_smiles[dt[[smiles_col]][hit_smiles]])
    }
    uniq_s <- unique(dt[[smiles_col]][!is.na(dt[[smiles_col]]) & dt[[smiles_col]] != ""])
    for (s in uniq_s) {
      idx <- which(dt[[smiles_col]] == s)
      vals <- dt[[name_col]][idx]
      vals <- vals[!is.na(vals) & vals != ""]
      if (length(vals) == 0) next
      preferred <- names(sort(table(vals), decreasing = TRUE))[1]
      dt[[name_col]][idx] <- preferred
    }
    dt
  }

  harmonize_smiles_by_inchikey <- function(dt, smiles_col, inchikey_col) {
    if (!all(c(smiles_col, inchikey_col) %in% names(dt))) return(dt)
    idx <- !is.na(dt[[smiles_col]]) & dt[[smiles_col]] != "" &
      !is.na(dt[[inchikey_col]]) & dt[[inchikey_col]] != ""
    if (!any(idx)) return(dt)
    map_dt <- dt[idx, .(smiles_val = get(smiles_col)[1]), by = .(ik = get(inchikey_col))]
    smi_map <- setNames(map_dt$smiles_val, map_dt$ik)
    hit <- !is.na(dt[[inchikey_col]]) & dt[[inchikey_col]] %in% names(smi_map)
    dt[[smiles_col]][hit] <- unname(smi_map[dt[[inchikey_col]][hit]])
    dt
  }

  harmonize_precursor_from_primary <- function(dt) {
    need_primary <- c("InChIKey", "SMILES", "InChI", "formula", "mass", "xLogP")
    need_prec <- c("precursor_InChIKey", "precursor_SMILES", "precursor_InChI", "precursor_formula", "precursor_mass", "precursor_xLogP")
    if (!all(c(need_primary, need_prec) %in% names(dt))) return(dt)
    ref <- dt[!is.na(InChIKey) & InChIKey != "" & !is.na(SMILES) & SMILES != "",
      .(InChIKey, SMILES, InChI, formula, mass, xLogP)]
    if (nrow(ref) == 0) return(dt)
    ref <- ref[!duplicated(InChIKey)]
    idx <- match(dt$precursor_InChIKey, ref$InChIKey)
    hit <- !is.na(idx)
    if (any(hit)) {
      dt$precursor_SMILES[hit] <- ref$SMILES[idx[hit]]
      dt$precursor_InChI[hit] <- ref$InChI[idx[hit]]
      dt$precursor_formula[hit] <- ref$formula[idx[hit]]
      dt$precursor_mass[hit] <- ref$mass[idx[hit]]
      dt$precursor_xLogP[hit] <- ref$xLogP[idx[hit]]
    }
    dt
  }

  post_json <- function(url, payload) {
    body_path <- tempfile(fileext = ".json")
    resp_path <- tempfile(fileext = ".json")
    err_path <- tempfile(fileext = ".log")
    json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
    writeLines(json, body_path, useBytes = TRUE)
    status <- suppressWarnings(system2(
      curl_bin,
      args = c(
        "-sS",
        "-H", "Content-Type:application/json",
        "-H", "Accept:application/json",
        "-X", "POST",
        "--data-binary", paste0("@", body_path),
        url
      ),
      stdout = resp_path,
      stderr = err_path
    ))
    resp_txt <- paste(readLines(resp_path, warn = FALSE), collapse = "\n")
    parsed <- tryCatch(jsonlite::fromJSON(resp_txt, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(parsed)) {
      return(parsed)
    }
    id_match <- regmatches(resp_txt, regexpr("/queries/\\d+", resp_txt))
    if (length(id_match) > 0 && nzchar(id_match)) {
      id_val <- gsub("/queries/", "", id_match)
      return(list(id = suppressWarnings(as.integer(id_val))))
    }
    if (!identical(status, 0)) {
      err_txt <- paste(readLines(err_path, warn = FALSE), collapse = "\n")
      stop("BioTransformer POST failed: ", err_txt, if (nzchar(resp_txt)) paste0("\nResponse:\n", resp_txt) else "")
    }
    stop("BioTransformer POST failed: Unexpected response:\n", resp_txt)
  }

  get_json <- function(url, max_retries = 5, retry_delay = 10) {
    for (attempt in seq_len(max_retries)) {
      resp_path <- tempfile(fileext = ".json")
      err_path <- tempfile(fileext = ".log")
      status <- suppressWarnings(system2(
        curl_bin,
        args = c("-sS", "-H", "Accept:application/json", url),
        stdout = resp_path,
        stderr = err_path
      ))
      resp_txt <- paste(readLines(resp_path, warn = FALSE), collapse = "\n")
      parsed <- tryCatch(jsonlite::fromJSON(resp_txt, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed)) {
        if (debug) {
          attr(parsed, "raw_json") <- resp_txt
        }
        return(parsed)
      }
      if (grepl("\"status\"\\s*:\\s*500", resp_txt)) {
        Sys.sleep(retry_delay)
        next
      }
      err_txt <- paste(readLines(err_path, warn = FALSE), collapse = "\n")
      stop("BioTransformer GET failed: ", err_txt, if (nzchar(resp_txt)) paste0("\nResponse:\n", resp_txt) else "")
    }
    stop("BioTransformer GET failed: server returned 500 after retries.")
  }

  base_url <- "http://biotransformer.ca/queries.json"
  tps_out <- make_empty_tps()
  parent_name_map <- data.table::data.table()

  parents <- harmonize_compound_columns(parents, "")

  parents_rows <- parents[, .(
    name = get("name"),
    formula = get("formula"),
    mass = get("mass"),
    rt = if ("rt" %in% names(parents)) get("rt") else NA_real_,
    SMILES = get("SMILES"),
    InChI = get("InChI"),
    InChIKey = get("InChIKey"),
    polarity = if ("polarity" %in% names(parents)) get("polarity") else NA_character_,
    fragments = if ("fragments" %in% names(parents)) get("fragments") else NA_character_,
    xLogP = if ("LogP" %in% names(parents)) get("LogP") else get("xLogP"),
    transformation = "parent",
    precursor_name = NA_character_,
    precursor_formula = NA_character_,
    precursor_mass = NA_real_,
    precursor_SMILES = NA_character_,
    precursor_InChI = NA_character_,
    precursor_InChIKey = NA_character_,
    precursor_xLogP = NA_real_
  )]
  if (length(extra_parent_cols) > 0) {
    parents_rows <- cbind(parents_rows, parents[, ..extra_parent_cols])
  }
  parents_rows <- parents_rows[, output_cols, with = FALSE]
  tps_out <- data.table::rbindlist(list(tps_out, parents_rows), fill = TRUE)
  if (nrow(parents_rows) > 0) {
    parent_name_map <- unique(parents_rows[
      (!is.na(SMILES) & SMILES != "") | (!is.na(InChIKey) & InChIKey != ""),
      .(SMILES, InChIKey, name)
    ])
  }

  lookup_parent_name <- function(smiles, inchikey) {
    if (nrow(parent_name_map) == 0) return(NA_character_)
    if (!is.na(smiles) && nzchar(smiles)) {
      nm <- parent_name_map$name[match(smiles, parent_name_map$SMILES)]
      if (!is.na(nm) && nzchar(nm)) return(nm)
    }
    NA_character_
  }
  extra_na <- as.list(rep(NA, length(extra_parent_cols)))
  names(extra_na) <- extra_parent_cols

  for (i in seq_len(nrow(parents))) {
    parent <- parents[i, ]
    if (is.na(parent$SMILES) || !nzchar(parent$SMILES)) {
      warning("Skipping parent without SMILES: ", parent$name)
      next
    }

    name_val <- trimws(as.character(parent$name))
    smiles_val <- trimws(as.character(parent$SMILES))
    if (requireNamespace("rcdk", quietly = TRUE)) {
      ok_smiles <- tryCatch(!is.null(rcdk::parse.smiles(smiles_val)[[1]]), error = function(e) FALSE)
      if (!ok_smiles) {
        warning("Skipping parent with invalid SMILES: ", name_val)
        next
      }
    }

    payload <- list(
      biotransformer_option = biotransformer_option,
      number_of_steps = number_of_steps,
      query_input = paste0(name_val, "\t", smiles_val),
      task_type = "PREDICTION"
    )

    post_res <- post_json(base_url, payload)
    if (is.null(post_res$id)) {
      warning("No query id returned for parent: ", parent$name)
      next
    }

    status_url <- paste0("https://biotransformer.ca/queries/", post_res$id, "/status")
    message("Submitted BioTransformer query id ", post_res$id, ". Status page: ", status_url)

    if (i < nrow(parents)) {
      Sys.sleep(throttle_sec)
    }

    query_url <- paste0("http://biotransformer.ca/queries/", post_res$id, ".json")
    status <- NA_character_
    res <- NULL
    for (j in seq_len(max_poll)) {
      res <- get_json(query_url)
      status <- tolower(as.character(res$status))
      message("Query ", post_res$id, " status: ", status)
      if (status %in% c("done", "failed")) {
        if (debug) {
          raw_json <- attr(res, "raw_json")
          if (!is.null(raw_json)) {
            log_dir <- file.path(getwd(), "log")
            if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
            out_path <- file.path(log_dir, paste0("biotransformer_query_", post_res$id, ".json"))
            writeLines(raw_json, out_path, useBytes = TRUE)
            message("Saved BioTransformer response JSON: ", out_path)
          }
        }
        break
      }
      Sys.sleep(poll_delay)
    }

    if (is.null(res) || status != "done") {
      warning("Prediction not completed for parent: ", parent$name)
      next
    }

    if (is.null(res$predictions) || length(res$predictions) == 0) {
      next
    }

    prod_rows <- list()
    preds <- res$predictions
    for (pred in preds) {
      if (!is.null(pred$biotransformations) && length(pred$biotransformations) > 0) {
        for (bt in pred$biotransformations) {
          if (is.null(bt$products) || length(bt$products) == 0) next
          reaction_val <- safe_scalar(bt$reaction_type)
          biosystem_val <- safe_scalar(bt$biosystem)
          transform_label <- reaction_val
          if (!is.na(biosystem_val) && nzchar(biosystem_val)) {
            if (!is.na(reaction_val) && nzchar(reaction_val)) {
              transform_label <- paste0(biosystem_val, ": ", reaction_val)
            } else {
              transform_label <- biosystem_val
            }
          }
          transform_label <- gsub(" transformation", "", transform_label, fixed = TRUE)
          # Direct precursor from BioTransformer substrates
          precursor_row <- NULL
          if (!is.null(bt$substrates) && length(bt$substrates) > 0) {
            precursor_row <- bt$substrates[[1]]
          }
          precursor_smiles <- if (!is.null(precursor_row)) safe_scalar(precursor_row$smiles) else as.character(parent$SMILES)
          precursor_inchikey <- if (!is.null(precursor_row)) safe_scalar(precursor_row$inchikey) else as.character(parent$InChIKey)
          precursor_name <- if (!is.null(precursor_row)) {
            prec_title <- safe_scalar(precursor_row$title)
            if (!is.na(prec_title) && nzchar(prec_title)) prec_title else precursor_inchikey
          } else {
            as.character(parent$name)
          }
          precursor_props <- if (!is.na(precursor_smiles) && nzchar(precursor_smiles)) {
            calc_props_from_smiles(precursor_smiles)
          } else {
            list(formula = NA_character_, mass = NA_real_, inchi = NA_character_, inchikey = NA_character_, logp = NA_real_, smiles = NA_character_)
          }
          if (!is.na(precursor_props$smiles) && nzchar(precursor_props$smiles)) {
            precursor_smiles <- precursor_props$smiles
          }
          if (!is.na(precursor_props$inchi) && nzchar(precursor_props$inchi)) {
            precursor_inchikey <- if (!is.na(precursor_inchikey) && nzchar(precursor_inchikey)) precursor_inchikey else precursor_props$inchikey
          }
          matched_parent_name <- lookup_parent_name(precursor_smiles, precursor_inchikey)
          if (!is.na(matched_parent_name) && nzchar(matched_parent_name)) {
            precursor_name <- matched_parent_name
          }

          for (prod in bt$products) {
            smiles <- safe_scalar(prod$smiles)
            props <- calc_props_from_smiles(smiles)
            formula_val <- props$formula
            if (is.na(formula_val) && requireNamespace("rcdk", quietly = TRUE) && !is.na(smiles) && nzchar(smiles)) {
              formula_val <- tryCatch({
                mol2 <- rcdk::parse.smiles(smiles)[[1]]
                calc_formula_from_mol(mol2)
              }, error = function(e) NA_character_)
            }
            prod_title <- safe_scalar(prod$title)
            prod_inchikey <- safe_scalar(prod$inchikey)
            prod_name <- if (!is.na(prod_title) && nzchar(prod_title)) prod_title else if (!is.na(prod_inchikey) && nzchar(prod_inchikey)) prod_inchikey else smiles
            smiles_out <- if (!is.na(props$smiles) && nzchar(props$smiles)) props$smiles else smiles
            inchi_out <- if (!is.na(props$inchi) && nzchar(props$inchi)) props$inchi else NA_character_
            inchikey_out <- if (!is.na(prod_inchikey) && nzchar(prod_inchikey)) prod_inchikey else props$inchikey
            matched_parent_name <- lookup_parent_name(smiles_out, inchikey_out)
            if (!is.na(matched_parent_name) && nzchar(matched_parent_name)) {
              prod_name <- matched_parent_name
            }
            prod_rows[[length(prod_rows) + 1]] <- data.table::as.data.table(c(list(
              name = as.character(prod_name),
              formula = as.character(formula_val),
              mass = as.numeric(props$mass),
              rt = NA_real_,
              SMILES = as.character(smiles_out),
              InChI = as.character(inchi_out),
              InChIKey = as.character(inchikey_out),
              polarity = as.character(if ("polarity" %in% names(parent)) parent$polarity else NA_character_),
              fragments = NA_character_,
              xLogP = as.numeric(props$logp),
              transformation = as.character(transform_label),
              precursor_name = as.character(precursor_name),
              precursor_formula = as.character(precursor_props$formula),
              precursor_mass = as.numeric(precursor_props$mass),
              precursor_SMILES = as.character(precursor_smiles),
              precursor_InChI = as.character(precursor_props$inchi),
              precursor_InChIKey = as.character(if (!is.na(precursor_inchikey) && nzchar(precursor_inchikey)) precursor_inchikey else precursor_props$inchikey),
              precursor_xLogP = as.numeric(precursor_props$logp)
            ), extra_na))
          }
        }
      }
    }

    if (length(prod_rows) > 0) {
      prod_tbl <- data.table::rbindlist(prod_rows, fill = TRUE)
      prod_tbl <- prod_tbl[, output_cols, with = FALSE]
      tps_out <- data.table::rbindlist(list(tps_out, prod_tbl), fill = TRUE)
    }
  }

  tps_out <- harmonize_compound_columns(tps_out, "")
  tps_out <- harmonize_compound_columns(tps_out, "precursor_")
  tps_out <- harmonize_smiles_by_inchikey(tps_out, "SMILES", "InChIKey")
  tps_out <- harmonize_smiles_by_inchikey(tps_out, "precursor_SMILES", "precursor_InChIKey")
  tps_out <- harmonize_precursor_from_primary(tps_out)
  tps_out <- harmonize_names_by_smiles(tps_out, "name", "SMILES", "InChIKey", parent_name_map)
  tps_out <- harmonize_names_by_smiles(tps_out, "precursor_name", "precursor_SMILES", "precursor_InChIKey", parent_name_map)
  tps_out[, ..output_cols]
}
