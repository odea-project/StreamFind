# MARK: General utility functions
# General utility functions -----

#' @noRd
.get_available_engines <- function() {
  StreamFind_env <- as.environment("package:StreamFind")
  available_engines <- ls(envir = StreamFind_env, pattern = "Engine")
  available_engines <- available_engines[
    sapply(available_engines, function(x) {
      "R6ClassGenerator" %in% is(get(x, envir = .GlobalEnv))
    })
  ]
  available_engines <- available_engines[!available_engines %in% "CoreEngine"]
  available_engines
}

#' @noRd
.get_available_processing_methods <- function(engine = NA_character_) {
  StreamFind_env <- as.environment("package:StreamFind")
  if (grepl("Engine$", engine)) engine <- gsub("Engine", "", engine)
  engine_methods_key <- paste0(engine, "Method_")
  return(ls(envir = StreamFind_env, pattern = paste0("^", engine_methods_key)))
}

#' @noRd
.get_available_methods <- function(engine = NA_character_) {
  if (grepl("Engine$", engine)) engine <- gsub("Engine", "", engine)
  processing_methods <- .get_available_processing_methods(engine)
  processing_methods <- gsub(paste0(engine, "Method_"), "", processing_methods)
  processing_methods <- gsub("_.*", "", processing_methods)
  return(unique(processing_methods))
}

#' .trim_vector
#'
#' @description Asset function for fast trimming of a vector based on a list of ranges.
#'
#' @param v A vector to trim based on minimum and maximum value pairs.
#' @param a A vector with minimum values to evaluate `v`.
#' @param b A vector with maximum values to evaluate `v`.
#'
#' @return A logical vector with the same length as `v` with \code{TRUE} for regions between `a`
#' and `b` value pairs.
#'
#' @noRd
#'
.trim_vector <- function(v, a, b) {
  rowSums(as.matrix(mapply(function(a, b) v >= a & v <= b, a = a, b = b))) > 0
}

#' .trim_spectra_targets
#'
#' @param traces A data.frame with spectra.
#' @param targets A data.frame with targets (minimum and maximum).
#' @param preMZr A data.frame with precursor minimum and maximum values as
#' `targets` but expanded with the isolation window.
#'
#' @return The filtered `traces` data.frame.
#'
#' @noRd
#'
.trim_spectra_targets <- function(traces, targets, with_im) {
  tg_list <- lapply(seq_len(nrow(targets)), function(z, traces, targets, with_im) {
    tg <- traces

    cutRt <- .trim_vector(tg$rt, targets$rtmin[z], targets$rtmax[z])

    tg <- tg[cutRt, ]

    if (nrow(tg) == 0) {
      return(data.table())
    }

    if (with_im) {
      cutIM <- .trim_vector(tg$drift, targets$driftmin[z], targets$driftmax[z])
      tg <- tg[cutIM, ]
    }

    if (nrow(tg) == 0) {
      return(data.table())
    }

    if ("polarity" %in% colnames(targets)) tg <- tg[tg$polarity == targets$polarity[z], ]

    if (nrow(tg) == 0) {
      return(data.table())
    }

    if (nrow(tg) > 0) {
      if (targets$precursor[z]) {
        tg <- tg[tg$level == 2, ]

        if (nrow(tg) == 0) {
          return(data.table())
        }

        cutPreMZ <- .trim_vector(tg$pre_mz, targets$mzmin[z], targets$mzmax[z])
        tg <- tg[cutPreMZ, ]
      } else {
        cutMZ <- .trim_vector(tg$mz, targets$mzmin[z], targets$mzmax[z])
        tg <- tg[cutMZ, ]
      }
    }

    if (nrow(tg) == 0) {
      return(data.table())
    }

    if (nrow(tg) > 0) tg$id <- targets$id[z]

    tg
  },
  traces = traces,
  targets = targets,
  with_im = with_im
  )

  tg_df <- rbindlist(tg_list, fill = TRUE)

  tg_df
}

#' @title .check_analyses_argument
#' 
#' @description Checks the analyses argument as a character/integer vector to match analyses names.
#' Returns a valid character vector with analysis names or `NULL` for non-matching. If `analyses`
#' is `NULL`, returns all analysis names.
#' 
#' @noRd
.check_analyses_argument <- function(obj, value) {
  if (is.null(value)) {
    names(obj)
  } else {
    
    if (is.numeric(value)) {
      value <- as.integer(value)
      if (any(value < 1) || any(value > length(names(obj)))) {
        warning("Invalid analysis index!")
        return(NULL)
      }
      value <- names(obj)[value]
    }
    
    if (!all(value %in% names(obj))) {
      warning("Defined analyses not found!")
      NULL
    } else {
      value
    }
  }
}

#' @title .convert_to_json
#' 
#' @description Converts an object to JSON format.
#' 
#' @noRd
.convert_to_json <- function(x) {
  jsonlite::toJSON(
    x,
    dataframe = "columns",
    Date = "ISO8601",
    POSIXt = "string",
    factor = "string",
    complex = "string",
    null = "null",
    na = "null",
    digits = 6,
    pretty = TRUE,
    force = TRUE
  )
}

#' @noRd
.get_colors <- function(obj) {
  colors <- c(
    brewer.pal(8, "Greys")[6],
    brewer.pal(8, "Greens")[6],
    brewer.pal(8, "Blues")[6],
    brewer.pal(8, "Oranges")[6],
    brewer.pal(8, "Purples")[6],
    brewer.pal(8, "PuRd")[6],
    brewer.pal(8, "YlOrRd")[6],
    brewer.pal(8, "PuBuGn")[6],
    brewer.pal(8, "GnBu")[6],
    brewer.pal(8, "BuPu")[6],
    brewer.pal(8, "Dark2")
  )
  
  Ncol <- length(unique(obj))
  
  if (Ncol > 18) {
    colors <- colorRampPalette(colors)(Ncol)
  }
  
  if (length(unique(obj)) < length(obj)) {
    Vcol <- colors[seq_len(Ncol)]
    Ncol <- length(obj)
    char <- NULL
    df <- data.frame(n = seq_len(Ncol), char = obj)
    count <- table(df$char)
    count <- as.data.frame(count)
    Vcol <- rep(Vcol, times = count[, "Freq"])
    names(Vcol) <- obj
  } else {
    Vcol <- colors[seq_len(Ncol)]
    names(Vcol) <- obj
  }
  
  Vcol
}

#' @noRd
.make_colorBy_varkey <- function(data = NULL, colorBy = NULL, legendNames = NULL) {
  
  if (!"id" %in% colnames(data)) {
    if ("feature" %in% colnames(data)) {
      data$id <- data$feature
    } else if ("group" %in% colnames(data)) {
      data$id <- data$group
    } else {
      data$id <- ""
    }
  }
  
  if (!"analysis" %in% colnames(data)) data$analysis <- ""
  
  data$id <- factor(data$id)
  
  data$analysis <- factor(data$analysis)
  
  if ("level" %in% colnames(data)) {
    if (!is.character(data$level)) {
      data$level <- paste("MS", data$level, sep = "")
    }
    data$level <- factor(data$level)
  }
  
  if ("polarity" %in% colnames(data)) {
    if (!is.character(data$polarity)) {
      pol_key <- c("positive", "negative", "not defined")
      names(pol_key) <- c("1", "-1", "0")
      data$polarity <- as.character(data$polarity)
      data$polarity <- pol_key[data$polarity]
    }
  }
  
  if ("analyses" %in% colorBy) {
    varkey <- data$analysis
  } else if (
    ("targets+analyses" %in% colorBy || "analyses+targets" %in% colorBy) && 
    "analysis" %in% colnames(data)) {
    
    if ("name" %in% colnames(data) & isTRUE(legendNames)) {
      varkey <- paste0(data$name, " - ", data$analysis)
    } else {
      varkey <- paste0(data$id, " - ", data$analysis)
    }
    
  } else if ("replicates" %in% colorBy && "replicate" %in% colnames(data)) {
    varkey <- data$replicate
    
  } else if (
    ("targets+replicates" %in% colorBy || "replicates+targets" %in% colorBy) &&
    "replicate" %in% colnames(data)) {
    
    if ("name" %in% colnames(data) & isTRUE(legendNames)) {
      varkey <- paste0(data$name, " - ", data$replicate)
    } else {
      varkey <- paste0(data$id, " - ", data$replicate)
    }
    
  } else if ("polarities" %in% colorBy && "polarity" %in% colnames(data)) {
    varkey <- data$polarity
    
  } else if (
    ("targets+polarities" %in% colorBy || "polarities+targets" %in% colorBy) &&
    "polarity" %in% colnames(data)) {
    
    if ("name" %in% colnames(data) & isTRUE(legendNames)) {
      varkey <- paste0(data$name, " - ", data$polarity)
    } else {
      varkey <- paste0(data$id, " - ", data$polarity)
    }
    
  } else if (
    ("analyses+polarities" %in% colorBy || "polarities+analyses" %in% colorBy) &&
    "polarity" %in% colnames(data)) {
    
    varkey <- paste0(data$analysis, " - ", data$polarity)
    
  } else if (
    ("replicates+polarities" %in% colorBy || "polarities+replicates" %in% colorBy) &&
    "polarity" %in% colnames(data)) {
    
    varkey <- paste0(data$replicate, " - ", data$polarity)
    
  } else if ("levels" %in% colorBy && "level" %in% colnames(data)) {
    varkey <- data$level
    
  } else if (
    ("levels+polarities" %in% colorBy || "polarities+levels" %in% colorBy) &&
    "polarity" %in% colnames(data) && "level" %in% colnames(data)) {
    
    varkey <- paste0(data$level, " - ", data$polarity)
    
  } else if (is.character(legendNames) && length(legendNames) == length(unique(data$id))) {
    leg <- legendNames
    names(leg) <- unique(data$id)
    varkey <- leg[data$id]
    
  } else if ("name" %in% colnames(data) && isTRUE(legendNames)) {
    varkey <- data$name
    
  } else {
    varkey <- data$id
  }
  data$var <- varkey
  data
}


# MARK: sqlite Cache functions
# sqlite Cache functions -----

#' @title recursiveApplyDT
#'
#' @description Recursive apply function for data.tables from patRoon package to use within the
#' CoreEngine.
#'
#' @noRd
.recursiveApplyDT <- function(l, f, appl = lapply, ...) {
  rec <- function(x) {
    if (isS4(x)) {
      for (sn in slotNames(x)) slot(x, sn) <- rec(slot(x, sn))
    } else if (is.list(x)) {
      if (is.data.table(x)) {
        x <- f(x)
      } else {
        x <- "attributes<-"(appl(x, rec, ...), attributes(x))
      }
    }
    return(x)
  }
  rec(l)
}

#' @title prepareDTForComparison
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
.prepareDTForComparison <- function(dt) {
  setattr(dt, ".internal.selfref", NULL)
  setindex(dt, NULL)
}

#' @title .make_hash_for_cache
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
.make_hash <- function(...) {
  args <- list(...)
  args <- .recursiveApplyDT(args, function(dt) .prepareDTForComparison(copy(dt)), sapply, simplify = FALSE)
  return(digest::digest(args, algo = "xxhash64"))
}

#' @title .get_cache_file
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
.openCacheDB <- function(file) {
  DBI::dbConnect(RSQLite::SQLite(), file)
}

#' @title .get_cache_file
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
.closeCacheDB <- function(db) {
  DBI::dbDisconnect(db)
}

#' @title .get_cache_file
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
.openCacheDBScope <- withr::local_(
  function(x, file) .openCacheDB(file), function(x) .closeCacheDB(x)
)

#' @title .save_cache_data
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
.dbWithWriteTransaction <- function(conn, code) {
  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  rollback <- function(e) {
    call <- DBI::dbExecute(conn, "ROLLBACK")
    if (identical(call, FALSE)) {
      stop(paste(
        "Failed to rollback transaction.",
        "Tried to roll back because an error occurred:",
        conditionMessage(e)
      ), call. = FALSE)
    }
    if (inherits(e, "error")) stop(e)
  }

  tryCatch(
    {
      res <- force(code)
      DBI::dbExecute(conn, "COMMIT")
      res
    },
    db_abort = rollback,
    error = rollback,
    interrupt = rollback
  )
}

#' @title .save_cache_sqlite_backend
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
#'
.save_cache_sqlite_backend <- function(file, category, data, hash) {
  db <- .openCacheDBScope(file = file)

  RSQLite::sqliteSetBusyHandler(db, 300 * 1000) # UNDONE: make configurable?

  df <- data.frame(d = I(list(fst::compress_fst(serialize(data, NULL, xdr = FALSE)))))

  .dbWithWriteTransaction(db, {
    DBI::dbExecute(
      db,
      sprintf("CREATE TABLE IF NOT EXISTS %s (hash TEXT UNIQUE, data BLOB)", category)
    )

    DBI::dbExecute(
      db,
      sprintf("INSERT OR IGNORE INTO %s VALUES ('%s', :d)", category, hash),
      params = df
    )
    
    DBI::dbExecute(
      db,
      sprintf("UPDATE %s SET data=(:d) WHERE changes()=0 AND hash='%s'", category, hash),
      params = df
    )

    if (DBI::dbGetQuery(db, sprintf("SELECT Count(*) FROM %s", category))[[1]] > 100000) {
      DBI::dbExecute(
        db,
        sprintf("DELETE FROM %s WHERE ROWID in (SELECT min(ROWID) FROM %s)", category, category)
      )
    }
  })
}

#' @title .load_cache_sqlite_backend
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
#'
.load_cache_sqlite_backend <- function(file, category, hashes) {
  db <- .openCacheDBScope(file = file)
  RSQLite::sqliteSetBusyHandler(db, 300 * 1000)
  ret <- NULL
  size_db <- nrow(
    DBI::dbGetQuery(
      db,
      sprintf("SELECT 1 FROM sqlite_master WHERE type='table' AND name='%s'", category)
    )
  ) 
  if (size_db > 0) {
    if (length(hashes) == 1) {
      df <- DBI::dbGetQuery(db, sprintf("SELECT data FROM %s WHERE hash='%s'", category, hashes))
      if (nrow(df) > 0) ret <- lapply(df$data, function(x) unserialize(fst::decompress_fst(x)))
    } else {
      df <- DBI::dbGetQuery(
        db,
        sprintf(
          "SELECT hash,data FROM %s WHERE hash IN (%s)",
          category,
          paste0(sprintf("'%s'", hashes), collapse = ",")
        )
      )
      if (nrow(df) > 0) {
        ret <- lapply(df$data, function(x) unserialize(fst::decompress_fst(x)))
        if (length(ret) > 0) {
          names(ret) <- df$hash
          ret <- ret[match(hashes, names(ret), nomatch = 0)]
        }
      }
    }
    if (!is.null(ret) && length(ret) == 1) ret <- ret[[1]]
  }
  ret <- .recursiveApplyDT(ret, setalloccol, sapply, simplify = FALSE)
  ret
}

#' @title .load_cache_sqlite
#'
#' @description Cache interface adapted from patRoon package.
#'
#' @noRd
.load_cache_sqlite <- function(category = NULL, ..., file = "cache.sqlite") {
  list_out <- list()
  hash <- .make_hash(...)
  data <- .load_cache_sqlite_backend(file, category, hash)
  list_out[["hash"]] <- hash
  list_out[["data"]] <- data
  list_out
}

#' @title .save_cache_sqlite
#'
#' @description Cache interface adapted from patRoon package.
#'
#' @noRd
.save_cache_sqlite <- function(category = NULL, data = NULL, hash = NULL, file = "cache.sqlite") {
  .save_cache_sqlite_backend(file, category, data, hash)
}

#' @title .info_cache_sqlite
#' 
#' @description Cache interface adapted from patRoon package.
#' 
#' @noRd
.info_cache_sqlite <- function(file = "cache.sqlite") {
  if (!file.exists(file)) {
    message("\U2139 No cache file found!")
    return(data.table::data.table())
  } else {
    db <- .openCacheDBScope(file = file)
    tables <- DBI::dbListTables(db)
    tableRows <- unlist(
      sapply(tables, function(tab) DBI::dbGetQuery(db, sprintf("SELECT Count(*) FROM %s", tab)))
    )
    
    if (length(tables) == 0) {
      message("\U2139 Cache file is empty!")
      return(data.table::data.table())
    }
    
    data.table::data.table(
      category = tables,
      size = tableRows
    )
    # idx <- seq_len(length(tables))
    # formatted_strings <- sprintf("%d: %s (%d rows)\n", idx, tables, tableRows)
    # combined_string <- paste(formatted_strings, collapse = "")
    # combined_string <- paste(
    #   combined_string, "all (removes complete cache database)\n",
    #   sep = ""
    # )  
    # combined_string
  }
}

#' @title .clear_cache_sqlite
#' 
#' @description Cache interface adapted from patRoon package.
#' 
#' @noRd
.clear_cache_sqlite <- function(what = NULL, file = "cache.sqlite") {
  
  valid <- any(
    c(
      checkmate::test_character(what, null.ok = TRUE),
      checkmate::test_integer(what, null.ok = TRUE)
    )
  )
  
  if (!valid) {
    stop("Invalid input for 'what'. Please provide a character vector or an integer vector.")
  }
  
  if (!file.exists(file)) {
    message("\U2139 No cache file found, nothing to do.")
    
  } else if ("all" %in% what) {
    
    if (unlink(file) != 0) {
      gc()
      if (unlink(file) != 0) {
        warning("Could not clear cache file!")
      } else {
        message("\U2713 All caches cleared!")
      }
    }
  } else {
    db <- .openCacheDBScope(file = file)
    tables <- DBI::dbListTables(db)
    
    .get_info_string <- function(tables, db, mode = "message", el = NULL) {
      tableRows <- unlist(
        sapply(tables, function(tab) DBI::dbGetQuery(db, sprintf("SELECT Count(*) FROM %s", tab)))
      )
      idx <- seq_len(length(tables))
      formatted_strings <- sprintf("%d: %s (%d rows)\n", idx, tables, tableRows)
      combined_string <- paste(formatted_strings, collapse = "")
      
      if (mode %in% "message") {
        combined_string <- paste(
          "Please specify which cache you want to remove. Available are:\n",
          combined_string, "all (removes complete cache database)\n",
          sep = ""
        )
      } else {
        combined_string <- paste(
          "No cache found that matches ", el , ". Available are:\n",
          combined_string, "all (removes complete cache database)\n",
          sep = ""
        )
      }
      
      combined_string
    }
    
    if (length(tables) == 0) {
      message("\U2139 Cache file is empty, nothing to do.")
      
    } else if (is.null(what)) {
      message(.get_info_string(tables, db))
      
    } else {
      if (is.integer(what)) {
        if (any(what < 1) || any(what > length(tables))) {
          message(.get_info_string(tables, db))
          return(invisible(NULL))
        }
        what <- tables[what]
      }
      
      if (length(what) == 0) {
        message(.get_info_string(tables, db))
        return(invisible(NULL))
      }
      
      for (el in what) {
        matchedTables <- grep(el, tables, value = TRUE)
        if (length(matchedTables) > 0) {
          for (tab in matchedTables) DBI::dbExecute(db, sprintf("DROP TABLE IF EXISTS %s", tab))
          DBI::dbExecute(db, "VACUUM")
          message("\U2713 Removed caches: ", paste0(matchedTables, collapse = ", "))
        } else {
          warning(.get_info_string(tables, db, mode = "warning", el = el))
        }
      }
    }
  }
  invisible(NULL)
}

# MARK: rds Cache functions
# rds Cache functions -----

#' @title .save_cache_rds
#'
#' @description Cache interface for rds files in a given folder.
#'
#' @noRd
.save_cache_rds <- function(category = NULL, data = NULL, hash = NULL, folder = "cache") {
  if (!dir.exists(folder)) dir.create(folder)
  file <- file.path(folder, paste0(category, "_", hash, ".rds"))
  saveRDS(data, file)
}

#' @title .load_chache_rds
#'
#' @description Cache interface for rds files in a given folder.
#'
#' @noRd
.load_chache_rds <- function(category = NULL, ..., folder = "cache") {
  list_out <- list()
  hash <- .make_hash(...)
  file <- file.path(folder, paste0(category, "_", hash, ".rds"))
  if (file.exists(file)) {
    data <- readRDS(file)
  } else {
    data <- NULL
  }
  list_out[["hash"]] <- hash
  list_out[["data"]] <- data
  list_out
}

#' @title .info_cache_rds
#' 
#' @description Cache interface for rds files in a given folder.
#' 
#' @noRd
.info_cache_rds <- function(folder = "cache") {
  files <- list.files(folder, pattern = ".rds$", full.names = TRUE)
  if (length(files) == 0) {
    return(data.table::data.table())
  }
  data <- lapply(files, function(x) {
    file_name <- tools::file_path_sans_ext(basename(x))
    category <- gsub("_[a-z0-9]+$", "", file_name)
    hash <- gsub("^.*_", "", file_name)
    size <- file.size(x)
    data.table::data.table(
      category = tools::file_path_sans_ext(basename(x)),
      hash = digest::digest(data, algo = "xxhash64"),
      size = object.size(data)
    )
  })
  data.table::rbindlist(data, fill = TRUE)
}

#' @title .clear_cache_rds
#' 
#' @description Cache interface for rds files in a given folder.
#' 
#' @noRd
.clear_cache_rds <- function(what = NULL, folder = "cache") {
  if (!dir.exists(folder)) {
    warning("Folder does not exist!")
    return(invisible(NULL))
  }
  
  if ("all" %in% what) {
    files <- list.files(folder, pattern = ".rds$", full.names = TRUE)
    if (length(files) > 0) {
      file.remove(files)
    } else {
      warning("No files to remove! Run get_cache_info() to get an overview.")
    }
  } else if (is.character(what)) {
    all_files <- list.files(folder, pattern = ".rds$", full.names = TRUE)
    remove_files <- character()
    for (i in what) {
      remove_files <- c(remove_files, all_files[grepl(i, all_files)])
    }
    
    if (length(remove_files) > 0) {
      file.remove(remove_files)
    } else {
      warning("No files to remove! Run get_cache_info() to get an overview.")
    }
  } else if (is.numeric(what)) {
    files <- list.files(folder, pattern = ".rds$", full.names = TRUE)
    if (length(files) > 0) {
      file.remove(files[what])
    } else {
      warning("No files to remove! Run get_cache_info() to get an overview.")
    }
  } else {
    warning("Nothing selected to remove! Run get_cache_info() to get an overview.")
  }
  return(invisible(NULL))
}
