#' @noRd
.get_available_engines <- function() {
  StreamFind_env <- as.environment("package:StreamFind")
  available_engines <- ls(envir = StreamFind_env, pattern = "Engine")
  available_engines <- available_engines[sapply(available_engines, function(x) "R6ClassGenerator" %in% is(get(x, envir = .GlobalEnv)))]
  available_engines <- available_engines[!available_engines %in% "CoreEngine"]
  available_engines
}

#' @noRd
.get_available_settings <- function(engine = NA_character_) {
  StreamFind_env <- as.environment("package:StreamFind")
  if (grepl("Engine$", engine)) engine <- gsub("Engine", "", engine)
  engine_settings_key <- paste0(engine, "Settings_")
  return(ls(envir = StreamFind_env, pattern = paste0("^", engine_settings_key)))
}

#' .trim_vector
#'
#' @description Asset function for fast trimming of a vector based on a list of ranges.
#'
#' @param v A vector to trim based on minimum and maximum value pairs.
#' @param a A vector with minimum values to evaluate `v`.
#' @param b A vector with maximum values to evaluate `v`.
#'
#' @return A logical vector with the same length as `v` with \code{TRUE} for regions between `a` and `b` value pairs.
#'
#' @noRd
#'
.trim_vector <- function(v, a, b) rowSums(as.matrix(mapply(function(a, b) v >= a & v <= b, a = a, b = b))) > 0

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

#' @title recursiveApplyDT
#'
#' @description Recursive apply function for data.tables from patRoon package to use within the CoreEngine.
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
.openCacheDBScope <- withr::local_(function(x, file) .openCacheDB(file), function(x) .closeCacheDB(x))

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

#' @title .save_cache_backend
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
#'
.save_cache_backend <- function(file, category, data, hash) {
  db <- .openCacheDBScope(file = file)

  RSQLite::sqliteSetBusyHandler(db, 300 * 1000) # UNDONE: make configurable?

  df <- data.frame(d = I(list(fst::compress_fst(serialize(data, NULL, xdr = FALSE)))))

  .dbWithWriteTransaction(db, {
    DBI::dbExecute(db, sprintf("CREATE TABLE IF NOT EXISTS %s (hash TEXT UNIQUE, data BLOB)", category))

    DBI::dbExecute(db, sprintf("INSERT OR IGNORE INTO %s VALUES ('%s', :d)", category, hash), params = df)
    DBI::dbExecute(db, sprintf("UPDATE %s SET data=(:d) WHERE changes()=0 AND hash='%s'", category, hash), params = df)

    if (DBI::dbGetQuery(db, sprintf("SELECT Count(*) FROM %s", category))[[1]] > 100000) {
      DBI::dbExecute(db, sprintf("DELETE FROM %s WHERE ROWID in (SELECT min(ROWID) FROM %s)", category, category))
    }
  })
}

#' @title .load_cache_backend
#'
#' @description From patRoon package to use within the CoreEngine.
#'
#' @noRd
#'
.load_cache_backend <- function(file, category, hashes) {
  db <- .openCacheDBScope(file = file)
  RSQLite::sqliteSetBusyHandler(db, 300 * 1000)
  ret <- NULL
  if (nrow(DBI::dbGetQuery(db, sprintf("SELECT 1 FROM sqlite_master WHERE type='table' AND name='%s'", category))) > 0) {
    if (length(hashes) == 1) {
      df <- DBI::dbGetQuery(db, sprintf("SELECT data FROM %s WHERE hash='%s'", category, hashes))
      if (nrow(df) > 0) ret <- lapply(df$data, function(x) unserialize(fst::decompress_fst(x)))
    } else {
      df <- DBI::dbGetQuery(db, sprintf("SELECT hash,data FROM %s WHERE hash IN (%s)", category, paste0(sprintf("'%s'", hashes), collapse = ",")))
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

#' @title .load_chache
#'
#' @description Cache interface adapted from patRoon package.
#'
#' @noRd
#'
.load_chache <- function(category = NULL, ..., file = "cache.sqlite") {
  list_out <- list()
  hash <- .make_hash(...)
  data <- .load_cache_backend(file, category, hash)
  list_out[["hash"]] <- hash
  list_out[["data"]] <- data
  list_out
}

#' @title .save_cache
#'
#' @description Cache interface adapted from patRoon package.
#'
#' @noRd
.save_cache <- function(category = NULL, data = NULL, hash = NULL, file = "cache.sqlite") {
  .save_cache_backend(file, category, data, hash)
}

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

#' Registers changes in the history private field.
#' @noRd
.register <- function(action = NA_character_, data = NA_character_, name = NA_character_, details = NA_character_) {
  date_time <- Sys.time()
  if (is.null(private$.history)) private$.history <- list()
  private$.history[[as.character.POSIXt(date_time)]] <- data.table(
    "time" = date_time,
    "action" = action,
    "data" = data,
    "name" = name,
    "details" = details
  )
  invisible(self)
}

#' Checks the analyses argument as a character/integer vector to match analyses names. Returns a valid character
#' vector with analysis names or `NULL` for non-matching. If `analyses` is `NULL`, returns all analysis names.
#' @noRd
.check_analyses_argument <- function(obj, value) {
  if (is.null(value)) {
    obj@names
  } else {
    analyses <- obj@names[value]
    if (!all(analyses %in% obj@names)) {
      warning("Defined analyses not found!")
      NULL
    } else {
      analyses
    }
  }
}
