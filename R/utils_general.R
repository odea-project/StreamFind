
#' .dispatch_process_method
#'
#' @description Function to dispatch the processing method according to the defined processing settings object.
#'
#' @noRd
#'
.dispatch_process_method <- function(method, settings, self, private) {
  
  call_method <- paste0(".s3_", method)
  
  method_to_settings <- sub(".s3_ms_", "", call_method)
  
  method_to_settings <- sub(".s3_", "", method_to_settings)
  
  settings <- private$.get_call_settings(settings, method_to_settings)
  
  if (is.null(settings)) return(FALSE)
  
  processed <- do.call(call_method, list(settings, self, private))
  
  if (processed) {
    
    if (!private$.settings_already_stored(settings)) self$add_settings(settings)
    
    private$.register("processed", NA_character_, settings$call, settings$software, NA_character_, settings$algorithm)
  }
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
      
      if (nrow(tg) == 0) return(NULL)
      
      if (with_im) {
        cutIM <- .trim_vector(tg$drift, targets$driftmin[z], targets$driftmax[z])
        tg <- tg[cutIM, ]
      }
      
      if (nrow(tg) == 0) return(NULL)
      
      if ("polarity" %in% colnames(targets)) tg <- tg[tg$polarity == targets$polarity[z], ]
      
      if (nrow(tg) == 0) return(NULL)
      
      if (nrow(tg) > 0) {
        
        if (!targets$precursor[z]) {
          
          tg <- tg[tg$level == 2, ]
          
          if (nrow(tg) == 0) return(NULL)
          
          cutPreMZ <- .trim_vector(tg$pre_mz, targets$mzmin[z], targets$mzmax[z])
          tg <- tg[cutPreMZ, ]
          
          # cutMZ <- .trim_vector(tg$mz, targets$mzmin[z], targets$mzmax[z])
          # tg <- tg[tg$level == 2 | (tg$level == 1 & cutMZ), ]
          # 
          # if (nrow(tg) > 0) {
          #   cutPreMZ <- .trim_vector(tg$pre_mz, preMZr$mzmin[z], preMZr$mzmax[z])
          #   tg <- tg[tg$level == 1 | (tg$level == 2 & cutPreMZ), ]
          # }
          
        } else {
          cutMZ <- .trim_vector(tg$mz, targets$mzmin[z], targets$mzmax[z])
          tg <- tg[cutMZ, ]
        }
      }
      
      if (nrow(tg) == 0) return(NULL)
      
      if (nrow(tg) > 0) tg$id <- targets$id[z]
      
      tg
    },
    traces = traces,
    targets = targets
  )
  
  tg_list <- tg_list[!is.null(tg_list)]
  
  tg_df <- do.call("rbind", tg_list)
  
  tg_df
}

#' .caches_data
#'
#' @description Check if cache is possible and enabled via the global options.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
.caches_data <- function() {
  if (requireNamespace("patRoon", quietly = TRUE)) {
    ret <- getOption("patRoon.cache.mode", default = "both")
    if (ret %in% c("both", "save", "load")) {
      TRUE
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}

#' .correlate_analysis_spectra
#'
#' @description Function to correlate MS spectra from analyses.
#'
#' @param spectra A data.table with columns "analysis", "mz" and "intensity".
#' Optionally, a column named "id" or "group" can be given to split the
#' data.table before correlation analysis by setting the argument
#' \code{splitSpectra} to \code{TRUE}. Note that when both "id" and "group"
#' columns are present "group" is used for splitting the data.table not "id".
#' If a column "replicate" is present and the argument \code{byReplicates}
#' is set to \code{TRUE}, the correlation is performed by replicate analysis
#' groups.
#' @param splitSpectra X.
#' @param byReplicates X.
#' @param decimals X.
#' @param minIntensity X.
#' @param method X.
#'
#' @return X.
#'
#' @noRd
#'
.correlate_analysis_spectra <- function(spectra,
                                        splitSpectra = FALSE,
                                        byReplicates = FALSE,
                                        decimals = 2,
                                        minIntensity = 1000,
                                        method = "pearson") {
  
  analysis <- NULL
  intensity <- NULL
  
  if (!is.data.table(spectra)) {
    warning("Spectra must be a data.table!")
    return(data.table())
  }
  
  if ("replicate" %in% colnames(spectra) & byReplicates) {
    spectra$analysis <- spectra$replicate
  } else {
    byReplicates <- FALSE
  }
  
  if (!"id" %in% colnames(spectra)) spectra$id <- NA_character_
  
  if ("group" %in% colnames(spectra)) spectra$id <- spectra$group
  
  if (!all(c("id", "analysis", "mz", "intensity") %in% colnames(spectra))) {
    warning("Spectra data.table does not containg mandatory columns!")
    return(data.table())
  }
  
  if (splitSpectra) {
    cor_list <- split(spectra, spectra$id)
  } else {
    cor_list <- list(spectra)
  }
  
  cor_list <- lapply(cor_list, function(x, minIntensity, decimals, method) {
    temp <- copy(x[, c("analysis", "mz", "intensity")])
    
    temp <- temp[temp$intensity >= minIntensity, ]
    
    for (i in unique(temp$analysis)) {
      temp$intensity[temp$analysis %in% i] <-
        temp$intensity[temp$analysis %in% i] /
        max(temp$intensity[temp$analysis %in% i])
    }
    
    temp$mz <- round(temp$mz, digits = decimals)
    
    mz <- NULL
    analysis <- NULL
    
    temp <- temp[
      data.table::CJ(analysis = analysis, mz = mz, unique = TRUE),
      on = list(analysis, mz)
    ]
    
    data.table::setnafill(temp, fill = 0, cols = "intensity")
    
    temp <- temp[, `:=`(intensity = sum(intensity)),
                 by = c("analysis", "mz")
    ][]
    
    temp <- unique(temp)
    
    temp <- matrix(temp$intensity,
                   nrow = length(unique(temp$mz)),
                   ncol = length(unique(temp$analysis)),
                   dimnames = list(
                     unique(temp$mz),
                     unique(temp$analysis)
                   )
    )
    
    temp <- cor(temp, method = method)
    
    temp <- as.data.table(temp, keep.rownames = "analysis")
    
    return(temp)
  }, decimals = decimals, minIntensity = minIntensity, method = method)
  
  id_col <- "id"
  
  if ("group" %in% colnames(spectra)) id_col <- "group"
  
  cor_list <- rbindlist(cor_list, idcol = "id")
  
  if (byReplicates) {
    setnames(cor_list, "analysis", "replicate")
  }
  
  cor_list
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
  
  tryCatch({res <- force(code);  DBI::dbExecute(conn, "COMMIT"); res }, db_abort = rollback, error = rollback, interrupt = rollback)
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
.load_chache = function(category, ...) {
  file <- "cache.sqlite"
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
.save_cache = function(category, data, hash) {
  file <- "cache.sqlite"
  .save_cache_backend(file, category, data, hash)
}
