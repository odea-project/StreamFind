#' @title clear_cache
#'
#' @description Clears cached data using the approach as in the patRoon package.
#' 
#' @param what A character vector with the names of the cache categories to clear. An integer
#' vector with the indices of the categories to clear can alternatively be given to remove
#' categories. If `NULL` (the default), the entire cache is cleared. Use the method
#' `get_cache_info` to get the cached categories.
#' @param file A character string specifying the cache file to use. Default is "cache.sqlite".
#' 
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' @export
#'
clear_cache <- function(what = NULL, file = "cache.sqlite") {
  
  if (is.numeric(what)) what <- as.integer(what)
  
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
      tableRows <- unlist(sapply(tables, function(tab) DBI::dbGetQuery(db, sprintf("SELECT Count(*) FROM %s", tab))))
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
