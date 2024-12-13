#' @title clear_cache
#'
#' @description Clears cached data using the approach as in the patRoon package.
#' 
#' @param what A character string specifying the cache to remove. If NULL, a list of available
#' caches is shown.
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
  checkmate::assertString(what, na.ok = FALSE, null.ok = TRUE)
  
  if (!file.exists(file)) {
    message("\U2139 No cache file found, nothing to do.")
    
  } else if (!is.null(what) && what == "all") {
    
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
    
    if (length(tables) == 0) {
      message("\U2139 Cache file is empty, nothing to do.")
      
    } else if (is.null(what) || !nzchar(what)) {
      tableRows <- unlist(sapply(tables, function(tab) DBI::dbGetQuery(db, sprintf("SELECT Count(*) FROM %s", tab))))
      formatted_strings <- sprintf("- %s (%d rows)\n", tables, tableRows)
      combined_string <- paste(formatted_strings, collapse = "")
      message("Please specify which cache you want to remove. Available are:\n",
        combined_string, "- all (removes complete cache database)\n",
        sep = ""
      )
      
    } else {
      matchedTables <- grep(what, tables, value = TRUE)
      
      if (length(matchedTables) == 0) {
        warning("No cache found that matches given pattern. Currently stored caches: ", paste0(tables, collapse = ", "))
        
      } else {
        for (tab in matchedTables) DBI::dbExecute(db, sprintf("DROP TABLE IF EXISTS %s", tab))
        DBI::dbExecute(db, "VACUUM")
        message("\U2713 Removed caches: ", paste0(matchedTables, collapse = ", "))
      }
    }
  }
  invisible(NULL)
}
