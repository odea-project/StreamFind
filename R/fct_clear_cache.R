#' @title clear_cache
#'
#' @description Clears cached data using the approach as in the patRoon package.
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
      printf("Please specify which cache you want to remove. Available are:\n%s", paste0(sprintf("- %s (%d rows)\n", tables, tableRows), collapse = ""))
      printf("- all (removes complete cache database)\n")
      
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
