#' @title save_cache.character
#' @description Saves cached data using the approach as in the patRoon package.
#' @param category A character vector with the names of the cache category to save.
#' @param data The data to be cached.
#' @param file A character string specifying the cache file to use. Default is "cache.sqlite".
#' @references
#' \insertRef{patroon01}{StreamFind}
#' 
#' \insertRef{patroon02}{StreamFind}
#' 
#' 
#' @export
#' @noRd
save_cache.character <- function(category = NULL, data = NULL, hash = NULL, file = getOption("StreamFind_cache_path")) {
  checkmate::assert_character(category, len = 1)
  caching_mode <- getOption("StreamFind_cache_mode")
  if (is.null(caching_mode)) {
    warning(
      "No caching mode set in options!",
      " Use `set_cache_mode` to set the caching mode to rds or sqlite."
    )
    return(invisible(NULL))
  }
  checkmate::assert_choice(caching_mode, c("rds", "sqlite")) 
  if (caching_mode == "rds") {
    .save_cache_rds(category, data, hash, folder = file)
  } else if (caching_mode == "sqlite") {
    .save_cache_sqlite(category, data, hash, file = file)
  } else {
    stop("Unknown caching mode!")
  }
  invisible(TRUE)
}
