#' @title load_cache.character
#' @description Loads cached data using the approach as in the patRoon package.
#' @param x A character vector with the names of the cache category to load.
#' @param file A character string specifying the cache file to use. Default is "cache.sqlite".
#' @references
#' \insertRef{patroon01}{StreamFind}
#' 
#' \insertRef{patroon02}{StreamFind}
#' 
#' 
#' @export
#' @noRd
load_cache.character <- function(x, ..., file = getOption("StreamFind_cache_path")) {
  checkmate::assert_character(x, len = 1)
  caching_mode <- getOption("StreamFind_cache_mode")
  if (is.null(caching_mode)) {
    warning(
      "No caching mode set in options!",
      " Use `set_cache_mode` to set the caching mode to rds or sqlite."
    )
    return(invisible(NULL))
  }
  checkmate::assert_choice(caching_mode, c("rds", "sqlite"))
  info <- get_cache_info(file)
  if (nrow(info) == 0) {
    warning("No cache categories found!")
  }
  if (caching_mode == "rds") {
    return(.load_chache_rds(category = x, ..., folder = file))
  } else if (caching_mode == "sqlite") {
    return(.load_cache_sqlite(category = x, ..., file = file))
  } else {
    stop("Unknown caching mode!")
  }
}
