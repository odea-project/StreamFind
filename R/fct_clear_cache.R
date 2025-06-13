#' @title clear_cache.character
#' 
#' @description Clears cached data using the approach as in the patRoon package.
#' 
#' @param x A character vector with the names of the cache categories to clear. An integer
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
#' 
#' @export
#' @noRd
S7::method(clear_cache, S7::class_character) <- function(x, file = getOption("StreamFind_cache_path"), ...) {
  
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
    .clear_cache_rds(x, file)
  } else if (caching_mode == "sqlite") {
    .clear_cache_sqlite(x, file)
  } else {
    stop("Unknown caching mode!")
  }
}

#' @export
#' @noRd
S7::method(clear_cache, S7::class_numeric) <- function(x, file = getOption("StreamFind_cache_path"), ...) {
  
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
    .clear_cache_rds(x, file)
  } else if (caching_mode == "sqlite") {
    .clear_cache_sqlite(x, file)
  } else {
    stop("Unknown caching mode!")
  }
}

#' @export
#' @noRd
S7::method(clear_cache, S7::class_missing) <- function(x, file = getOption("StreamFind_cache_path"), ...) {
  
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
    .clear_cache_rds(x, file)
  } else if (caching_mode == "sqlite") {
    .clear_cache_sqlite(x, file)
  } else {
    stop("Unknown caching mode!")
  }
}
