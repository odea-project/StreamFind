#' @title get_cache_info
#'
#' @description Gets information about the saved cache categories.
#' 
#' @param x A character string specifying the cache file or path to use. Default is a relative 
#' "cache" directory of the working directory.
#' @param ... Not used.
#' 
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' @return A data.table with the categories saved in the cache.
#' 
#' @aliases get_cache_info
#'
#' @export
#' @noRd
S7::method(get_cache_info, S7::class_character) <- function(x, ...) {
  
  caching_mode <- getOption("StreamFind_cache_mode")
  
  if (is.null(caching_mode)) {
    warning(
      "No caching mode set in options!",
      " Use options('StreamFind_cache_mode' = 'rds') to set the caching mode to rds or ",
      "options('StreamFind_cache_mode' = 'sqlite') to set the caching mode to sqlite."
    )
    return(invisible(NULL))
  }
  
  checkmate::assert_choice(caching_mode, c("rds", "sqlite"))
  
  if (caching_mode == "rds") {
    .info_cache_rds(x)
  } else if (caching_mode == "sqlite") {
    .info_cache_sqlite(x)
  } else {
    stop("Unknown caching mode!")
  }
}

#' @export
#' @noRd
S7::method(get_cache_info, S7::class_missing) <- function(x, ...) {
  caching_mode <- getOption("StreamFind_cache_mode")
  
  if (is.null(caching_mode)) {
    warning(
      "No caching mode set in options!",
      " Use options('StreamFind_cache_mode' = 'rds') to set the caching mode to rds or ",
      "options('StreamFind_cache_mode' = 'sqlite') to set the caching mode to sqlite."
    )
    return(invisible(NULL))
  }
  
  checkmate::assert_choice(caching_mode, c("rds", "sqlite"))
  
  caching_path <- getOption("StreamFind_cache_path")
  
  if (is.null(caching_path)) {
    stop("No cache path set in options!")
  }
  
  if (caching_mode == "rds") {
    
    if (!dir.exists(caching_path)) {
      stop("Cache path does not exist!")
    }
    
    .info_cache_rds(caching_path)
    
  } else if (caching_mode == "sqlite") {
    
    if (!file.exists(caching_path)) {
      stop("Cache file does not exist!")
    }
    
    .info_cache_sqlite(caching_path)
    
  } else {
    stop("Unknown caching mode!")
  }
}
