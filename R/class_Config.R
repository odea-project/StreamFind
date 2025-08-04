# MARK: ConfigParameter
# ConfigParameter -----
#' @title Configuration Parameter
#' 
#' @description The `ConfigParameter` S3 class represents a configuration parameter in StreamFind.
#' The `ConfigParameter` is a list of at least two elements - `name` and `description`.
#' 
#' @param name Name of the parameter.
#' @param description Description of the parameter.
#' 
#' @export
#' 
ConfigParameter <- function(name = NA_character_, description = NA_character_) {
  x <- structure(
    list(
      name = name,
      description = description
    ),
    class = "ConfigParameter"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid ConfigParameter object")
  }
}

#' @describeIn ConfigParameter Validate a `ConfigParameter` object.
#' @param x An object to validate.
#' @return NULL if the object is valid, otherwise an error is thrown.
#' @export
#' 
validate_object.ConfigParameter <- function(x) {
  checkmate::assert_class(x, "ConfigParameter")
  checkmate::assert_list(x, min.len = 2, names = "named")
  checkmate::assert_names(names(x), must.include = c("name", "description"))
  checkmate::assert_character(x$name, len = 1)
  checkmate::assert_character(x$description, len = 1)
  NULL
}

# MARK: Config
# Config -----
#' @title Generic Configuration
#' 
#' @description The `Config` S3 class represents a configuration object and is essentially a list of
#' [StreamFind::ConfigParameter] class objects.
#' 
#' @param parameters A list of [StreamFind::ConfigParameter] objects.
#' 
#' @return A `Config` object as a list of [StreamFind::ConfigParameter] objects.
#' 
#' @export
#' 
Config <- function(parameters = list()) {
  if (is.list(parameters)) {
    if (length(parameters) > 0) {
      parameter_names <- vapply(parameters, function(x) {
        if ("ConfigParameter" %in% class(x)) {
          if (is.null(validate_object(x))) {
            x$name
          } else {
            NA_character_
          }
        } else {
          NA_character_
        }
      }, NA_character_)
      parameters <- parameters[!is.na(parameter_names)]
      parameter_names <- parameter_names[!is.na(parameter_names)]
      names(parameters) <- parameter_names
    }
  } else {
    stop("Parameters must be a list of ConfigParameter objects.")
  }
  parameters <- structure(
    parameters,
    class = "Config"
  )
  if (is.null(validate_object(parameters))) {
    return(parameters)
  } else {
    stop("Invalid Config object")
  }
}

#' @describeIn Config Validate a `Config` object.
#' @param x An object to validate.
#' @return NULL if the object is valid, otherwise an error is thrown.
#' @export
#' 
validate_object.Config <- function(x) {
  checkmate::assert_list(x)
  if (length(x) > 0) {
    for (i in seq_along(x)) {
      if (!is.null(validate_object(x[[i]]))) {
        stop(sprintf("Element %d is not a ConfigParameter object.", i))
      }
    }
  }
  NULL
}

# MARK: ENGINE CONFIGURATION
# ENGINE CONFIGURATION -----

# MARK: ConfigCache
## ConfigCache -----
#' @title Configuration Parameter for Caching
#' 
#' @description The `ConfigCache` class is a `ConfigParameter` for the caching behavior.
#' 
#' @param value Logical indicating whether to enable or disable caching.
#' @param mode Character indicating the caching mode (e.g., "rds" or "sqlite").
#' @param folder Character indicating the folder for caching (for "rds" mode).
#' @param file Character indicating the file for caching (for "sqlite" mode).
#' 
#' @export
#' 
ConfigCache <- function(value = TRUE, mode = "rds", folder = "cache", file = "cache.sqlite") {
  x <- structure(
    list(
      name = "Cache results",
      description = "Enable/disable caching of results between processing steps.",
      value = value,
      mode = mode,
      folder = folder,
      file = file
    ),
    class = c("ConfigCache", "ConfigParameter")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid ConfigCache object")
  }
}

#' @describeIn ConfigCache Validate a `ConfigCache` object.
#' @param x An object to validate.
#' @return NULL if the object is valid, otherwise an error is thrown.
#' @export
#' 
validate_object.ConfigCache <- function(x) {
  checkmate::assert_logical(x$value, max.len = 1)
  checkmate::assert_choice(x$mode, c("rds", "sqlite"))
  checkmate::assert_character(x$file)
  checkmate::assert_true(tools::file_ext(x$file) %in% "sqlite")
  NULL
}

# MARK: size.ConfigCache
#' @describeIn ConfigCache Get the size of the cache.
#' @param x A `ConfigCache` object.
#' @return A named numeric vector with the size of the cache in bytes, KB, MB, or GB.
#' @export
#' 
size.ConfigCache <- function(x) {
  if ("sqlite" %in% x$mode) {
    if (file.exists(x$file)) {
      size <- file.size(x$file)
    } else {
      message("Cache file does not exist!")
      return(NA_real_)
    }
  } else if ("rds" %in% x$mode) {
    if (dir.exists(x$folder)) {
      size <- sum(file.size(list.files(x$folder, full.names = TRUE)))
    } else {
      message("Cache folder does not exist!")
      return(NA_real_)
    }
  } else {
    message("Invalid cache mode!")
    return(NA_real_)
  }
  if (size > 1024^3) {
    size <- size / 1024^3
    unit <- "GB"
  } else if (size > 1024^2) {
    size <- size / 1024^2
    unit <- "MB"
  } else if (size > 1024) {
    size <- size / 1024
    unit <- "KB"
  } else {
    unit <- "bytes"
  }
  names(size) <- unit
  size
}

# MARK: info.ConfigCache
#' @describeIn ConfigCache Get information about the cache.
#' @param x A `ConfigCache` object.
#' @return A `data.table` with the names and number of rows in each table for SQLite cache, or a list of files for RDS cache.
#' @export
#' 
info.ConfigCache <- function(x) {
  if ("sqlite" %in% x$mode) {
    if (file.exists(x$file)) {
      db <- .openCacheDBScope(file = x$file)
      tables <- DBI::dbListTables(db)
      if (length(tables) == 0) {
        message("\U2139 Cache file is empty.")
      } else {
        tableRows <- sapply(tables, function(tab) {
          DBI::dbGetQuery(db, sprintf("SELECT Count(*) FROM %s", tab))
        })
        tableRows <- unlist(tableRows)
        return(data.table::data.table(name = tables, rows = tableRows))
      }
    } else {
      message("Cache file does not exist!")
    }
    data.table::data.table()
  } else if ("rds" %in% x$mode) {
    .info_cache_rds(x$folder)
  } else {
    message("Invalid cache mode!")
    data.table::data.table()
  }
}

#' @export
#' @noRd
load_cache.ConfigCache <- function(x, category = NULL, ...) {
  if (x$value) {
    if ("sqlite" %in% x$mode) {
      .load_cache_sqlite(category = category, ..., file = x$file)
    } else if ("rds" %in% x$mode) {
      .load_chache_rds(category = category, ..., folder = x$folder)
    } else {
      message("Invalid cache mode!")
      return(NULL)
    }
  } else {
    message("Cache is disabled!")
    return(NULL)
  }
}

#' @export
#' @noRd
save_cache.ConfigCache <- function(x, category = NULL, data = NULL, hash = NULL) {
  if (x$value) {
    if ("sqlite" %in% x$mode) {
      .save_cache_sqlite(category, data, hash, file = x$file)
    } else if ("rds" %in% x$mode) {
      .save_cache_rds(category, data, hash, folder = x$folder)
    } else {
      message("Invalid cache mode!")
      return(invisible(NULL))
    }
  } else {
    message("Cache is disabled!")
    return(invisible(NULL))
  }
}

#' @export
#' @noRd
clear_cache.ConfigCache <- function(x, what = NULL, ...) {
  if (x$value) {
    if ("sqlite" %in% x$mode) {
      clear_cache(what, file = x$file)
    } else if ("rds" %in% x$mode) {
      .clear_cache_rds(what, folder = x$folder)
    } else {
      message("Invalid cache mode!")
      return(invisible(NULL))
    }
  } else {
    message("Cache is disabled!")
    return(invisible(NULL))
  }
}

# MARK: EngineConfig
## EngineConfig -----
#' @title Engine Configuration
#' @description Class representing the engine configuration, inheriting from [StreamFind::Config].
#' @export
#' 
EngineConfig <- function() {
  x <- structure(
    list(
      "ConfigCache" = ConfigCache()
    ),
    class = c("EngineConfig", "Config")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid EngineConfig object")
  } 
}

# MARK: APP CONFIGURATION
# APP CONFIGURATION -----

# MARK: ConfigDurationNotifications
## ConfigDurationNotifications -----
#' @title Configuration Parameter for Duration of Notifications
#' 
#' @description Class representing a configuration for the duration of pop-up notifications in the
#' app, inhiberiting from [StreamFind::ConfigParameter].
#' 
#' @param value Duration in seconds for pop-up notifications.
#' 
#' @export
#' 
ConfigDurationNotifications <- function(value = 10) {
  x <- structure(
    list(
      name = "Duration of pop-up notifications",
      description = "Duration in seconds for pop-up notifications",
      value = as.numeric(value)
    ),
    class = c("ConfigDurationNotifications", "ConfigParameter")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid ConfigDurationNotifications object")
  }
}

#' @describeIn ConfigDurationNotifications Validate a `ConfigDurationNotifications` object.
#' @param x An object to validate.
#' @return NULL if the object is valid, otherwise an error is thrown.
#' @export
validate_object.ConfigDurationNotifications <- function(x) {
  checkmate::assert_character(x$name)
  checkmate::assert_character(x$description)
  checkmate::assert_numeric(x$value, max.len = 1)
  NULL
}

# MARK: ConfigExtraRoots
## ConfigExtraRoots -----
#' @title Configuration Parameter for Extra Root Directories
#' 
#' @description Class representing a configuration for extra root directories for file selection
#' in the app, inheriting from [StreamFind::ConfigParameter].
#' 
#' @param value Character string representing extra root directories for file selection.
#' 
#' @export
#' 
ConfigExtraRoots <- function(value = "") {
    x <- structure(
    list(
      name = "Extra Root Directories",
      description = "Extra root directories for file selection. Add spaces between directories.",
      value = as.character(value)
    ),
    class = c("ConfigExtraRoots", "ConfigParameter")
  )
    
}

#' @describeIn ConfigExtraRoots Validate a `ConfigExtraRoots` object.
#' @param x An object to validate.
#' @return NULL if the object is valid, otherwise an error is thrown.
#' @export
#' 
validate_object.ConfigExtraRoots <- function(x) {
  checkmate::assert_character(x$name)
  checkmate::assert_character(x$description)
  checkmate::assert_character(x$value)
  if (x$value != "") {
    checkmate::assert_true(all(dir.exists(x$value)))
  }
  NULL
}

# MARK: AppConfig
## AppConfig -----
#' @title App Configuration
#' @description Class representing the app configuration, inheriting from [StreamFind::Config].
#' @export
#' 
AppConfig <- function() {
  x <- structure(
    list(
      "ConfigDurationNotifications" = ConfigDurationNotifications(),
      "ConfigExtraRoots" = ConfigExtraRoots()
    ),
    class = c("AppConfig", "Config")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid AppConfig object")
  }
}
