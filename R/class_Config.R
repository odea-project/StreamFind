# MARK: ConfigParameter
# ConfigParameter -----
#' @export
#' @noRd
ConfigParameter <- S7::new_class(
  name = "ConfigParameter",
  package = "StreamFind",
  properties = list(
    name = S7::new_property(S7::class_character),
    description = S7::new_property(S7::class_character)
  ),
  constructor = function(name = NA_character_, description = NA_character_) {
    S7::new_object(S7::S7_object(), name = name, description = description)
  },
  validator = function(self) {
    checkmate::assert_character(self@name)
    checkmate::assert_character(self@description)
    NULL
  }
)

#' @export
#' @noRd
S7::method(`$`, ConfigParameter) <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
S7::method(`$<-`, ConfigParameter) <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}

# MARK: Config
# Config -----
#' @export
#' @noRd
Config <- S7::new_class(
  name = "Config",
  package = "StreamFind",
  properties = list(
    parameters = S7::new_property(S7::class_list),
    config_frame = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
        if (length(self@parameters) == 0) return(data.frame())
        data.frame(
          name = sapply(self@parameters, function(x) x@name),
          value = sapply(self@parameters, function(x) as.character(x@value)),
          description = sapply(self@parameters, function(x) x@description)
        )
      }
    )
  ),
  constructor = function(parameters = list()) {
    S7::new_object(
      S7::S7_object(),
      parameters = parameters
    )
  },
  validator = function(self) {
    checkmate::assert_list(self@parameters)
    
    if (length(self@parameters) > 0) {
      checkmate::assert_true(
        all(vapply(self@parameters, function(x) is(x, "StreamFind::ConfigParameter"), FALSE))
      )
    }
    NULL
  }
)

#' @export
#' @noRd
S7::method(names, Config) <- function(x) {
  names(x@parameters)
}

#' @export
#' @noRd
S7::method(`$`, Config) <- function(x, i) {
  parameters_list <- x@parameters
  if (missing(i)) return(parameters_list)
  if (is.character(i)) {
    return(parameters_list[[i]])
  } else {
    stop("Invalid parameters getter type")
  }
}

#' @export
#' @noRd
S7::method(`$<-`, Config) <- function(x, i, value) {
  parameters_list <- x@parameters
  if (missing(i)) return(parameters_list)
  if (is.character(i)) {
    parameters_list[[i]] <- value
    x@parameters <- parameters_list
    return(x)
  } else {
    stop("Invalid parameters setter type")
  }
}

#' @export
#' @noRd
S7::method(`[`, Config) <- function(x, i) {
  parameters_list <- x@parameters
  if (missing(i)) return(parameters_list)
  if (is.numeric(i)) {
    return(parameters_list[i])
  } else if (is.character(i)) {
    return(parameters_list[i])
  } else if (is.logical(i)) {
    return(parameters_list[i])
  } else {
    stop("Invalid parameters subset type")
  }
}

#' @export
#' @noRd
S7::method(`[<-`, Config) <- function(x, i, value) {
  parameters_list <- x@parameters
  if (missing(i)) return(parameters_list)
  if (is.numeric(i)) {
    parameters_list[i] <- value
    x@parameters <- parameters_list
    return(x)
  } else if (is.character(i)) {
    parameters_list[i] <- value
    x@parameters <- parameters_list
    return(x)
  } else if (is.logical(i)) {
    parameters_list[i] <- value
    x@parameters <- parameters_list
    return(x)
  } else {
    stop("Invalid parameters setter type")
  }
}

#' @export
#' @noRd
S7::method(`[[`, Config) <- function(x, i) {
  parameters_list <- x@parameters
  if (missing(i)) return(parameters_list)
  if (is.numeric(i)) {
    return(parameters_list[[i]])
  } else if (is.character(i)) {
    return(parameters_list[[i]])
  } else {
    stop("Invalid parameters subset type")
  }
}

#' @export
#' @noRd
S7::method(`[[<-`, Config) <- function(x, i, value) {
  parameters_list <- x@parameters
  if (missing(i)) return(parameters_list)
  if (is.numeric(i)) {
    parameters_list[i] <- value
    x@parameters <- parameters_list
    return(x)
  } else if (is.character(i)) {
    parameters_list[[i]] <- value
    x@parameters <- parameters_list
    return(x)
  } else {
    stop("Invalid parameters setter type")
  }
}

# MARK: ENGINE CONFIGURATION
# ENGINE CONFIGURATION -----

# MARK: ConfigCache
## ConfigCache -----
#' @export
#' @noRd
ConfigCache <- S7::new_class(
  name = "ConfigCache",
  parent = ConfigParameter,
  package = "StreamFind",
  properties = list(
    value = S7::new_property(S7::class_logical),
    mode = S7::new_property(S7::class_character, default = "rds"),
    folder = S7::new_property(S7::class_character, default = "cache"),
    file = S7::new_property(S7::class_character, default = "cache.sqlite"),
    size = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        if ("sqlite" %in% self@mode) {
          if (file.exists(self@file)) {
            size <- file.size(self@file)
          } else {
            message("Cache file does not exist!")
            return(NA_real_)
          }
        } else if ("rds" %in% self@mode) {
          if (dir.exists(self@folder)) {
            size <- sum(file.size(list.files(self@folder, full.names = TRUE)))
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
    ),
    info = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
        if ("sqlite" %in% self@mode) {
          if (file.exists(self@file)) {
            db <- .openCacheDBScope(file = self@file)
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
          
        } else if ("rds" %in% self@mode) {
          .info_cache_rds(self@folder)
          
        } else {
          message("Invalid cache mode!")
          data.table::data.table()
        }
      }
    )
  ),
  constructor = function() {
    S7::new_object(
      S7::S7_object(),
      name = "Cache results",
      description = "Enable/disable caching of results between processing steps.",
      value = TRUE,
      mode = "rds",
      file = "cache.sqlite",
      folder = "cache"
    )
  },
  validator = function(self) {
    checkmate::assert_logical(self@value, max.len = 1)
    checkmate::assert_choice(self@mode, c("rds", "sqlite"))
    checkmate::assert_character(self@file)
    checkmate::assert_true(tools::file_ext(self@file) %in% "sqlite")
    NULL
  }
)

#' @export
#' @noRd
S7::method(load_cache, ConfigCache) <- function(x, category = NULL, ...) {
  if (x@value) {
    if ("sqlite" %in% x@mode) {
      .load_cache_sqlite(category = category, ..., file = x@file)
    } else if ("rds" %in% x@mode) {
      .load_chache_rds(category = category, ..., folder = x@folder)
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
S7::method(save_cache, ConfigCache) <- function(x, category = NULL, data = NULL, hash = NULL) {
  if (x@value) {
    if ("sqlite" %in% x@mode) {
      .save_cache_sqlite(category, data, hash, file = x@file)
    } else if ("rds" %in% x@mode) {
      .save_cache_rds(category, data, hash, folder = x@folder)
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
clear_cache.ConfigCache <- function(x, what = NULL) {
  if (x@value) {
    if ("sqlite" %in% x@mode) {
      clear_cache(what, file = x@file)
    } else if ("rds" %in% x@mode) {
      .clear_cache_rds(what, folder = x@folder)
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
#' @export
#' @noRd
EngineConfig <- S7::new_class(
  name = "EngineConfig",
  package = "StreamFind",
  parent = Config,
  constructor = function() {
    S7::new_object(
      Config(
        parameters = list(
          "ConfigCache" = ConfigCache()
        )
      )
    )
  },
  validator = function(self) {
    NULL
  }
)

# MARK: APP CONFIGURATION
# APP CONFIGURATION -----

# MARK: ConfigDurationNotifications
## ConfigDurationNotifications -----
#' @export
#' @noRd
ConfigDurationNotifications <- S7::new_class(
  name = "ConfigDurationNotifications",
  parent = ConfigParameter,
  package = "StreamFind",
  properties = list(value = S7::new_property(S7::class_numeric)),
  constructor = function(value = 10) {
    S7::new_object(
      S7::S7_object(),
      name = "Duration of pop-up notifications",
      description = "Duration in seconds for pop-up notifications",
      value = as.numeric(value)
    )
  },
  validator = function(self) {
    checkmate::assert_character(self@name)
    checkmate::assert_character(self@description)
    checkmate::assert_numeric(self@value, max.len = 1)
    NULL
  }
)

# MARK: ConfigExtraRoots
## ConfigExtraRoots -----
#' @export
#' @noRd
ConfigExtraRoots <- S7::new_class(
  name = "ConfigExtraRoots",
  parent = ConfigParameter,
  package = "StreamFind",
  properties = list(value = S7::new_property(S7::class_character)),
  constructor = function(value = "") {
    S7::new_object(
      S7::S7_object(),
      name = "Extra Root Directories",
      description = "Extra root directories for file selection. Add spaces between directories.",
      value = value
    )
  },
  validator = function(self) {
    checkmate::assert_character(self@name)
    checkmate::assert_character(self@description)
    checkmate::assert_character(self@value)
    if (self@value != "") {
      checkmate::assert_true(all(dir.exists(self@value)))
    }
    NULL
  }
)

# MARK: AppConfig
## AppConfig -----
#' @export
#' @noRd
AppConfig <- S7::new_class(
  name = "AppConfig",
  package = "StreamFind",
  parent = Config,
  constructor = function() {
    S7::new_object(
      Config(
        parameters = list(
          "ConfigDurationNotifications" = ConfigDurationNotifications(),
          "ConfigExtraRoots" = ConfigExtraRoots()
        )
      )
    )
  },
  validator = function(self) {
    NULL
  }
)
