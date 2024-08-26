
#' @export
#' @noRd
Workflow <- S7::new_class("Workflow", package = "StreamFind",
  
  properties = list(
    
    settings = S7::new_property(S7::class_list, default = list()),
    
    methods = S7::new_property(S7::class_character,
      getter = function(self) {
        vapply(self@settings, function(x) paste0(x$engine, "Settings_", x$method, "_", x$algorithm), NA_character_)
      },
      default = NA_character_
    ),
    
    names = S7::new_property(S7::class_character,
      getter = function(self) {
        vapply(self@settings, function(x) paste0(x$method, "_", x$algorithm), NA_character_)
      },
      default = NA_character_
    ),
    
    length = S7::new_property(S7::class_numeric, getter = function(self) length(self@settings), default = 0),
    
    overview = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (length(self) > 0) {
        data.frame(
          method = vapply(self@settings, function(x) x$method, NA_character_),
          algorithm = vapply(self@settings, function(x) x$algorithm, NA_character_),
          number_permitted = vapply(self@settings, function(x) x$number_permitted, NA_real_),
          version = vapply(self@settings, function(x) x$version, NA_character_),
          software = vapply(self@settings, function(x) x$software, NA_character_),
          developer = vapply(self@settings, function(x) x$developer, NA_character_),
          contact = vapply(self@settings, function(x) x$contact, NA_character_),
          link = vapply(self@settings, function(x) x$link, NA_character_),
          doi = vapply(self@settings, function(x) x$doi, NA_character_)
        )
      } else {
        data.frame()
      }
    })
  ),
  
  constructor = function(settings = list()) {
    
    if (!(is.list(settings) || is(settings, "StreamFind::Workflow"))) stop("settings must be a list!")
    
    if (is.list(settings)) {
      settings <- lapply(settings, function(x) {
        if (is.list(x) && !is(x, "ProcessingSettings")) {
          x <- as.ProcessingSettings(x)
        }
        x
      })
    } else {
      settings <- settings@settings
    }
    
    S7::new_object(S7::S7_object(), settings = settings)
  },
  
  validator = function(self) {
    valid <- TRUE
    
    if (!is.list(self@settings)) {
      warning("settings must be a list!")
      valid <- FALSE
    }
    
    if (length(self@settings) > 0) {
      if (!all(vapply(self@settings, function(x) is(x, "StreamFind::ProcessingSettings"), FALSE))) {
        warning("All settings must be a ProcessingSettings object!")
        valid <- FALSE
      }
      
      engine <- unique(vapply(self@settings, function(x) x$engine, NA_character_))
      
      if (length(unique(engine)) > 1) {
        warning("All settings must have the same engine!")
        valid <- FALSE
      }
      
      methods <- self@methods

      available_methods <- .get_available_settings(engine)
      if (!all(methods %in% available_methods)) {
        warning("All methods must be available for the engine!")
        valid <- FALSE
      }
      
      permitted <- vapply(self@settings, function(x) x$number_permitted, NA_real_)
      
      if (any(permitted == 1)) {
        unique_methods <- unique(methods[permitted == 1])
        if (length(unique_methods) != length(methods[permitted == 1])) {
          warning("All settings with number_permitted == 1 must be unique!")
          valid <- FALSE
        }
      }
    }
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(length, Workflow) <- function(x) {
  length(x@settings)
}

#' @export
#' @noRd
S7::method(names, Workflow) <- function(x) {
  x@names
}

#' @export
#' @noRd
S7::method(`$`, Workflow) <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
S7::method(`[`, Workflow) <- function(x, i) {
  if (is.numeric(i)) {
    if (any(i < 1) || any(i > length(x))) stop("Index out of bounds!")
    x@settings[i]
  } else {
    stop("Index must be numeric!")
  }
}

#' @export
#' @noRd
S7::method(`[<-`, Workflow) <- function(x, i, value) {
  if (is.numeric(i)) {
    x@settings[i] <- value
    x
  } else {
    stop("Index must be numeric!")
  }
}

#' @export
#' @noRd
S7::method(`[[`, Workflow) <- function(x, i) {
  if (is.numeric(i)) {
    x@settings[[i]]
  } else {
    stop("Index must be numeric!")
  }
}

#' @export
#' @noRd
S7::method(`[[<-`, Workflow) <- function(x, i, value) {
  if (is.numeric(i)) {
    x@settings[[i]] <- value
    x
  } else {
    stop("Index must be numeric!")
  }
}

#' @export
#' @noRd
S7::method(as.list, Workflow) <- function(x) {
  settings <- lapply(x@settings, function(s) as.list(s))
  names(settings) <- x@calls
  settings
}

#' @export
#' @noRd
S7::method(save, Workflow) <- function(x, format = "json", name = "workflow", path = getwd()) {
  if (x@length > 0) {
    if (format %in% "json") {
      settings <- lapply(x@settings, function(s) {
        list(
          engine = s@engine,
          method = s@method,
          algorithm = s@algorithm,
          parameters = s@parameters,
          number_permitted = s@number_permitted,
          version = s@version,
          software = s@software,
          developer = s@developer,
          contact = s@contact,
          link = s@link,
          doi = s@doi
        )
      })
      names(settings) <- x@names
      settings <- .convert_to_json(settings)
    }
    .save_data_to_file(settings, format, name, path)
  } else {
    warning("No settings to save!")
  }
}

#' @export
#' @noRd
S7::method(read, Workflow) <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      return(Workflow(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::Workflow")) return(res)
  }
  NULL
}

#' @export
#' @noRd
S7::method(show, Workflow) <- function(x, ...) {
  cat("\n")
  cat("Workflow")
  if (length(x) > 0) {
    cat("\n")
    names_settings <- vapply(x@settings, function(x) x$method, "")
    algorithms <- vapply(x@settings, function(x) x$algorithm, "")
    cat(
      paste0(" ", seq_len(length(names_settings)), ": ", names_settings, " (", algorithms, ")"),
      sep = "\n"
    )
  } else {
    cat(" empty \n")
  }
}
