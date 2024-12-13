#' @export
#' @noRd
Workflow <- S7::new_class(
  name = "Workflow",
  package = "StreamFind",
  properties = list(
    
    # MARK: settings
    # settings ----
    settings = S7::new_property(S7::class_list, default = list()),
    
    # MARK: methods
    # methods ----
    methods = S7::new_property(
      S7::class_character,
      getter = function(self) {
        if (length(self) == 0) return(character())
        vapply(self@settings, function(x) {
          paste0(x$engine, "Settings_", x$method, "_", x$algorithm)
        }, NA_character_)
      }
    ),
    
    # MARK: overview
    # overview ----
    overview = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
        if (length(self) > 0) {
          data.frame(
            index = seq_along(self@settings),
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
      }
    )
  ),
  
  # MARK: constructor
  constructor = function(settings = list()) {
    if (!(is.list(settings) || is(settings, "StreamFind::Workflow"))) {
      stop("settings must be a list!")
    }

    if (is.list(settings)) {
      settings <- lapply(settings, function(x) {
        if (is.list(x) && !is(x, "ProcessingSettings")) {
          tryCatch({
            x <- as.ProcessingSettings(x)
          }, error = function(e) {
            warning("Error converting to ProcessingSettings object!")
            x <- NULL
          })
        }
        x
      })
    } else {
      settings <- settings@settings
    }
    
    if (length(settings) > 0) {
      w_names <- vapply(settings, function(z) paste0(z$method, "_", z$algorithm), NA_character_)
      w_idx <- seq_along(w_names)
      w_names <- paste0(w_idx, "_", w_names)
      names(settings) <- w_names
      
      engine <- vapply(settings, function(z) z$engine, NA_character_)[1]
      possible_methods <- .get_available_methods(engine)
      workflow_methods <- NA_character_
      for (i in names(settings)) {
        
        if (!settings[[i]]$method %in% possible_methods) {
          warning(
            "Method ", settings[[i]]$method, " not available for the engine ", engine, "Engine!"
          )
          settings[[i]] <- NULL
          next
        }
        
        ri <- settings[[i]]$required
        if (!all(ri %in% workflow_methods)) {
          warning(
            "Required methods not present! Please include first: \n", paste(ri, collapse = "\n")
          )
          settings[[i]] <- NULL
          next
        }
        workflow_methods <- c(workflow_methods, settings[[i]]$method)
      }
    }
    S7::new_object(S7::S7_object(), settings = settings)
  },
  
  # MARK: validator
  validator = function(self) {
    checkmate::assert_list(self@settings)
    if (length(self) > 0) {
      for (i in seq_len(length(self))) {
        checkmate::assert_true(is(self@settings[[i]], "StreamFind::ProcessingSettings"))
      }
      
      engine <- unique(vapply(self@settings, function(x) x$engine, NA_character_))
      if (length(engine) > 1) {
        stop("All settings must have the same engine!")
      }
      
      methods <- self@methods
      available_methods <- .get_available_settings(engine)
      if (!all(methods %in% available_methods)) {
        stop("All methods must be available for the engine!")
      }
      
      permitted <- vapply(self@settings, function(x) x$number_permitted, NA_real_)
      if (any(permitted == 1)) {
        unique_methods <- unique(methods[permitted == 1])
        if (length(unique_methods) != length(methods[permitted == 1])) {
          stop("All settings with number_permitted == 1 must be unique!")
        }
      }
    }
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
  names(x@settings)
}

#' @export
#' @noRd
S7::method(`$`, Workflow) <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
S7::method(`[`, Workflow) <- function(x, i) {
  if (is.numeric(i) || is.logical(i) || is.character(i)) {
    x@settings[i]
  } else {
    stop("Index must be numeric, logical or character!")
  }
}

#' @export
#' @noRd
S7::method(`[<-`, Workflow) <- function(x, i, value) {
  if (is.numeric(i) || is.logical(i) || is.character(i)) {
    x@settings[i] <- value
    
    if (length(x$settings) > 0) {
      w_names <- vapply(x$settings, function(z) paste0(z$method, "_", z$algorithm), NA_character_)
      w_idx <- seq_along(w_names)
      w_names <- paste0(w_idx, "_", w_names)
      names(x@settings) <- w_names
      
      engine <- vapply(x@settings, function(z) z$engine, NA_character_)[1]
      possible_methods <- .get_available_methods(engine)
      workflow_methods <- NA_character_
      for (i in names(x@settings)) {
        
        if (!x@settings[[i]]$method %in% possible_methods) {
          warning(
            "Method ", x@settings[[i]]$method, " not available for the engine ", engine, "Engine!"
          )
          x@settings[[i]] <- NULL
          next
        }
        
        ri <- x@settings[[i]]$required
        if (!all(ri %in% workflow_methods)) {
          warning(
            "Required methods not present! Please include first: \n", paste(ri, collapse = "\n")
          )
          x@settings[[i]] <- NULL
          next
        }
        workflow_methods <- c(workflow_methods, x@settings[[i]]$method)
      }
    }
    x
  } else {
    stop("Index must be numeric, logical or character!")
  }
}

#' @export
#' @noRd
S7::method(`[[`, Workflow) <- function(x, i) {
  if (is.numeric(i) || is.logical(i) || is.character(i)) {
    x@settings[[i]]
  } else {
    stop("Index must be numeric, logical or character!")
  }
}

#' @export
#' @noRd
S7::method(`[[<-`, Workflow) <- function(x, i, value) {
  if (is.numeric(i) || is.logical(i) || is.character(i)) {
    x@settings[[i]] <- value

    if (length(x$settings) > 0) {
      w_names <- vapply(x$settings, function(z) paste0(z$method, "_", z$algorithm), NA_character_)
      w_idx <- seq_along(w_names)
      w_names <- paste0(w_idx, "_", w_names)
      names(x@settings) <- w_names
      
      engine <- vapply(x@settings, function(z) z$engine, NA_character_)[1]
      possible_methods <- .get_available_methods(engine)
      workflow_methods <- NA_character_
      for (i in names(x@settings)) {
        
        if (!x@settings[[i]]$method %in% possible_methods) {
          warning(
            "Method ", x@settings[[i]]$method, " not available for the engine ", engine, "Engine!"
          )
          x@settings[[i]] <- NULL
          next
        }
        
        ri <- x@settings[[i]]$required
        if (!all(ri %in% workflow_methods)) {
          warning(
            "Required methods not present! Please include first: \n", paste(ri, collapse = "\n")
          )
          x@settings[[i]] <- NULL
          next
        }
        workflow_methods <- c(workflow_methods, x@settings[[i]]$method)
      }
    }
    x
  } else {
    stop("Index must be numeric, logical or character!")
  }
}

#' @export
#' @noRd
S7::method(as.list, Workflow) <- function(x) {
  settings <- lapply(x@settings, function(s) as.list(s))
  names(settings) <- names(x@settings)
  settings
}

#' @export
#' @noRd
S7::method(save, Workflow) <- function(x, file = "workflow.rds") {
  if (length(x) > 0) {
    format <- tools::file_ext(file)
    if (format %in% "json") {
      settings <- lapply(x@settings, function(s) {
        list(
          engine = s@engine,
          method = s@method,
          required = s@required,
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
      names(settings) <- names(x)
      settings <- .convert_to_json(settings)
      write(settings, file)
    } else if (format %in% "rds") {
      saveRDS(x, file)
    } else {
      warning("Format not supported!")
    }
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
    if (is(res, "StreamFind::Workflow")) {
      return(res)
    } else {
      warning("File is not a Workflow object!")
    }
  }
  x
}

#' @export
#' @noRd
S7::method(show, Workflow) <- function(x, ...) {
  if (length(x) > 0) {
    names_settings <- vapply(x@settings, function(x) x$method, "")
    algorithms <- vapply(x@settings, function(x) x$algorithm, "")
    cat(
      paste0(seq_len(length(names_settings)), ": ", names_settings, " (", algorithms, ")"),
      sep = "\n"
    )
  } else {
    cat("empty")
  }
}
