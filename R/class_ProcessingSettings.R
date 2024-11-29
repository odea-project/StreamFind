#' @export
#' @noRd
ProcessingSettings <- S7::new_class("ProcessingSettings",
  package = "StreamFind",
  properties = list(
    engine = S7::new_property(S7::class_character, default = NA_character_),
    method = S7::new_property(S7::class_character, default = NA_character_),
    algorithm = S7::new_property(S7::class_character, default = NA_character_),
    parameters = S7::new_property(S7::class_list, default = list()),
    number_permitted = S7::new_property(S7::class_numeric, default = NA_real_),
    version = S7::new_property(S7::class_character, default = NA_character_),
    software = S7::new_property(S7::class_character, default = NA_character_),
    developer = S7::new_property(S7::class_character, default = NA_character_),
    contact = S7::new_property(S7::class_character, default = NA_character_),
    link = S7::new_property(S7::class_character, default = NA_character_),
    doi = S7::new_property(S7::class_character, default = NA_character_),
    call = S7::new_property(S7::class_character, getter = function(self) {
      paste0(self@engine, "Settings_", self@method, "_", self@algorithm)
    })
  ),
  validator = function(self) {
    checkmate::assert_character(self@engine, len = 1)
    checkmate::assert_character(self@method, len = 1)
    checkmate::assert_character(self@algorithm, len = 1)
    checkmate::assert_list(self@parameters)
    checkmate::assert_numeric(self@number_permitted, len = 1)
    checkmate::assert_character(self@version, len = 1)
    checkmate::assert_character(self@software, len = 1)
    checkmate::assert_character(self@developer, len = 1)
    checkmate::assert_character(self@contact, len = 1)
    checkmate::assert_character(self@link, len = 1)
    checkmate::assert_character(self@doi, len = 1)
    if (is.list(self@parameters)) {
      lapply(names(self@parameters), function(x) {
        param <- self@parameters[[x]]
        if (!(is.data.frame(param) || is.character(param) || is.numeric(param) || is.integer(param) || is.logical(param) || is.null(param))) {
          stop(paste("Invalid type for parameter ", x, " in ", self$method, "_", self$algorithm, "!", collapse = "", sep = ""))
        }
      })
    }
    NULL
  }
)

#' @export
#' @noRd
as.ProcessingSettings <- function(value) {
  if (length(value) == 1 && is.list(value)) value <- value[[1]]
  if (is.list(value)) {
    must_have_elements <- c("engine", "method", "algorithm", "parameters")
    if (!all(must_have_elements %in% names(value))) {
      return(NULL)
    }
    if (!"version" %in% names(value)) value$version <- NA_character_
    if (is.na(value$version)) value$version <- as.character(packageVersion("StreamFind"))
  }
  settings_constructor <- paste0(value$engine, "Settings_", value$method, "_", value$algorithm)
  available_settings <- .get_available_settings(value$engine)
  if (!settings_constructor %in% available_settings) {
    warning(paste0(settings_constructor, " not available!"))
    return(NULL)
  }
  return(do.call(settings_constructor, value$parameters))
}

#' @export
#' @noRd
S7::method(`$`, ProcessingSettings) <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
S7::method(`$<-`, ProcessingSettings) <- function(x, i, value) {
  if (i %in% "parameters") S7::prop(x, i) <- value
  return(x)
}

#' @export
#' @noRd
S7::method(as.list, ProcessingSettings) <- function(x) {
  list(
    engine = x@engine,
    method = x@method,
    algorithm = x@algorithm,
    parameters = x@parameters,
    number_permitted = x@number_permitted,
    version = x@version,
    software = x@software,
    developer = x@developer,
    contact = x@contact,
    link = x@link,
    doi = x@doi
  )
}

#' @export
#' @noRd
S7::method(save, ProcessingSettings) <- function(x, file = "settings.json") {
  format <- tools::file_ext(file)
  if (format %in% "json") {
    x <- .convert_to_json(as.list(x))
    write(x, file)
  } else if (format %in% "rds") {
    saveRDS(x, file)
  } else {
    warning("Invalid format!")
  }
  invisible(NULL)
}

#' @export
#' @noRd
S7::method(read, ProcessingSettings) <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      return(as.ProcessingSettings(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::ProcessingSettings")) {
      return(res)
    }
  }
  NULL
}

#' @export
#' @noRd
S7::method(show, ProcessingSettings) <- function(x, ...) {
  cat("\n")
  cat("", class(x)[1], "\n")
  cat(
    " engine       ", x@engine, "\n",
    " method       ", x@method, "\n",
    " algorithm    ", x@algorithm, "\n",
    " version      ", x@version, "\n",
    " software     ", x@software, "\n",
    " developer    ", x@developer, "\n",
    " contact      ", x@contact, "\n",
    " link         ", x@link, "\n",
    " doi          ", x@doi, "\n",
    sep = ""
  )

  if (isS4(x@parameters) || length(x@parameters) == 1) {
    if (is.list(x@parameters)) {
      if (isS4(x@parameters[[1]])) {
        cat("\n")
        print(x@parameters[[1]])
      } else {
        cat("\n")
        cat(" parameters: ", "\n")
        for (i in seq_len(length(x@parameters))) {
          if (is.data.frame(x@parameters[[i]])) {
            cat("  - ", names(x@parameters)[i], " (only head rows)", "\n")
            cat("\n")
            print(head(x@parameters[[i]]), quote = FALSE)
            cat("\n")
          } else if (is.list(x@parameters[[i]])) {
            cat("  - ", names(x@parameters)[i], ": ", "\n")
            for (i2 in seq_len(length(x@parameters[[i]]))) {
              cat("      - ", names(x@parameters[[i]])[i2], x@parameters[[i]][[i2]], "\n")
            }
          } else if ("function" %in% is(x@parameters[[i]])) {
            cat("  - ", names(x@parameters)[i])
            quote(x@parameters[[i]])
            cat("\n")
          } else {
            cat("  - ", names(x@parameters)[i], x@parameters[[i]], "\n")
          }
        }
      }
    } else {
      cat("\n")
      print(x@parameters)
    }
  } else {
    cat("\n")
    cat(" parameters: ")

    if (length(x@parameters) == 0) {
      cat("empty ", "\n")
    } else {
      cat("\n")

      for (i in seq_len(length(x@parameters))) {
        if (is.data.frame(x@parameters[[i]])) {
          cat("  - ", names(x@parameters)[i], " (only head rows)", "\n")
          cat("\n")
          print(head(x@parameters[[i]]), quote = FALSE)
          cat("\n")
        } else if (is.list(x@parameters[[i]])) {
          cat("  - ", names(x@parameters)[i], ": ", "\n")
          for (i2 in seq_len(length(x@parameters[[i]]))) {
            cat("      - ", names(x@parameters[[i]])[i2], x@parameters[[i]][[i2]], "\n")
          }
        } else if ("function" %in% is(x@parameters[[i]])) {
          cat("  - ", names(x@parameters)[i], ":\n")
          print(x@parameters[[i]])
          cat("\n")
        } else {
          cat("  - ", names(x@parameters)[i], x@parameters[[i]], "\n")
        }
      }
    }
  }
}
