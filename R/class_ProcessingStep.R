#' @title ProcessingStep Class
#' 
#' @description The ProcessingStep class is used to define a processing step within a
#' \code{\link{Workflow}}. It contains information about the data type, method, algorithm,
#' parameters, and other relevant metadata for the processing step. The ProcessingStep is the
#' parent class of all processing methods in StreamFind.
#' 
#' @param data_type A character string representing the data type (e.g., "MassSpec", "Raman").
#' @param method A character string representing the method used (e.g., "BaselineCorrection").
#' @param required A character vector of required preceding methods.
#' @param algorithm A character string representing the algorithm used (e.g., "baseline_als").
#' @param parameters A list of parameters for the processing step.
#' @param number_permitted A numeric value indicating the number of permitted instances.
#' @param version A character string representing the version of the processing step.
#' @param software A character string representing the original software.
#' @param developer A character string representing the developer of the processing step.
#' @param contact A character string representing the contact information for the developer.
#' @param link A character string representing the link to the origin of the algorithm or link to
#' additional information.
#' @param doi A character string representing the DOI of the algorithm or additional information.
#' 
#' @slot call (getter) A character string representing the call to the processing step.
#' 
#' @export
#' 
ProcessingStep <- S7::new_class(
  name = "ProcessingStep",
  package = "StreamFind",
  properties = list(
    data_type = S7::new_property(S7::class_character, default = NA_character_),
    method = S7::new_property(S7::class_character, default = NA_character_),
    required = S7::new_property(S7::class_character, default = NA_character_),
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
      paste0(self@data_type, "Method_", self@method, "_", self@algorithm)
    })
  ),
  validator = function(self) {
    checkmate::assert_character(self@data_type, len = 1)
    checkmate::assert_character(self@method, len = 1)
    checkmate::assert_character(self@required)
    checkmate::assert_true(
      all(self@required %in% c(.get_available_methods(self@data_type), NA_character_))
    )
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
        invalid_param <- is.data.frame(param) || is.character(param) || is.numeric(param)
        invalid_param <- invalid_param || is.integer(param) || is.logical(param) || is.null(param)
        invalid_param <- !invalid_param
        if (invalid_param) {
          stop(
            paste(
              "Invalid type for parameter ", x, " in ", self$method,
              "_", self$algorithm, "!", collapse = "", sep = ""
            )
          )
        }
      })
    }
    NULL
  }
)

#' @export
#' @noRd
as.ProcessingStep <- function(value) {
  if (length(value) == 1 && is.list(value)) value <- value[[1]]
  if (is.list(value)) {
    must_have_elements <- c("data_type", "method", "algorithm", "parameters")
    if (!all(must_have_elements %in% names(value))) {
      return(NULL)
    }
    if (!"version" %in% names(value)) value$version <- NA_character_
    if (is.na(value$version)) value$version <- as.character(packageVersion("StreamFind"))
  }
  settings_constructor <- paste0(value$data_type, "Method_", value$method, "_", value$algorithm)
  available_settings <- .get_available_processing_methods(value$data_type)
  if (!settings_constructor %in% available_settings) {
    warning(paste0(settings_constructor, " not available!"))
    return(NULL)
  }
  return(do.call(settings_constructor, value$parameters))
}

#' @export
#' @noRd
`$.StreamFind::ProcessingStep` <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
`$<-.StreamFind::ProcessingStep` <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}

#' @export
#' @noRd
S7::method(as.list, ProcessingStep) <- function(x, ...) {
  list(
    data_type = x@data_type,
    method = x@method,
    required = x@required,
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
S7::method(save, ProcessingStep) <- function(x, file = "settings.json") {
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
S7::method(read, ProcessingStep) <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      return(as.ProcessingStep(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::ProcessingStep")) {
      return(res)
    }
  }
  NULL
}

#' @export
#' @noRd
S7::method(show, ProcessingStep) <- function(x, ...) {
  cat("\n")
  cat("", class(x)[1], "\n")
  cat(
    " data_type    ", x@data_type, "\n",
    " method       ", x@method, "\n",
    " required     ", paste(x@required, collapse = "; "), "\n",
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
