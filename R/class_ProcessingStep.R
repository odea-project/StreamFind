#' @title Generic ProcessingStep class and methods
#' @description The `ProcessingStep` class is used to define a processing step within a [StreamFind::Workflow]. It contains information about the data type, method, algorithm, parameters, and other relevant metadata for the processing step. The `ProcessingStep` is the parent class of all processing methods in StreamFind.
#' @param type A character string representing the data type (e.g., "MassSpec", "Raman").
#' @param method A character string representing the method used (e.g., "BaselineCorrection").
#' @param required A character vector of required preceding methods.
#' @param algorithm A character string representing the algorithm used (e.g., "baseline_als").
#' @param input_class A character string representing the class of the input data.
#' @param output_class A character string representing the class of the output data.
#' @param parameters A list of parameters for the processing step.
#' @param number_permitted A numeric value indicating the number of permitted instances.
#' @param version A character string representing the version of the processing step.
#' @param software A character string representing the original software.
#' @param developer A character string representing the developer of the processing step.
#' @param contact A character string representing the contact information for the developer.
#' @param link A character string representing the link to the origin of the algorithm or link to
#' additional information.
#' @param doi A character string representing the DOI of the algorithm or additional information.
#' @return A `ProcessingStep` object. Fundamentally, it is a list with class `ProcessingStep` and a data specific method class (e.g., `RamanMethod_AverageSpectra_native`) with the following elements:
#' - `type`: The data type (e.g., "MassSpec", "Raman").
#' - `method`: The method used (e.g., "BaselineCorrection").
#' - `required`: A character vector of required preceding methods.
#' - `algorithm`: The algorithm used (e.g., "baseline_als").
#' - `input_class`: The class of the input data.
#' - `output_class`: The class of the output data.
#' - `parameters`: A list of parameters for the processing step.
#' - `number_permitted`: The number of permitted instances.
#' - `version`: The version of the processing step.
#' - `software`: The original software used.
#' - `developer`: The developer of the processing step.
#' - `contact`: The contact information for the developer.
#' - `link`: A link to the origin of the algorithm or additional information.
#' - `doi`: The DOI of the algorithm or additional information.
#'
#' @export
#'
ProcessingStep <- function(
  type = NA_character_,
  method = NA_character_,
  required = NA_character_,
  algorithm = NA_character_,
  input_class = NA_character_,
  output_class = NA_character_,
  parameters = list(),
  number_permitted = NA_real_,
  version = NA_character_,
  software = NA_character_,
  developer = NA_character_,
  contact = NA_character_,
  link = NA_character_,
  doi = NA_character_
) {
  if (!is.na(method)) {
    call <- c(paste0(type, "Method_", method, "_", algorithm), "ProcessingStep")
  } else {
    call <- "ProcessingStep"
  }
  x <- structure(
    list(
      type = type,
      method = method,
      required = required,
      algorithm = algorithm,
      input_class = input_class,
      output_class = output_class,
      parameters = parameters,
      number_permitted = number_permitted,
      version = version,
      software = software,
      developer = developer,
      contact = contact,
      link = link,
      doi = doi
    ),
    class = call
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid ProcessingStep object!")
  }
}

# MARK: ProcessingStep methods
#' @describeIn ProcessingStep Validate the `ProcessingStep` object, returns `NULL` if valid.
#' @param x A `ProcessingStep` object.
#' @export
#'
validate_object.ProcessingStep = function(x) {
  checkmate::assert_character(x$type, len = 1)
  checkmate::assert_character(x$method, len = 1)
  checkmate::assert_character(x$required)
  checkmate::assert_true(
    all(x$required %in% c(.get_available_methods(x$type), NA_character_))
  )
  checkmate::assert_character(x$algorithm, len = 1)
  checkmate::assert_character(x$input_class, len = 1)
  checkmate::assert_character(x$output_class, len = 1)
  checkmate::assert_list(x$parameters)
  checkmate::assert_numeric(x$number_permitted, len = 1)
  checkmate::assert_character(x$version, len = 1)
  checkmate::assert_character(x$software, len = 1)
  checkmate::assert_character(x$developer, len = 1)
  checkmate::assert_character(x$contact, len = 1)
  checkmate::assert_character(x$link, len = 1)
  checkmate::assert_character(x$doi, len = 1)
  if (is.list(x$parameters)) {
    lapply(names(x$parameters), function(z) {
      param <- x$parameters[[z]]
      invalid_param <- is.data.frame(param) ||
        is.character(param) ||
        is.numeric(param)
      invalid_param <- invalid_param ||
        is.integer(param) ||
        is.logical(param) ||
        is.null(param)
      invalid_param <- !invalid_param
      if (invalid_param) {
        stop(
          paste(
            "Invalid type for parameter ",
            z,
            " in ",
            self$method,
            "_",
            self$algorithm,
            "!",
            collapse = "",
            sep = ""
          )
        )
      }
    })
  }
  NULL
}

# MARK: as
#' @describeIn ProcessingStep Convert a list or JSON object to a `ProcessingStep` object.
#' @param value A list or JSON object containing the parameters for the processing step.
#' @export
#'
as.ProcessingStep <- function(value) {
  if (length(value) == 1 && is.list(value)) {
    value <- value[[1]]
  }
  if (is.list(value)) {
    must_have_elements <- c("type", "method", "algorithm", "parameters")
    if (!all(must_have_elements %in% names(value))) {
      return(NULL)
    }
    if (!"version" %in% names(value)) {
      value$version <- NA_character_
    }
    if (is.na(value$version)) {
      value$version <- as.character(packageVersion("StreamFind"))
    }
  }
  settings_constructor <- paste0(
    value$type,
    "Method_",
    value$method,
    "_",
    value$algorithm
  )
  available_settings <- .get_available_processing_methods(value$type)
  if (!settings_constructor %in% available_settings) {
    warning(paste0(settings_constructor, " not available!"))
    return(NULL)
  }
  return(do.call(settings_constructor, value$parameters))
}

# MARK: save
#' @describeIn ProcessingStep Save a `ProcessingStep` object to a file in JSON or RDS format.
#' @param x A `ProcessingStep` object.
#' @param file A character string specifying the file path to save the object.
#' The file extension should be `.json` or `.rds`.
#' @export
#'
save.ProcessingStep <- function(x, file = "settings.json") {
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

# MARK: read
#' @describeIn ProcessingStep Read a `ProcessingStep` object from a file in JSON or RDS format,
#' returning the updated object.
#' @param x A `ProcessingStep` object.
#' @param file A character string specifying the file path to read the object from.
#' The file extension should be `.json` or `.rds`.
#' @export
#'
read.ProcessingStep <- function(x, file) {
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

# MARK: show
#' @describeIn ProcessingStep Show the details of a `ProcessingStep` object.
#' @param x A `ProcessingStep` object.
#' @param ... Additional arguments (not used).
#' @export
#'
show.ProcessingStep <- function(x, ...) {
  cat("\n")
  cat("", class(x)[1], "\n")
  cat(
    " type         ",
    x$type,
    "\n",
    " method       ",
    x$method,
    "\n",
    " required     ",
    paste(x$required, collapse = "; "),
    "\n",
    " algorithm    ",
    x$algorithm,
    "\n",
    " input_class  ",
    x$input_class,
    "\n",
    " output_class ",
    x$output_class,
    "\n",
    " version      ",
    x$version,
    "\n",
    " software     ",
    x$software,
    "\n",
    " developer    ",
    x$developer,
    "\n",
    " contact      ",
    x$contact,
    "\n",
    " link         ",
    x$link,
    "\n",
    " doi          ",
    x$doi,
    "\n",
    sep = ""
  )
  if (isS4(x$parameters) || length(x$parameters) == 1) {
    if (is.list(x$parameters)) {
      if (isS4(x$parameters[[1]])) {
        cat("\n")
        print(x$parameters[[1]])
      } else {
        cat("\n")
        cat(" parameters: ", "\n")
        for (i in seq_len(length(x$parameters))) {
          if (is.data.frame(x$parameters[[i]])) {
            cat("  - ", names(x$parameters)[i], " (only head rows)", "\n")
            cat("\n")
            print(head(x$parameters[[i]]), quote = FALSE)
            cat("\n")
          } else if (is.list(x$parameters[[i]])) {
            cat("  - ", names(x$parameters)[i], ": ", "\n")
            for (i2 in seq_len(length(x$parameters[[i]]))) {
              cat(
                "      - ",
                names(x$parameters[[i]])[i2],
                x$parameters[[i]][[i2]],
                "\n"
              )
            }
          } else if ("function" %in% is(x$parameters[[i]])) {
            cat("  - ", names(x$parameters)[i])
            quote(x$parameters[[i]])
            cat("\n")
          } else {
            cat("  - ", names(x$parameters)[i], x$parameters[[i]], "\n")
          }
        }
      }
    } else {
      cat("\n")
      print(x$parameters)
    }
  } else {
    cat("\n")
    cat(" parameters: ")
    if (length(x$parameters) == 0) {
      cat("empty ", "\n")
    } else {
      cat("\n")
      for (i in seq_len(length(x$parameters))) {
        if (is.data.frame(x$parameters[[i]])) {
          cat("  - ", names(x$parameters)[i], " (only head rows)", "\n")
          cat("\n")
          print(head(x$parameters[[i]]), quote = FALSE)
          cat("\n")
        } else if (is.list(x$parameters[[i]])) {
          cat("  - ", names(x$parameters)[i], ": ", "\n")
          for (i2 in seq_len(length(x$parameters[[i]]))) {
            cat(
              "      - ",
              names(x$parameters[[i]])[i2],
              x$parameters[[i]][[i2]],
              "\n"
            )
          }
        } else if ("function" %in% is(x$parameters[[i]])) {
          cat("  - ", names(x$parameters)[i], ":\n")
          print(x$parameters[[i]])
          cat("\n")
        } else {
          cat("  - ", names(x$parameters)[i], x$parameters[[i]], "\n")
        }
      }
    }
  }
}
