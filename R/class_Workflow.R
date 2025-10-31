# MARK: Workflow
# Workflow -----
#' @title Generic (top level) Workflow class and methods
#' @description The `Workflow` class is an ordered list of [StreamFind::ProcessingStep] objects dedicated to a specific type of data.
#' 
#' @param processing_steps A list of [StreamFind::ProcessingStep] objects.
#' 
#' @export
#' 
Workflow <- function(processing_steps = list()) {
  if (!is.list(processing_steps)) {
    stop("The argument processing_steps must be a list!")
  }
  processing_steps <- unclass(processing_steps)
  attributes(processing_steps) <- NULL
  if (is.list(processing_steps) && length(processing_steps) > 0) {
    processing_steps <- lapply(processing_steps, function(z) {
      if (is.list(z) && !is(z, "ProcessingStep")) {
        tryCatch({
          z <- as.ProcessingStep(z)
        }, error = function(e) {
          warning("Error converting/validating a ProcessingStep object!")
          z <- NULL
        })
      }
      z
    })
  }
  type <- NA_character_
  if (length(processing_steps) > 0) {
    type <- vapply(processing_steps, function(z) z$type, NA_character_)[1]
    possible_methods <- .get_available_methods(type)
    workflow_methods <- NA_character_
    for (i in seq_along(processing_steps)) {
      if (!processing_steps[[i]]$method %in% possible_methods) {
        warning(
          "Method ", processing_steps[[i]]$method, " not available for the engine ", engine, "Engine!"
        )
        processing_steps[[i]] <- NULL
        next
      }
      ri <- processing_steps[[i]]$required
      if (!all(ri %in% workflow_methods)) {
        warning(
          "Required methods not present! Please include first: \n", paste(ri, collapse = "\n")
        )
        processing_steps[[i]] <- NULL
        next
      }
      workflow_methods <- c(workflow_methods, processing_steps[[i]]$method)
    }
    w_names <- vapply(processing_steps, function(z) paste0(z$method, "_", z$algorithm), NA_character_)
    w_idx <- seq_along(w_names)
    w_names <- paste0(w_idx, "_", w_names)
    names(processing_steps) <- w_names
  }
  processing_steps <- structure(processing_steps, class = "Workflow", type = type)
  if (is.null(validate_object(processing_steps))) {
    return(processing_steps)
  } else {
    stop("Workflow object is not valid!")
  }
}
 
#' @describeIn Workflow Validate a Workflow object, returning NULL if valid or an error if not.
#' @param x A Workflow object.
#' @export
#' 
validate_object.Workflow <- function(x) {
  checkmate::assert_list(x)
  if (length(x) > 0) {
    lapply(x, function(x) {
      checkmate::assert_true(is(x, "ProcessingStep"))
    })
    type <- unique(vapply(x, function(z) z$type, NA_character_))
    if (length(type) > 1) {
      stop("All ProcessingStep objects must be for the same type of data!")
    }
    methods <- get_methods(x)
    available_methods <- .get_available_processing_methods(type)
    if (!all(methods %in% available_methods)) {
      stop("All processing methods must be available for the defined type of data!")
    }
    permitted <- vapply(x, function(x) x$number_permitted, NA_real_)
    if (any(permitted == 1)) {
      unique_methods <- unique(methods[permitted == 1])
      if (length(unique_methods) != length(methods[permitted == 1])) {
        stop("All ProcessingStep objects with number_permitted == 1 must be unique!")
      }
    }
  }
  NULL
}

#' @describeIn Workflow Convert a Workflow object to a data frame.
#' @param x A Workflow object.
#' @export
#'
info.Workflow <- function(x) {
  if (length(x) > 0) {
    data.frame(
      index = seq_along(x),
      type = vapply(x, function(z) z$type, NA_character_),
      method = vapply(x, function(z) x$method, NA_character_),
      algorithm = vapply(x, function(z) x$algorithm, NA_character_),
      input_class = vapply(x, function(z) x$input_class, NA_character_),
      output_class = vapply(x, function(z) x$output_class, NA_character_),
      number_permitted = vapply(z, function(x) x$number_permitted, NA_real_),
      version = vapply(x, function(z) x$version, NA_character_),
      software = vapply(x, function(z) x$software, NA_character_),
      developer = vapply(x, function(z) x$developer, NA_character_),
      contact = vapply(x, function(z) x$contact, NA_character_),
      link = vapply(x, function(z) x$link, NA_character_),
      doi = vapply(x, function(z) x$doi, NA_character_)
    )
  } else {
    data.frame()
  }
}

#' @describeIn Workflow Get the names of the methods in a Workflow object.
#' @param x A `Workflow` object.
#' @export
#'
get_methods.Workflow <- function(x) {
  if (length(x) == 0) return(character())
  vapply(x, function(z) {
    paste0(z$type, "Method_", z$method, "_", z$algorithm)
  }, NA_character_)
}

#' @describeIn Workflow Subset a Workflow object.
#' @param x A Workflow object.
#' @param i An index or logical vector indicating which elements to keep.
#' @export
#' 
`[.Workflow` <- function(x, i) {
  NextMethod()
}

#' @describeIn Workflow Replace elements in a Workflow object, returning a modified `Workflow`
#' object.
#' @param x A `Workflow` object.
#' @param i An index or logical vector indicating which elements to replace.
#' @param value A list of [StreamFind::ProcessingStep] objects to replace the selected
#' elements with.
#' @export
#' 
`[<-.Workflow` <- function(x, i, value) {
  x <- NextMethod()
  x <- Workflow(x)
  x
}

#' @describeIn Workflow Extract elements from a Workflow object using `[[`.
#' @param x A `Workflow` object.
#' @param i An index indicating which element to extract.
#' @export
#' 
`[[.Workflow` <- function(x, i) {
  NextMethod()
}

#' @describeIn Workflow Replace elements in a Workflow object using `[[<-`.
#' @param x A `Workflow` object.
#' @param i An index indicating which element to replace or a character to add a new element.
#' @param value A [StreamFind::ProcessingStep] object to replace the selected element with.
#' @export
#' 
`[[<-.Workflow` <- function(x, i, value) {
  if (!is.null(validate_object(value))) {
    stop("Value must be a valid ProcessingStep object!")
  }
  x <- NextMethod()
  x <- Workflow(x)
  x
}

#' @describeIn Workflow Save a Workflow object to a file.
#' @param x A `Workflow` object.
#' @param file A file path to save the Workflow object to.
#' Supported formats are `.json` and `.rds`.
#' @export
#' 
save.Workflow <- function(x, file = "workflow.rds") {
  if (length(x) > 0) {
    format <- tools::file_ext(file)
    if (format %in% "json") {
      processing_steps <- lapply(x, function(s) {
        unclass(s)
      })
      names(processing_steps) <- names(x)
      processing_steps <- .convert_to_json(processing_steps)
      write(processing_steps, file)
    } else if (format %in% "rds") {
      saveRDS(x, file)
    } else {
      warning("Format not supported!")
    }
  } else {
    warning("No processing_steps to save!")
  }
}

#' @describeIn Workflow Read a Workflow object from a file.
#' @param x A `Workflow` object.
#' @param file A file path to read the Workflow object from.
#' Supported formats are `.json` and `.rds`.
#' @export
#' 
read.Workflow <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      return(Workflow(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "Workflow")) {
      return(res)
    } else {
      warning("File is not a Workflow object!")
    }
  }
  x
}

#' @describeIn Workflow Show the contents of a Workflow object.
#' @param x A Workflow object.
#' @param ... Additional arguments (not used).
#' @export
#' 
show.Workflow <- function(x, ...) {
  if (length(x) > 0) {
    names_processing_steps <- vapply(x, function(z) z$method, "")
    algorithms <- vapply(x, function(z) z$algorithm, "")
    cat(
      paste0(
        seq_len(length(names_processing_steps)),
        ": ",
        names_processing_steps,
        " (",
        algorithms,
        ")"
      ),
      sep = "\n"
    )
  } else {
    cat("empty")
  }
}
